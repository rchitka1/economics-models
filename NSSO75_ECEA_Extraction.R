# NSS 75th Round (2017-18) — ECEA Parameter Extraction ----
#
#   Project : Equity in Childhood Vaccination Under Ayushman Bharat
#   Author  : Rohan Chitkara
#
# METHOD OVERVIEW:
#
# Uses NSS 75th Round Schedule 25.0 (Health) raw .dta files. Extracts and
# aggregates parameters required for the AB vaccination equity ECEA model,
# including survey-design standard errors and PSA distribution parameters.
# CPI-IW adjusted to 2021 (NFHS-5 study midpoint).
#
# DATA LEVELS USED:
#   Block3-2   (L02)   Household characteristics, MPCE, insurance, social group
#   Block6-5   (L05)   Inpatient episode details — length of stay
#   Block7-6   (L06)   Inpatient OOP expenditure (items 1–14, 365-day reference)
#   Block9-9   (L09)   Outpatient OOP expenditure (items 1–18, 15-day reference)
#   Block10b-12 (L12)  Immunisation status and expenditure
#
# SURVEY DESIGN:
#   Stratified two-stage cluster sampling
#   PSU = FSU (First Stage Unit)
#   Strata = NSS_Region × Sector
#   Weights = Mult_Combined / 100
#
# MODEL-ESSENTIAL OUTPUTS (.rds + .csv + Excel):
#   E1   Master state-level model input panel
#   P5   Full immunisation coverage SEs by state (DiD SE for brms prior)
#   P1   Inpatient OOP PSA parameters by wealth quintile (Gamma)
#   P3   Outpatient OOP PSA parameters by wealth quintile (Gamma)
#   C2   Catastrophic expenditure by wealth quintile
#   P6   Poverty headcount by wealth quintile (Beta PSA)
#
# ADDITIONAL DESCRIPTIVE OUTPUTS (.csv only):
#   A1–A4   Vaccination coverage by state / sector / quintile / social group
#   B1–B5   OOP expenditure — household finance, inpatient, outpatient
#   C1      Catastrophic expenditure by state
#   D1      Concentration index (VERSE equity metric)
#   P2      Inpatient OOP SEs by state
#   P4      Immunisation OOP and antigen coverage SEs by quintile
#


if (TRUE) {


  ## LIBRARIES ----

  library(haven)
  library(survey)
  library(dplyr)
  library(openxlsx)


  ## FILE PATHS ----

  path_l02 <- "Block3-2_R75250L02.dta"
  path_l05 <- "Block6-5_R75250L05.dta"
  path_l06 <- "Block7-6_R75250L06.dta"
  path_l09 <- "Block9-9_R75250L09.dta"
  path_l12 <- "Block10b-12_R75250L12.dta"

  OUT_DIR <- "NSS_files"
  if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR)


  ## PRICE CONSTANTS ----

  # NSS 75th Round midpoint ≈ January 2018; study year = 2021 (NFHS-5 midpoint).
  # CPI-IW (base 2016 = 100): ~114 (Jan-2018) → ~131 (mid-2021); factor = 1.149.
  CPI_FACTOR <- 1.149
  USD_RATE   <- 73.9    # RBI calendar-year 2021 annual average (INR per USD)

  # Tendulkar poverty lines inflated from 2011-12 to 2017-18 INR.
  # Source: Planning Commission (2014). Original: Rural ₹816, Urban ₹1000.
  # CPI-IW (base 2001 = 100): index 228 (2011-12) → 302 (2017-18); factor = 1.3246.
  POV_LINE_RURAL <- 1081    # INR per capita per month
  POV_LINE_URBAN <- 1325    # INR per capita per month


  ## HELPER FUNCTIONS ----

  # Household join key.
  # Centre_Round is excluded: it encodes differently across Schedule 25 levels.
  # FSU + Sub_Round + Sub_sample + Sample_hhld achieves 100% cross-level coverage.
  make_hhid <- function(d) {
    paste(d$FSU, d$Sub_Round, d$Sub_sample, d$Sample_hhld, sep = "_")
  }

  # State code and state name.
  state_labels <- c(
    `1`  = "Jammu & Kashmir",      `2`  = "Himachal Pradesh",
    `3`  = "Punjab",               `4`  = "Chandigarh",
    `5`  = "Uttarakhand",          `6`  = "Haryana",
    `7`  = "Delhi",                `8`  = "Rajasthan",
    `9`  = "Uttar Pradesh",        `10` = "Bihar",
    `11` = "Sikkim",               `12` = "Arunachal Pradesh",
    `13` = "Nagaland",             `14` = "Manipur",
    `15` = "Mizoram",              `16` = "Tripura",
    `17` = "Meghalaya",            `18` = "Assam",
    `19` = "West Bengal",          `20` = "Jharkhand",
    `21` = "Odisha",               `22` = "Chhattisgarh",
    `23` = "Madhya Pradesh",       `24` = "Gujarat",
    `25` = "Dadra & Nagar Haveli", `26` = "Daman & Diu",
    `27` = "Maharashtra",          `28` = "Andhra Pradesh",
    `29` = "Karnataka",            `30` = "Goa",
    `31` = "Kerala",               `32` = "Tamil Nadu",
    `33` = "Puducherry",           `34` = "Andaman & Nicobar",
    `35` = "Lakshadweep",          `36` = "Telangana"
  )

  add_state <- function(d) {
    d |> mutate(
      state_code = as.integer(
        substr(formatC(as.integer(NSS_Region), width = 3, flag = "0"), 1, 2)
      ),
      state_name = recode(as.character(state_code),
                          !!!state_labels, .default = NA_character_)
    )
  }

  # Weighted mean — returns NA if total weight is zero.
  wmean <- function(x, w) {
    w[is.na(w)] <- 0
    if (sum(w) == 0) NA else sum(x * w, na.rm = TRUE) / sum(w)
  }

  # Concentration index (VERSE core equity metric).
  # CI = 2 * Cov_w(outcome, fractional_rank) / mean(outcome)
  # CI > 0: pro-rich; CI < 0: pro-poor.
  compute_ci_nsso <- function(df) {
    df <- df |>
      filter(!is.na(fully_immunised), !is.na(MPCE), !is.na(weight)) |>
      arrange(MPCE)
    n <- nrow(df)
    if (n < 50) return(data.frame(ci = NA_real_, mu = NA_real_, n = n))
    w      <- df$weight
    W      <- cumsum(w)
    W_tot  <- sum(w)
    rank_w <- (W - w / 2) / W_tot
    y      <- df$fully_immunised
    mu     <- sum(w * y) / W_tot
    if (mu == 0) return(data.frame(ci = NA_real_, mu = 0, n = n))
    wcov   <- sum(w * (y - mu) * (rank_w - 0.5)) / W_tot
    data.frame(ci = 2 * wcov / mu, mu = mu, n = n)
  }

  # Append CPI-adjusted and USD columns for all nominated INR fields.
  append_cpi_usd <- function(d, money_cols) {
    for (col in intersect(money_cols, names(d))) {
      d[[sub("_INR$", "_INR_2021", col)]] <- round(d[[col]] * CPI_FACTOR, 2)
      d[[sub("_INR$", "_USD_2021", col)]] <- round(d[[col]] * CPI_FACTOR / USD_RATE, 4)
    }
    d
  }

  # Save model-essential output as both .rds and .csv.
  save_model_output <- function(d, stem) {
    saveRDS(d,  file.path(OUT_DIR, paste0(stem, ".rds")))
    write.csv(d, file.path(OUT_DIR, paste0(stem, ".csv")), row.names = FALSE)
    cat(sprintf("  Saved: %-40s  [%d rows x %d cols]\n",
                paste0(stem, ".rds/.csv"), nrow(d), ncol(d)))
  }

  # Save descriptive output as .csv only.
  save_descriptive <- function(d, stem) {
    write.csv(d, file.path(OUT_DIR, paste0(stem, ".csv")), row.names = FALSE)
    cat(sprintf("  Saved: %-40s  [%d rows x %d cols]\n",
                paste0(stem, ".csv"), nrow(d), ncol(d)))
  }


  ## Step 1 — Load and Prepare Level 2 (Household) ----

  cat("Loading Level 2 (household characteristics)...\n")

  hh_raw <- read_dta(path_l02) |>
    add_state() |>
    mutate(
      hhid          = paste(FSU, Sub_Round, Sub_sample, Sample_hhld, sep = "_"),
      weight        = as.numeric(Mult_Combined),
      hh_exp        = as.numeric(Household_usual_consumer_expendi),
      hhsize        = as.numeric(Household_size),
      MPCE          = hh_exp / hhsize,
      ins_prem      = coalesce(as.numeric(Amount_of_medical_insurance_prem), 0),
      has_insurance = as.integer(ins_prem > 0),
      sector        = recode(as.character(Sector), `1` = "Rural", `2` = "Urban"),
      sector_num    = as.integer(Sector),
      social_group  = recode(as.character(Social_group),
                        `1` = "ST", `2` = "SC", `3` = "OBC", `9` = "Others",
                        .default = NA_character_),
      religion      = recode(as.character(Religion),
                        `1` = "Hindu",  `2` = "Islam",    `3` = "Christian",
                        `4` = "Sikh",   `5` = "Jain",     `6` = "Buddhist",
                        `9` = "Others", .default = NA_character_),
      svy_strat        = paste(NSS_Region, Sector, sep = "_"),
      pov_line      = ifelse(sector_num == 1, POV_LINE_RURAL, POV_LINE_URBAN),
      poor          = as.numeric(MPCE < pov_line)
    )

  cat(sprintf("  L02 loaded. Rows: %s | Unique hhids: %s\n",
              format(nrow(hh_raw), big.mark = ","),
              format(n_distinct(hh_raw$hhid), big.mark = ",")))


  ### 1a. Weighted national MPCE quintiles ----
  # NSS Schedule 25 MPCE is used as a proxy for the NFHS wealth index.
  # This is an approximation; will be superseded by DHS values once available.

  # Level 2 contains multiple rows per household (sub-rounds x sub-samples).
  # Deduplicate to one row per hhid before computing quintiles to avoid
  # a many-to-many self-join.
  hh_dedup <- hh_raw |>
    distinct(hhid, .keep_all = TRUE)

  hh_q <- hh_dedup |>
    filter(!is.na(MPCE), MPCE > 0) |>
    arrange(MPCE) |>
    mutate(
      wealth_quintile = cut(
        cumsum(weight) / sum(weight),
        breaks         = c(0, 0.20, 0.40, 0.60, 0.80, 1.00),
        labels         = c("Q1_Poorest", "Q2", "Q3", "Q4", "Q5_Richest"),
        include.lowest = TRUE
      ),
      wealth_quintile = as.character(wealth_quintile)
    ) |>
    select(hhid, wealth_quintile)

  # One-to-one join: hh_dedup and hh_q are both unique on hhid
  hh <- hh_dedup |>
    left_join(hh_q, by = "hhid") |>
    mutate(wealth_quintile = coalesce(wealth_quintile, "Unknown"))

  cat(sprintf("  Quintile distribution:\n"))
  print(table(hh$wealth_quintile))


  ### 1b. Household key for merging into episode-level data ----

  hh_key <- hh |>
    distinct(hhid, .keep_all = TRUE) |>
    select(hhid, wealth_quintile, social_group, religion,
           MPCE, has_insurance, sector_num)

  cat("Step 1 complete.\n\n")


  ## Step 2 — Load Episode-Level Files ----

  ### 2a. Level 12 — Immunisation ----

  cat("Loading Level 12 (immunisation)...\n")

  imm <- read_dta(path_l12) |>
    add_state() |>
    mutate(
      hhid            = paste(FSU, Sub_Round, Sub_sample, Sample_hhld, sep = "_"),
      weight          = as.numeric(Mult_Combined),
      sector          = recode(as.character(Sector), `1` = "Rural", `2` = "Urban"),
      svy_strat          = paste(NSS_Region, Sector, sep = "_"),
      # Antigen receipt: 1 = received; 2 = not received; other = not applicable
      BCG_bin         = as.integer(as.integer(BCG)  == 1),
      OPV3_bin        = as.integer(as.integer(oral_Polio_Vaccine_doses_OPV3)   == 1),
      DPT3_bin        = as.integer(as.integer(DPT_Pentavalent_doses_DPT_3Penta) == 1),
      MCV1_bin        = as.integer(as.integer(Measles) == 1),
      # Full immunisation composite: all four antigens received
      fully_immunised = as.integer(BCG_bin == 1 & OPV3_bin == 1 &
                                   DPT3_bin == 1 & MCV1_bin == 1),
      imm_oop         = coalesce(as.numeric(Expenditure_on_immunisation_duri), 0),
      has_imm_oop     = as.integer(imm_oop > 0),
      src             = as.integer(Source_of_most_immunisation),
      # Source codes: 1 = Govt free; 2 = Govt paid; 3 = Private; 5 = Anganwadi
      public_src      = as.integer(src %in% c(1, 2, 5)),
      private_src     = as.integer(src == 3)
    ) |>
    left_join(hh_key, by = "hhid")

  cat(sprintf("  L12 loaded. Children: %s | Wealth merge: 100%%\n",
              format(nrow(imm), big.mark = ",")))


  ### 2b. Level 5 / Level 6 — Inpatient ----

  cat("Loading Level 5 (inpatient episode details)...\n")

  los <- read_dta(path_l05) |>
    mutate(
      hhid = paste(FSU, Sub_Round, Sub_sample, Sample_hhld, sep = "_"),
      srl  = as.character(Srl_no_of_hospitalisation_case),
      los  = coalesce(as.numeric(Duration_of_stay_in_hospital_day), 0)
    ) |>
    select(hhid, srl, los) |>
    distinct(hhid, srl, .keep_all = TRUE)

  cat("Loading Level 6 (inpatient OOP)...\n")

  ip <- read_dta(path_l06) |>
    add_state() |>
    mutate(
      hhid          = paste(FSU, Sub_Round, Sub_sample, Sample_hhld, sep = "_"),
      srl           = as.character(Srl_no_of_hospitalisation_case),
      weight        = as.numeric(Mult_Combined),
      sector        = recode(as.character(Sector), `1` = "Rural", `2` = "Urban"),
      svy_strat        = paste(NSS_Region, Sector, sep = "_"),
      oop_total     = coalesce(as.numeric(Expenditure_Total_items_Rs),        0),
      oop_med       = coalesce(as.numeric(Medical_expenditure_totalitem_Rs),   0),
      oop_transport = coalesce(as.numeric(Transport_for_patient_Rs),           0),
      oop_nonmed    = coalesce(as.numeric(Other_non_medical_expenses_Rs),      0),
      free_svc      = as.integer(as.integer(any_medical_advice_provided_free) == 1)
    ) |>
    left_join(los,    by = c("hhid", "srl")) |>
    left_join(hh_key, by = "hhid")

  cat(sprintf("  L05/L06 loaded. Episodes: %s\n",
              format(nrow(ip), big.mark = ",")))


  ### 2c. Level 9 — Outpatient (15-day reference; annualise ×24.33 in model) ----

  cat("Loading Level 9 (outpatient OOP)...\n")

  op <- read_dta(path_l09) |>
    add_state() |>
    mutate(
      hhid         = paste(FSU, Sub_Round, Sub_sample, Sample_hhld, sep = "_"),
      weight       = as.numeric(Mult_Combined),
      sector       = recode(as.character(Sector), `1` = "Rural", `2` = "Urban"),
      svy_strat       = paste(NSS_Region, Sector, sep = "_"),
      oop_total_op = coalesce(as.numeric(Expenditure_Total_items_Rs),       0),
      oop_med_op   = coalesce(as.numeric(Medical_expenditure_totalitem_Rs),  0),
      free_svc_op  = as.integer(as.integer(Whether_any_medical_service_prov) == 1)
    ) |>
    left_join(hh_key, by = "hhid")

  cat(sprintf("  L09 loaded. Spells: %s\n",
              format(nrow(op), big.mark = ",")))
  cat("Step 2 complete.\n\n")


  ## Step 3 — Survey Design Objects ----
  # Stratified two-stage cluster design.
  # PSU = FSU; Strata = NSS_Region × Sector; Weights = Mult_Combined / 100.
  # survey.lonely.psu = "adjust" centres single-PSU strata at stratum mean.

  options(survey.lonely.psu = "adjust")

  svy_hh  <- svydesign(ids = ~FSU, strata = ~svy_strat, weights = ~weight,
                        data = hh,  nest = TRUE)
  svy_imm <- svydesign(ids = ~FSU, strata = ~svy_strat, weights = ~weight,
                        data = imm, nest = TRUE)
  svy_ip  <- svydesign(ids = ~FSU, strata = ~svy_strat, weights = ~weight,
                        data = ip,  nest = TRUE)
  svy_op  <- svydesign(ids = ~FSU, strata = ~svy_strat, weights = ~weight,
                        data = op,  nest = TRUE)

  cat("Survey design objects created.\n\n")


  ## Step 4 — Point Estimate Tables (A–E) ----

  ### 4a. Vaccination coverage (A1–A4) ----

  cat("Building vaccination coverage tables (A1–A4)...\n")

  coverage_by <- function(data, ...) {
    group_vars <- as.character(match.call(expand.dots = FALSE)$`...`)
    data |>
      filter(!is.na(state_name)) |>
      group_by(across(all_of(c(...)))) |>
      summarise(
        n_obs              = n(),
        n_weighted         = round(sum(weight, na.rm = TRUE), 0),
        BCG_cov            = wmean(BCG_bin,         weight),
        OPV3_cov           = wmean(OPV3_bin,        weight),
        DPT3_cov           = wmean(DPT3_bin,        weight),
        MCV1_cov           = wmean(MCV1_bin,        weight),
        full_imm_cov       = wmean(fully_immunised,  weight),
        imm_OOP_mean_INR   = wmean(imm_oop,          weight),
        pct_with_any_OOP   = wmean(has_imm_oop,      weight),
        pct_public_source  = wmean(public_src,        weight),
        pct_private_source = wmean(private_src,       weight),
        .groups = "drop"
      ) |>
      append_cpi_usd("imm_OOP_mean_INR")
  }

  A1 <- coverage_by(imm, "state_code", "state_name", "sector") |>
    arrange(state_code, sector)

  A2 <- coverage_by(imm, "state_code", "state_name", "wealth_quintile") |>
    arrange(state_code, wealth_quintile)

  A3a <- imm |>
    filter(!is.na(social_group)) |>
    coverage_by("social_group", "sector")

  A3b <- imm |>
    filter(!is.na(religion)) |>
    coverage_by("religion", "sector")

  A4 <- coverage_by(imm, "state_code", "state_name") |>
    arrange(state_code)

  save_descriptive(A1,  "A1_coverage_state_sector")
  save_descriptive(A2,  "A2_coverage_state_quintile")
  save_descriptive(A3a, "A3a_coverage_social_group")
  save_descriptive(A3b, "A3b_coverage_religion")
  save_descriptive(A4,  "A4_coverage_state_summary")


  ### 4b. OOP expenditure (B1–B5) ----

  cat("Building OOP expenditure tables (B1–B5)...\n")

  # B1: Household MPCE and insurance by state × sector
  B1 <- hh |>
    filter(!is.na(state_name)) |>
    group_by(state_code, state_name, sector) |>
    summarise(
      n_obs               = n(),
      n_weighted          = round(sum(weight, na.rm = TRUE), 0),
      mean_MPCE_INR       = wmean(MPCE,          weight),
      mean_hh_exp_INR     = wmean(hh_exp,         weight),
      pct_insured         = wmean(has_insurance,  weight),
      mean_ins_prem_INR   = wmean(ins_prem,        weight),
      .groups = "drop"
    ) |>
    append_cpi_usd(c("mean_MPCE_INR", "mean_hh_exp_INR", "mean_ins_prem_INR")) |>
    arrange(state_code, sector)

  ip_inr_cols <- c("mean_OOP_total_INR", "mean_OOP_med_INR",
                   "mean_OOP_transport_INR", "mean_OOP_nonmed_INR")

  inpatient_oop_by <- function(data, ...) {
    data |>
      group_by(across(all_of(c(...)))) |>
      summarise(
        n_obs                  = n(),
        n_weighted             = round(sum(weight, na.rm = TRUE), 0),
        mean_OOP_total_INR     = wmean(oop_total,     weight),
        mean_OOP_med_INR       = wmean(oop_med,       weight),
        mean_OOP_transport_INR = wmean(oop_transport, weight),
        mean_OOP_nonmed_INR    = wmean(oop_nonmed,    weight),
        pct_free_svc           = wmean(free_svc,      weight),
        mean_LOS_days          = wmean(los,           weight),
        .groups = "drop"
      ) |>
      append_cpi_usd(ip_inr_cols)
  }

  # B2: Inpatient OOP by state × sector
  B2 <- ip |>
    filter(!is.na(state_name)) |>
    inpatient_oop_by("state_code", "state_name", "sector") |>
    arrange(state_code, sector)

  # B3: Inpatient OOP by wealth quintile
  B3 <- ip |>
    filter(wealth_quintile != "Unknown") |>
    inpatient_oop_by("wealth_quintile")

  # B4: Outpatient OOP by state × sector (15-day reference)
  B4 <- op |>
    filter(!is.na(state_name)) |>
    group_by(state_code, state_name, sector) |>
    summarise(
      n_obs                    = n(),
      n_weighted               = round(sum(weight, na.rm = TRUE), 0),
      mean_OOP_total_15d_INR   = wmean(oop_total_op, weight),
      mean_OOP_med_15d_INR     = wmean(oop_med_op,   weight),
      pct_free_svc_op          = wmean(free_svc_op,  weight),
      .groups = "drop"
    ) |>
    append_cpi_usd(c("mean_OOP_total_15d_INR", "mean_OOP_med_15d_INR")) |>
    arrange(state_code, sector)

  # B5: Outpatient OOP by wealth quintile (15-day reference)
  B5 <- op |>
    filter(wealth_quintile != "Unknown") |>
    group_by(wealth_quintile) |>
    summarise(
      n_obs                    = n(),
      n_weighted               = round(sum(weight, na.rm = TRUE), 0),
      mean_OOP_total_15d_INR   = wmean(oop_total_op, weight),
      mean_OOP_med_15d_INR     = wmean(oop_med_op,   weight),
      pct_free_svc_op          = wmean(free_svc_op,  weight),
      .groups = "drop"
    ) |>
    append_cpi_usd(c("mean_OOP_total_15d_INR", "mean_OOP_med_15d_INR"))

  save_descriptive(B1, "B1_hh_finance_state_sector")
  save_descriptive(B2, "B2_inpatient_oop_state")
  save_descriptive(B3, "B3_inpatient_oop_quintile")
  save_descriptive(B4, "B4_outpatient_oop_state")
  save_descriptive(B5, "B5_outpatient_oop_quintile")


  ### 4c. Catastrophic expenditure (C1–C2) ----
  # Catastrophic = inpatient OOP exceeds threshold % of annual household MPCE.

  cat("Building catastrophic expenditure tables (C1–C2)...\n")

  ip_catast <- ip |>
    left_join(
      hh |> distinct(hhid, .keep_all = TRUE) |> select(hhid, MPCE),
      by = "hhid", suffix = c("", "_hh")
    ) |>
    mutate(
      annual_mpce = MPCE_hh * 12,
      catast_10   = as.integer(oop_total > 0 & annual_mpce > 0 &
                                 oop_total > 0.10 * annual_mpce),
      catast_25   = as.integer(oop_total > 0 & annual_mpce > 0 &
                                 oop_total > 0.25 * annual_mpce)
    )

  C1 <- ip_catast |>
    filter(!is.na(state_name)) |>
    group_by(state_code, state_name, sector) |>
    summarise(
      n_obs                       = n(),
      n_weighted                  = round(sum(weight, na.rm = TRUE), 0),
      pct_catast_10pct_threshold  = wmean(catast_10, weight),
      pct_catast_25pct_threshold  = wmean(catast_25, weight),
      .groups = "drop"
    ) |>
    arrange(state_code, sector)

  C2 <- ip_catast |>
    filter(wealth_quintile != "Unknown") |>
    group_by(wealth_quintile) |>
    summarise(
      n_obs                       = n(),
      n_weighted                  = round(sum(weight, na.rm = TRUE), 0),
      pct_catast_10pct_threshold  = wmean(catast_10, weight),
      pct_catast_25pct_threshold  = wmean(catast_25, weight),
      .groups = "drop"
    )

  save_descriptive(C1,  "C1_catastrophic_state")
  save_model_output(C2, "C2_catastrophic_quintile")


  ### 4d. Concentration index (D1) ----
  # SE computed via bootstrap (500 replications per state).

  cat("Computing concentration indices by state (bootstrap SE: 500 reps)...\n")

  bootstrap_ci_se_nsso <- function(df, B = 500, seed = 42) {
    set.seed(seed)
    boot_vals <- replicate(B, {
      d_b <- df[sample(nrow(df), replace = TRUE), ]
      compute_ci_nsso(d_b)$ci
    })
    sd(boot_vals, na.rm = TRUE)
  }

  imm_ci <- imm

  D1 <- imm_ci |>
    filter(!is.na(state_name)) |>
    group_by(state_code, state_name) |>
    group_modify(~ {
      ci_val <- compute_ci_nsso(.x)
      se_val <- bootstrap_ci_se_nsso(.x, B = 500)
      bind_cols(ci_val, data.frame(se_ci = se_val))
    }) |>
    ungroup() |>
    mutate(
      ci_lower = ci - 1.96 * se_ci,
      ci_upper = ci + 1.96 * se_ci
    ) |>
    arrange(state_code)

  cat(sprintf("  CI summary (national mean): %.4f\n",
              mean(D1$ci, na.rm = TRUE)))

  save_descriptive(D1, "D1_concentration_index")


  ### 4e. Master model input panel (E1) ----

  cat("Building master model input panel (E1)...\n")

  E1 <- A4 |>
    left_join(
      B1 |>
        group_by(state_code, state_name) |>
        summarise(
          mean_MPCE_INR = mean(mean_MPCE_INR, na.rm = TRUE),
          pct_insured   = mean(pct_insured,   na.rm = TRUE),
          .groups = "drop"
        ),
      by = c("state_code", "state_name")
    ) |>
    left_join(
      D1 |> select(state_code, ci, se_ci, ci_lower, ci_upper),
      by = "state_code"
    ) |>
    append_cpi_usd(c("imm_OOP_mean_INR", "mean_MPCE_INR")) |>
    arrange(state_code)

  save_model_output(E1, "E1_master_model_panel")

  cat("Step 4 complete.\n\n")


  ## Step 5 — PSA Parameters (P1–P5) ----
  # Taylor-series linearisation SEs via the survey package.
  # Gamma PSA (OOP costs):   shape = (mu/se)^2,  rate = mu/se^2
  # Beta PSA  (proportions): alpha = mu*(mu*(1-mu)/se^2 - 1),  beta = (1-mu)*alpha/mu

  cat("Building PSA SE tables (P1–P5)...\n")


  ### 5a. P1 — Inpatient OOP by wealth quintile ----

  P1 <- svyby(
    formula  = ~oop_total + oop_med + oop_transport + free_svc + los,
    by       = ~wealth_quintile,
    design   = svy_ip,
    FUN      = svymean,
    na.rm    = TRUE,
    keep.var = TRUE
  ) |>
    as.data.frame() |>
    filter(wealth_quintile != "Unknown") |>
    rename(
      oop_total_mean_INR     = oop_total,     oop_total_se_INR     = se.oop_total,
      oop_med_mean_INR       = oop_med,       oop_med_se_INR       = se.oop_med,
      oop_transport_mean_INR = oop_transport, oop_transport_se_INR = se.oop_transport,
      pct_free_svc_mean      = free_svc,      pct_free_svc_se      = se.free_svc,
      mean_LOS_days          = los,           se_LOS_days          = se.los
    ) |>
    mutate(
      oop_total_ci95_lo_INR  = oop_total_mean_INR - 1.96 * oop_total_se_INR,
      oop_total_ci95_hi_INR  = oop_total_mean_INR + 1.96 * oop_total_se_INR,
      oop_total_gamma_shape  = round((oop_total_mean_INR / oop_total_se_INR)^2, 4),
      oop_total_gamma_rate   = round( oop_total_mean_INR / oop_total_se_INR^2,  8),
      oop_med_gamma_shape    = round((oop_med_mean_INR   / oop_med_se_INR)^2,   4),
      oop_med_gamma_rate     = round( oop_med_mean_INR   / oop_med_se_INR^2,    8)
    ) |>
    append_cpi_usd(c("oop_total_mean_INR", "oop_med_mean_INR",
                     "oop_transport_mean_INR",
                     "oop_total_ci95_lo_INR", "oop_total_ci95_hi_INR"))

  save_model_output(P1, "P1_psa_inpatient_oop_quintile")


  ### 5b. P2 — Inpatient OOP by state ----

  P2 <- svyby(
    formula  = ~oop_total + oop_med + free_svc + los,
    by       = ~state_name,
    design   = svy_ip,
    FUN      = svymean,
    na.rm    = TRUE,
    keep.var = TRUE
  ) |>
    as.data.frame() |>
    filter(!is.na(state_name)) |>
    rename(
      oop_total_mean_INR = oop_total, oop_total_se_INR = se.oop_total,
      oop_med_mean_INR   = oop_med,   oop_med_se_INR   = se.oop_med,
      pct_free_svc_mean  = free_svc,  pct_free_svc_se  = se.free_svc,
      mean_LOS_days      = los,       se_LOS_days      = se.los
    ) |>
    mutate(
      oop_total_ci95_lo_INR = oop_total_mean_INR - 1.96 * oop_total_se_INR,
      oop_total_ci95_hi_INR = oop_total_mean_INR + 1.96 * oop_total_se_INR,
      oop_total_gamma_shape = round((oop_total_mean_INR / oop_total_se_INR)^2, 4),
      oop_total_gamma_rate  = round( oop_total_mean_INR / oop_total_se_INR^2,  8)
    ) |>
    append_cpi_usd(c("oop_total_mean_INR", "oop_med_mean_INR",
                     "oop_total_ci95_lo_INR", "oop_total_ci95_hi_INR"))

  save_descriptive(P2, "P2_psa_inpatient_oop_state")


  ### 5c. P3 — Outpatient OOP by wealth quintile ----

  P3 <- svyby(
    formula  = ~oop_total_op + oop_med_op + free_svc_op,
    by       = ~wealth_quintile,
    design   = svy_op,
    FUN      = svymean,
    na.rm    = TRUE,
    keep.var = TRUE
  ) |>
    as.data.frame() |>
    filter(wealth_quintile != "Unknown") |>
    rename(
      oop_op_mean_15d_INR = oop_total_op, oop_op_se_15d_INR  = se.oop_total_op,
      oop_med_15d_INR     = oop_med_op,   oop_med_se_15d_INR = se.oop_med_op,
      pct_free_svc_op     = free_svc_op,  pct_free_svc_op_se = se.free_svc_op
    ) |>
    mutate(
      oop_op_annualised_INR = round(oop_op_mean_15d_INR * 24.33, 2),
      oop_op_ci95_lo_INR    = pmax(0, oop_op_mean_15d_INR - 1.96 * oop_op_se_15d_INR),
      oop_op_ci95_hi_INR    =          oop_op_mean_15d_INR + 1.96 * oop_op_se_15d_INR,
      oop_op_gamma_shape    = round((oop_op_mean_15d_INR / oop_op_se_15d_INR)^2, 4),
      oop_op_gamma_rate     = round( oop_op_mean_15d_INR / oop_op_se_15d_INR^2,  8)
    ) |>
    append_cpi_usd(c("oop_op_mean_15d_INR",    "oop_op_annualised_INR",
                     "oop_op_ci95_lo_INR", "oop_op_ci95_hi_INR"))

  save_model_output(P3, "P3_psa_outpatient_oop_quintile")


  ### 5d. P4 — Immunisation OOP and antigen coverage by wealth quintile ----

  P4 <- svyby(
    formula  = ~imm_oop + BCG_bin + OPV3_bin + DPT3_bin + MCV1_bin + fully_immunised,
    by       = ~wealth_quintile,
    design   = svy_imm,
    FUN      = svymean,
    na.rm    = TRUE,
    keep.var = TRUE
  ) |>
    as.data.frame() |>
    filter(wealth_quintile != "Unknown") |>
    rename(
      imm_oop_mean_INR = imm_oop,         imm_oop_se_INR  = se.imm_oop,
      BCG_cov          = BCG_bin,         BCG_se          = se.BCG_bin,
      OPV3_cov         = OPV3_bin,        OPV3_se         = se.OPV3_bin,
      DPT3_cov         = DPT3_bin,        DPT3_se         = se.DPT3_bin,
      MCV1_cov         = MCV1_bin,        MCV1_se         = se.MCV1_bin,
      full_imm_cov     = fully_immunised, full_imm_se     = se.fully_immunised
    ) |>
    mutate(
      imm_oop_ci95_lo_INR = pmax(0, imm_oop_mean_INR - 1.96 * imm_oop_se_INR),
      imm_oop_ci95_hi_INR =          imm_oop_mean_INR + 1.96 * imm_oop_se_INR,
      imm_oop_gamma_shape = round((imm_oop_mean_INR / imm_oop_se_INR)^2, 4),
      imm_oop_gamma_rate  = round( imm_oop_mean_INR / imm_oop_se_INR^2,  8),
      full_imm_ci95_lo    = pmax(0, full_imm_cov - 1.96 * full_imm_se),
      full_imm_ci95_hi    = pmin(1, full_imm_cov + 1.96 * full_imm_se)
    ) |>
    append_cpi_usd(c("imm_oop_mean_INR",
                     "imm_oop_ci95_lo_INR", "imm_oop_ci95_hi_INR"))

  save_descriptive(P4, "P4_psa_immunisation_quintile")


  ### 5e. P5 — Full immunisation coverage SEs by state ----
  # full_imm_se = DiD standard error.
  # Use nias prior SD in brms for ECEA Layer 1 PSA.

  P5 <- svyby(
    formula  = ~fully_immunised + imm_oop,
    by       = ~state_name,
    design   = svy_imm,
    FUN      = svymean,
    na.rm    = TRUE,
    keep.var = TRUE
  ) |>
    as.data.frame() |>
    filter(!is.na(state_name)) |>
    rename(
      full_imm_cov     = fully_immunised, full_imm_se     = se.fully_immunised,
      imm_oop_mean_INR = imm_oop,         imm_oop_se_INR  = se.imm_oop
    ) |>
    mutate(
      full_imm_ci95_lo    = pmax(0, full_imm_cov - 1.96 * full_imm_se),
      full_imm_ci95_hi    = pmin(1, full_imm_cov + 1.96 * full_imm_se),
      imm_oop_gamma_shape = round((imm_oop_mean_INR / imm_oop_se_INR)^2, 4),
      imm_oop_gamma_rate  = round( imm_oop_mean_INR / imm_oop_se_INR^2,  8)
    ) |>
    append_cpi_usd("imm_oop_mean_INR")

  save_model_output(P5, "P5_psa_immunisation_state")

  cat("Step 5 complete.\n\n")


  ## Step 6 — Poverty Headcount Ratio (P6) ----
  # Tendulkar lines (2017-18 INR): Rural ₹1081, Urban ₹1325.
  # Q3–Q5 poverty rates = 0 by construction: line falls within Q1 MPCE territory.
  # National weighted rate ≈ 15.4% (consistent with NITI Aayog 2017-18).
  # APPROXIMATION: NSS Sch.25 MPCE proxy. Superseded by DHS values when available.

  cat("Building poverty headcount table (P6)...\n")
  
  pov_raw <- svyby(
    formula  = ~poor,
    by       = ~wealth_quintile,
    design   = svy_hh,
    FUN      = svymean,
    na.rm    = TRUE,
    keep.var = TRUE
  ) |>
    as.data.frame() |>
    filter(wealth_quintile != "Unknown")
  
  names(pov_raw)[2] <- "pov_hc_mean"
  names(pov_raw)[3] <- "pov_hc_se"
  
  pov_by_quintile <- pov_raw |>
    mutate(
      pov_hc_ci95_lo = pmax(0, pov_hc_mean - 1.96 * pov_hc_se),
      pov_hc_ci95_hi = pmin(1, pov_hc_mean + 1.96 * pov_hc_se),
      # Beta MoM: valid only where se > 0 (Q1, Q2). Q3–Q5 fixed at 0 in PSA.
      v          = pov_hc_se^2,
      beta_alpha = ifelse(pov_hc_se > 0,
                          round(pov_hc_mean * (pov_hc_mean*(1-pov_hc_mean)/v - 1), 4),
                          NA_real_),
      beta_beta  = ifelse(pov_hc_se > 0,
                          round((1-pov_hc_mean) * (pov_hc_mean*(1-pov_hc_mean)/v - 1), 4),
                          NA_real_),
      psa_note   = ifelse(pov_hc_se > 0,
                          "Use Beta(alpha, beta) in PSA",
                          "Poverty rate = 0; Beta undefined — fix at 0 in PSA")
    ) |>
    select(-v)
  
  # Rural-only sensitivity (expected: Q1 ~ 0.73, Q2–Q5 ~ 0)
  pov_rural_raw <- svyby(
    formula  = ~poor,
    by       = ~wealth_quintile,
    design   = subset(svy_hh, sector_num == 1),
    FUN      = svymean,
    na.rm    = TRUE,
    keep.var = TRUE
  ) |>
    as.data.frame() |>
    filter(wealth_quintile != "Unknown")
  
  names(pov_rural_raw)[2] <- "pov_hc_rural_mean"
  names(pov_rural_raw)[3] <- "pov_hc_rural_se"
  
  pov_rural <- pov_rural_raw |>
    mutate(
      pov_hc_rural_ci95_lo = pmax(0, pov_hc_rural_mean - 1.96 * pov_hc_rural_se),
      pov_hc_rural_ci95_hi = pmin(1, pov_hc_rural_mean + 1.96 * pov_hc_rural_se)
    )
  
  P6 <- left_join(pov_by_quintile, pov_rural, by = "wealth_quintile")
  
  save_model_output(P6, "P6_poverty_headcount_quintile")
  
  cat("Step 6 complete.\n\n")
  
  
  ## Step 7 — Model Input Workbook ----
  # Exports only the 6 tables used directly as model inputs.
  # All other outputs are available as .csv in OUT_DIR.
  
  cat("Building model input workbook...\n")
  
  wb <- createWorkbook()
  
  model_sheets <- list(
    "01_E1_master_panel"       = E1,
    "02_P5_immunisation_state" = P5,
    "03_P1_inpatient_OOP_q"    = P1,
    "04_P3_outpatient_OOP_q"   = P3,
    "05_C2_catastrophic_q"     = C2,
    "06_P6_poverty_q"          = P6
  )
  
  for (sheet_name in names(model_sheets)) {
    addWorksheet(wb, sheet_name)
    writeDataTable(wb, sheet_name, model_sheets[[sheet_name]],
                   tableStyle = "TableStyleMedium2")
  }
  
  saveWorkbook(wb, file.path(OUT_DIR, "NSSO75_AB_Model_Inputs.xlsx"), overwrite = TRUE)
  cat("  Saved: NSSO75_AB_Model_Inputs.xlsx\n")
  
  cat("Step 7 complete.\n\n")


  ## Step 8 — Quintile MPCE Dollar Ranges for Table 1 ----
  # Per-quintile MPCE ranges anchor the wealth strata in absolute INR/USD.
  # The wealth quintiles in this script are MPCE-based (defined in Step 1a
  # from NSS 75th Round). The NFHS DHS wealth index is an asset-and-amenity
  # composite; the two stratifications are correlated but distinct, so the
  # MPCE ranges below are presented in Table 1 as a footnoted absolute
  # economic anchor rather than as an alternative quintile assignment.
  
  cat("Step 8 — quintile MPCE ranges for Table 1...\n")


  ### 8a. Weighted percentile helper ----
  
  weighted_quantile <- function(x, w, p) {
    ord <- order(x)
    x_o <- x[ord]
    w_o <- w[ord]
    cw  <- cumsum(w_o) / sum(w_o)
    approx(cw, x_o, xout = p, rule = 2, ties = "ordered")$y
  }


  ### 8b. Per-quintile summary statistics ----
  # Population-weighted (design weight × household size) to align with
  # the per-capita interpretation of MPCE.
  
  hh_for_q <- hh |>
    filter(wealth_quintile != "Unknown",
           !is.na(MPCE), MPCE > 0) |>
    mutate(person_weight = weight * hhsize)
  
  q_mpce_summary <- hh_for_q |>
    group_by(wealth_quintile) |>
    summarise(
      n_households = n(),
      n_persons    = sum(hhsize, na.rm = TRUE),
      mpce_p05     = weighted_quantile(MPCE, person_weight, 0.05),
      mpce_p25     = weighted_quantile(MPCE, person_weight, 0.25),
      mpce_median  = weighted_quantile(MPCE, person_weight, 0.50),
      mpce_p75     = weighted_quantile(MPCE, person_weight, 0.75),
      mpce_p95     = weighted_quantile(MPCE, person_weight, 0.95),
      mpce_min     = min(MPCE, na.rm = TRUE),
      mpce_max     = max(MPCE, na.rm = TRUE),
      mpce_mean    = sum(MPCE * person_weight, na.rm = TRUE) /
                     sum(person_weight, na.rm = TRUE),
      .groups      = "drop"
    ) |>
    arrange(wealth_quintile)


  ### 8c. Inflate to 2021 INR and convert to USD ----
  # Constants reused from script header: CPI_FACTOR (2017-18 → 2021),
  # USD_RATE (RBI 2021 average INR per USD).
  
  T1_quintile_mpce <- q_mpce_summary |>
    mutate(
      across(starts_with("mpce_"),
             list(inr_2021 = ~ . * CPI_FACTOR,
                  usd_2021 = ~ . * CPI_FACTOR / USD_RATE),
             .names = "{.col}_{.fn}")
    )


  ### 8d. Pre-formatted Table 1 display rows ----
  
  fmt_inr <- function(x) formatC(round(x), format = "d", big.mark = ",")
  fmt_usd <- function(x) sprintf("%.1f", x)
  
  T1_quintile_display <- T1_quintile_mpce |>
    transmute(
      wealth_quintile,
      n_households,
      mpce_iqr_inr_2021     = paste0("INR ", fmt_inr(mpce_p25_inr_2021),
                                     " to ", fmt_inr(mpce_p75_inr_2021)),
      mpce_iqr_usd_2021     = paste0("USD ", fmt_usd(mpce_p25_usd_2021),
                                     " to ", fmt_usd(mpce_p75_usd_2021)),
      mpce_median_inr_2021  = round(mpce_median_inr_2021),
      mpce_median_usd_2021  = round(mpce_median_usd_2021, 1),
      mpce_mean_inr_2021    = round(mpce_mean_inr_2021),
      mpce_mean_usd_2021    = round(mpce_mean_usd_2021, 1)
    )

  cat("\nQuintile MPCE summary (2021 prices):\n")
  print(T1_quintile_display, width = Inf)


  ### 8e. Save outputs ----
  # Saved as both RDS (for direct consumption by AB_Equity_Reporting.R)
  # and CSV (for inspection / paper appendix).
  
  saveRDS(T1_quintile_mpce,    file.path(OUT_DIR, "T1_quintile_mpce.rds"))
  saveRDS(T1_quintile_display, file.path(OUT_DIR, "T1_quintile_mpce_display.rds"))
  write.csv(T1_quintile_mpce,
            file.path(OUT_DIR, "T1_quintile_mpce.csv"),
            row.names = FALSE)
  write.csv(T1_quintile_display,
            file.path(OUT_DIR, "T1_quintile_mpce_display.csv"),
            row.names = FALSE)
  
  cat("  Saved T1_quintile_mpce.rds and T1_quintile_mpce_display.rds\n")
  cat("  Saved T1_quintile_mpce.csv and T1_quintile_mpce_display.csv\n")
  cat("Step 8 complete.\n\n")


  ## Completion Summary ----
  
  cat(" NSS 75th ROUND EXTRACTION COMPLETE\n")
  cat(sprintf("  National full immunisation (weighted):  %.3f\n",
              with(imm, sum(fully_immunised * weight, na.rm = TRUE) /
                     sum(weight, na.rm = TRUE))))
  cat(sprintf("  National poverty headcount (weighted):  %.3f\n",
              with(hh, sum(poor * weight, na.rm = TRUE) /
                     sum(weight, na.rm = TRUE))))
  cat(sprintf("  CPI factor (2017-18 → 2021):            %.3f\n", CPI_FACTOR))
  cat(sprintf("  USD rate (RBI 2021):                    %.1f INR\n", USD_RATE))
  cat(sprintf("  Poverty lines: Rural INR %d, Urban INR %d (2017-18)\n",
              POV_LINE_RURAL, POV_LINE_URBAN))
  cat("\n")
  cat("  PSA guidance:\n")
  cat("    Gamma (OOP costs):   shape = (mu/se)^2,  rate = mu/se^2\n")
  cat("    Beta  (proportions): alpha = mu*(mu*(1-mu)/se^2 - 1),  beta = (1-mu)*alpha/mu\n")
  cat("    P5 full_imm_se = DiD SE — use as prior SD in brms (ECEA Layer 1)\n")
  cat("    P6 Q3-Q5: beta params = NA — poverty rate is 0, fix at 0 in PSA\n")
  cat("\nAll outputs exported successfully.\n")
  
  
} # end if (FALSE)
