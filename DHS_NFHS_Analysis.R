# NFHS-4 (2015-16) AND NFHS-5 (2019-21) DATA EXTRACTION AND ANALYSIS ----
#
#   Project : Equity in Childhood Vaccination Under Ayushman Bharat
#   Author  : Rohan Chitkara
#
# METHOD OVERVIEW:
#
# Uses raw DHS Child Recode (KR) and Women's Recode (IR)
# files for NFHS-4 (2015-16) and NFHS-5 (2019-21), trims each to the
# analytic variable set required for the distributional ECEA, and exports
# each trimmed dataset for downstream analysis.
#
# OUTPUT FILES (all written to DHS_files/):
#   kr4_NFHS4_trimmed.rds / .csv    Child Recode, NFHS-4
#   kr5_NFHS5_trimmed.rds / .csv    Child Recode, NFHS-5
#   ir4_NFHS4_trimmed.rds / .csv    Women's Recode, NFHS-4
#   ir5_NFHS5_trimmed.rds / .csv    Women's Recode, NFHS-5
#   kr_appended.rds / .csv          Stacked child recode, both rounds
#   ir_appended.rds / .csv          Stacked women's recode, both rounds
#   kr_eligible.rds / .csv          Children 12-23 months, outcomes constructed
#   coverage_state.rds / .csv       Weighted coverage by state and round
#   coverage_quintile.rds / .csv    Weighted coverage by state, quintile, round
#   pop_q.rds / .csv                Weighted population counts by state x quintile
#   ci_state.rds / .csv             Concentration index by state and round
#   did_panel.rds / .csv            State-round panel ready for DiD model
#   u5mr_qs.rds / .csv              U5MR wealth gradient by state x quintile x round
#


# DHS DATA LOADING AND TRIMMING ----

if (FALSE) {
  
  
  ## LIBRARIES ----
  
  library(haven)
  library(dplyr)
  library(tidyr)
  
  
  ## FILE PATHS ----
  
  path_kr4 <- "2015-2016_India_DHS_Children's_Recode.DTA"
  path_kr5 <- "2019-2021_India_DHS_Children's_Recode.DTA"
  path_ir4 <- "2015-2016_India_DHS_Women's_Recode.DTA"
  path_ir5 <- "2019-2021_India_DHS_Women's_Recode.DTA"
  
  out_dir <- "DHS_files"
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  
  ## Helper Function ----
  
  safe_cols <- function(path, required, optional = character(0)) {
    actual <- names(read_dta(path, n_max = 0))
    missing_required <- setdiff(required, actual)
    if (length(missing_required) > 0) {
      warning("Required variables not found in ", basename(path), ": ",
              paste(missing_required, collapse = ", "))
    }
    present_required <- intersect(required, actual)
    present_optional <- intersect(optional, actual)
    unique(c(present_required, present_optional))
  }
  
  
  ## Variable Lists ----
  # Defined once and reused.
  # Required = must exist in both rounds.
  # Optional = may be absent in one round.
  
  # --- KR required variables (present in both NFHS-4 and NFHS-5) ---
  kr_required <- c(
    # Survey design
    "caseid", "v001", "v002", "v003", "v005",
    "v021", "v022", "v023", "v024", "v025",
    # Child identifiers
    "b4", "b8", "bidx",
    # Child survival
    "b5", "b7",
    # Vaccination card
    "h1",
    # BCG
    "h2", "h2d", "h2m", "h2y",
    # DPT doses 1, 2, 3
    "h3", "h3d", "h3m", "h3y",
    "h5", "h5d", "h5m", "h5y",
    "h7", "h7d", "h7m", "h7y",
    # OPV doses 0, 1, 2, 3
    "h0",
    "h4", "h4d", "h4m", "h4y",
    "h6", "h6d", "h6m", "h6y",
    "h8", "h8d", "h8m", "h8y",
    # MCV1
    "h9", "h9d", "h9m", "h9y",
    # Vitamin A
    "h33",
    # Child morbidity
    "h11", "h22",
    # Anthropometry
    "hw1", "hw70", "hw71", "hw72",
    # Interview timing
    "v006", "v007",
    # Maternal characteristics
    "v012", "v106", "v149",
    "v190", "v191",
    "v130", "v131",
    "v151", "v152",
    # Maternal health service use
    "v393",
    # Media access
    "v157", "v158", "v159",
    # Household amenities
    "v113", "v116", "v119"
  )
  
  # --- KR optional variables (may be absent in one round) ---
  kr_optional <- c(
    # Child identifiers
    "midx",
    # Child survival — neonatal
    "b6",
    # Maternal health service use (present in KR as most recent birth vars)
    "m14", "m14_1", "m15", "m15_1", "m17", "m17_1",
    # Pentavalent3 (supersedes h7 post-2012)
    "h52",
    # MCV1 alternate coding
    "h13",
    # MCV2 / MR (NFHS-5)
    "h9a", "h9ad", "h9am", "h9ay",
    # IPV (NFHS-5)
    "h59",
    # PCV doses 1, 2, 3 (NFHS-5)
    "h54", "h56", "h58",
    # Rotavirus doses 1, 2, 3 (NFHS-5)
    "h62", "h64", "h66",
    # Partner education
    "v701",
    # Health insurance — general and scheme-specific
    "v481",
    "v481a", "v481b", "v481c", "v481d", "v481e", "v481f", "v481k",
    # Household size
    "v136",
    # Geographic
    "sdist", "v140"
  )
  
  # --- IR required variables ---
  ir_required <- c(
    # Survey design
    "caseid", "v001", "v002", "v003", "v005",
    "v021", "v022", "v023", "v024", "v025",
    # Interview timing
    "v006", "v007",
    # Woman characteristics
    "v012", "v013",
    "v106", "v107", "v149",
    "v130", "v131",
    "v151", "v152",
    "v190", "v191",
    # Reproductive history
    "v201", "v202", "v203", "v212", "v213", "v218",
    # Family planning
    "v302", "v313", "v626a",
    # Health system access and barriers
    "v393", "v394",
    "v467b", "v467c", "v467d", "v467f",
    # Empowerment
    "v501", "v502",
    "v743a", "v743b", "v743d",
    # Media access
    "v157", "v158", "v159",
    # Household amenities
    "v113", "v116", "v119",
    # Anthropometry
    "v437", "v438", "v445",
    # HIV testing
    "v781"
  )
  
  # --- IR optional variables ---
  ir_optional <- c(
    # ANC — place and number of visits (most recent birth)
    "m13",   "m13_1",
    "m14",   "m14_1",  "m14a",  "m14a_1", "m14b", "m14b_1",
    # Delivery
    "m15",   "m15_1",
    "m17",   "m17_1",
    "m18",   "m18_1",
    # Postnatal care
    "m50",   "m50_1",
    "m51",   "m51_1",
    "m52",   "m52_1",
    # Partner education
    "v701",
    # Health insurance — general and scheme-specific
    "v481",
    "v481a", "v481b", "v481c", "v481d", "v481e", "v481f", "v481k",
    # Household size
    "v136",
    # NFHS-5 additions
    "v169a", "v169b", "v170", "v171a",
    "s119", "s118b",
    # Geographic
    "sdist", "v140"
  )
  
  
  ## Child Recode ----
  
  ### 1. NFHS-4 (2015-16) ----
  
  cat("Peeking at NFHS-4 KR column names...\n")
  cols_kr4 <- safe_cols(path_kr4, kr_required, kr_optional)
  cat("Columns to read:", length(cols_kr4), "\n")
  
  cat("Loading NFHS-4 Child Recode (selected columns only)...\n")
  kr4 <- read_dta(path_kr4, col_select = all_of(cols_kr4)) |>
    mutate(
      round       = "NFHS-4",
      survey_year = 2015
    )
  
  cat("NFHS-4 KR loaded. Dimensions:", dim(kr4), "\n")
  
  
  ### 2. NFHS-5 (2019-21) ----
  
  cat("Peeking at NFHS-5 KR column names...\n")
  cols_kr5 <- safe_cols(path_kr5, kr_required, kr_optional)
  cat("Columns to read:", length(cols_kr5), "\n")
  
  cat("Loading NFHS-5 Child Recode (selected columns only)...\n")
  kr5 <- read_dta(path_kr5, col_select = all_of(cols_kr5)) |>
    mutate(
      round       = "NFHS-5",
      survey_year = 2019
    )
  
  cat("NFHS-5 KR loaded. Dimensions:", dim(kr5), "\n")
  
  
  ## Women's Recode ----
  
  ### 3. NFHS-4 (2015-16) ----
  
  cat("Peeking at NFHS-4 IR column names...\n")
  cols_ir4 <- safe_cols(path_ir4, ir_required, ir_optional)
  cat("Columns to read:", length(cols_ir4), "\n")
  
  cat("Loading NFHS-4 Women's Recode (selected columns only)...\n")
  ir4 <- read_dta(path_ir4, col_select = all_of(cols_ir4)) |>
    mutate(
      round       = "NFHS-4",
      survey_year = 2015
    )
  
  cat("NFHS-4 IR loaded. Dimensions:", dim(ir4), "\n")
  
  
  ### 4. NFHS-5 (2019-21) ----
  
  cat("Peeking at NFHS-5 IR column names...\n")
  cols_ir5 <- safe_cols(path_ir5, ir_required, ir_optional)
  cat("Columns to read:", length(cols_ir5), "\n")
  
  cat("Loading NFHS-5 Women's Recode (selected columns only)...\n")
  ir5 <- read_dta(path_ir5, col_select = all_of(cols_ir5)) |>
    mutate(
      round       = "NFHS-5",
      survey_year = 2019
    )
  
  cat("NFHS-5 IR loaded. Dimensions:", dim(ir5), "\n")
  
  
  ## Codebook Summary ----
  
  summarise_vars <- function(df, label) {
    cat("\n====", label, "====\n")
    cat("Rows:", nrow(df), " | Columns:", ncol(df), "\n\n")
    df |>
      summarise(across(
        everything(),
        list(
          n_nonmiss = ~as.character(sum(!is.na(.))),
          n_miss    = ~as.character(sum(is.na(.))),
          min       = ~as.character(suppressWarnings(min(., na.rm = TRUE))),
          max       = ~as.character(suppressWarnings(max(., na.rm = TRUE))),
          n_unique  = ~as.character(dplyr::n_distinct(., na.rm = TRUE))
        ),
        .names = "{.col}__{.fn}"
      )) |>
      pivot_longer(
        everything(),
        names_to  = c("variable", "stat"),
        names_sep = "__"
      ) |>
      pivot_wider(names_from = stat, values_from = value) |>
      print(n = Inf)
  }
  
  summarise_vars(kr4, "NFHS-4 Child Recode")
  summarise_vars(kr5, "NFHS-5 Child Recode")
  summarise_vars(ir4, "NFHS-4 Women's Recode")
  summarise_vars(ir5, "NFHS-5 Women's Recode")
  
  
  ## Export ----
  
  cat("\nExporting files...\n")
  
  saveRDS(kr4, file.path(out_dir, "kr4_NFHS4_trimmed.rds"))
  saveRDS(kr5, file.path(out_dir, "kr5_NFHS5_trimmed.rds"))
  saveRDS(ir4, file.path(out_dir, "ir4_NFHS4_trimmed.rds"))
  saveRDS(ir5, file.path(out_dir, "ir5_NFHS5_trimmed.rds"))
  
  write.csv(zap_labels(kr4), file.path(out_dir, "kr4_NFHS4_trimmed.csv"), row.names = FALSE)
  write.csv(zap_labels(kr5), file.path(out_dir, "kr5_NFHS5_trimmed.csv"), row.names = FALSE)
  write.csv(zap_labels(ir4), file.path(out_dir, "ir4_NFHS4_trimmed.csv"), row.names = FALSE)
  write.csv(zap_labels(ir5), file.path(out_dir, "ir5_NFHS5_trimmed.csv"), row.names = FALSE)
  
  cat("\nAll files exported successfully.\n")
  cat("  kr4_NFHS4_trimmed.rds / .csv\n")
  cat("  kr5_NFHS5_trimmed.rds / .csv\n")
  cat("  ir4_NFHS4_trimmed.rds / .csv\n")
  cat("  ir5_NFHS5_trimmed.rds / .csv\n")
  
} # end if (FALSE)


# DHS DATA ANALYSIS ----

if (TRUE) {
  
  
  ## LIBRARIES ----
  
  library(haven)
  library(dplyr)
  library(tidyr)
  library(survey)
  
  
  ## FILE PATHS ----
  
  out_dir <- "DHS_files"
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  path_kr4 <- file.path(out_dir, "kr4_NFHS4_trimmed.rds")
  path_kr5 <- file.path(out_dir, "kr5_NFHS5_trimmed.rds")
  path_ir4 <- file.path(out_dir, "ir4_NFHS4_trimmed.rds")
  path_ir5 <- file.path(out_dir, "ir5_NFHS5_trimmed.rds")
  
  path_hwc <- "hwc_data.csv"
  
  
  ## Step 2 — Append NFHS-4 and NFHS-5 ----
  
  ### 2a. Load trimmed files ----
  
  cat("Loading trimmed RDS files...\n")
  
  kr4 <- readRDS(path_kr4)
  kr5 <- readRDS(path_kr5)
  ir4 <- readRDS(path_ir4)
  ir5 <- readRDS(path_ir5)
  
  cat("KR4:", nrow(kr4), "rows |", ncol(kr4), "cols\n")
  cat("KR5:", nrow(kr5), "rows |", ncol(kr5), "cols\n")
  cat("IR4:", nrow(ir4), "rows |", ncol(ir4), "cols\n")
  cat("IR5:", nrow(ir5), "rows |", ncol(ir5), "cols\n")


  ### 2b. State code crosswalk ----
  # NFHS-4 v024 uses alphabetical state coding; NFHS-5 uses geographic
  # (census-based) coding. The hwc_data.csv uses the NFHS-5 coding.
  # Must align NFHS-4 state codes before appending.
  # Derived directly from the haven value labels in the .dta files.

  nfhs4_to_hwc <- c(
    "1"  = 35,  # andaman and nicobar islands  → hwc 35
    "2"  = 28,  # andhra pradesh               → hwc 28
    "3"  = 12,  # arunachal pradesh            → hwc 12
    "4"  = 18,  # assam                        → hwc 18
    "5"  = 10,  # bihar                        → hwc 10
    "6"  = 4,   # chandigarh                   → hwc 4
    "7"  = 22,  # chhattisgarh                 → hwc 22
    "8"  = 25,  # dadra and nagar haveli       → hwc 25 (merged UT in NFHS-5)
    "9"  = 25,  # daman and diu                → hwc 25 (merged with dadra)
    "10" = 30,  # goa                          → hwc 30
    "11" = 24,  # gujarat                      → hwc 24
    "12" = 6,   # haryana                      → hwc 6
    "13" = 2,   # himachal pradesh             → hwc 2
    "14" = 1,   # jammu and kashmir            → hwc 1
    "15" = 20,  # jharkhand                    → hwc 20
    "16" = 29,  # karnataka                    → hwc 29
    "17" = 32,  # kerala                       → hwc 32
    "18" = 31,  # lakshadweep                  → hwc 31
    "19" = 23,  # madhya pradesh               → hwc 23
    "20" = 27,  # maharashtra                  → hwc 27
    "21" = 14,  # manipur                      → hwc 14
    "22" = 17,  # meghalaya                    → hwc 17
    "23" = 15,  # mizoram                      → hwc 15
    "24" = 13,  # nagaland                     → hwc 13
    "25" = 7,   # delhi                        → hwc 7
    "26" = 21,  # odisha                       → hwc 21
    "27" = 34,  # puducherry                   → hwc 34
    "28" = 3,   # punjab                       → hwc 3
    "29" = 8,   # rajasthan                    → hwc 8
    "30" = 11,  # sikkim                       → hwc 11
    "31" = 33,  # tamil nadu                   → hwc 33
    "32" = 16,  # tripura                      → hwc 16
    "33" = 9,   # uttar pradesh                → hwc 9
    "34" = 5,   # uttarakhand                  → hwc 5
    "35" = 19,  # west bengal                  → hwc 19
    "36" = 36   # telangana                    → hwc 36
  )

  recode_nfhs4_state <- function(df) {
    df |>
      mutate(
        v024_nfhs4_raw = as.integer(v024),
        v024 = as.integer(
          nfhs4_to_hwc[as.character(as.integer(v024))]
        )
      )
  }

  kr4 <- recode_nfhs4_state(kr4)
  ir4 <- recode_nfhs4_state(ir4)

  cat("NFHS-4 state codes recoded to NFHS-5 / hwc_data coding.\n")
  cat("Unique NFHS-4 state codes after recode:", sort(unique(as.integer(kr4$v024))), "\n")
  
  
  ### 2c. Append child recode ----
  
  cat("Appending child recode files...\n")
  
  kr <- bind_rows(kr4, kr5) |>
    mutate(
      state_code = as.integer(v024),
      urban      = as.integer(v025 == 1),
      wt         = v005 / 1e6
    )
  
  cat("KR appended:", nrow(kr), "rows |", ncol(kr), "cols\n")
  
  
  ### 2d. Append women's recode ----
  
  cat("Appending women's recode files...\n")
  
  ir <- bind_rows(ir4, ir5) |>
    mutate(
      state_code = as.integer(v024),
      urban      = as.integer(v025 == 1),
      wt         = v005 / 1e6
    )
  
  cat("IR appended:", nrow(ir), "rows |", ncol(ir), "cols\n")
  
  
  ### 2e. Export appended files ----
  
  saveRDS(kr, file.path(out_dir, "kr_appended.rds"))
  saveRDS(ir, file.path(out_dir, "ir_appended.rds"))
  write.csv(zap_labels(kr), file.path(out_dir, "kr_appended.csv"), row.names = FALSE)
  write.csv(zap_labels(ir), file.path(out_dir, "ir_appended.csv"), row.names = FALSE)
  
  cat("Step 2 complete.\n")
  
  
  ## Step 3 — Construct Vaccination Outcome Variables ----
  # DHS vaccination codes:
  #   0 = not received
  #   1 = vaccination card
  #   2 = mother's report
  #   3 = vaccination card (date recorded)
  #   8 = don't know
  #   NA = missing / not asked
  #
  # Recode: 1, 2, 3 => 1 (received); 0 => 0 (not received); 8, NA => NA
  
  ### 3a. Helper recode function ----
  
  vax_recode <- function(x) {
    case_when(
      x %in% c(1, 2, 3) ~ 1L,
      x == 0             ~ 0L,
      TRUE               ~ NA_integer_
    )
  }
  
  
  ### 3b. Construct antigen indicators and composite outcome ----
  
  kr <- kr |>
    mutate(
      
      # --- Individual antigen indicators ---
      bcg      = vax_recode(h2),
      dpt1     = vax_recode(h3),
      dpt2     = vax_recode(h5),
      dpt3     = vax_recode(h7),
      opv0     = vax_recode(h0),
      opv1     = vax_recode(h4),
      opv2     = vax_recode(h6),
      opv3     = vax_recode(h8),
      mcv1     = vax_recode(h9),
      
      # --- Pentavalent3: prefer h52 if present, else fall back to h7 ---
      # h52 is present in NFHS-5 only; for NFHS-4 rows h52 is NA
      penta3   = case_when(
        !is.na(h52) & h52 %in% c(1, 2, 3) ~ 1L,
        !is.na(h52) & h52 == 0             ~ 0L,
        !is.na(dpt3)                        ~ dpt3,
        TRUE                                ~ NA_integer_
      ),
      
      # --- Four-antigen composite (primary outcome, fixed across both rounds) ---
      # BCG + DPT3/Penta3 + OPV3 + MCV1
      # Received = 1 only if all four confirmed received
      # Not received = 0 if any one confirmed not received
      # NA if any is missing and none confirmed not received
      fully_vaccinated = case_when(
        bcg == 1 & penta3 == 1 & opv3 == 1 & mcv1 == 1 ~ 1L,
        bcg == 0 | penta3 == 0 | opv3 == 0 | mcv1 == 0 ~ 0L,
        TRUE                                             ~ NA_integer_
      ),
      
      # --- Age in months ---
      # NOTE: In NFHS, b8 is age in YEARS (0-4), not months. hw1 is the
      # anthropometry module age in months and is the correct variable.
      # hw1 is NA only for deceased children (b5 = 0), which is correct
      # behaviour — deceased children are excluded from the eligible cohort.
      age_months  = as.integer(hw1),
      eligible    = as.integer(age_months >= 12 & age_months <= 23 & b5 == 1),
      
      # --- Equity stratifiers ---
      wealth_q    = as.integer(v190),      # 1 = poorest, 5 = richest
      wealth_cont = as.numeric(v191),      # continuous score for CI rank
      female      = as.integer(b4 == 2),
      mat_educ    = as.integer(v106),      # 0 = none, 3 = higher
      sc_st       = as.integer(v131 %in% c(992, 993)),
      
      # --- Post indicator for DiD ---
      post        = as.integer(round == "NFHS-5")
      
    )
  
  
  ### 3c. Restrict to eligible children ----
  
  kr_eligible <- kr |>
    filter(eligible == 1) |>
    select(
      # --- Survey design ---
      caseid, v021, v022, wt,
      
      # --- Geography and round ---
      state_code, round, post, survey_year, urban,
      
      # --- Primary outcome ---
      fully_vaccinated,
      
      # --- Individual antigen indicators ---
      # dpt3 and penta3 needed; dpt1/dpt2 retained for dropout analysis
      bcg, dpt1, dpt2, dpt3, penta3, opv3, mcv1,
      
      # --- Sensitivity analysis antigen set (NFHS-5 only; NA for NFHS-4) ---
      any_of(c("h9a", "h54", "h56", "h58", "h59", "h62", "h64", "h66")),
      
      # --- Age ---
      age_months,
      
      # --- VERSE Z variables (unfair determinants) ---
      wealth_q,           # v190 — quintile grouping
      wealth_cont,        # v191 — continuous score for fractional rank
      female,             # b4
      mat_educ,           # v106
      sc_st,              # v131 SC/ST flag
      any_of("v701"),     # partner education
      
      # --- Health insurance (VERSE Z variable) ---
      any_of("v481"),
      any_of(c("v481a", "v481b", "v481c", "v481d", "v481e", "v481f", "v481k")),
      
      # --- Urban/rural (VERSE Z variable — also captured in urban above) ---
      v025,
      
      # --- Child survival — for U5MR wealth gradient (Stage 3 ECEA) ---
      b5, b7,
      any_of("b6")
    )
  
  cat("Eligible children (12-23 months):\n")
  cat("  NFHS-4:", sum(kr_eligible$round == "NFHS-4"), "\n")
  cat("  NFHS-5:", sum(kr_eligible$round == "NFHS-5"), "\n")
  
  cat("\nFull vaccination coverage (unweighted):\n")
  print(
    kr_eligible |>
      group_by(round) |>
      summarise(
        coverage = mean(fully_vaccinated, na.rm = TRUE),
        n        = n(),
        n_miss   = sum(is.na(fully_vaccinated))
      )
  )
  
  saveRDS(kr_eligible, file.path(out_dir, "kr_eligible.rds"))
  write.csv(zap_labels(kr_eligible), file.path(out_dir, "kr_eligible.csv"), row.names = FALSE)

  cat("Step 3 complete.\n")


  ### 3d. U5MR wealth gradient ----

  cat("Computing U5MR wealth gradient...\n")

  kr_all <- readRDS(file.path(out_dir, "kr_appended.rds")) |>
    mutate(
      state_code = as.integer(v024),
      wealth_q   = as.integer(v190),
      wt         = v005 / 1e6,
      u5_death   = as.integer(b5 == 0)
    ) |>
    filter(!is.na(state_code), !is.na(wealth_q), !is.na(wt))

  u5mr_qs <- kr_all |>
    group_by(round, state_code, wealth_q) |>
    summarise(
      deaths   = sum(wt * u5_death, na.rm = TRUE),
      births   = sum(wt, na.rm = TRUE),
      u5mr_raw = deaths / births * 1000,
      .groups  = "drop"
    ) |>
    group_by(round, state_code) |>
    mutate(
      u5mr_state_mean = weighted.mean(u5mr_raw, births, na.rm = TRUE),
      u5mr_scalar     = u5mr_raw / u5mr_state_mean
    ) |>
    ungroup()

  saveRDS(u5mr_qs, file.path(out_dir, "u5mr_qs.rds"))
  write.csv(u5mr_qs, file.path(out_dir, "u5mr_qs.csv"), row.names = FALSE)
  rm(kr_all)

  cat("U5MR gradient complete.\n")


  ## Step 4 — Survey-Weighted Coverage Estimates ----

  ### 4a. Helper function ----
  
  svyby_clean <- function(design, by_formula, round_label) {
    raw      <- svyby(~fully_vaccinated, by_formula, design,
                      svymean, na.rm = TRUE) |>
      as.data.frame()
    by_vars  <- all.vars(by_formula)
    other    <- setdiff(seq_len(ncol(raw)), which(names(raw) %in% by_vars))
    est_idx  <- other[1]
    se_idx   <- other[2]
    names(raw)[est_idx] <- "coverage"
    names(raw)[se_idx]  <- "se"
    raw$round <- round_label
    raw
  }
  
  
  ### 4b. Survey design objects ----
  # DHS uses stratified two-stage cluster sampling.
  # v021 = primary sampling unit; v022 = stratum (state x urban/rural)
  #
  # fully_vaccinated is coerced to numeric before entering the design so
  # svyby() treats it consistently as a continuous mean rather than a factor.
  # survey.lonely.psu = "adjust" centres the
  # single observation at the stratum mean, contributing zero to variance.
  
  options(survey.lonely.psu = "adjust")
  
  svy4 <- svydesign(
    ids     = ~v021,
    strata  = ~v022,
    weights = ~wt,
    data    = filter(kr_eligible, round == "NFHS-4") |>
      mutate(fully_vaccinated = as.numeric(fully_vaccinated)),
    nest    = TRUE
  )
  
  svy5 <- svydesign(
    ids     = ~v021,
    strata  = ~v022,
    weights = ~wt,
    data    = filter(kr_eligible, round == "NFHS-5") |>
      mutate(fully_vaccinated = as.numeric(fully_vaccinated)),
    nest    = TRUE
  )
  
  
  ### 4c. National coverage by round ----
  
  nat4 <- svymean(~fully_vaccinated, svy4, na.rm = TRUE)
  nat5 <- svymean(~fully_vaccinated, svy5, na.rm = TRUE)
  
  cat("\nNational full vaccination coverage:\n")
  cat("  NFHS-4:", round(coef(nat4) * 100, 1), "% (SE:",
      round(SE(nat4) * 100, 2), "%)\n")
  cat("  NFHS-5:", round(coef(nat5) * 100, 1), "% (SE:",
      round(SE(nat5) * 100, 2), "%)\n")
  
  
  ### 4d. Coverage by state ----
  
  coverage_state <- bind_rows(
    svyby_clean(svy4, ~state_code, "NFHS-4"),
    svyby_clean(svy5, ~state_code, "NFHS-5")
  ) |>
    mutate(
      ci_lower = coverage - 1.96 * se,
      ci_upper = coverage + 1.96 * se
    )
  
  cat("State-level coverage computed for", nrow(coverage_state), "state-round cells.\n")
  
  
  ### 4e. Coverage by state x wealth quintile ----
  
  coverage_quintile <- bind_rows(
    svyby_clean(svy4, ~state_code + wealth_q, "NFHS-4"),
    svyby_clean(svy5, ~state_code + wealth_q, "NFHS-5")
  ) |>
    mutate(
      ci_lower = coverage - 1.96 * se,
      ci_upper = coverage + 1.96 * se
    )
  
  cat("Quintile x state coverage computed for",
      nrow(coverage_quintile), "cells.\n")
  
  
  ### 4f. Quintile population distribution by state x round ----
  # NFHS-4 and NFHS-5 design weights are not comparable in absolute terms
  # across rounds — NFHS-4 over-sampled smaller states for district-level
  # representativeness, inflating raw weighted sums relative to NFHS-5.
  # Raw sum(wt) therefore cannot be used as a cross-round population count.
  #
  # The correct approach for the ECEA is to compute the within-state
  # quintile proportion from the DHS weights (valid for relative
  # distributions within a round), then apply that proportion to an
  # external state-level population estimate (e.g., census projections).
  #
  # prop_q   = within-state quintile share of eligible children (DHS-derived)
  # n_q      = unweighted cell sample size (for variance diagnostics)
  # wt_sum_q = raw weighted sum (retained for audit; not directly comparable)

  pop_q <- kr_eligible |>
    group_by(round, state_code, wealth_q) |>
    summarise(
      n_q      = n(),
      wt_sum_q = sum(wt, na.rm = TRUE),
      .groups  = "drop"
    ) |>
    group_by(round, state_code) |>
    mutate(
      wt_sum_state = sum(wt_sum_q),
      prop_q       = wt_sum_q / wt_sum_state
    ) |>
    ungroup()
  # prop_q sums to 1.0 within each state-round cell and is directly
  # comparable across rounds. Multiply by an external pop_12_23_s
  # estimate to recover the quintile population count for the ECEA.
  
  
  ### 4g. Export ----
  
  saveRDS(coverage_state, file.path(out_dir, "coverage_state.rds"))
  saveRDS(coverage_quintile, file.path(out_dir, "coverage_quintile.rds"))
  saveRDS(pop_q, file.path(out_dir, "pop_q.rds"))
  write.csv(coverage_state, file.path(out_dir, "coverage_state.csv"), row.names = FALSE)
  write.csv(coverage_quintile, file.path(out_dir, "coverage_quintile.csv"), row.names = FALSE)
  write.csv(pop_q, file.path(out_dir, "pop_q.csv"), row.names = FALSE)
  
  cat("Step 4 complete.\n")
  
  
  ## Step 5 — Concentration Index by State and Round ----
  # Wagstaff (2002) weighted CI:
  #   CI = (2 / mu) * cov_w(y, r_w)
  # where r_w is the weighted fractional rank of the wealth score (v191)
  # and mu is the weighted mean of the outcome y.
  # SE computed via bootstrap (500 replications per state).
  
  ### 5a. CI computation function ----
  
  compute_ci <- function(df) {
    df <- df |>
      filter(!is.na(fully_vaccinated), !is.na(wealth_cont), !is.na(wt)) |>
      arrange(wealth_cont)
    
    n <- nrow(df)
    if (n < 50) return(tibble(ci = NA_real_, mu = NA_real_, n = n))
    
    w      <- df$wt
    W      <- cumsum(w)
    W_tot  <- sum(w)
    rank_w <- (W - w / 2) / W_tot
    
    y  <- df$fully_vaccinated
    mu <- sum(w * y) / W_tot
    if (mu == 0) return(tibble(ci = NA_real_, mu = 0, n = n))
    
    wcov <- sum(w * (y - mu) * (rank_w - 0.5)) / W_tot
    ci   <- 2 * wcov / mu
    
    tibble(ci = ci, mu = mu, n = n)
  }
  
  
  ### 5b. Bootstrap SE function ----
  
  bootstrap_ci_se <- function(df, B = 500, seed = 42) {
    set.seed(seed)
    boot_vals <- replicate(B, {
      d_b <- df[sample(nrow(df), replace = TRUE), ]
      compute_ci(d_b)$ci
    })
    sd(boot_vals, na.rm = TRUE)
  }
  
  
  ### 5c. Compute CI by state and round ----
  
  cat("Computing concentration indices by state and round...\n")
  cat("(Bootstrap SE: 500 replications per state)\n")
  
  ci_state <- kr_eligible |>
    group_by(round, state_code) |>
    group_modify(~ {
      ci_val <- compute_ci(.x)
      se_val <- bootstrap_ci_se(.x, B = 500)
      bind_cols(ci_val, tibble(se_ci = se_val))
    }) |>
    ungroup() |>
    mutate(
      ci_lower = ci - 1.96 * se_ci,
      ci_upper = ci + 1.96 * se_ci
    )
  
  cat("\nConcentration index summary:\n")
  print(
    ci_state |>
      group_by(round) |>
      summarise(
        ci_mean = mean(ci, na.rm = TRUE),
        ci_min  = min(ci, na.rm = TRUE),
        ci_max  = max(ci, na.rm = TRUE)
      )
  )
  
  saveRDS(ci_state, file.path(out_dir, "ci_state.rds"))
  write.csv(ci_state, file.path(out_dir, "ci_state.csv"), row.names = FALSE)
  
  cat("Step 5 complete.\n")
  
  
  ## Step 6 — Merge with HWC Panel for DiD ----
  
  ### 6a. Build state-round panel ----
  
  did_base <- ci_state |>
    left_join(coverage_state, by = c("round", "state_code")) |>
    mutate(post = as.integer(round == "NFHS-5"))
  
  
  ### 6b. Merge HWC intensity data ----

  hwc <- read.csv(path_hwc) |>
    mutate(exclude_from_did = tolower(exclude_from_did) == "true")

  did_panel <- did_base |>
    left_join(
      hwc |> select(state_code, x_s, x_s_sensitivity, exclude_from_did, data_notes),
      by = "state_code"
    ) |>
    filter(!exclude_from_did, state_code != 7)  # excludes Ladakh, Lakshadweep, Delhi

  cat("DiD panel dimensions:", dim(did_panel), "\n")  # expect 66 rows (33 states x 2 rounds)
  cat("Rows by round:\n")
  print(table(did_panel$post))
  cat("x_s summary:\n")
  print(summary(did_panel$x_s))
  
  saveRDS(did_panel, file.path(out_dir, "did_panel.rds"))
  write.csv(did_panel, file.path(out_dir, "did_panel.csv"), row.names = FALSE)

  cat("Step 6 complete.\n")


  ## Results Workbook Export ----
  # Writes all analytic tibbles to a single Excel workbook, one sheet per
  # tibble, for use in the results section.

  library(openxlsx)

  wb <- createWorkbook()

  results_sheets <- list(
    "01_coverage_national"  = data.frame(
      round    = c("NFHS-4", "NFHS-5"),
      coverage = c(coef(nat4), coef(nat5)),
      se       = c(SE(nat4),  SE(nat5))
    ),
    "02_coverage_state"     = coverage_state,
    "03_coverage_quintile"  = coverage_quintile,
    "04_pop_q"              = pop_q,
    "05_ci_state"           = ci_state,
    "06_u5mr_qs"            = u5mr_qs,
    "07_did_panel"          = did_panel
  )

  for (sheet_name in names(results_sheets)) {
    addWorksheet(wb, sheet_name)
    writeDataTable(wb, sheet_name, results_sheets[[sheet_name]],
                   tableStyle = "TableStyleMedium2")
  }

  saveWorkbook(wb, "NFHS_Results_Tibbles.xlsx", overwrite = TRUE)
  cat("Results workbook saved: NFHS_Results_Tibbles.xlsx\n")
  cat("\nAll outputs exported successfully.\n")

} # end if (FALSE)