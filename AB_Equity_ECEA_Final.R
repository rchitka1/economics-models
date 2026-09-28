# AB EQUITY ECONOMIC EVALUATION MODEL — STATIC ECEA, BAYESIAN DiD, SENSITIVITY ----
#
#   Project     : Equity in Childhood Vaccination Under Ayushman Bharat
#   Author      : Rohan Chitkara
#   Supervisor  : Bryan Patenaude
#   AI Use      : Claude AI used for debugging and code audit. Model structure &
#                 code written by author
#
# METHOD OVERVIEW:
#
# Distributional Extended Cost-Effectiveness Analysis (ECEA) of the Health
# and Wellness Centre (HWC) component of Ayushman Bharat across 32 Indian
# states, using NFHS-4 (pre-period) and NFHS-5 (post-period). The model
# proceeds in four stages following extraction and analysis of survey data:
#
#   Stage 1b — Concentration index re-ranking (national-within-round) and
#              quintile-specific rural-urban weighted poverty line.
#   Stage 2a — Coverage DiD via OLS with state fixed effects and clustered
#              standard errors, on pooled NFHS-4 / NFHS-5 microdata.
#   Stage 2b — Equity DiD (concentration index) via Bayesian hierarchical
#              measurement-error model in brms.
#   Stage 3  — Six-step ECEA producing equity-stratified ICERs and NMB.
#              Cost is allocated to the immunisation function via immune_alloc
#              share of total HWC cost.
#   Stage 4  — One-way sensitivity analysis, 1,000-draw PSA, and
#              CEAC / CERAC across a willingness-to-pay grid.
#              Equity-anchored PSA and CEAC drawn from the Stage 2b
#              Bayesian posterior of the CI coefficients.
#
#   Required Files  : AB_equity_inputs.rds (built by build_inputs.R from
#                     AB_Equity_Inputs_Final_v2.xlsx + DHS_files/kr_eligible.rds)
#   Survey Analysis : DHS_NFHS_Analysis.R , NSSO75_ECEA_Extraction.R performed
#                     on survey data.
#

# EQUITY MODEL ----

if (TRUE) {
  
  
  ## 1. STAGE 1 - SETUP AND DATA LOAD ----
  
  if (TRUE) {
    
    
    ### LIBRARIES ----
    
    library(readxl)
    library(dplyr)
    library(tidyr)
    library(purrr)
    library(haven)
    
    
    ### FILE PATHS ----
    
    path_bundle <- "AB_equity_inputs.rds"
    
    if (!file.exists(path_bundle))
      stop("Inputs bundle not found at ", path_bundle,
           " — run build_inputs.R first.")
    
    inputs <- readRDS(path_bundle)
    cat("Loaded inputs bundle: ", length(inputs), " elements.\n", sep = "")
    
    out_dir <- "ECEA_files"
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    
    
    ### Helper Functions ----
    
    read_R <- function(sheet) inputs[[sheet]]
    
    param_lookup <- function(pl, id) {
      row <- pl |> filter(param_id == id)
      if (nrow(row) == 0) stop("param_id not found: ", id)
      list(
        base  = suppressWarnings(as.numeric(row$base[1])),
        lower = suppressWarnings(as.numeric(row$lower[1])),
        upper = suppressWarnings(as.numeric(row$upper[1]))
      )
    }
    
    
    ### State Keys and Reference ----
    
    state_key <- read_R("R_state_key") |>
      rename(nfhs_code = nfhs_state_code_actual)
    
    state_lookup <- state_key |>
      select(nfhs_code, state_ut, state_abbrev, model_state_code)
    
    ref_key <- read_R("R_reference_key")
    
    
    ### Parameter Master ----
    
    pl <- read_R("R_parameters_long")
    
    
    ### NFHS State and Quintile Tables ----
    
    cov_state <- read_R("R_nfhs_coverage_state") |>
      left_join(state_lookup, by = c("state_code" = "nfhs_code")) |>
      select(round, state_code, state_ut, coverage, se, ci_lower, ci_upper)
    
    cov_quintile <- read_R("R_nfhs_coverage_quintile") |>
      left_join(state_lookup, by = c("state_code" = "nfhs_code")) |>
      select(round, state_code, state_ut, wealth_q, coverage, se, ci_lower, ci_upper)
    
    pop_q <- read_R("R_nfhs_pop_quintile") |>
      left_join(state_lookup, by = c("state_code" = "nfhs_code")) |>
      select(round, state_code, state_ut, wealth_q, n_q, wt_sum_q, wt_sum_state, prop_q)
    
    ci_state <- read_R("R_nfhs_ci_state") |>
      left_join(state_lookup, by = c("state_code" = "nfhs_code")) |>
      select(round, state_code, state_ut, ci, mu, n, se_ci, ci_lower, ci_upper)
    
    u5mr_qs <- read_R("R_nfhs_u5mr_qs") |>
      left_join(state_lookup, by = c("state_code" = "nfhs_code")) |>
      select(round, state_code, state_ut, wealth_q, deaths, births,
             u5mr_raw, u5mr_state_mean, u5mr_scalar) |>
      mutate(u5mr_scalar = ifelse(is.na(u5mr_scalar) | u5mr_scalar == 0,
                                  1.0, u5mr_scalar))
    
    
    ### Treatment Intensity ----
    
    treat <- read_R("R_treatment_intensity") |>
      select(state_ut,
             nfhs5_phase,
             x_s_primary  = xs_primary_feb2019_per_100k,
             x_s_sens     = xs_sensitivity_mar2020_per_100k,
             hwc_oper     = hwc_oper_feb2019,
             hwc_sc_2020  = hwc_sc_mar2020_rhs,
             hwc_phc_2020 = hwc_phc_mar2020_rhs,
             hwc_uphc_2020 = hwc_uphc_mar2020_rhs,
             population_2021,
             pop_12_23_2021)
    
    
    ### DiD Panel ----
    
    did_panel <- read_R("R_did_panel") |>
      left_join(state_lookup, by = c("state_code" = "nfhs_code")) |>
      select(round, post, state_code, state_ut, x_s, x_s_sensitivity,
             ci, mu, n, se_ci, coverage, se, exclude_from_did)
    
    did_panel_bayes <- did_panel |>
      filter(!is.na(ci) & !is.na(se_ci))
    
    
    ### OOP and Poverty ----
    
    oop_q <- read_R("R_oop_quintile") |>
      mutate(wealth_q = match(wealth_quintile,
                              c("Q1_Poorest","Q2","Q3","Q4","Q5_Richest"))) |>
      select(wealth_q, wealth_quintile,
             oop_mean_2018   = oop_total_mean_inr,
             oop_se_2018     = oop_total_se_inr,
             oop_mean_2021   = oop_total_mean_inr_2021,
             gamma_shape     = oop_total_gamma_shape,
             gamma_rate_2021 = oop_total_gamma_rate)
    
    pov_lines <- read_R("R_poverty_lines")
    pov_line_rural_2018 <- pov_lines |>
      filter(Symbol == "pov_line_rural_2018") |> pull(Base) |> as.numeric()
    pov_line_urban_2018 <- pov_lines |>
      filter(Symbol == "pov_line_urban_2018") |> pull(Base) |> as.numeric()
    
    pov_hc <- read_R("R_pov_hc_quintile") |>
      mutate(
        beta_alpha = suppressWarnings(as.numeric(beta_alpha)),
        beta_beta  = suppressWarnings(as.numeric(beta_beta))
      ) |>
      select(wealth_q, wealth_quintile, pov_hc_mean, pov_hc_se,
             beta_alpha, beta_beta)
    
    
    ### Concentration index re-ranking and quintile-weighted poverty line ----
    # Step a: state-round CI on national-within-round fractional ranks.
    # Reference distribution is all eligible children pooled nationally
    # within each round, ranked by v191 using DHS sampling weights. A
    # common reference distribution is required for cross-state CI
    # comparability; within-state ranking confounds the equity gradient
    # with shifts in within-state wealth composition. v191 is round-
    # specific (scale differs ~10× between rounds), so ranks are
    # constructed within round.
    #
    # Step b: rural-urban weighted poverty line by national quintile.
    # The rural-only Tendulkar 2017-18 line (1,081) is replaced by a
    # quintile-specific weighted average of rural and urban lines (1,081
    # and 1,325) using the rural-urban share of the NFHS-5 12-23m cohort
    # within each national wealth quintile. Applied to pov_hc_mean to
    # maintain internal consistency between line and headcount.
    
    if (TRUE) {
      
      cat("\n===== Stage 1b — CI re-ranking and quintile poverty line =====\n")
      
      kr_eligible <- inputs$kr_eligible |>
        mutate(fully_vaccinated = as.numeric(fully_vaccinated))
      
      cat("Loaded kr_eligible:", nrow(kr_eligible), "rows\n")
      cat("  NFHS-4:", sum(kr_eligible$round == "NFHS-4"), "\n")
      cat("  NFHS-5:", sum(kr_eligible$round == "NFHS-5"), "\n")
      
      
      ## National-within-round fractional ranks ----
      
      add_national_rank <- function(df) {
        df <- df |>
          filter(!is.na(fully_vaccinated), !is.na(wealth_cont), !is.na(wt)) |>
          arrange(wealth_cont)
        w <- df$wt; W <- cumsum(w); W_tot <- sum(w)
        df$rank_natl <- (W - w / 2) / W_tot
        df
      }
      
      kr_ranked <- kr_eligible |>
        group_by(round) |>
        group_modify(~ add_national_rank(.x)) |>
        ungroup()
      
      compute_ci_natl <- function(df) {
        df <- df |>
          filter(!is.na(fully_vaccinated), !is.na(rank_natl), !is.na(wt))
        n <- nrow(df)
        if (n < 50) return(tibble(ci = NA_real_, mu = NA_real_, n = n))
        w <- df$wt; y <- df$fully_vaccinated; r <- df$rank_natl
        W_tot <- sum(w)
        mu <- sum(w * y) / W_tot
        if (mu == 0) return(tibble(ci = NA_real_, mu = 0, n = n))
        wcov <- sum(w * (y - mu) * (r - 0.5)) / W_tot
        tibble(ci = 2 * wcov / mu, mu = mu, n = n)
      }
      
      bootstrap_ci_se <- function(df, B = 500, seed = 42) {
        set.seed(seed)
        boot_vals <- replicate(B, {
          d_b <- df[sample(nrow(df), replace = TRUE), ]
          compute_ci_natl(d_b)$ci
        })
        sd(boot_vals, na.rm = TRUE)
      }
      
      cat("Computing state-round CI under national-within-round ranking ")
      cat("(500-bootstrap SE per cell)...\n")
      
      ci_state_natl <- kr_ranked |>
        group_by(round, state_code) |>
        group_modify(~ {
          ci_val <- compute_ci_natl(.x)
          se_val <- bootstrap_ci_se(.x, B = 500)
          bind_cols(ci_val, tibble(se_ci = se_val))
        }) |>
        ungroup() |>
        mutate(
          ci_lower = ci - 1.96 * se_ci,
          ci_upper = ci + 1.96 * se_ci
        )
      
      cat("National-rank CI summary:\n")
      print(ci_state_natl |>
              group_by(round) |>
              summarise(ci_mean = mean(ci, na.rm = TRUE),
                        ci_min  = min(ci,  na.rm = TRUE),
                        ci_max  = max(ci,  na.rm = TRUE),
                        n_states = sum(!is.na(ci)),
                        .groups = "drop"))
      
      compute_curve <- function(df) {
        df <- df |> arrange(rank_natl) |>
          mutate(cum_pop = cumsum(wt) / sum(wt),
                 cum_y   = cumsum(wt * fully_vaccinated) /
                   sum(wt * fully_vaccinated))
        grid <- seq(0, 1, by = 0.001)
        tibble(cum_pop = grid,
               cum_y   = approx(df$cum_pop, df$cum_y, xout = grid,
                                yleft = 0, yright = 1)$y)
      }
      
      concentration_curve_data <- kr_ranked |>
        group_by(round) |> group_modify(~ compute_curve(.x)) |> ungroup()
      
      saveRDS(concentration_curve_data,
              file.path(out_dir, "concentration_curve_data.rds"))
      
      # Override ci_state with the national-rank values, preserving the
      # state_lookup join structure used downstream.
      ci_state <- ci_state_natl |>
        left_join(state_lookup, by = c("state_code" = "nfhs_code")) |>
        select(round, state_code, state_ut, ci, mu, n, se_ci, ci_lower, ci_upper)
      
      # Override the ci/se_ci columns inside did_panel and did_panel_bayes
      # so Stage 2b consumes the national-rank CI panel.
      did_panel <- did_panel |>
        select(-ci, -se_ci, -mu, -n) |>
        left_join(ci_state |> select(round, state_code, ci, se_ci, mu, n),
                  by = c("round", "state_code")) |>
        select(round, post, state_code, state_ut, x_s, x_s_sensitivity,
               ci, mu, n, se_ci, coverage, se, exclude_from_did)
      
      did_panel_bayes <- did_panel |>
        filter(!is.na(ci) & !is.na(se_ci))
      
      saveRDS(ci_state,        file.path(out_dir, "ci_state_natl_rank.rds"))
      saveRDS(did_panel_bayes, file.path(out_dir, "did_panel_bayes_natl_rank.rds"))
      
      
      ## Rural-urban weighted poverty line by quintile ----
      
      pov_line_rural_2018 <- pov_lines |>
        filter(Symbol == "pov_line_rural_2018") |> pull(Base) |> as.numeric()
      pov_line_urban_2018 <- pov_lines |>
        filter(Symbol == "pov_line_urban_2018") |> pull(Base) |> as.numeric()
      
      ru_share_q <- kr_eligible |>
        filter(round == "NFHS-5",
               !is.na(wealth_q), !is.na(v025), !is.na(wt)) |>
        mutate(
          is_rural = as.integer(v025 == 2),
          is_urban = as.integer(v025 == 1)
        ) |>
        group_by(wealth_q) |>
        summarise(
          n_children   = n(),
          rural_share  = weighted.mean(is_rural, wt, na.rm = TRUE),
          urban_share  = weighted.mean(is_urban, wt, na.rm = TRUE),
          .groups      = "drop"
        ) |>
        arrange(wealth_q) |>
        mutate(pov_line_weighted = pov_line_rural_2018 * rural_share +
                                   pov_line_urban_2018 * urban_share)
      
      pov_line_q_vec        <- ru_share_q$pov_line_weighted
      names(pov_line_q_vec) <- as.character(ru_share_q$wealth_q)
      
      cat("\nRural-urban share by national wealth quintile (NFHS-5):\n")
      print(ru_share_q, width = Inf)
      cat("\nWeighted poverty line per quintile (INR/person/month):\n")
      print(round(pov_line_q_vec))
      
      saveRDS(ru_share_q,    file.path(out_dir, "ru_share_q.rds"))
      saveRDS(pov_line_q_vec, file.path(out_dir, "pov_line_q_vec.rds"))
      
      # Clean up working objects to avoid downstream namespace clutter.
      rm(kr_ranked, ci_state_natl, add_national_rank, compute_ci_natl,
         bootstrap_ci_se)
      
      cat("\nStage 1b complete.\n")
    }
    
    
    ### HWC Costs ----
    
    hwc_costs <- read_R("R_hwc_costs") |>
      select(state_ut, facility_type, hwc_count_mar2020,
             unit_cost_2021    = unit_cost_2020_21_inr,
             lower_2021        = lower_2020_21_inr,
             upper_2021        = upper_2020_21_inr,
             gamma_shape, gamma_rate_2021 = gamma_rate_2020_21,
             cost_specificity)
    
    
    ### IMI Exposure ----
    # State-level IMI treatment variable for the augmented DiD.
    # Primary spec: imi_exposure_intensity = (Phase 1 + IMI 2.0 districts) / total
    #   - Uncapped (can exceed 1 for states with district overlap across rounds)
    #   - Captures cumulative district-rounds of exposure per district
    # Sensitivity: imi_any_district_exposure (capped at 1)
    # Source: published district lists for IMI Phase 1 and IMI 2.0
    # NOTE: R_imi_exposure has title on row 1 + 3 blank rows; headers on row 5 → skip = 4
    
    imi_exposure <- read_R("R_imi_exposure") |>
      mutate(nfhs_state_code = suppressWarnings(as.numeric(nfhs_state_code))) |>
      filter(!is.na(nfhs_state_code), !is.na(state_ut)) |>
      select(nfhs_state_code, state_ut,
             imi_phase_1_districts, imi_2_0_districts, total_districts_2018,
             imi_exposure_intensity, imi_any_district_exposure,
             imi_phase_1_only, imi_2_0_only)
    
    
    ### IMI Costs ----
    # State-level cost per IMI dose. 5 states have primary data;
    # 27 others imputed via population-weighted mean.
    # NOTE: R_imi_costs has 3 title rows + 1 blank; headers on row 5 → skip = 4
    
    imi_costs <- read_R("R_imi_costs") |>
      mutate(nfhs_state_code = suppressWarnings(as.numeric(nfhs_state_code))) |>
      filter(!is.na(nfhs_state_code), !is.na(state_ut)) |>
      select(nfhs_state_code, state_ut, state_pop_2017,
             cost_per_dose_usd_2019_mean,
             cost_per_dose_inr_2021_mean,
             ci95_lo_inr_2021, ci95_hi_inr_2021,
             sd_inr_2021, gamma_shape_imi = gamma_shape, gamma_rate_imi = gamma_rate,
             data_status)
    
    
    ### IMI Metadata ----
    # Paper-level scalar parameters for IMI: USD-INR conversion, CPI, IMI horizon,
    # persistence parameters, displacement factor.
    # NOTE: R_metadata has 3 title rows + 1 blank; headers on row 5 → skip = 4
    
    imi_metadata <- read_R("R_metadata") |>
      filter(!is.na(param_id))
    
    imi_meta_lookup <- function(id) {
      row <- imi_metadata |> filter(param_id == id)
      if (nrow(row) == 0) return(NA)
      suppressWarnings(as.numeric(row$value[1]))
    }
    
    
    ### IMI Phase-Weighted Sensitivity ----
    # Round-weighted IMI exposure with block-level discount for UP/Bihar.
    # Used as supplementary appendix sensitivity in Stage 4 OWSA.
    # NOTE: R_imi_phaseweighted has 4 title rows + a parameter row + 1 blank;
    # headers on row 6 → skip = 5
    
    imi_phaseweighted <- read_R("R_imi_phaseweighted") |>
      mutate(nfhs_state_code = suppressWarnings(as.numeric(nfhs_state_code))) |>
      filter(!is.na(nfhs_state_code), !is.na(state_ut)) |>
      select(nfhs_state_code, state_ut,
             imi_phase_1_districts, imi_phase_1_rounds,
             imi_2_0_districts, imi_2_0_rounds,
             block_level_flag, total_districts_2018,
             weighted_intensity_full, weighted_intensity_discounted)
    
    
    ### GBD DALYs ----
    
    gbd_causes <- c("Tuberculosis","Diphtheria","Tetanus","Pertussis","Measles")
    
    gbd_dalys <- read_R("R_gbd_under5") |>
      filter(measure_name == "DALYs (Disability-Adjusted Life Years)",
             metric_name  == "Rate",
             year %in% c(2015, 2019),
             cause_name %in% gbd_causes) |>
      select(state_ut = location_name, cause_name, year,
             daly_rate = val, daly_lower = lower, daly_upper = upper)
    
    gbd_total_dalys <- read_R("R_gbd_under5") |>
      filter(measure_name == "DALYs (Disability-Adjusted Life Years)",
             metric_name  == "Number",
             year == 2019,
             cause_name %in% gbd_causes) |>
      group_by(cause_name) |>
      summarise(total_dalys = sum(val, na.rm = TRUE), .groups = "drop")
    
    gbd_burden_prop <- read_R("R_gbd_burden_prop") |>
      filter(year %in% c(2015, 2019),
             cause_name %in% gbd_causes,
             measure_name == "DALYs") |>
      select(year, state_ut = state, cause_name, proportion)
    
    
    ### Scalar Parameters ----
    
    params <- list(
      discount_rate       = param_lookup(pl, "r_c")$base,
      horizon_years       = param_lookup(pl, "t")$base,
      annuity_T           = param_lookup(pl, "af_t")$base,
      gdp_pc_inr          = param_lookup(pl, "gdp_pc_inr")$base,
      wtp_base            = 73500,
      wtp_low             = 44100,
      wtp_high            = 147000,
      cpi_2018_2021       = 1.149,
      usd_inr_2021        = 74.0,
      n_psa               = 1000,
      immune_alloc_base   = 0.10,
      immune_alloc_lower  = 0.06,
      immune_alloc_upper  = 0.14,
      # IMI parameters
      horizon_imi_base    = 2,        # 2-year effective horizon (one-shot)
      horizon_imi_low     = 1,        # OWSA lower
      horizon_imi_high    = 5,        # OWSA upper
      imi_persistence_base = 0.50,    # Year-2 persistence of year-1 effect
      imi_persistence_low  = 0.00,
      imi_persistence_high = 1.00,
      imi_displacement    = 0.63,     # ~63% of IMI doses incremental (rest displace routine)
      # Years of under-5 disease exposure prevented per child fully vaccinated.
      # Vaccination at 12-23 months protects through under-5 period (~ages 1-5).
      # Default 4 years (ages 1-2-3-4); OWSA range 3-5 years.
      under5_protection_years        = 4,
      under5_protection_years_lower  = 3,
      under5_protection_years_upper  = 5,
      ve = list(
        bcg     = list(base = 0.50, lower = 0.20, upper = 0.80, dist = "beta"),
        dpt3p   = list(base = 0.85, lower = 0.71, upper = 0.93, dist = "beta"),
        mcv1    = list(base = 0.85, lower = 0.70, upper = 0.95, dist = "beta"),
        dpt_d   = list(base = 0.97, lower = 0.95, upper = 0.99, dist = "fixed"),
        dpt_t   = list(base = 0.90, lower = 0.85, upper = 0.97, dist = "fixed"),
        opv3    = list(base = 0.95, lower = 0.90, upper = 0.99, dist = "fixed")
      ),
      covid_states = c(
        "Andhra Pradesh","Bihar","Gujarat","Haryana","Himachal Pradesh",
        "Karnataka","Kerala","Maharashtra","Punjab","Rajasthan",
        "Tamil Nadu","Telangana","Uttar Pradesh"
      )
    )
    
    # IMI annuity factor at base 2-year horizon
    params$annuity_imi <- (1 - (1 + params$discount_rate)^(-params$horizon_imi_base)) /
      params$discount_rate
    
    
    ### Sanity Checks ----
    
    cat("\n===== Section 1 summary =====\n")
    cat("\nState lookup:", nrow(state_lookup), "rows.\n")
    cat("\nDiD panel:\n")
    cat("  rows total           :", nrow(did_panel), "\n")
    cat("  rows for coverage DiD:", sum(!is.na(did_panel$coverage)), "\n")
    cat("  rows for Bayesian DiD:", nrow(did_panel_bayes), "\n")
    cat("\nBayesian DiD exclusions (NA ci):\n")
    print(did_panel |> filter(is.na(ci)) |> select(round, state_ut, n, se_ci))
    cat("\nU5MR scalar fallback cells set to 1.0:",
        sum(u5mr_qs$u5mr_scalar == 1.0), "\n")
    cat("\nWTP threshold (base case):", params$wtp_base, "INR per DALY.\n")
    cat("Annuity factor (HWC):", params$annuity_T, "(r =", params$discount_rate,
        ", T =", params$horizon_years, "yr).\n")
    cat("Annuity factor (IMI):", round(params$annuity_imi, 3),
        "(r =", params$discount_rate, ", T =", params$horizon_imi_base, "yr).\n")
    cat("Immunisation cost allocation (HWC base):",
        params$immune_alloc_base * 100, "% of HWC cost\n")
    
    cat("\n----- IMI exposure data (state-level treatment variable) -----\n")
    cat("  states with IMI data:", nrow(imi_exposure), "\n")
    cat("  total Phase 1 districts:", sum(imi_exposure$imi_phase_1_districts, na.rm = TRUE), "\n")
    cat("  total IMI 2.0 districts :", sum(imi_exposure$imi_2_0_districts, na.rm = TRUE), "\n")
    cat("  range of imi_exposure_intensity: [",
        round(min(imi_exposure$imi_exposure_intensity, na.rm = TRUE), 3), ",",
        round(max(imi_exposure$imi_exposure_intensity, na.rm = TRUE), 3), "]\n\n")
    cat("Top 5 highest-IMI states:\n")
    print(imi_exposure |>
            arrange(desc(imi_exposure_intensity)) |>
            select(state_ut, imi_phase_1_districts, imi_2_0_districts,
                   total_districts_2018, imi_exposure_intensity) |>
            head(5))
    cat("\nBottom 5 lowest-IMI states:\n")
    print(imi_exposure |>
            arrange(imi_exposure_intensity) |>
            select(state_ut, imi_phase_1_districts, imi_2_0_districts,
                   total_districts_2018, imi_exposure_intensity) |>
            head(5))
    
    cat("\n----- IMI cost data (state-level cost per dose) -----\n")
    cat("  states with IMI cost data:", nrow(imi_costs), "\n")
    cat("  Primary cost data:",
        sum(grepl("primary", imi_costs$data_status, ignore.case = TRUE), na.rm = TRUE), "\n")
    cat("  Imputed (pop-wtd mean):",
        sum(grepl("imputed", imi_costs$data_status, ignore.case = TRUE), na.rm = TRUE), "\n")
    cat("  range of cost_per_dose_inr_2021: [",
        round(min(imi_costs$cost_per_dose_inr_2021_mean, na.rm = TRUE)), ",",
        round(max(imi_costs$cost_per_dose_inr_2021_mean, na.rm = TRUE)), "] INR\n\n")
    cat("Primary cost data states:\n")
    print(imi_costs |>
            filter(grepl("primary", data_status, ignore.case = TRUE)) |>
            select(state_ut, cost_per_dose_usd_2019_mean, cost_per_dose_inr_2021_mean,
                   data_status))
    
    cat("\n----- IMI metadata parameters loaded -----\n")
    print(imi_metadata |> select(param_id, value, units) |> head(10))
    
    cat("\n----- IMI phaseweighted sensitivity (round-weighted with block discount) -----\n")
    cat("  rows in phaseweighted sheet :", nrow(imi_phaseweighted), "\n")
    cat("  range of weighted_intensity_full       : [",
        round(min(imi_phaseweighted$weighted_intensity_full, na.rm = TRUE), 3), ",",
        round(max(imi_phaseweighted$weighted_intensity_full, na.rm = TRUE), 3), "]\n")
    cat("  range of weighted_intensity_discounted : [",
        round(min(imi_phaseweighted$weighted_intensity_discounted, na.rm = TRUE), 3), ",",
        round(max(imi_phaseweighted$weighted_intensity_discounted, na.rm = TRUE), 3), "]\n")
    cat("  block-level states (UP, Bihar):\n")
    print(imi_phaseweighted |>
            filter(block_level_flag == 1) |>
            select(state_ut, weighted_intensity_full, weighted_intensity_discounted))
    
    cat("\nSection 1 complete.\n")
    
  }
  
  
  ## 2. STAGE 2A — COVERAGE DiD ----
  
  if (TRUE) {
    
    
    ### LIBRARIES ----
    
    library(fixest)
    library(broom)
    
    
    ### Load microdata ----
    
    kr <- inputs$kr_eligible |>
      filter(!is.na(fully_vaccinated))
    
    
    ### Build estimation panel ----
    
    treat_panel <- treat |>
      left_join(state_lookup, by = "state_ut") |>
      select(state_code = nfhs_code, x_s_primary, x_s_sens)
    
    imi_panel <- imi_exposure |>
      select(state_code = nfhs_state_code, imi_intensity = imi_exposure_intensity,
             imi_capped = imi_any_district_exposure)
    
    did_state_codes <- unique(did_panel$state_code)
    
    kr_did <- kr |>
      filter(state_code %in% did_state_codes) |>
      left_join(treat_panel, by = "state_code") |>
      left_join(imi_panel,   by = "state_code")
    
    cat("\nMicrodata after filter to DiD states:\n")
    cat("  rows  :", nrow(kr_did), "\n")
    cat("  states:", length(unique(kr_did$state_code)), "\n")
    cat("  rounds:", paste(unique(kr_did$round), collapse = ", "), "\n")
    cat("  IMI intensity merged:", sum(!is.na(kr_did$imi_intensity)), "rows\n")
    
    cat("\nState-level treatment variables (for verification):\n")
    print(kr_did |>
            distinct(state_code, x_s_primary, imi_intensity) |>
            arrange(state_code) |>
            head(10))
    
    
    ### Estimate primary specification ----
    # Joint HWC + IMI + interaction DiD:
    # FIC ~ post + post:HWC + post:IMI + post:HWC:IMI + state_FE
    
    did_fit <- feols(
      fully_vaccinated ~ post + post:x_s_primary + post:imi_intensity +
        post:x_s_primary:imi_intensity | state_code,
      data    = kr_did,
      weights = ~wt,
      cluster = ~state_code
    )
    
    
    ### Extract coefficients ----
    
    did_summary <- broom::tidy(did_fit)
    print(did_summary)
    
    # HWC main effect
    beta3_cov <- did_summary |> filter(term == "post:x_s_primary")
    beta3_cov_estimate <- beta3_cov$estimate
    beta3_cov_se       <- beta3_cov$std.error
    beta3_cov_p        <- beta3_cov$p.value
    
    # IMI main effect
    beta_imi <- did_summary |> filter(term == "post:imi_intensity")
    beta_imi_estimate <- if (nrow(beta_imi) > 0) beta_imi$estimate    else NA_real_
    beta_imi_se       <- if (nrow(beta_imi) > 0) beta_imi$std.error   else NA_real_
    beta_imi_p        <- if (nrow(beta_imi) > 0) beta_imi$p.value     else NA_real_
    
    # Interaction (complementarity)
    beta_inter <- did_summary |> filter(term == "post:x_s_primary:imi_intensity")
    beta_inter_estimate <- if (nrow(beta_inter) > 0) beta_inter$estimate  else NA_real_
    beta_inter_se       <- if (nrow(beta_inter) > 0) beta_inter$std.error else NA_real_
    beta_inter_p        <- if (nrow(beta_inter) > 0) beta_inter$p.value   else NA_real_
    
    
    ### Diagnostics ----
    
    cat("\n===== Stage 2a coverage DiD =====\n")
    print(summary(did_fit))
    
    cat("\nCausal estimands:\n")
    cat("  beta3_cov (HWC density × post):\n")
    cat("    estimate :", round(beta3_cov_estimate, 5), "\n")
    cat("    SE       :", round(beta3_cov_se, 5), "\n")
    cat("    95% CI   : [",
        round(beta3_cov_estimate - 1.96 * beta3_cov_se, 5), ",",
        round(beta3_cov_estimate + 1.96 * beta3_cov_se, 5), "]\n")
    cat("    p-value  :", format.pval(beta3_cov_p, digits = 3), "\n")
    
    cat("  beta_IMI (IMI intensity × post):\n")
    cat("    estimate :", round(beta_imi_estimate, 5), "\n")
    cat("    SE       :", round(beta_imi_se, 5), "\n")
    if (!is.na(beta_imi_se)) {
      cat("    95% CI   : [",
          round(beta_imi_estimate - 1.96 * beta_imi_se, 5), ",",
          round(beta_imi_estimate + 1.96 * beta_imi_se, 5), "]\n")
    }
    cat("    p-value  :", format.pval(beta_imi_p, digits = 3), "\n")
    
    cat("  beta_inter (HWC × IMI × post complementarity):\n")
    cat("    estimate :", round(beta_inter_estimate, 5), "\n")
    cat("    SE       :", round(beta_inter_se, 5), "\n")
    if (!is.na(beta_inter_se)) {
      cat("    95% CI   : [",
          round(beta_inter_estimate - 1.96 * beta_inter_se, 5), ",",
          round(beta_inter_estimate + 1.96 * beta_inter_se, 5), "]\n")
    }
    cat("    p-value  :", format.pval(beta_inter_p, digits = 3), "\n")
    
    cat("\n  n obs    :", nobs(did_fit), "\n")
    cat("  n states :", length(unique(kr_did$state_code)), "\n")
    
    
    ### Save outputs ----
    
    stage2a <- list(
      fit            = did_fit,
      summary        = did_summary,
      # HWC slots
      estimate       = beta3_cov_estimate,
      se             = beta3_cov_se,
      p_value        = beta3_cov_p,
      # IMI slots
      beta_imi_estimate   = beta_imi_estimate,
      beta_imi_se         = beta_imi_se,
      beta_imi_p          = beta_imi_p,
      beta_inter_estimate = beta_inter_estimate,
      beta_inter_se       = beta_inter_se,
      beta_inter_p        = beta_inter_p,
      n_obs               = nobs(did_fit),
      n_states            = length(unique(kr_did$state_code))
    )
    
    saveRDS(stage2a, file.path(out_dir, "stage2a_did_coverage.rds"))
    
    
    ### Sensitivity analysis: exclude ceiling states (NFHS-4 FIC > 0.80) ----
    
    # Ceiling effect: high-baseline states (Kerala, Punjab, Goa, Puducherry, etc.)
    # have little room for coverage to grow and several experienced post-period
    # declines (likely COVID-related). HWCs were preferentially deployed in these
    # already-high-baseline states, confounding the HWC density coefficient.
    # This sensitivity restricts the panel to states with NFHS-4 FIC < 0.80
    # to test whether the negative HWC coefficient is driven by ceiling states.
    
    cat("\n----- Sensitivity: ceiling-state exclusion (NFHS-4 FIC < 0.80) -----\n")
    
    ceiling_threshold <- 0.80
    
    # Identify ceiling states from cov_state at NFHS-4
    ceiling_states <- cov_state |>
      filter(round == "NFHS-4", coverage >= ceiling_threshold) |>
      pull(state_ut)
    
    cat("Ceiling states excluded (NFHS-4 FIC >=", ceiling_threshold, "):\n  ",
        paste(ceiling_states, collapse = ", "), "\n", sep = "")
    
    # Get state codes to match
    ceiling_codes <- state_lookup |>
      filter(state_ut %in% ceiling_states) |>
      pull(nfhs_code)
    
    kr_did_subsample <- kr_did |> filter(!state_code %in% ceiling_codes)
    
    cat("Subsample n: ", nrow(kr_did_subsample), " (",
        nrow(kr_did) - nrow(kr_did_subsample), " observations excluded)\n", sep = "")
    cat("Subsample states: ", length(unique(kr_did_subsample$state_code)),
        " (vs full panel ", length(unique(kr_did$state_code)), ")\n", sep = "")
    
    did_fit_sens <- feols(
      fully_vaccinated ~ post + post:x_s_primary + post:imi_intensity +
        post:x_s_primary:imi_intensity | state_code,
      data    = kr_did_subsample,
      weights = ~wt,
      cluster = ~state_code
    )
    
    did_summary_sens <- broom::tidy(did_fit_sens)
    
    cat("\nCeiling-states-excluded DiD coefficients:\n")
    print(did_summary_sens)
    
    cat("\nSide-by-side comparison (HWC main effect):\n")
    cat("  Full panel (32 states):     beta3_cov =",
        round(stage2a$estimate, 5), " (SE ",
        round(stage2a$se, 5), ")\n", sep = "")
    
    sens_main <- did_summary_sens |> filter(term == "post:x_s_primary")
    cat("  Excl ceiling (",
        length(unique(kr_did_subsample$state_code)), " states): beta3_cov =",
        round(sens_main$estimate, 5), " (SE ",
        round(sens_main$std.error, 5), ")\n", sep = "")
    
    stage2a_sens <- list(
      fit = did_fit_sens,
      summary = did_summary_sens,
      ceiling_states = ceiling_states,
      ceiling_threshold = ceiling_threshold,
      n_obs = nobs(did_fit_sens),
      n_states = length(unique(kr_did_subsample$state_code))
    )
    
    saveRDS(stage2a_sens,
            file.path(out_dir, "stage2a_did_coverage_sensitivity.rds"))
    
    cat("\nStage 2a complete. Saved to outputs/stage2a_did_coverage.rds\n")
    cat("Sensitivity saved to outputs/stage2a_did_coverage_sensitivity.rds\n")
    
  }
  
  
  ## 3. STAGE 2B — BAYESIAN EQUITY DiD ----
  
  if (TRUE) {
    
    
    ### LIBRARIES ----
    
    library(brms)
    library(posterior)
    
    
    ### Build estimation panel ----
    
    bayes_panel <- did_panel_bayes |>
      mutate(post = as.integer(round == "NFHS-5")) |>
      left_join(imi_panel, by = "state_code")
    
    
    ### Specify priors ----
    
    priors_bayes <- c(
      prior(normal(0, 0.1), class = "Intercept"),
      prior(normal(0, 0.1), class = "b"),
      prior(normal(0, 0.1), class = "sd", lb = 0)
    )
    
    
    ### Fit measurement-error hierarchical model ----
    # CI_st ~ post + post:x_s + post:imi_intensity + post:x_s:imi_intensity + (1|state)
    # with measurement-error specification on CI
    
    bayes_fit <- brm(
      formula = ci | se(se_ci, sigma = FALSE) ~
        post + x_s + imi_intensity +
        post:x_s + post:imi_intensity + post:x_s:imi_intensity +
        (1 | state_code),
      data    = bayes_panel,
      family  = gaussian(),
      prior   = priors_bayes,
      chains  = 4,
      iter    = 4000,
      warmup  = 2000,
      cores   = 4,
      seed    = 42,
      control = list(adapt_delta = 0.95)
    )
    
    
    ### Convergence diagnostics ----
    
    cat("\n===== Stage 2b Bayesian equity DiD =====\n")
    print(summary(bayes_fit))
    
    rhats <- rhat(bayes_fit)
    cat("\nMax R-hat       :", round(max(rhats, na.rm = TRUE), 4), "\n")
    cat("Params R-hat>1.01:", sum(rhats > 1.01, na.rm = TRUE), "of",
        length(rhats), "\n")
    
    
    ### Extract posterior draws (HWC, IMI, interaction) ----
    
    draws <- posterior::as_draws_df(bayes_fit)
    
    # HWC × post
    beta3_ci_draws <- draws[["b_post:x_s"]]
    
    # IMI × post
    beta_imi_ci_draws <- if ("b_post:imi_intensity" %in% names(draws))
      draws[["b_post:imi_intensity"]] else rep(NA_real_, nrow(draws))
    
    # HWC × IMI × post interaction
    beta_inter_ci_draws <- if ("b_post:x_s:imi_intensity" %in% names(draws))
      draws[["b_post:x_s:imi_intensity"]] else rep(NA_real_, nrow(draws))
    
    # Summary statistics
    beta3_ci_median <- median(beta3_ci_draws)
    beta3_ci_q025   <- quantile(beta3_ci_draws, 0.025)
    beta3_ci_q975   <- quantile(beta3_ci_draws, 0.975)
    prob_negative   <- mean(beta3_ci_draws < 0)
    
    beta_imi_ci_median <- median(beta_imi_ci_draws, na.rm = TRUE)
    beta_imi_ci_q025   <- quantile(beta_imi_ci_draws, 0.025, na.rm = TRUE)
    beta_imi_ci_q975   <- quantile(beta_imi_ci_draws, 0.975, na.rm = TRUE)
    prob_imi_negative  <- mean(beta_imi_ci_draws < 0, na.rm = TRUE)
    
    beta_inter_ci_median <- median(beta_inter_ci_draws, na.rm = TRUE)
    beta_inter_ci_q025   <- quantile(beta_inter_ci_draws, 0.025, na.rm = TRUE)
    beta_inter_ci_q975   <- quantile(beta_inter_ci_draws, 0.975, na.rm = TRUE)
    prob_inter_positive  <- mean(beta_inter_ci_draws > 0, na.rm = TRUE)
    
    
    ### Diagnostics ----
    
    cat("\nCausal estimand beta3_CI (HWC × post):\n")
    cat("  posterior median   :", round(beta3_ci_median, 5), "\n")
    cat("  95% credible interval: [",
        round(beta3_ci_q025, 5), ",",
        round(beta3_ci_q975, 5), "]\n")
    cat("  Pr(beta3_CI < 0)   :", round(prob_negative, 3), "\n")
    
    cat("\nCausal estimand beta_IMI_CI (IMI × post):\n")
    cat("  posterior median   :", round(beta_imi_ci_median, 5), "\n")
    cat("  95% credible interval: [",
        round(beta_imi_ci_q025, 5), ",",
        round(beta_imi_ci_q975, 5), "]\n")
    cat("  Pr(beta_IMI_CI < 0):", round(prob_imi_negative, 3),
        " (negative = pro-poor equity gain)\n")
    
    cat("\nCausal estimand beta_inter_CI (complementarity):\n")
    cat("  posterior median   :", round(beta_inter_ci_median, 5), "\n")
    cat("  95% credible interval: [",
        round(beta_inter_ci_q025, 5), ",",
        round(beta_inter_ci_q975, 5), "]\n")
    cat("  Pr(beta_inter > 0) :", round(prob_inter_positive, 3),
        " (positive = HWC and IMI reinforce each other)\n")
    
    cat("\n  n states           :", length(unique(bayes_panel$state_code)), "\n")
    cat("  n obs              :", nrow(bayes_panel), "\n")
    
    
    ### Save outputs ----
    
    stage2b <- list(
      fit              = bayes_fit,
      draws            = draws,
      # HWC slots
      beta3_ci_draws   = beta3_ci_draws,
      posterior_median = beta3_ci_median,
      posterior_ci_lo  = beta3_ci_q025,
      posterior_ci_hi  = beta3_ci_q975,
      prob_negative    = prob_negative,
      # IMI slots
      beta_imi_ci_draws    = beta_imi_ci_draws,
      beta_imi_ci_median   = beta_imi_ci_median,
      beta_imi_ci_q025     = beta_imi_ci_q025,
      beta_imi_ci_q975     = beta_imi_ci_q975,
      prob_imi_negative    = prob_imi_negative,
      beta_inter_ci_draws  = beta_inter_ci_draws,
      beta_inter_ci_median = beta_inter_ci_median,
      beta_inter_ci_q025   = beta_inter_ci_q025,
      beta_inter_ci_q975   = beta_inter_ci_q975,
      prob_inter_positive  = prob_inter_positive,
      n_states         = length(unique(bayes_panel$state_code)),
      n_obs            = nrow(bayes_panel),
      rhat_max         = max(rhats, na.rm = TRUE)
    )
    
    saveRDS(stage2b, file.path(out_dir, "stage2b_brms_fit.rds"))
    
    cat("\nStage 2b complete. Saved to outputs/stage2b_brms_fit.rds\n")
    
    # Posterior summary for fixed effects
    summary_fixed <- as.data.frame(summary(bayes_fit)$fixed)
    print(summary_fixed)
    
    # Manual extraction of the three coefficients of interest with sign-consistency probabilities
    draws <- as_draws_df(bayes_fit)
    
    stage2b_summary <- data.frame(
      term = c("post:x_s",
               "post:imi_intensity",
               "post:x_s:imi_intensity"),
      median = c(median(draws[["b_post:x_s"]]),
                 median(draws[["b_post:imi_intensity"]]),
                 median(draws[["b_post:x_s:imi_intensity"]])),
      q025   = c(quantile(draws[["b_post:x_s"]], 0.025),
                 quantile(draws[["b_post:imi_intensity"]], 0.025),
                 quantile(draws[["b_post:x_s:imi_intensity"]], 0.025)),
      q975   = c(quantile(draws[["b_post:x_s"]], 0.975),
                 quantile(draws[["b_post:imi_intensity"]], 0.975),
                 quantile(draws[["b_post:x_s:imi_intensity"]], 0.975)),
      pr_neg = c(mean(draws[["b_post:x_s"]] < 0),
                 mean(draws[["b_post:imi_intensity"]] < 0),
                 mean(draws[["b_post:x_s:imi_intensity"]] < 0))
    )
    
    print(stage2b_summary, row.names = FALSE)
    
    saveRDS(stage2b_summary, file.path(out_dir, "stage2b_did_equity.rds"))
    
  }
  
  
  ## 4. STAGE 3 — DETERMINISTIC ECEA ----
  
  if (TRUE) {
    
    
    ### Pre-compute state-level inputs ----
    
    # Marginal HWC upgrade cost per facility per year (INR 2021).
    # Calibrated so total all-India HWC cost matches MoHFW actuals for the
    # study window (FY 2019-20 actual = INR 16.5B; FY 2020-21 BE = INR 16.0B).
    # Source: CPR India Budget Brief 2021-22; PRS DFG analysis 2022-23.
    # Singh et al. 2021 facility-level estimates triangulated against actuals.
    # Verification: 25k SHC × 100k + 12k PHC × 1M + 1.5k UPHC × 2M ≈ INR 17.5B
    hwc_marginal_cost_per_facility <- c(
      "HWC-SC"   =   50000,
      "HWC-PHC"  =  500000,
      "HWC-UPHC" = 1000000
    )
    
    iphs_pop <- c("HWC-SC" = 5000, "HWC-PHC" = 30000, "HWC-UPHC" = 50000)
    
    did_state_uts <- did_panel |> distinct(state_ut) |> pull(state_ut)
    
    
    ### State HWC cost function ----
    
    build_state_hwc_cost <- function(iphs_vec     = iphs_pop,
                                     hwc          = hwc_costs,
                                     af_t         = params$annuity_T,
                                     marginal_vec = hwc_marginal_cost_per_facility) {
      hwc |>
        filter(state_ut %in% did_state_uts) |>
        mutate(
          marginal_cost      = marginal_vec[facility_type],
          facility_year_cost = hwc_count_mar2020 * marginal_cost
        ) |>
        group_by(state_ut) |>
        summarise(annual_cost = sum(facility_year_cost, na.rm = TRUE),
                  .groups = "drop") |>
        mutate(C_prog_s = annual_cost * af_t)
    }
    
    state_hwc_cost <- build_state_hwc_cost()
    
    
    ### ECEA function ----
    
    cause_to_ve <- function(ve_list) {
      c("Tuberculosis" = ve_list$bcg$base,
        "Diphtheria"   = ve_list$dpt_d$base,
        "Tetanus"      = ve_list$dpt_t$base,
        "Pertussis"    = ve_list$dpt3p$base,
        "Measles"      = ve_list$mcv1$base)
    }
    
    run_ecea <- function(beta3_cov,
                         ve_by_cause,
                         oop_by_quintile,
                         pov_hc_by_quintile,
                         pov_line,
                         af_t,
                         wtp,
                         immune_alloc = params$immune_alloc_base,
                         modifier_q   = rep(1, 5),
                         floor_global = TRUE,
                         state_cost   = state_hwc_cost,
                         u5mr_mult    = 1.0,
                         daly_rate_mult = 1.0,
                         floor_mode   = c("global","covid","none")) {
      
      floor_mode <- match.arg(floor_mode)
      
      ve_composite <- gbd_total_dalys |>
        mutate(ve = ve_by_cause[cause_name]) |>
        summarise(vc = sum(total_dalys * ve) / sum(total_dalys)) |>
        pull(vc)
      
      state_daly_rate <- gbd_dalys |>
        filter(year == 2019) |>
        group_by(state_ut) |>
        summarise(daly_rate_s = sum(daly_rate, na.rm = TRUE) * daly_rate_mult,
                  .groups = "drop")
      
      ecea_panel <- treat |>
        filter(state_ut %in% did_state_uts) |>
        dplyr::select(state_ut, x_s_primary, pop_12_23_2021) |>
        left_join(state_daly_rate, by = "state_ut") |>
        left_join(state_cost |> dplyr::select(state_ut, C_prog_s), by = "state_ut") |>
        tidyr::crossing(wealth_q = 1:5) |>
        left_join(pop_q |> filter(round == "NFHS-5") |>
                    dplyr::select(state_ut, wealth_q, prop_q),
                  by = c("state_ut", "wealth_q")) |>
        left_join(u5mr_qs |> filter(round == "NFHS-5") |>
                    dplyr::select(state_ut, wealth_q, u5mr_scalar),
                  by = c("state_ut", "wealth_q")) |>
        mutate(
          u5mr_scalar     = u5mr_scalar * u5mr_mult,
          # Per-VPD-episode OOP cost (INR 2021), anchored to Farooqui et al.
          # 2022 PLOS ONE estimates from NSSO 75th round (2017-18):
          #   Outpatient OOPE childhood infection: ~INR 1,200-1,800/episode
          #   Hospitalisation OOPE childhood infection: ~INR 5,000-8,000/episode
          #   Weighted (~80% OP, 20% hospitalisation): ~INR 2,200/episode
          # No quintile gradient applied — utilisation differences captured by
          # care-seeking probability elsewhere in the model.
          oop             = 2200,  # INR per VPD episode averted
          pov_hc_v        = pov_hc_by_quintile[wealth_q],
          # pov_line is a length-5 vector indexed by quintile.
          pov_line_v      = pov_line[wealth_q],
          modifier        = modifier_q[wealth_q],
          delta_cov_q_raw = beta3_cov * x_s_primary * modifier,
          delta_cov_q     = if (floor_mode == "global") {
            pmax(delta_cov_q_raw, 0)
          } else if (floor_mode == "covid") {
            ifelse(state_ut %in% params$covid_states,
                   pmax(delta_cov_q_raw, 0),
                   delta_cov_q_raw)
          } else { delta_cov_q_raw },
          pop_q_size      = prop_q * pop_12_23_2021,
          n_q             = delta_cov_q * pop_q_size,
          DALY_rate_qs    = daly_rate_s * u5mr_scalar,
          # Lifetime DALYs averted per FIC: protection over under-5 years
          # VE no longer applied because GBD daly_rate is already
          # the residual burden after current vaccination; n_q × daly_rate
          # approximates the disease prevented per newly-vaccinated child.
          B_health_q      = n_q * DALY_rate_qs * params$under5_protection_years / 1e5,
          PE_averted_q    = n_q * oop * params$under5_protection_years,
          pov_cases_q     = (PE_averted_q / pov_line_v) * pov_hc_v,
          # Annualise the 10-yr discounted cost so it matches the 1-cohort benefit time scale
          # Annual programme cost allocated to immunisation, distributed across
          # quintiles by share of cohort population (children 12-23m).
          # This is the total cost incurred to produce the policy effect,
          # not just the per-FIC marginal cost of new vaccinations.
          # Cost numerator: programme cost only (Verguet ECEA convention).
          # OOP averted is reported separately as financial risk protection
          # via pov_cases_q below; not netted into the cost-effectiveness ratio.
          C_prog_annual_state = (C_prog_s / af_t) * immune_alloc,
          C_prog_q            = C_prog_annual_state * pop_q_size / pop_12_23_2021,
          C_net_q             = C_prog_q
        )
      
      by_quintile <- ecea_panel |>
        group_by(wealth_q) |>
        summarise(
          n_q          = sum(n_q, na.rm = TRUE),
          B_health_q   = sum(B_health_q, na.rm = TRUE),
          PE_averted_q = sum(PE_averted_q, na.rm = TRUE),
          pov_cases_q  = sum(pov_cases_q, na.rm = TRUE),
          C_net_q      = sum(C_net_q, na.rm = TRUE),
          .groups = "drop"
        ) |>
        mutate(
          ICER_health_q = ifelse(B_health_q > 1e-6, C_net_q / B_health_q, NA_real_),
          ICER_FRP_q    = ifelse(pov_cases_q > 1e-6, C_net_q / pov_cases_q, NA_real_),
          NMB_q         = wtp * B_health_q - C_net_q
        )
      
      list(ve_composite = ve_composite,
           immune_alloc = immune_alloc,
           panel        = ecea_panel,
           by_quintile  = by_quintile)
    }
    
    
    ### Base-case execution ----
    
    ve_base_vec <- cause_to_ve(params$ve)
    oop_base    <- oop_q$oop_mean_2021
    pov_hc_base <- pov_hc$pov_hc_mean
    
    ecea_base <- run_ecea(
      beta3_cov          = stage2a$estimate,
      ve_by_cause        = ve_base_vec,
      oop_by_quintile    = oop_base,
      pov_hc_by_quintile = pov_hc_base,
      pov_line           = pov_line_q_vec,
      af_t               = params$annuity_T,
      wtp                = params$wtp_base,
      immune_alloc       = params$immune_alloc_base,
      modifier_q         = rep(1, 5),
      floor_mode         = "global"
    )
    
    cat("\n----- Sanity check: HWC budget magnitude vs MoHFW actuals -----\n")
    annual_hwc_modelled <- sum(state_hwc_cost$C_prog_s) / params$annuity_T
    cat("  Modelled all-India HWC annual cost: INR ",
        format(round(annual_hwc_modelled / 1e9, 1), big.mark = ","),
        " billion\n", sep = "")
    cat("  MoHFW actual FY 2019-20:            INR 16.5 billion (study window)\n")
    cat("  MoHFW budget  FY 2020-21:           INR 16.0 billion (study window)\n")
    cat("  MoHFW budget  FY 2022-23:           INR 50.0 billion (post-study)\n")
    cat("  Ratio (modelled / MoHFW study):     ",
        round(annual_hwc_modelled / 16.5e9, 2), "\n", sep = "")
    cat("  Target: ratio between 0.7x and 1.5x for face validity within study window.\n")
    cat("  Sources: CPR India Budget Brief 2021-22; PRS DFG analysis 2022-23.\n")
    
    cat("\n----- Sanity check: cost per FIC gained at upper-CI scenario -----\n")
    upper_ci <- stage2a$estimate + 1.96 * stage2a$se
    sanity <- run_ecea(beta3_cov = upper_ci,
                       ve_by_cause = ve_base_vec,
                       oop_by_quintile = oop_base,
                       pov_hc_by_quintile = pov_hc_base,
                       pov_line = pov_line_q_vec,
                       af_t = params$annuity_T,
                       wtp = params$wtp_base,
                       immune_alloc = params$immune_alloc_base,
                       floor_mode = "global")
    n_fic <- sum(sanity$by_quintile$n_q, na.rm = TRUE)
    c_total <- sum(sanity$by_quintile$C_net_q, na.rm = TRUE)
    if (n_fic > 0) {
      cat("  Total FIC gained (annual): ", format(round(n_fic), big.mark = ","), "\n", sep = "")
      cat("  Total cost (annual INR):   ", format(round(c_total), big.mark = ","), "\n", sep = "")
      cat("  Cost per FIC gained (INR): ", format(round(c_total / n_fic), big.mark = ","), "\n", sep = "")
      cat("  Cost per FIC gained (USD): ", round(c_total / n_fic / 74), "\n", sep = "")
      cat("  Literature anchor:         Chatterjee 2018 = $32 / Clarke-Deelder 2024 = $83 / OHE 2024 = $30-50\n")
    }
    
    ### Diagnostics — base case ----
    
    cat("\n===== Stage 3 base-case ECEA (global floor) =====\n")
    cat("VE composite (descriptive, not in benefit formula):",
        round(ecea_base$ve_composite, 4), "\n")
    cat("Under-5 protection years:", params$under5_protection_years, "\n")
    cat("β₃_cov used            :", round(stage2a$estimate, 5), "\n")
    cat("Immunisation allocation:", params$immune_alloc_base * 100, "% of HWC cost\n")
    cat("HWC programme cost (full, INR):",
        format(sum(state_hwc_cost$C_prog_s), big.mark = ","), "\n")
    cat("Cost allocated to immunisation (INR):",
        format(sum(state_hwc_cost$C_prog_s) * params$immune_alloc_base,
               big.mark = ","), "\n\n")
    cat("Quintile-level results (national aggregate):\n")
    print(ecea_base$by_quintile, n = 5, width = Inf)
    
    
    ### Diagnostic — state-level cost breakdown ----
    
    cat("\n----- State-level HWC programme cost (10-year discounted) -----\n")
    cat("Showing full cost; allocated immunisation cost is", 
        params$immune_alloc_base * 100, "% of these values.\n\n")
    state_cost_print <- state_hwc_cost |>
      arrange(desc(C_prog_s)) |>
      mutate(
        annual_cost_inr_b   = round(annual_cost / 1e9, 2),
        C_prog_s_inr_b      = round(C_prog_s    / 1e9, 2),
        C_alloc_inr_b       = round(C_prog_s * params$immune_alloc_base / 1e9, 2),
        share_pct           = round(100 * C_prog_s / sum(C_prog_s), 1)
      ) |>
      dplyr::select(state_ut, annual_cost_inr_b, C_prog_s_inr_b,
                    C_alloc_inr_b, share_pct)
    print(state_cost_print, n = Inf)
    
    
    ### Diagnostic — state-level ECEA breakdown ----
    
    cat("\n----- State-level ECEA outputs (sum across quintiles, base case) -----\n")
    state_ecea_print <- ecea_base$panel |>
      group_by(state_ut, x_s_primary) |>
      summarise(
        delta_cov_mean = mean(delta_cov_q, na.rm = TRUE),
        n_total        = sum(n_q,          na.rm = TRUE),
        B_health_total = sum(B_health_q,   na.rm = TRUE),
        PE_total       = sum(PE_averted_q, na.rm = TRUE),
        pov_total      = sum(pov_cases_q,  na.rm = TRUE),
        C_net_total    = sum(C_net_q,      na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(desc(C_net_total))
    print(state_ecea_print, n = Inf, width = Inf)
    
    
    ### Diagnostic — no-floor (COVID-only) comparator run ----
    
    ecea_nofloor <- run_ecea(
      beta3_cov          = stage2a$estimate,
      ve_by_cause        = ve_base_vec,
      oop_by_quintile    = oop_base,
      pov_hc_by_quintile = pov_hc_base,
      pov_line           = pov_line_q_vec,
      af_t               = params$annuity_T,
      wtp                = params$wtp_base,
      immune_alloc       = params$immune_alloc_base,
      modifier_q         = rep(1, 5),
      floor_mode         = "covid"
    )
    
    cat("\n----- Diagnostic: COVID-only-floor comparator (NOT for headline) -----\n")
    print(ecea_nofloor$by_quintile, n = 5, width = Inf)
    
    
    ### Save outputs ----
    
    saveRDS(ecea_base,    file.path(out_dir, "ecea_base.rds"))
    saveRDS(ecea_nofloor, file.path(out_dir, "ecea_nofloor_diag.rds"))
    
    cat("\nStage 3 complete. Saved to outputs/ecea_base.rds (and nofloor diagnostic)\n")
    
  }
  
  
  ## 4b. STAGE 3b — IMI COST-EFFECTIVENESS ----
  
  if (TRUE) {
    
    
    ### IMI ECEA function ----
    
    run_ecea_imi <- function(beta_imi,
                             ve_by_cause,
                             oop_by_quintile,
                             pov_hc_by_quintile,
                             pov_line,
                             wtp,
                             cost_per_dose_imi   = NULL,    # state-level vector
                             horizon_imi         = params$horizon_imi_base,
                             imi_persistence     = params$imi_persistence_base,
                             imi_displacement    = params$imi_displacement,
                             u5mr_mult           = 1.0,
                             daly_rate_mult      = 1.0,
                             modifier_q          = rep(1, 5),
                             floor_mode          = c("global","none")) {
      
      floor_mode <- match.arg(floor_mode)
      
      # VE composite (same as HWC arm)
      ve_composite <- gbd_total_dalys |>
        mutate(ve = ve_by_cause[cause_name]) |>
        summarise(vc = sum(total_dalys * ve) / sum(total_dalys)) |>
        pull(vc)
      
      # State DALY rates
      state_daly_rate <- gbd_dalys |>
        filter(year == 2019) |>
        group_by(state_ut) |>
        summarise(daly_rate_s = sum(daly_rate, na.rm = TRUE) * daly_rate_mult,
                  .groups = "drop")
      
      # IMI annuity (2-year base)
      af_imi <- (1 - (1 + params$discount_rate)^(-horizon_imi)) / params$discount_rate
      
      # Persistence multiplier: year-1 effect + persistence × year-1 effect over horizon
      # Effective effect = year-1 effect × (1 + persistence × (annuity_imi - 1))
      # Simplification: effect_total = year-1 × persistence_factor
      persistence_factor <- 1 + imi_persistence * (af_imi - 1)
      
      # State-level IMI cost: cost-per-dose × incremental doses (with displacement adjustment)
      if (is.null(cost_per_dose_imi)) {
        cost_lookup <- imi_costs |>
          select(state_ut, cost_per_dose_inr_2021_mean) |>
          rename(cost_per_dose = cost_per_dose_inr_2021_mean)
      } else {
        cost_lookup <- tibble(state_ut = names(cost_per_dose_imi),
                              cost_per_dose = cost_per_dose_imi)
      }
      
      ecea_panel <- treat |>
        filter(state_ut %in% did_state_uts) |>
        dplyr::select(state_ut, pop_12_23_2021) |>
        left_join(state_daly_rate, by = "state_ut") |>
        left_join(imi_exposure |>
                    dplyr::select(state_ut, imi_intensity = imi_exposure_intensity,
                                  imi_phase_1_districts, imi_2_0_districts),
                  by = "state_ut") |>
        left_join(cost_lookup, by = "state_ut") |>
        tidyr::crossing(wealth_q = 1:5) |>
        left_join(pop_q |> filter(round == "NFHS-5") |>
                    dplyr::select(state_ut, wealth_q, prop_q),
                  by = c("state_ut", "wealth_q")) |>
        left_join(u5mr_qs |> filter(round == "NFHS-5") |>
                    dplyr::select(state_ut, wealth_q, u5mr_scalar),
                  by = c("state_ut", "wealth_q")) |>
        mutate(
          imi_intensity   = ifelse(is.na(imi_intensity), 0, imi_intensity),
          cost_per_dose   = ifelse(is.na(cost_per_dose), 0, cost_per_dose),
          u5mr_scalar     = u5mr_scalar * u5mr_mult,
          oop             = 2200,  # INR per VPD episode averted (Farooqui 2022)
          pov_hc_v        = pov_hc_by_quintile[wealth_q],
          # pov_line is a length-5 vector indexed by quintile.
          pov_line_v      = pov_line[wealth_q],
          modifier        = modifier_q[wealth_q],
          # IMI year-1 coverage gain per district-round
          delta_cov_q_raw = beta_imi * imi_intensity * modifier,
          delta_cov_q     = if (floor_mode == "global") pmax(delta_cov_q_raw, 0)
          else delta_cov_q_raw,
          # Persistence factor is a multi-year integral; we keep cost and benefit
          # both on a 1-cohort/1-year basis. The persistence factor stays as a
          # cumulative-effect multiplier on benefits, but cost should also be
          # spread across the 2-year effect window: divide cost by horizon_imi.
          delta_cov_q     = delta_cov_q * persistence_factor,
          pop_q_size      = prop_q * pop_12_23_2021,
          n_q             = delta_cov_q * pop_q_size,
          DALY_rate_qs    = daly_rate_s * u5mr_scalar,
          # Lifetime DALYs averted per FIC: protection over under-5 years
          # VE no longer applied because GBD daly_rate is already
          # the residual burden after current vaccination; n_q × daly_rate
          # approximates the disease prevented per newly-vaccinated child.
          B_health_q      = n_q * DALY_rate_qs * params$under5_protection_years / 1e5,
          PE_averted_q    = n_q * oop * params$under5_protection_years,
          pov_cases_q     = (PE_averted_q / pov_line_v) * pov_hc_v,
          # Cost = incremental doses × cost-per-dose
          # Doses delivered = n_q × VE_composite × (vaccines per FIC ≈ 7) / displacement
          # Cost = state-level IMI cost × pop_q proportion
          # Simplified: state IMI cost ≈ cost_per_dose × Phase 1 + IMI 2.0 districts × 5,000 doses/district/round × 4 rounds × 2 phases
          # Empirical scaling: ~$13.7M for 40 districts ≈ $343K per district per IMI round
          # Scaling: state cost = imi_intensity × total_districts × $343K × 2 rounds (Phase 1 + IMI 2.0) × INR conversion
          # IMI cost anchored to Clarke-Deelder 2024: $13.7M for 40 districts
          # in IMI Phase 1 = ~INR 25M per district per phase (in 2021 INR).
          # Total state IMI cost = (Phase 1 + IMI 2.0 districts) × INR 25M.
          # Annualised over horizon_imi = 2 years.
          # Displacement adjustment: only ~63% of IMI doses are incremental
          # (rest displace routine vaccinations that would have happened anyway).
          imi_phase_1_dist  = ifelse(is.na(imi_phase_1_districts), 0,
                                     imi_phase_1_districts),
          imi_2_0_dist      = ifelse(is.na(imi_2_0_districts), 0,
                                     imi_2_0_districts),
          imi_total_dist    = imi_phase_1_dist + imi_2_0_dist,
          imi_cost_per_dist = 25e6,  # INR 25M per district-phase (Clarke-Deelder 2024)
          imi_state_cost    = imi_total_dist * imi_cost_per_dist *
            imi_displacement / horizon_imi,
          C_imi_q           = imi_state_cost * pop_q_size / pop_12_23_2021,
          C_net_q           = C_imi_q
        )
      
      by_quintile <- ecea_panel |>
        group_by(wealth_q) |>
        summarise(
          n_q          = sum(n_q, na.rm = TRUE),
          B_health_q   = sum(B_health_q, na.rm = TRUE),
          PE_averted_q = sum(PE_averted_q, na.rm = TRUE),
          pov_cases_q  = sum(pov_cases_q, na.rm = TRUE),
          C_net_q      = sum(C_net_q, na.rm = TRUE),
          .groups = "drop"
        ) |>
        mutate(
          ICER_health_q = ifelse(B_health_q > 1e-6, C_net_q / B_health_q, NA_real_),
          ICER_FRP_q    = ifelse(pov_cases_q > 1e-6, C_net_q / pov_cases_q, NA_real_),
          NMB_q         = wtp * B_health_q - C_net_q
        )
      
      list(ve_composite      = ve_composite,
           horizon_imi       = horizon_imi,
           imi_persistence   = imi_persistence,
           persistence_factor = persistence_factor,
           panel             = ecea_panel,
           by_quintile       = by_quintile)
    }
    
    
    ### Base-case IMI ECEA ----
    
    ecea_imi_base <- run_ecea_imi(
      beta_imi           = stage2a$beta_imi_estimate,
      ve_by_cause        = ve_base_vec,
      oop_by_quintile    = oop_base,
      pov_hc_by_quintile = pov_hc_base,
      pov_line           = pov_line_q_vec,
      wtp                = params$wtp_base,
      horizon_imi        = params$horizon_imi_base,
      imi_persistence    = params$imi_persistence_base,
      imi_displacement   = params$imi_displacement,
      modifier_q         = rep(1, 5),
      floor_mode         = "global"
    )
    
    
    ### Diagnostics — IMI base case ----
    
    cat("\n===== Stage 3b IMI cost-effectiveness =====\n")
    cat("beta_IMI used         :", round(stage2a$beta_imi_estimate, 5), "\n")
    cat("Horizon (years)       :", params$horizon_imi_base, "\n")
    cat("Persistence factor    :", round(ecea_imi_base$persistence_factor, 3), "\n")
    cat("Displacement adjustment:", params$imi_displacement, "\n")
    cat("Total IMI programme cost (INR):",
        format(sum(ecea_imi_base$panel$imi_state_cost, na.rm = TRUE),
               big.mark = ","), "\n\n")
    cat("Quintile-level IMI results:\n")
    print(ecea_imi_base$by_quintile, n = 5, width = Inf)
    
    
    ### Side-by-side comparison: HWC ICER vs IMI ICER ----
    
    cat("\n===== HWC vs IMI ICER comparison (base case, Q1 focus) =====\n")
    hwc_q1   <- ecea_base$by_quintile[1, ]
    imi_q1   <- ecea_imi_base$by_quintile[1, ]
    cat("HWC Q1: B =", round(hwc_q1$B_health_q, 1),
        " C =", round(hwc_q1$C_net_q, 0),
        " ICER =", round(hwc_q1$ICER_health_q, 0), "INR/DALY",
        " NMB =", round(hwc_q1$NMB_q, 0), "INR\n")
    cat("IMI Q1: B =", round(imi_q1$B_health_q, 1),
        " C =", round(imi_q1$C_net_q, 0),
        " ICER =", round(imi_q1$ICER_health_q, 0), "INR/DALY",
        " NMB =", round(imi_q1$NMB_q, 0), "INR\n")
    
    
    ### Save outputs ----
    
    saveRDS(ecea_imi_base, file.path(out_dir, "ecea_imi_base.rds"))
    
    cat("\nStage 3b complete. Saved to outputs/ecea_imi_base.rds\n")
    
  }
  
  
  ## 4c. STAGE 3c — Quintile-specific, Joint, Equity-anchored ECEA ----
  
  if (TRUE) {
    
    
    ### Quintile-specific ICERs and NMBs (HWC and IMI in long format) ----
    # Extracts the by_quintile output from Stage 3 (HWC) and Stage 3b (IMI)
    # and combines into a long-format table. Each row is one programme × one quintile.
    
    quintile_summary <- bind_rows(
      ecea_base$by_quintile     |> mutate(programme = "HWC"),
      ecea_imi_base$by_quintile |> mutate(programme = "IMI")
    ) |>
      select(programme, wealth_q, n_q, B_health_q, PE_averted_q, pov_cases_q,
             C_net_q, ICER_health_q, ICER_FRP_q, NMB_q)
    
    cat("\n===== Quintile-specific ICERs and NMBs =====\n")
    print(quintile_summary, n = Inf, width = Inf)
    
    
    ### Joint HWC + IMI annualised cost-effectiveness ----
    # HWC and IMI use different horizons (10 yr vs 2 yr). To combine them we
    # annualise both: divide cost and benefit by their respective annuity factor,
    # then sum the per-year quantities. The joint ICER is the per-year cost
    # divided by the per-year benefit; the joint NMB uses the base WTP threshold.
    
    hwc_ann <- ecea_base$by_quintile |>
      transmute(
        wealth_q,
        C_hwc_annual = C_net_q / params$annuity_T,
        B_hwc_annual = B_health_q / params$annuity_T
      )
    
    imi_ann <- ecea_imi_base$by_quintile |>
      transmute(
        wealth_q,
        C_imi_annual = C_net_q / params$annuity_imi,
        B_imi_annual = B_health_q / params$annuity_imi
      )
    
    joint_summary <- hwc_ann |>
      left_join(imi_ann, by = "wealth_q") |>
      mutate(
        C_joint_annual = C_hwc_annual + C_imi_annual,
        B_joint_annual = B_hwc_annual + B_imi_annual,
        ICER_joint = ifelse(B_joint_annual > 1e-6, C_joint_annual / B_joint_annual, NA_real_),
        NMB_joint  = params$wtp_base * B_joint_annual - C_joint_annual
      )
    
    cat("\n===== Joint HWC + IMI annualised analysis =====\n")
    cat("HWC annuity factor:", round(params$annuity_T, 3), "(T =", params$horizon_years, "yr)\n")
    cat("IMI annuity factor:", round(params$annuity_imi, 3), "(T =", params$horizon_imi_base, "yr)\n\n")
    print(joint_summary, n = Inf, width = Inf)
    
    
    ### Equity-anchored Q1 coverage gain implied by Bayesian beta3_CI ----
    # The Bayesian DiD on the concentration index found Pr(beta3_CI < 0) = 0.987
    # for HWC and Pr(beta_IMI_CI < 0) = 1.000 for IMI. A negative coefficient on
    # CI means pro-poor redistribution. We translate this to an implied
    # coverage gain at Q1 using a rank-centered concentration-index decomposition.
    #
    # Under pure redistribution (mean coverage unchanged), the implied
    # coverage shift in quintile q is:
    #   delta_cov_q = (mu_state * delta_CI / 0.40) * (R_q - 0.5)
    # where R_q is the fractional rank for quintile q (0.1, 0.3, ..., 0.9)
    # and delta_CI = beta_CI * x_s for HWC or beta_IMI_CI * imi_intensity for IMI.
    
    fractional_rank <- c(0.1, 0.3, 0.5, 0.7, 0.9)
    rank_centered   <- fractional_rank - 0.5  # -0.4, -0.2, 0, 0.2, 0.4
    rank_var        <- sum(rank_centered^2)   # 0.40
    
    # State baseline mean FIC from NFHS-4
    baseline_mu <- cov_state |>
      filter(round == "NFHS-4") |>
      select(state_ut, mu_baseline = coverage)
    
    # State HWC and IMI exposure
    state_exposure <- treat |>
      filter(state_ut %in% did_state_uts) |>
      select(state_ut, x_s_primary) |>
      left_join(imi_exposure |> select(state_ut, imi_intensity = imi_exposure_intensity),
                by = "state_ut")
    
    # Bayesian coefficients
    beta_ci_hwc <- stage2b$posterior_median
    beta_ci_imi <- stage2b$beta_imi_ci_median
    
    equity_anchored <- state_exposure |>
      left_join(baseline_mu, by = "state_ut") |>
      tidyr::crossing(wealth_q = 1:5) |>
      mutate(
        rank_centered_q  = rank_centered[wealth_q],
        delta_CI_state   = beta_ci_hwc * x_s_primary +
          beta_ci_imi * ifelse(is.na(imi_intensity), 0, imi_intensity),
        delta_cov_q_eq   = (mu_baseline * delta_CI_state / rank_var) * rank_centered_q
      )
    
    cat("\n===== Equity-anchored implied coverage gain by quintile (Bayesian DiD) =====\n")
    cat("HWC beta_CI median:", round(beta_ci_hwc, 5), "\n")
    cat("IMI beta_CI median:", round(beta_ci_imi, 5), "\n")
    cat("Sign convention: negative beta_CI = pro-poor (positive gain for Q1, negative for Q5)\n\n")
    
    cat("State-level implied Q1 coverage gain (top 10 states by gain):\n")
    print(equity_anchored |>
            filter(wealth_q == 1) |>
            select(state_ut, x_s_primary, imi_intensity, mu_baseline,
                   delta_CI_state, delta_cov_q_eq) |>
            arrange(desc(delta_cov_q_eq)) |>
            head(10))
    
    cat("\nNational quintile-level mean implied coverage gain:\n")
    print(equity_anchored |>
            group_by(wealth_q) |>
            summarise(
              n_states         = n(),
              mean_delta_cov   = mean(delta_cov_q_eq, na.rm = TRUE),
              median_delta_cov = median(delta_cov_q_eq, na.rm = TRUE),
              .groups = "drop"
            ))
    
    
    ### No-floor sensitivity for full panel (equity-redistribution interpretation) ----
    # The global floor pmax(delta_cov, 0) is conservative for cost-effectiveness
    # but masks redistribution patterns. Re-run Stage 3 base case with floor_mode
    # = "none" so that negative state-level coverage shifts enter the calculation.
    # Use only for redistribution interpretation, not for ICER reporting.
    
    ecea_nofloor_full <- run_ecea(
      beta3_cov          = stage2a$estimate,
      ve_by_cause        = ve_base_vec,
      oop_by_quintile    = oop_base,
      pov_hc_by_quintile = pov_hc_base,
      pov_line           = pov_line_q_vec,
      af_t               = params$annuity_T,
      wtp                = params$wtp_base,
      immune_alloc       = params$immune_alloc_base,
      modifier_q         = rep(1, 5),
      floor_mode         = "none"
    )
    
    cat("\n===== No-floor full-panel sensitivity (redistribution interpretation only) =====\n")
    cat("All states allowed negative delta_cov. NOT for ICER reporting.\n\n")
    print(ecea_nofloor_full$by_quintile, n = 5, width = Inf)
    
    
    ### Save outputs ----
    
    ### Bottom-40% vs Top-20% aggregate ICER (PMJAY-aligned policy framing) ----
    
    aggregate_quintiles <- quintile_summary |>
      mutate(group = case_when(
        wealth_q %in% 1:2 ~ "Bottom 40% (Q1+Q2)",
        wealth_q == 5     ~ "Top 20% (Q5)",
        TRUE              ~ "Middle 40% (Q3+Q4)"
      )) |>
      group_by(programme, group) |>
      summarise(
        n_q          = sum(n_q,          na.rm = TRUE),
        B_health_q   = sum(B_health_q,   na.rm = TRUE),
        PE_averted_q = sum(PE_averted_q, na.rm = TRUE),
        pov_cases_q  = sum(pov_cases_q,  na.rm = TRUE),
        C_net_q      = sum(C_net_q,      na.rm = TRUE),
        NMB_q        = sum(NMB_q,        na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        ICER_health = ifelse(B_health_q > 1e-6, C_net_q / B_health_q, NA_real_),
        ICER_FRP    = ifelse(pov_cases_q > 1e-6, C_net_q / pov_cases_q, NA_real_)
      )
    
    cat("\n===== Aggregate ICERs by policy-relevant grouping =====\n")
    print(aggregate_quintiles, n = Inf, width = Inf)
    
    
    ### Save outputs ----
    
    saveRDS(quintile_summary,    file.path(out_dir, "quintile_summary.rds"))
    saveRDS(joint_summary,       file.path(out_dir, "joint_summary.rds"))
    saveRDS(equity_anchored,     file.path(out_dir, "equity_anchored.rds"))
    saveRDS(ecea_nofloor_full,   file.path(out_dir, "ecea_nofloor_full.rds"))
    saveRDS(aggregate_quintiles, file.path(out_dir, "aggregate_quintiles.rds"))
    
    cat("\nStage 3c complete. Saved quintile, joint, equity-anchored, no-floor, and aggregate outputs.\n")
    
  }
  
  
  ## 4d. STAGE 3d — EQUITY-ANCHORED ECEA (combines level + redistribution) ----
  
  if (TRUE) {
    
    
    ### Equity-anchored ECEA function ----
    # Combines two causal estimands:
    #   (1) State-level average coverage shift from Stage 2a (frequentist DiD)
    #       delta_cov_avg_s = b1*x_s + b2*imi_s + b3*x_s*imi_s
    #   (2) State-level redistribution from Stage 2b (Bayesian equity DiD)
    #       delta_CI_s = c1*x_s + c2*imi_s + c3*x_s*imi_s
    # Quintile-specific gain decomposes the redistribution onto rank-centered
    # weights (Wagstaff rank-centered decomposition):
    #   delta_cov_q,s = delta_cov_avg_s + (mu_s * delta_CI_s / 0.40) * (R_q - 0.5)
    # No global floor: redistribution intentionally produces negative shifts
    # at upper quintiles to balance positive shifts at lower quintiles.
    
    run_ecea_equity <- function(beta_cov_main, beta_cov_imi, beta_cov_inter,
                                beta_ci_main,  beta_ci_imi,  beta_ci_inter,
                                ve_by_cause,
                                oop_by_quintile,
                                pov_hc_by_quintile,
                                pov_line,
                                wtp,
                                immune_alloc        = params$immune_alloc_base,
                                modifier_q          = rep(1, 5),
                                state_cost_hwc      = state_hwc_cost,
                                horizon_imi         = params$horizon_imi_base,
                                imi_persistence     = params$imi_persistence_base,
                                imi_displacement    = params$imi_displacement,
                                u5mr_mult           = 1.0,
                                daly_rate_mult      = 1.0) {
      
      # VE composite (national-burden weighted)
      ve_composite <- gbd_total_dalys |>
        mutate(ve = ve_by_cause[cause_name]) |>
        summarise(vc = sum(total_dalys * ve) / sum(total_dalys)) |>
        pull(vc)
      
      # State DALY rate
      state_daly_rate <- gbd_dalys |>
        filter(year == 2019) |>
        group_by(state_ut) |>
        summarise(daly_rate_s = sum(daly_rate, na.rm = TRUE) * daly_rate_mult,
                  .groups = "drop")
      
      # Baseline state mean coverage from NFHS-4
      baseline_mu <- cov_state |>
        filter(round == "NFHS-4") |>
        select(state_ut, mu_baseline = coverage)
      
      # Rank-centered weights for Wagstaff decomposition
      fractional_rank <- c(0.1, 0.3, 0.5, 0.7, 0.9)
      rank_centered   <- fractional_rank - 0.5
      rank_var        <- sum(rank_centered^2)
      
      # IMI-specific helpers
      af_imi <- (1 - (1 + params$discount_rate)^(-horizon_imi)) / params$discount_rate
      persistence_factor <- 1 + imi_persistence * (af_imi - 1)
      
      # IMI cost lookup
      imi_cost_lookup <- imi_costs |>
        select(state_ut, cost_per_dose_inr_2021_mean) |>
        rename(cost_per_dose = cost_per_dose_inr_2021_mean)
      
      ecea_panel <- treat |>
        filter(state_ut %in% did_state_uts) |>
        dplyr::select(state_ut, x_s_primary, pop_12_23_2021) |>
        left_join(imi_exposure |>
                    dplyr::select(state_ut, imi_intensity = imi_exposure_intensity,
                                  imi_phase_1_districts, imi_2_0_districts),
                  by = "state_ut") |>
        left_join(state_daly_rate, by = "state_ut") |>
        left_join(baseline_mu,     by = "state_ut") |>
        left_join(state_cost_hwc |> dplyr::select(state_ut, C_prog_s),
                  by = "state_ut") |>
        left_join(imi_cost_lookup, by = "state_ut") |>
        tidyr::crossing(wealth_q = 1:5) |>
        left_join(pop_q |> filter(round == "NFHS-5") |>
                    dplyr::select(state_ut, wealth_q, prop_q),
                  by = c("state_ut", "wealth_q")) |>
        left_join(u5mr_qs |> filter(round == "NFHS-5") |>
                    dplyr::select(state_ut, wealth_q, u5mr_scalar),
                  by = c("state_ut", "wealth_q")) |>
        mutate(
          imi_intensity   = ifelse(is.na(imi_intensity), 0, imi_intensity),
          mu_baseline     = ifelse(is.na(mu_baseline) | mu_baseline <= 0,
                                   0.7, mu_baseline),
          cost_per_dose   = ifelse(is.na(cost_per_dose), 0, cost_per_dose),
          u5mr_scalar     = u5mr_scalar * u5mr_mult,
          oop             = 2200,  # INR per VPD episode averted (Farooqui 2022)
          pov_hc_v        = pov_hc_by_quintile[wealth_q],
          # pov_line is a length-5 vector indexed by quintile.
          pov_line_v      = pov_line[wealth_q],
          modifier        = modifier_q[wealth_q],
          rank_centered_q = rank_centered[wealth_q],
          
          # State-level average coverage shift (Stage 2a)
          delta_cov_avg   = beta_cov_main  * x_s_primary +
            beta_cov_imi   * imi_intensity +
            beta_cov_inter * x_s_primary * imi_intensity,
          
          # State-level CI shift (Stage 2b)
          delta_CI        = beta_ci_main   * x_s_primary +
            beta_ci_imi    * imi_intensity +
            beta_ci_inter  * x_s_primary * imi_intensity,
          
          # Quintile redistribution component
          delta_cov_q_redist = (mu_baseline * delta_CI / rank_var) * rank_centered_q,
          
          # Combined: average + redistribution (no floor, allows negative at top quintile)
          delta_cov_q     = (delta_cov_avg + delta_cov_q_redist) * modifier,
          
          pop_q_size      = prop_q * pop_12_23_2021,
          n_q             = delta_cov_q * pop_q_size,
          DALY_rate_qs    = daly_rate_s * u5mr_scalar,
          # Lifetime DALYs averted per FIC: protection over under-5 years
          # VE no longer applied because GBD daly_rate is already
          # the residual burden after current vaccination; n_q × daly_rate
          # approximates the disease prevented per newly-vaccinated child.
          B_health_q      = n_q * DALY_rate_qs * params$under5_protection_years / 1e5,
          PE_averted_q    = n_q * oop * params$under5_protection_years,
          pov_cases_q     = (PE_averted_q / pov_line_v) * pov_hc_v,
          
          # HWC: total annual cost distributed by quintile cohort share
          C_prog_annual_state = (C_prog_s / params$annuity_T) * immune_alloc,
          C_hwc_alloc_q       = C_prog_annual_state * pop_q_size / pop_12_23_2021,
          # IMI cost anchored to Clarke-Deelder 2024 district-phase cost
          # (~INR 25M per district per IMI phase). Total state cost is
          # district count × per-district cost × displacement / horizon.
          imi_phase_1_dist    = ifelse(is.na(imi_phase_1_districts), 0,
                                       imi_phase_1_districts),
          imi_2_0_dist        = ifelse(is.na(imi_2_0_districts), 0,
                                       imi_2_0_districts),
          imi_total_dist      = imi_phase_1_dist + imi_2_0_dist,
          imi_state_cost      = imi_total_dist * 25e6 *
            imi_displacement / horizon_imi,
          C_imi_q             = imi_state_cost * pop_q_size / pop_12_23_2021,
          # Verguet ECEA convention: programme cost only enters C_net.
          C_net_q             = C_hwc_alloc_q + C_imi_q
        )
      
      by_quintile <- ecea_panel |>
        group_by(wealth_q) |>
        summarise(
          n_q          = sum(n_q,          na.rm = TRUE),
          B_health_q   = sum(B_health_q,   na.rm = TRUE),
          PE_averted_q = sum(PE_averted_q, na.rm = TRUE),
          pov_cases_q  = sum(pov_cases_q,  na.rm = TRUE),
          C_hwc_q      = sum(C_hwc_alloc_q, na.rm = TRUE),
          C_imi_q      = sum(C_imi_q,      na.rm = TRUE),
          C_net_q      = sum(C_net_q,      na.rm = TRUE),
          .groups = "drop"
        ) |>
        mutate(
          ICER_health_q = ifelse(B_health_q > 1e-6, C_net_q / B_health_q, NA_real_),
          ICER_FRP_q    = ifelse(pov_cases_q > 1e-6, C_net_q / pov_cases_q, NA_real_),
          NMB_q         = wtp * B_health_q - C_net_q
        )
      
      list(ve_composite = ve_composite,
           panel        = ecea_panel,
           by_quintile  = by_quintile)
    }
    
    
    ### Base-case equity-anchored ECEA ----
    
    ecea_equity_base <- run_ecea_equity(
      beta_cov_main      = stage2a$estimate,
      beta_cov_imi       = stage2a$beta_imi_estimate,
      beta_cov_inter     = stage2a$beta_inter_estimate,
      beta_ci_main       = stage2b$posterior_median,
      beta_ci_imi        = stage2b$beta_imi_ci_median,
      beta_ci_inter      = stage2b$beta_inter_ci_median,
      ve_by_cause        = ve_base_vec,
      oop_by_quintile    = oop_base,
      pov_hc_by_quintile = pov_hc_base,
      pov_line           = pov_line_q_vec,
      wtp                = params$wtp_base,
      immune_alloc       = params$immune_alloc_base
    )
    
    
    ### Diagnostics ----
    
    cat("\n===== Stage 3d Equity-Anchored ECEA =====\n")
    cat("Coefficients used (combined Stage 2a + Stage 2b):\n")
    cat("  Stage 2a (level coverage):\n")
    cat("    beta_cov_main (HWC)    :", round(stage2a$estimate, 5), "\n")
    cat("    beta_cov_imi  (IMI)    :", round(stage2a$beta_imi_estimate, 5), "\n")
    cat("    beta_cov_inter         :", round(stage2a$beta_inter_estimate, 5), "\n")
    cat("  Stage 2b (CI redistribution):\n")
    cat("    beta_ci_main  (HWC)    :", round(stage2b$posterior_median, 5), "\n")
    cat("    beta_ci_imi   (IMI)    :", round(stage2b$beta_imi_ci_median, 5), "\n")
    cat("    beta_ci_inter          :", round(stage2b$beta_inter_ci_median, 5), "\n\n")
    
    cat("Quintile-level results (level + redistribution combined, no floor):\n")
    print(ecea_equity_base$by_quintile, n = 5, width = Inf)
    
    cat("\nState-level implied Q1 coverage gain (top 10 by gain):\n")
    print(ecea_equity_base$panel |>
            filter(wealth_q == 1) |>
            select(state_ut, x_s_primary, imi_intensity, mu_baseline,
                   delta_cov_avg, delta_CI, delta_cov_q_redist, delta_cov_q) |>
            arrange(desc(delta_cov_q)) |>
            head(10))
    
    cat("\nState-level implied Q5 coverage gain (bottom 10 — redistribution losses):\n")
    print(ecea_equity_base$panel |>
            filter(wealth_q == 5) |>
            select(state_ut, x_s_primary, imi_intensity,
                   delta_cov_avg, delta_cov_q_redist, delta_cov_q) |>
            arrange(delta_cov_q) |>
            head(10))
    
    
    ### Save outputs ----
    
    saveRDS(ecea_equity_base, file.path(out_dir, "ecea_equity_base.rds"))
    
    cat("\nStage 3d complete. Saved equity-anchored ECEA results.\n")
    
  }
  
  
  ## 5. STAGE 4 — EXPLORATORY DATA ANALYSIS AND DESCRIPTIVES ----
  
  if (TRUE) {
    
    
    ### LIBRARIES ----
    
    library(srvyr)
    
    
    ### Microdata-based demographics (Table 1 population characteristics) ----
    
    cat("\n===== Stage 4: building Table 1 demographics from kr microdata =====\n")
    
    kr_table1 <- kr |>
      filter(!is.na(fully_vaccinated))
    
    kr_svy <- kr_table1 |>
      as_survey_design(weights = wt)
    
    sample_n <- kr_table1 |>
      group_by(round) |>
      summarise(n_unweighted = n(), .groups = "drop")
    
    # Note: kr_eligible.rds contains age_months but not maternal age (v012).
    # If maternal age is needed for Table 1, add v012 to the kr_eligible
    # selection in DHS_NFHS_Analysis.R Step 3c and re-export.
    continuous <- kr_svy |>
      group_by(round) |>
      summarise(
        mean_age_months   = survey_mean(age_months, na.rm = TRUE, vartype = NULL)
      )
    
    pct_female     <- kr_svy |> group_by(round) |>
      summarise(pct_female     = survey_mean(female == 1, na.rm = TRUE, vartype = NULL))
    pct_urban      <- kr_svy |> group_by(round) |>
      summarise(pct_urban      = survey_mean(urban  == 1, na.rm = TRUE, vartype = NULL))
    pct_sc_st      <- kr_svy |> group_by(round) |>
      summarise(pct_sc_st      = survey_mean(sc_st  == 1, na.rm = TRUE, vartype = NULL))
    
    educ_dist <- kr_svy |>
      filter(!is.na(mat_educ)) |>
      group_by(round, mat_educ) |>
      summarise(pct = survey_mean(vartype = NULL), .groups = "drop") |>
      mutate(category = case_when(
        mat_educ == 0 ~ "none",
        mat_educ == 1 ~ "primary",
        mat_educ == 2 ~ "secondary",
        mat_educ == 3 ~ "higher"
      )) |>
      select(round, category, pct) |>
      pivot_wider(names_from = category, values_from = pct,
                  names_prefix = "pct_educ_")
    
    wealth_dist <- kr_svy |>
      filter(!is.na(wealth_q)) |>
      group_by(round, wealth_q) |>
      summarise(pct = survey_mean(vartype = NULL), .groups = "drop") |>
      pivot_wider(names_from = wealth_q, values_from = pct,
                  names_prefix = "pct_wealth_q")
    
    fic_ci_national <- kr_svy |>
      group_by(round) |>
      summarise(fic_national = survey_mean(fully_vaccinated, na.rm = TRUE,
                                           vartype = NULL))
    
    table1_demographics <- sample_n |>
      left_join(continuous,      by = "round") |>
      left_join(pct_female,      by = "round") |>
      left_join(pct_urban,       by = "round") |>
      left_join(pct_sc_st,       by = "round") |>
      left_join(educ_dist,       by = "round") |>
      left_join(wealth_dist,     by = "round") |>
      left_join(fic_ci_national, by = "round")
    
    saveRDS(table1_demographics, file.path(out_dir, "table1_demographics.rds"))
    
    cat("\nTable 1 demographics (survey-weighted, NFHS-4 vs NFHS-5):\n")
    print(table1_demographics, n = Inf, width = Inf)
    
    
    ### State-level descriptives (treatment, exposure, baseline, post) ----
    
    state_descriptives <- treat |>
      filter(state_ut %in% did_state_uts) |>
      select(state_ut, nfhs5_phase, x_s_primary,
             pop_12_23_2021, population_2021) |>
      left_join(imi_exposure |>
                  select(state_ut, imi_phase_1_districts,
                         imi_2_0_districts, total_districts_2018,
                         imi_exposure_intensity, imi_any_district_exposure),
                by = "state_ut") |>
      left_join(cov_state |>
                  filter(round == "NFHS-4") |>
                  select(state_ut, FIC_NFHS4 = coverage),
                by = "state_ut") |>
      left_join(cov_state |>
                  filter(round == "NFHS-5") |>
                  select(state_ut, FIC_NFHS5 = coverage),
                by = "state_ut") |>
      left_join(ci_state |>
                  filter(round == "NFHS-4") |>
                  select(state_ut, CI_NFHS4 = ci),
                by = "state_ut") |>
      left_join(ci_state |>
                  filter(round == "NFHS-5") |>
                  select(state_ut, CI_NFHS5 = ci),
                by = "state_ut") |>
      mutate(
        delta_FIC = FIC_NFHS5 - FIC_NFHS4,
        delta_CI  = CI_NFHS5  - CI_NFHS4
      )
    
    cat("\n===== State-level descriptives table =====\n")
    print(state_descriptives, n = Inf, width = Inf)
    
    
    ### National coverage and equity dynamics by round ----
    
    national_dynamics <- cov_state |>
      filter(state_ut %in% did_state_uts) |>
      group_by(round) |>
      summarise(
        n_states   = n(),
        mean_FIC   = mean(coverage, na.rm = TRUE),
        median_FIC = median(coverage, na.rm = TRUE),
        sd_FIC     = sd(coverage, na.rm = TRUE),
        .groups = "drop"
      ) |>
      left_join(
        ci_state |>
          filter(state_ut %in% did_state_uts) |>
          group_by(round) |>
          summarise(
            mean_CI   = mean(ci, na.rm = TRUE),
            median_CI = median(ci, na.rm = TRUE),
            sd_CI     = sd(ci, na.rm = TRUE),
            .groups = "drop"
          ),
        by = "round"
      )
    
    cat("\n===== National coverage and equity dynamics =====\n")
    print(national_dynamics, n = Inf, width = Inf)
    
    
    ### Quintile-level coverage dynamics across all DiD states ----
    
    quintile_dynamics <- cov_quintile |>
      filter(state_ut %in% did_state_uts) |>
      group_by(round, wealth_q) |>
      summarise(
        n_states   = n(),
        mean_FIC   = mean(coverage, na.rm = TRUE),
        median_FIC = median(coverage, na.rm = TRUE),
        sd_FIC     = sd(coverage, na.rm = TRUE),
        .groups = "drop"
      )
    
    cat("\n===== Quintile-level coverage dynamics (DiD states only) =====\n")
    print(quintile_dynamics, n = Inf, width = Inf)
    
    
    ### Pairwise correlations between treatment and outcome variables ----
    
    treatment_vars <- state_descriptives |>
      select(x_s_primary, imi_exposure_intensity, FIC_NFHS4, FIC_NFHS5,
             CI_NFHS4, CI_NFHS5, delta_FIC, delta_CI) |>
      filter(complete.cases(across(everything())))
    
    cat("\n===== Pairwise correlations between treatment and outcome variables =====\n")
    cor_mat <- cor(treatment_vars, use = "pairwise.complete.obs")
    print(round(cor_mat, 3))
    
    
    ### State-level changes between rounds (sorted by FIC change) ----
    
    state_shifts <- state_descriptives |>
      select(state_ut, nfhs5_phase, x_s_primary, imi_exposure_intensity,
             FIC_NFHS4, FIC_NFHS5, delta_FIC,
             CI_NFHS4, CI_NFHS5, delta_CI) |>
      arrange(desc(delta_FIC))
    
    cat("\n===== State-level coverage and equity changes (sorted by ΔFIC) =====\n")
    print(state_shifts, n = Inf, width = Inf)
    
    
    ### Population coverage breakdown for context ----
    
    population_summary <- treat |>
      filter(state_ut %in% did_state_uts) |>
      summarise(
        total_pop_2021  = sum(population_2021, na.rm = TRUE),
        total_pop_12_23 = sum(pop_12_23_2021,  na.rm = TRUE),
        n_states        = n()
      ) |>
      mutate(
        pop_share_12_23 = round(100 * total_pop_12_23 / total_pop_2021, 2)
      )
    
    cat("\n===== Population summary across DiD states =====\n")
    print(population_summary)
    
    
    ### EAG vs non-EAG state stratification (Indian policy framework) ----
    
    eag_states <- c("Uttar Pradesh", "Bihar", "Madhya Pradesh", "Rajasthan",
                    "Chhattisgarh", "Jharkhand", "Odisha", "Uttarakhand")
    
    eag_lookup <- state_descriptives |>
      mutate(eag_status = ifelse(state_ut %in% eag_states, "EAG", "non-EAG")) |>
      select(state_ut, eag_status)
    
    eag_descriptives <- state_descriptives |>
      left_join(eag_lookup, by = "state_ut") |>
      group_by(eag_status) |>
      summarise(
        n_states           = n(),
        total_pop_12_23    = sum(pop_12_23_2021,   na.rm = TRUE),
        mean_x_s           = mean(x_s_primary,     na.rm = TRUE),
        mean_imi           = mean(imi_exposure_intensity, na.rm = TRUE),
        mean_FIC_NFHS4     = mean(FIC_NFHS4,       na.rm = TRUE),
        mean_FIC_NFHS5     = mean(FIC_NFHS5,       na.rm = TRUE),
        mean_delta_FIC     = mean(delta_FIC,       na.rm = TRUE),
        mean_CI_NFHS4      = mean(CI_NFHS4,        na.rm = TRUE),
        mean_CI_NFHS5      = mean(CI_NFHS5,        na.rm = TRUE),
        mean_delta_CI      = mean(delta_CI,        na.rm = TRUE),
        .groups = "drop"
      )
    
    cat("\n===== EAG vs non-EAG state-group descriptives =====\n")
    cat("EAG states (Empowered Action Group):", paste(eag_states, collapse = ", "), "\n\n")
    print(eag_descriptives, n = Inf, width = Inf)
    
    
    ### EAG vs non-EAG aggregate ICER from Stage 3d equity-anchored panel ----
    # Computed only if ecea_equity_base is in scope (Stage 3d completed).
    
    if (exists("ecea_equity_base")) {
      
      eag_state_panel <- ecea_equity_base$panel |>
        left_join(eag_lookup, by = "state_ut")
      
      eag_aggregate <- eag_state_panel |>
        group_by(eag_status, wealth_q) |>
        summarise(
          n_q        = sum(n_q,        na.rm = TRUE),
          B_health_q = sum(B_health_q, na.rm = TRUE),
          C_net_q    = sum(C_net_q,    na.rm = TRUE),
          .groups    = "drop"
        ) |>
        mutate(
          ICER_health_q = ifelse(B_health_q > 1e-6, C_net_q / B_health_q, NA_real_),
          NMB_q         = params$wtp_base * B_health_q - C_net_q
        )
      
      cat("\n===== EAG vs non-EAG aggregate ECEA results (Stage 3d) =====\n")
      print(eag_aggregate, n = Inf, width = Inf)
      
      saveRDS(eag_aggregate, file.path(out_dir, "eag_aggregate_ecea.rds"))
    }
    
    
    ### Save EDA outputs ----
    
    saveRDS(state_descriptives,  file.path(out_dir, "state_descriptives.rds"))
    saveRDS(national_dynamics,   file.path(out_dir, "national_dynamics.rds"))
    saveRDS(quintile_dynamics,   file.path(out_dir, "quintile_dynamics.rds"))
    saveRDS(state_shifts,        file.path(out_dir, "state_shifts.rds"))
    saveRDS(cor_mat,             file.path(out_dir, "correlation_matrix.rds"))
    saveRDS(population_summary,  file.path(out_dir, "population_summary.rds"))
    saveRDS(eag_descriptives,    file.path(out_dir, "eag_descriptives.rds"))
    saveRDS(eag_lookup,          file.path(out_dir, "eag_lookup.rds"))
    
    cat("\nStage 4 EDA complete. Saved descriptives, Table 1 demographics, and EAG outputs.\n")
    
  }
  
  
  ## 6. STAGE 5 — COST-ALLOCATION SENSITIVITY (D1) ----
  
  if (TRUE) {
    
    
    ### ECEA function parameterised by alpha ----
    
    run_ecea_alpha <- function(beta3_cov, immune_alloc, wtp = params$wtp_base) {
      
      state_daly_rate <- gbd_dalys |>
        filter(year == 2019) |>
        group_by(state_ut) |>
        summarise(daly_rate_s = sum(daly_rate, na.rm = TRUE), .groups = "drop")
      
      panel <- treat |>
        filter(state_ut %in% did_state_uts) |>
        left_join(state_daly_rate, by = "state_ut") |>
        left_join(state_hwc_cost |> select(state_ut, C_prog_s), by = "state_ut") |>
        tidyr::crossing(wealth_q = 1:5) |>
        left_join(pop_q |> filter(round == "NFHS-5") |>
                    select(state_ut, wealth_q, prop_q),
                  by = c("state_ut", "wealth_q")) |>
        left_join(u5mr_qs |> filter(round == "NFHS-5") |>
                    select(state_ut, wealth_q, u5mr_scalar),
                  by = c("state_ut", "wealth_q")) |>
        mutate(
          oop                 = 2200,
          pov_hc_v            = pov_hc_base[wealth_q],
          # pov_line_q_vec is length-5, indexed by quintile.
          pov_line_v          = pov_line_q_vec[wealth_q],
          delta_cov_q         = pmax(beta3_cov * x_s_primary, 0),
          pop_q_size          = prop_q * pop_12_23_2021,
          n_q                 = delta_cov_q * pop_q_size,
          DALY_rate_qs        = daly_rate_s * u5mr_scalar,
          # Lifetime DALY framing — under5_protection_years multiplier;
          # VE not applied (see Stage 3 commentary)
          B_health_q          = n_q * DALY_rate_qs *
            params$under5_protection_years / 1e5,
          PE_averted_q        = n_q * oop * params$under5_protection_years,
          pov_cases_q         = (PE_averted_q / pov_line_v) * pov_hc_v,
          # Annualised cost; Verguet convention (programme cost only in C_net)
          C_prog_annual_state = (C_prog_s / params$annuity_T) * immune_alloc,
          C_prog_q            = C_prog_annual_state * pop_q_size / pop_12_23_2021,
          C_net_q             = C_prog_q
        )
      
      panel |>
        group_by(wealth_q) |>
        summarise(
          n_q          = sum(n_q,          na.rm = TRUE),
          B_health_q   = sum(B_health_q,   na.rm = TRUE),
          C_net_q      = sum(C_net_q,      na.rm = TRUE),
          PE_averted_q = sum(PE_averted_q, na.rm = TRUE),
          pov_cases_q  = sum(pov_cases_q,  na.rm = TRUE),
          .groups = "drop"
        ) |>
        mutate(
          ICER_health_q = ifelse(B_health_q > 1e-6, C_net_q / B_health_q, NA_real_),
          NMB_q         = wtp * B_health_q - C_net_q
        )
    }
    
    
    ### Run sensitivity grid ----
    
    alpha_grid <- c(0.01, 0.02, 0.03, 0.05, 0.07, 0.10, 0.15, 0.20, 0.25, 0.30)
    beta3_cov_point <- stage2a$estimate
    beta3_cov_upper <- stage2a$estimate + 1.96 * stage2a$se
    
    sensitivity_results <- map_dfr(alpha_grid, function(alpha) {
      res_point <- run_ecea_alpha(beta3_cov_point, alpha)
      res_upper <- run_ecea_alpha(beta3_cov_upper, alpha)
      bind_rows(
        res_point |> mutate(scenario = "Point estimate", alpha = alpha),
        res_upper |> mutate(scenario = "Upper 95% CI",   alpha = alpha)
      )
    })
    
    saveRDS(sensitivity_results, file.path(out_dir, "alpha_sensitivity.rds"))
    
    
    ### Diagnostic ----
    
    cat("\n===== Stage 5 cost-allocation sensitivity =====\n")
    cat("Alpha grid: ", paste(alpha_grid, collapse = ", "), "\n", sep = "")
    cat("Scenarios: point estimate (frequentist) and upper 95% CI\n\n")
    
    cat("Q1 ICER summary across alpha grid (upper 95% CI scenario):\n")
    print(sensitivity_results |>
            filter(scenario == "Upper 95% CI", wealth_q == 1) |>
            select(alpha, B_health_q, C_net_q, ICER_health_q, NMB_q),
          n = Inf, width = Inf)
    
    cat("\nStage 5 complete. Saved to outputs/alpha_sensitivity.rds\n")
    
  }
  
  
  ## 6b. STAGE 5b — UNCERTAINTY ANALYSIS ----
  
  if (TRUE) {
    
    
    ### LIBRARIES ----
    
    library(tibble)
    library(purrr)
    library(mgcv)
    
    
    ### 5.1 Helper: extract NMB_Q1 from a run_ecea() result ----
    
    nmb_q1  <- function(res) res$by_quintile$NMB_q[1]
    icer_q1 <- function(res) res$by_quintile$ICER_health_q[1]
    
    
    ### 5.2 OWSA — primary tornado on NMB_Q1 at base WTP ----
    
    owsa_param_grid <- tribble(
      ~param,                ~lower,                          ~upper,
      "beta3_cov",           stage2a$estimate - 1.96 * stage2a$se,
      stage2a$estimate + 1.96 * stage2a$se,
      "immune_alloc",        params$immune_alloc_lower,       params$immune_alloc_upper,
      "shc_unit_cost",       30000,                           80000,
      "phc_unit_cost",       300000,                          800000,
      "iphs_shc",            4000,                            6000,
      "iphs_phc",            24000,                           36000,
      "iphs_uphc",           40000,                           60000,
      "ve_bcg",              params$ve$bcg$lower,             params$ve$bcg$upper,
      "ve_dpt3p",            params$ve$dpt3p$lower,           params$ve$dpt3p$upper,
      "ve_mcv1",             params$ve$mcv1$lower,            params$ve$mcv1$upper,
      "ve_dpt_d",            params$ve$dpt_d$lower,           params$ve$dpt_d$upper,
      "ve_dpt_t",            params$ve$dpt_t$lower,           params$ve$dpt_t$upper,
      "u5mr_gradient",       1.0,                             1.5,
      "oop_q1",              13132.59,                        15894.99,
      "pov_hc_q1",           0.7917,                          0.8269,
      "pov_line",            pov_line_rural_2018 * 0.9,       pov_line_rural_2018 * 1.1,
      "discount_rate",       0.0,                             0.05,
      "time_horizon",        5,                               15
    )
    
    owsa_floor_scenarios <- tibble(
      param = "floor_mode",
      lower = "covid",
      upper = "none"
    )
    
    af_from_rT <- function(r, T_y) {
      if (r == 0) T_y else (1 - (1 + r)^(-T_y)) / r
    }
    
    run_owsa_one <- function(param_name, value, beta3 = stage2a$estimate,
                             wtp_use = params$wtp_base, mode = "global",
                             alloc = params$immune_alloc_base) {
      ve_vec <- ve_base_vec
      oop_vec <- oop_base
      pov_vec <- pov_hc_base
      # pov_line_use is a length-5 vector (per-quintile weighted line).
      # OWSA varies the line by ±10%; scale the vector uniformly by the
      # multiplier implied by the bound relative to the rural reference.
      pov_line_use <- pov_line_q_vec
      af_use <- params$annuity_T
      iphs_use <- iphs_pop
      shc_cost_override <- NULL
      phc_cost_override <- NULL
      u5_mult <- 1.0
      floor_use <- mode
      alloc_use <- alloc
      
      if      (param_name == "beta3_cov")       beta3 <- value
      else if (param_name == "immune_alloc")    alloc_use <- value
      else if (param_name == "shc_unit_cost")   shc_cost_override <- value
      else if (param_name == "phc_unit_cost")   phc_cost_override <- value
      else if (param_name == "iphs_shc")        iphs_use["HWC-SC"]   <- value
      else if (param_name == "iphs_phc")        iphs_use["HWC-PHC"]  <- value
      else if (param_name == "iphs_uphc")       iphs_use["HWC-UPHC"] <- value
      else if (param_name == "ve_bcg")          ve_vec["Tuberculosis"] <- value
      else if (param_name == "ve_dpt3p")        ve_vec["Pertussis"]    <- value
      else if (param_name == "ve_mcv1")         ve_vec["Measles"]      <- value
      else if (param_name == "ve_dpt_d")        ve_vec["Diphtheria"]   <- value
      else if (param_name == "ve_dpt_t")        ve_vec["Tetanus"]      <- value
      else if (param_name == "u5mr_gradient")   u5_mult <- value
      else if (param_name == "oop_q1")          oop_vec[1] <- value
      else if (param_name == "pov_hc_q1")       pov_vec[1] <- value
      else if (param_name == "pov_line")        pov_line_use <- pov_line_q_vec * (value / pov_line_rural_2018)
      else if (param_name == "discount_rate")   af_use <- af_from_rT(value, params$horizon_years)
      else if (param_name == "time_horizon")    af_use <- af_from_rT(params$discount_rate, value)
      else if (param_name == "floor_mode")      floor_use <- value
      
      marginal_vec_use <- hwc_marginal_cost_per_facility
      if (!is.null(shc_cost_override))
        marginal_vec_use["HWC-SC"] <- shc_cost_override
      if (!is.null(phc_cost_override)) {
        marginal_vec_use["HWC-PHC"]  <- phc_cost_override
        marginal_vec_use["HWC-UPHC"] <- phc_cost_override * 2
      }
      
      state_cost_use <- build_state_hwc_cost(iphs_vec     = iphs_use,
                                             hwc          = hwc_costs,
                                             af_t         = af_use,
                                             marginal_vec = marginal_vec_use)
      
      run_ecea(beta3_cov          = beta3,
               ve_by_cause        = ve_vec,
               oop_by_quintile    = oop_vec,
               pov_hc_by_quintile = pov_vec,
               pov_line           = pov_line_use,
               af_t               = af_use,
               wtp                = wtp_use,
               immune_alloc       = alloc_use,
               modifier_q         = rep(1, 5),
               floor_mode         = floor_use,
               state_cost         = state_cost_use,
               u5mr_mult          = u5_mult)
    }
    
    owsa_continuous <- owsa_param_grid |>
      mutate(lower = as.numeric(lower), upper = as.numeric(upper)) |>
      pmap_dfr(function(param, lower, upper) {
        nmb_lo <- nmb_q1(run_owsa_one(param, lower))
        nmb_hi <- nmb_q1(run_owsa_one(param, upper))
        tibble(param = param, value_lo = lower, value_hi = upper,
               nmb_lo = nmb_lo, nmb_hi = nmb_hi)
      })
    
    owsa_floor <- owsa_floor_scenarios |>
      pmap_dfr(function(param, lower, upper) {
        nmb_lo <- nmb_q1(run_owsa_one(param, lower))
        nmb_hi <- nmb_q1(run_owsa_one(param, upper))
        tibble(param = param, value_lo = lower, value_hi = upper,
               nmb_lo = nmb_lo, nmb_hi = nmb_hi)
      })
    
    nmb_base_q1 <- nmb_q1(ecea_base)
    
    owsa_nmb <- bind_rows(
      owsa_continuous |> mutate(value_lo = as.character(value_lo),
                                value_hi = as.character(value_hi)),
      owsa_floor
    ) |>
      mutate(
        delta_lo = nmb_lo - nmb_base_q1,
        delta_hi = nmb_hi - nmb_base_q1,
        range    = abs(nmb_hi - nmb_lo)
      ) |>
      arrange(desc(range))
    
    cat("\n===== Stage 5b: OWSA on NMB_Q1 at base WTP =====\n")
    print(owsa_nmb, n = Inf, width = Inf)
    
    
    ### 5.3 OWSA — supplementary tornado on NMB_Q1 at upper-CI β₃_cov ----
    
    beta3_upper_ci <- stage2a$estimate + 1.96 * stage2a$se
    
    nmb_base_q1_uci <- nmb_q1(run_owsa_one("beta3_cov", beta3_upper_ci))
    
    owsa_nmb_uci <- owsa_param_grid |>
      filter(param != "beta3_cov") |>
      mutate(lower = as.numeric(lower), upper = as.numeric(upper)) |>
      pmap_dfr(function(param, lower, upper) {
        nmb_lo <- nmb_q1(run_owsa_one(param, lower, beta3 = beta3_upper_ci))
        nmb_hi <- nmb_q1(run_owsa_one(param, upper, beta3 = beta3_upper_ci))
        tibble(param = param, value_lo = lower, value_hi = upper,
               nmb_lo = nmb_lo, nmb_hi = nmb_hi)
      }) |>
      mutate(
        delta_lo = nmb_lo - nmb_base_q1_uci,
        delta_hi = nmb_hi - nmb_base_q1_uci,
        range    = abs(nmb_hi - nmb_lo)
      ) |>
      arrange(desc(range))
    
    cat("\n===== OWSA on NMB_Q1 conditional on β₃_cov = +0.014 (upper 95% CI) =====\n")
    cat("Conditional base NMB_Q1:", format(nmb_base_q1_uci, big.mark = ","), "INR\n\n")
    print(owsa_nmb_uci, n = Inf, width = Inf)
    
    
    ### 5.4 PSA — 1,000 joint draws ----
    
    set.seed(42)
    n_psa <- params$n_psa
    
    beta_mom <- function(mu, sd) {
      v <- sd^2
      a <- mu * (mu * (1 - mu) / v - 1)
      b <- (1 - mu) * (mu * (1 - mu) / v - 1)
      list(a = a, b = b)
    }
    
    ve_bcg_mom   <- beta_mom(params$ve$bcg$base,
                             (params$ve$bcg$upper - params$ve$bcg$lower) / (2 * 1.96))
    ve_dpt3p_mom <- beta_mom(params$ve$dpt3p$base,
                             (params$ve$dpt3p$upper - params$ve$dpt3p$lower) / (2 * 1.96))
    ve_mcv1_mom  <- beta_mom(params$ve$mcv1$base,
                             (params$ve$mcv1$upper - params$ve$mcv1$lower) / (2 * 1.96))
    immune_alloc_mom <- beta_mom(params$immune_alloc_base,
                                 (params$immune_alloc_upper - params$immune_alloc_lower) / (2 * 1.96))
    
    beta3_cov_draws    <- rnorm(n_psa, stage2a$estimate, stage2a$se)
    beta3_ci_draws_psa <- sample(stage2b$beta3_ci_draws, n_psa, replace = TRUE)
    
    ve_bcg_draws       <- rbeta(n_psa, ve_bcg_mom$a,   ve_bcg_mom$b)
    ve_dpt3p_draws     <- rbeta(n_psa, ve_dpt3p_mom$a, ve_dpt3p_mom$b)
    ve_mcv1_draws      <- rbeta(n_psa, ve_mcv1_mom$a,  ve_mcv1_mom$b)
    immune_alloc_draws <- rbeta(n_psa, immune_alloc_mom$a, immune_alloc_mom$b)
    
    oop_draws <- map_dfc(seq_len(5), function(q) {
      shape <- oop_q$gamma_shape[q]
      rate  <- oop_q$gamma_rate_2021[q]
      tibble(!!paste0("oop_q", q) := rgamma(n_psa, shape, rate))
    })
    
    pov_q1_draws <- rbeta(n_psa, pov_hc$beta_alpha[1], pov_hc$beta_beta[1])
    pov_q2_draws <- rbeta(n_psa, pov_hc$beta_alpha[2], pov_hc$beta_beta[2])
    
    phc_row <- hwc_costs |> filter(facility_type == "HWC-PHC") |> slice(1)
    phc_unit_cost_draws <- rgamma(n_psa, shape = 36, rate = 36/500000)
    
    daly_state_year <- gbd_dalys |>
      filter(year == 2019) |>
      group_by(state_ut) |>
      summarise(rate_mean  = sum(daly_rate, na.rm = TRUE),
                rate_lower = sum(daly_lower, na.rm = TRUE),
                rate_upper = sum(daly_upper, na.rm = TRUE),
                .groups = "drop") |>
      mutate(
        log_mean = log(pmax(rate_mean, 1e-6)),
        log_sd   = (log(pmax(rate_upper, 1e-6)) - log(pmax(rate_lower, 1e-6))) / (2 * 1.96)
      )
    
    daly_rate_mult_draws <- exp(rnorm(n_psa, 0, mean(daly_state_year$log_sd, na.rm = TRUE)))
    
    cat("\n===== Stage 4: running PSA (", n_psa, " draws) =====\n", sep = "")
    
    psa_results <- map_dfr(seq_len(n_psa), function(k) {
      ve_vec <- c(
        Tuberculosis = ve_bcg_draws[k],
        Diphtheria   = params$ve$dpt_d$base,
        Tetanus      = params$ve$dpt_t$base,
        Pertussis    = ve_dpt3p_draws[k],
        Measles      = ve_mcv1_draws[k]
      )
      oop_vec <- as.numeric(oop_draws[k, ])
      pov_vec <- c(pov_q1_draws[k], pov_q2_draws[k], 0, 0, 0)
      
      marginal_vec_psa <- hwc_marginal_cost_per_facility
      marginal_vec_psa["HWC-PHC"]  <- phc_unit_cost_draws[k]
      marginal_vec_psa["HWC-UPHC"] <- phc_unit_cost_draws[k] * 2
      if (exists("shc_unit_cost_draws"))
        marginal_vec_psa["HWC-SC"] <- shc_unit_cost_draws[k]
      
      state_cost_psa <- build_state_hwc_cost(iphs_vec     = iphs_pop,
                                             hwc          = hwc_costs,
                                             af_t         = params$annuity_T,
                                             marginal_vec = marginal_vec_psa)
      
      res <- run_ecea(
        beta3_cov          = beta3_cov_draws[k],
        ve_by_cause        = ve_vec,
        oop_by_quintile    = oop_vec,
        pov_hc_by_quintile = pov_vec,
        pov_line           = pov_line_q_vec,
        af_t               = params$annuity_T,
        wtp                = params$wtp_base,
        immune_alloc       = immune_alloc_draws[k],
        modifier_q         = rep(1, 5),
        floor_mode         = "global",
        state_cost         = state_cost_psa,
        daly_rate_mult     = daly_rate_mult_draws[k]
      )
      
      bq <- res$by_quintile
      tibble(
        draw         = k,
        beta3_cov    = beta3_cov_draws[k],
        beta3_ci     = beta3_ci_draws_psa[k],
        ve_composite = res$ve_composite,
        immune_alloc = immune_alloc_draws[k],
        B_q1 = bq$B_health_q[1], B_q2 = bq$B_health_q[2], B_q3 = bq$B_health_q[3],
        B_q4 = bq$B_health_q[4], B_q5 = bq$B_health_q[5],
        C_q1 = bq$C_net_q[1],    C_q2 = bq$C_net_q[2],    C_q3 = bq$C_net_q[3],
        C_q4 = bq$C_net_q[4],    C_q5 = bq$C_net_q[5],
        NMB_q1 = bq$NMB_q[1], NMB_q2 = bq$NMB_q[2], NMB_q3 = bq$NMB_q[3],
        NMB_q4 = bq$NMB_q[4], NMB_q5 = bq$NMB_q[5]
      )
    })
    
    saveRDS(psa_results, file.path(out_dir, "psa_results.rds"))
    
    cat("\nPSA summary at base WTP (INR 73,500):\n")
    cat("  Pr(any positive B_q1):", round(mean(psa_results$B_q1 > 0), 3), "\n")
    cat("  Pr(NMB_q1 > 0)       :", round(mean(psa_results$NMB_q1 > 0), 3), "\n")
    cat("  Pr(NMB_q5 > 0)       :", round(mean(psa_results$NMB_q5 > 0), 3), "\n")
    
    
    ### 5.5 CEAC and CERAC ----
    
    wtp_grid <- seq(0, 200000, by = 1000)
    
    ceac <- map_dfr(wtp_grid, function(lambda) {
      tibble(
        wtp     = lambda,
        ceac_q1 = mean(lambda * psa_results$B_q1 - psa_results$C_q1 > 0),
        ceac_q2 = mean(lambda * psa_results$B_q2 - psa_results$C_q2 > 0),
        ceac_q3 = mean(lambda * psa_results$B_q3 - psa_results$C_q3 > 0),
        ceac_q4 = mean(lambda * psa_results$B_q4 - psa_results$C_q4 > 0),
        ceac_q5 = mean(lambda * psa_results$B_q5 - psa_results$C_q5 > 0)
      )
    })
    
    expected_loss_q <- function(B, C, lambda) {
      nmb <- lambda * B - C
      if (mean(nmb) >= 0) mean(pmax(-nmb, 0)) else mean(pmax(nmb, 0))
    }
    
    cerac <- map_dfr(wtp_grid, function(lambda) {
      tibble(
        wtp   = lambda,
        el_q1 = expected_loss_q(psa_results$B_q1, psa_results$C_q1, lambda),
        el_q2 = expected_loss_q(psa_results$B_q2, psa_results$C_q2, lambda),
        el_q3 = expected_loss_q(psa_results$B_q3, psa_results$C_q3, lambda),
        el_q4 = expected_loss_q(psa_results$B_q4, psa_results$C_q4, lambda),
        el_q5 = expected_loss_q(psa_results$B_q5, psa_results$C_q5, lambda)
      )
    })
    
    saveRDS(list(ceac = ceac, cerac = cerac),
            file.path(out_dir, "ceac_data.rds"))
    
    cat("\nCEAC at anchor thresholds:\n")
    print(ceac |> filter(wtp %in% c(44100, 73500, 147000)),
          n = Inf, width = Inf)
    
    
    ### 5.6 Save Stage 5b outputs ----
    
    saveRDS(owsa_nmb,     file.path(out_dir, "owsa_nmb.rds"))
    saveRDS(owsa_nmb_uci, file.path(out_dir, "owsa_nmb_conditional.rds"))
    
    cat("\nStage 5b complete. Saved OWSA, PSA, and CEAC/CERAC outputs.\n")
  }
  
  
  ## 6c. STAGE 5c — EQUITY-ANCHORED UNCERTAINTY (PSA + CEAC) ----
  # Sample from the Stage 2b posterior of the three CI coefficients jointly
  # and from the same Stage 5b PSA distributions for VE, OOP, headcount,
  # and unit costs. Compute equity-anchored NMB by quintile draw-by-draw.
  # CEAC across the WTP grid then summarises Pr(NMB_q > 0) for each
  # quintile, providing the equity-anchored complement to the level CEAC.
  
  if (TRUE) {
    
    library(tibble)
    library(purrr)
    
    cat("\n===== Stage 5c: Equity-anchored PSA and CEAC =====\n")
    cat("PSA draws sample CI coefficients from the Bayesian posterior;\n")
    cat("NMB computed via the equity-anchored ECEA (level + redistribution).\n\n")
    
    
    ### Equity-anchored PSA ----
    
    set.seed(42)
    n_psa_eq <- params$n_psa
    
    bayes_idx <- sample(length(stage2b$beta3_ci_draws), n_psa_eq, replace = TRUE)
    
    beta_ci_main_draws  <- stage2b$beta3_ci_draws[bayes_idx]
    beta_ci_imi_draws   <- stage2b$beta_imi_ci_draws[bayes_idx]
    beta_ci_inter_draws <- stage2b$beta_inter_ci_draws[bayes_idx]
    
    beta_cov_main_draws  <- rnorm(n_psa_eq, stage2a$estimate,            stage2a$se)
    beta_cov_imi_draws   <- rnorm(n_psa_eq, stage2a$beta_imi_estimate,   stage2a$beta_imi_se)
    beta_cov_inter_draws <- rnorm(n_psa_eq, stage2a$beta_inter_estimate, stage2a$beta_inter_se)
    
    cat("Running ", n_psa_eq, " equity-anchored PSA draws...\n", sep = "")
    
    psa_equity <- map_dfr(seq_len(n_psa_eq), function(k) {
      ve_vec <- c(
        Tuberculosis = ve_bcg_draws[k],
        Diphtheria   = params$ve$dpt_d$base,
        Tetanus      = params$ve$dpt_t$base,
        Pertussis    = ve_dpt3p_draws[k],
        Measles      = ve_mcv1_draws[k]
      )
      oop_vec <- as.numeric(oop_draws[k, ])
      pov_vec <- c(pov_q1_draws[k], pov_q2_draws[k], 0, 0, 0)
      
      marginal_vec_psa <- hwc_marginal_cost_per_facility
      marginal_vec_psa["HWC-PHC"]  <- phc_unit_cost_draws[k]
      marginal_vec_psa["HWC-UPHC"] <- phc_unit_cost_draws[k] * 2
      if (exists("shc_unit_cost_draws"))
        marginal_vec_psa["HWC-SC"] <- shc_unit_cost_draws[k]
      
      state_cost_psa <- build_state_hwc_cost(iphs_vec     = iphs_pop,
                                             hwc          = hwc_costs,
                                             af_t         = params$annuity_T,
                                             marginal_vec = marginal_vec_psa)
      
      res <- run_ecea_equity(
        beta_cov_main      = beta_cov_main_draws[k],
        beta_cov_imi       = beta_cov_imi_draws[k],
        beta_cov_inter     = beta_cov_inter_draws[k],
        beta_ci_main       = beta_ci_main_draws[k],
        beta_ci_imi        = beta_ci_imi_draws[k],
        beta_ci_inter      = beta_ci_inter_draws[k],
        ve_by_cause        = ve_vec,
        oop_by_quintile    = oop_vec,
        pov_hc_by_quintile = pov_vec,
        pov_line           = pov_line_q_vec,
        wtp                = params$wtp_base,
        immune_alloc       = params$immune_alloc_base,
        state_cost_hwc     = state_cost_psa,
        daly_rate_mult     = daly_rate_mult_draws[k]
      )
      
      bq <- res$by_quintile
      tibble(
        draw          = k,
        beta_cov_main = beta_cov_main_draws[k],
        beta_ci_main  = beta_ci_main_draws[k],
        beta_ci_imi   = beta_ci_imi_draws[k],
        B_q1 = bq$B_health_q[1], B_q2 = bq$B_health_q[2], B_q3 = bq$B_health_q[3],
        B_q4 = bq$B_health_q[4], B_q5 = bq$B_health_q[5],
        C_q1 = bq$C_net_q[1],    C_q2 = bq$C_net_q[2],    C_q3 = bq$C_net_q[3],
        C_q4 = bq$C_net_q[4],    C_q5 = bq$C_net_q[5],
        NMB_q1 = bq$NMB_q[1], NMB_q2 = bq$NMB_q[2], NMB_q3 = bq$NMB_q[3],
        NMB_q4 = bq$NMB_q[4], NMB_q5 = bq$NMB_q[5]
      )
    })
    
    cat("\nEquity-anchored PSA summary at base WTP (INR 73,500):\n")
    cat("  Pr(B_q1 > 0)        :", round(mean(psa_equity$B_q1 > 0), 3), "\n")
    cat("  Pr(NMB_q1 > 0)      :", round(mean(psa_equity$NMB_q1 > 0), 3), "\n")
    cat("  Pr(NMB_q5 > 0)      :", round(mean(psa_equity$NMB_q5 > 0), 3), "\n")
    cat("  Mean NMB_q1 (INR)   :", format(mean(psa_equity$NMB_q1), big.mark=","), "\n")
    cat("  Mean NMB_q5 (INR)   :", format(mean(psa_equity$NMB_q5), big.mark=","), "\n")
    
    
    ### Equity-anchored CEAC ----
    
    wtp_grid_eq <- seq(0, 200000, by = 1000)
    
    ceac_equity <- map_dfr(wtp_grid_eq, function(lambda) {
      tibble(
        wtp     = lambda,
        ceac_q1 = mean(lambda * psa_equity$B_q1 - psa_equity$C_q1 > 0),
        ceac_q2 = mean(lambda * psa_equity$B_q2 - psa_equity$C_q2 > 0),
        ceac_q3 = mean(lambda * psa_equity$B_q3 - psa_equity$C_q3 > 0),
        ceac_q4 = mean(lambda * psa_equity$B_q4 - psa_equity$C_q4 > 0),
        ceac_q5 = mean(lambda * psa_equity$B_q5 - psa_equity$C_q5 > 0)
      )
    })
    
    cat("\nEquity-anchored CEAC at anchor thresholds:\n")
    print(ceac_equity |> filter(wtp %in% c(44100, 73500, 147000)),
          n = Inf, width = Inf)
    
    
    ### Save Stage 5c outputs ----
    
    saveRDS(psa_equity,  file.path(out_dir, "psa_equity.rds"))
    saveRDS(ceac_equity, file.path(out_dir, "ceac_equity.rds"))
    
    cat("\nStage 5c complete. Saved equity-anchored PSA and CEAC.\n")
    
  }
  
  
}  #