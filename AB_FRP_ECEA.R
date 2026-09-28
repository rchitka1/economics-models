# AB FRP-ECEA — FINANCIAL RISK PROTECTION ECEA - CAPSTONE EXTENSION ----
#
#   Project     : Financial Risk Protection in Childhood Vaccination Under 
#                 Ayushman Bharat
#   Author      : Rohan Chitkara
#   Supervisor  : Bryan Patenaude
#
# AIMS
#
# Quantify the financial-protection of routine childhood vaccination delivered
# under the Ayushman Bharat Health and Wellness Centre (HWC) scale-up between
# NFHS-4 (2015-16) and NFHS-5 (2019-21), stratified by household wealth quintile
# across Indian states.
#
# RESEARCH QUESTION
#
# What is the distributional impact of HWC-era childhood vaccination on cases,
# DALYs, household out-of-pocket spending, catastrophic health expenditure,
# medical impoverishment, and money-metric value of insurance across the
# Indian wealth distribution, and is the policy cost-effective at the HTAIn
# and 1x GDPpc willingness-to-pay thresholds?
#
# OBJECTIVE
#
# Produce a static cohort Extended Cost-Effectiveness Analysis (ECEA) of
# routine childhood vaccination under Ayushman Bharat for the five
# in-scope vaccine-preventable diseases (tuberculosis, diphtheria, pertussis,
# tetanus, measles), generating decision-grade estimates of incremental cases
# averted, DALYs averted, OOP averted, CHE counts averted, impoverishment
# averted, money-metric value of insurance (MMVI), ICERs, net monetary
# benefit, Wagstaff concentration indices on every outcome, and probabilistic
# uncertainty bounds via OWSA, PSA, CEAC, CEAP, and distributional CEAC.
#
# METHODOLOGY BY SECTION
#
#   Stage 1   Setup and load. Loads bundled inputs (AB_FRP_inputs.rds),
#             binds global constants (CRRA, FX, WTP, CHE, household size,
#             routine and IMI derived costing).
#   Stage 2   Core analysis frame. Builds the state x quintile x disease
#             frame with NFHS population weights, u5mr scalars, and
#             per-FIC cost per state.
#   Stage 3   Antigen-specific Delta-coverage. Ingests NFHS-derived bcg/
#             dpt3/opv3/mcv1 changes; maps antigens to scope diseases;
#             computes incremental coverage.
#   Stage 4   Cases and DALYs averted via u5mr scalar burden allocation and
#             VE (capstone-aligned: BCG 0.50, DPT-D 0.97, DPT-P 0.85,
#             DPT-T 0.90, MCV1 0.85).
#   Stage 5   Per-case OOP composite by disease and quintile. Within-q
#             income CV from HCES, household-level OOP CV from sampling
#             gamma, Monte Carlo over the bivariate distribution.
#   Stage 6   Household-level distributions. CHE-10/25/40 counts and
#             medical impoverishment counts via Monte Carlo.
#   Stage 7   Money-Metric Value of Insurance (CRRA). Quintile care-seeking
#             from linear gradient.
#   Stage 8   Cost allocation. Two-arm full-programme cost: 2018 per-FIC
#             routine cost (national mean INR 2,833) PLUS 2021 per-dose
#             IMI cost weighted by derived state IMI exposure intensity.
#             Uptake-weighted allocation to (state x quintile x disease).
#             ICERs and NMB at HTAIn / 1x GDPpc.
#   Stage 9   Wagstaff bivariate concentration index over each outcome.
#   Stage 10  Uncertainty. OWSA over routine cost, IMI cost, CRRA,
#             household OOP CV, within-q income CV; 1,000-iter PSA on VE
#             Beta and gamma; CEAC; CEAP scatter; distributional CEAC by quintile.
#   Stage 11  Urban-rural sector stratified analysis.
#   Stage 12  Tables and figures. Tables 1, 2, 4, 6 + consolidated Table 7
#             parameter library. Figures 1 (7-panel quintile distribution),
#             2 (OWSA tornado + CEAC), 3 (CEAP), 4 (distributional CEAC).
#
# LIMITATIONS
#
#   Static cohort design  : single cohort year, no dynamic transmission.
#                           Indirect (herd) effects of pertussis and measles
#                           not captured; true DALYs averted are likely
#                           underestimated.
#   Care-seeking proxy    : National gradient with no state-level variation;
#                           CI-consistent linear-rank imputation
#   Cost transferability  : Per-FIC cost is a 7-state, 24-district national
#                           weighted mean applied uniformly; does not capture
#                           state-level cost heterogeneity. IMI per-dose cost
#                           is state-specific for 5 sampled states (UP, Bihar,
#                           MP, Rajasthan, Maharashtra); other IMI-implementing
#                           states carry the weighted national mean.
#   IMI exposure measure  : Cumulative district-rounds per state from capstone
#                           DiD; assumes 1 marginal dose per IMI-reached child
#                           as a fixed scalar.
#   PMJAY                 : Demand-side PMJAY OOP offset not modelled 
#                           (vaccination averts the entire OOP cost
#                           regardless of who would have paid).
#   VPD attribution       : Cost attribution to the 5 scope VPDs is implicit
#                           in the per-FIC anchor (FIC = BCG + DPT3 + OPV3 +
#                           MCV1, covering all 5 scope diseases). Vaccines
#                           outside scope (e.g. Hep B, rotavirus) excluded.
#
# DISCUSSION AND ANALYSIS OF RESULTS
#
# Headline takeaway. AB-HWC scale-up between NFHS-4 and NFHS-5 produces
# pro-poor health and financial-protection gains. DALY averted concentration
# index is strongly negative (CI ~ -0.39); FRP outcomes are more pro-poor
# still (CHE-40 ~ -0.49; impoverishment ~ -0.73). The bulk of impoverishment
# averted accrues to Q1+Q2 (~98%).
#
# Disease mix. Pertussis and measles contribute the largest share of DALYs
# averted (~84% combined), reflecting India GBD 2019 under-5 case-fatality
# ratios; DPT3-channel diseases (diphtheria, tetanus) and BCG (TB) contribute
# smaller shares. Pertussis dominance is a feature of the Indian context
# (high DPT3 coverage already, but residual high CFR in unvaccinated infants)
# and distinguishes this analysis from earlier Ethiopia ECEA work where
# measles dominates.
#
# MMVI gradient non-monotonicity. The MMVI panel is highest at Q2, not Q1.
# This is the care-seeking gradient propagating through the engine:
# Q1 has the largest burden but the lowest care-seeking probability (0.344),
# so its avertable insurance value is bounded by realised utilisation. Q2
# combines substantial burden with intermediate care-seeking (0.512),
# producing the largest MMVI per quintile. 
#
# Robustness. CEAC at P(CE) = 1 across the full WTP range under the v4
# infrastructure-attribution costing; expected to climb from P(CE) near 0 at
# WTP = 0 to P(CE) ~ 1 at WTP ~ 30,000-40,000 INR per DALY under v5 full-
# programme costing. CEAP scatter populates the trade-off quadrant (positive
# cost, positive DALYs) rather than the all-dominant pattern of v4.
#
# POLICY IMPLICATIONS
#
#   Allocative efficiency      : The intervention is cost-effective at all
#                                Indian WTP thresholds, including the
#                                conservative HTAIn (INR 40,000/DALY).
#                                Expansion of HWC routine immunisation is
#                                supported on standard CEA grounds.
#   Distributional equity      : Expansion delivers strongly pro-poor health
#                                and FRP benefits. AB-HWC reduces,
#                                vaccination-related inequities across the
#                                Indian wealth distribution.
#   Financial protection       : Per-cohort OOP averted (~INR 1.5 billion in
#                                base case) plus MMVI (~INR 12 billion) makes
#                                the FRP value comparable to the DALY-
#                                monetised health value.
#   IMI complementarity        : States with documented IMI campaign rounds
#                                receive the largest absolute health and FRP
#                                gains; campaign-style supply pushes are
#                                complementary to HWC infrastructure.
#   Operational implication    : The strongest pro-poor signals are on the
#                                CHE-40 and impoverishment thresholds,
#                                where 84-98% of cases averted accrue to
#                                Q1+Q2. Prioritising HWC scale-up in high-
#                                burden, low-coverage states (UP, Bihar, MP)
#                                is justified on both efficiency and equity
#                                grounds simultaneously.
#
#   Perspectives        : public payer (cost), household (FRP).
#   Reference currency  : 2020-21 INR.
#   Discount rate       : 3% per annum
#   CRRA coefficient    : eta = 3 base (OWSA 1-5).
#   WTP thresholds      : INR 40,000 (HTAIn); INR 146,000 (1x GDPpc 2020-21).
#
#   Required Files  : AB_FRP_inputs.rds (compiled inputs; extraction from survey data).
#   Output Dir      : FRP_ECEA_files/
#   Note            : If extraction R files required, please contact RC
#

# FRP-ECEA MODEL ----

if (TRUE) {
  
  
  ## 1. STAGE 1 - SETUP AND DATA LOAD ----
  
  if (TRUE) {
    
    
    ### LIBRARIES ----
    
    library(dplyr)
    library(tidyr)
    library(tibble)
    library(purrr)
    library(stringr)
    library(scales)
    library(ggplot2)
    library(patchwork)
    
    set.seed(20260510)
    
    
    ### FILE PATHS ----
    
    path_bundle <- "AB_FRP_inputs.rds"
    
    if (!file.exists(path_bundle))
      stop("FRP inputs bundle not found at ", path_bundle,
           " — run 01_build_AB_FRP_inputs.R first.")
    
    frp <- readRDS(path_bundle)
    cat("Loaded FRP inputs bundle: ", length(frp), " elements.\n", sep = "")
    
    out_dir <- "FRP_ECEA_files"
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    
    
    ### Helper Functions ----
    
    scalar <- function(nm) {
      k <- with(frp$R_constants, setNames(value, parameter))
      unname(k[nm])
    }
    
    # Lognormal mu, sigma from mean and CV (used in CHE, poverty, MMVI).
    ln_mu_sigma <- function(mean, cv) {
      sigma <- sqrt(log(1 + cv^2))
      mu    <- log(mean) - sigma^2 / 2
      list(mu = mu, sigma = sigma)
    }
    
    
    ### Scalar Parameters ----
    
    params <- list(
      crra             = scalar("crra_eta"),
      hh_size          = scalar("household_size"),
      c_routine_FIC    = scalar("c_routine_per_FIC_inr_2020_21"),
      c_routine_lower  = scalar("c_routine_per_FIC_inr_lower"),
      c_routine_upper  = scalar("c_routine_per_FIC_inr_upper"),
      imi_doses        = scalar("imi_doses_per_child"),
      care_natl        = scalar("care_seeking_national_fallback"),
      wtp_htain        = scalar("wtp_htain_inr_per_daly"),
      wtp_gdppc        = scalar("wtp_gdppc_2020_21_inr_per_daly"),
      che_low          = scalar("che_threshold_low"),
      che_mid          = scalar("che_threshold_mid"),
      che_high         = scalar("che_threshold_high"),
      psa_n            = as.integer(scalar("psa_n_iter")),
      mc_n             = as.integer(scalar("mc_n_per_cell")),
      fx_inr_usd       = scalar("fx_inr_per_usd_2020_21")
    )
    params$wtp_grid <- seq(0, params$wtp_gdppc * 1.1, length.out = 60)
    
    
    ### Scope and Care-Seeking Map ----
    
    scope_diseases <- frp$R_disease_antigen$disease
    
    states_in <- frp$R_state_key |>
      filter(in_frp_scope) |>
      select(state_ut, model_state_code)
    
    approach <- frp$R_metadata$coverage_approach
    
    
    ### Sanity Checks ----
    
    cat("\n===== Stage 1 summary =====\n")
    cat("States in scope   :", nrow(states_in), "\n")
    cat("Diseases          :", length(scope_diseases), "\n")
    cat("Coverage approach :", approach, "\n")
    cat("CRRA eta          :", params$crra, "\n")
    cat("WTP HTAIn         :", params$wtp_htain, "\n")
    cat("WTP 1x GDPpc      :", params$wtp_gdppc, "\n")
    cat("PSA iterations    :", params$psa_n, "\n")
    cat("MC draws per cell :", params$mc_n, "\n")
    
    cat("\nStage 1 complete.\n")
  }
  
  
  ## 2. STAGE 2 - CORE ANALYSIS FRAME ----
  
  if (TRUE) {
    
    
    ### NFHS-5 quintile population shares ----
    
    pop_q <- frp$R_nfhs_pop_quintile |>
      filter(round == "NFHS-5") |>
      select(model_state_code, wealth_q, prop_q)
    
    
    ### GBD under-5 burden per state x disease ----
    
    burden <- frp$R_gbd_under5 |>
      filter(measure_name %in% c("Incidence",
                                 "DALYs (Disability-Adjusted Life Years)"),
             location_name != "India") |>
      mutate(measure = if_else(measure_name == "Incidence", "incidence", "dalys"),
             disease = tolower(cause_name)) |>
      select(state_ut = location_name, disease, measure, val) |>
      pivot_wider(names_from = measure, values_from = val) |>
      rename(state_incidence = incidence, state_dalys = dalys) |>
      mutate(dalys_per_case = if_else(state_incidence > 0,
                                      state_dalys / state_incidence, 0))
    
    
    ### u5mr scalars per state x quintile ----
    # Quintile burden allocation. Floor near-zero scalars at 0.10; impute
    # NAs at quintile mean to avoid zero-burden cells.
    
    u5_q <- frp$R_nfhs_u5mr_qs |>
      filter(round == "NFHS-5") |>
      select(model_state_code, wealth_q, u5mr_scalar)
    
    q_means <- u5_q |>
      group_by(wealth_q) |>
      summarise(q_mean = mean(u5mr_scalar, na.rm = TRUE), .groups = "drop")
    
    u5_q <- u5_q |>
      left_join(q_means, by = "wealth_q") |>
      mutate(u5mr_scalar = coalesce(u5mr_scalar, q_mean),
             u5mr_scalar = pmax(u5mr_scalar, 0.10)) |>
      select(model_state_code, wealth_q, u5mr_scalar)
    
    
    ### VE, OOP, consumption joins ----
    
    ve <- frp$R_VE |>
      select(disease, ve = ve_mean, ve_alpha = beta_alpha, ve_beta = beta_beta)
    
    oop_disease <- frp$R_disease_oop_anchor |>
      select(disease, per_case_oop_national = per_case_oop_2020_21)
    
    consume_q <- frp$R_annual_consumption_quintile |>
      select(wealth_q, mpce_rural, mpce_urban,
             mpce_national_weighted, annual_hh_consumption)
    
    
    ### Frame assembly ----
    
    frame <- expand_grid(state_ut = states_in$state_ut,
                         wealth_q = 1:5,
                         disease  = scope_diseases) |>
      left_join(states_in,   by = "state_ut") |>
      left_join(pop_q,       by = c("model_state_code", "wealth_q")) |>
      left_join(burden,      by = c("state_ut", "disease")) |>
      left_join(ve,          by = "disease") |>
      left_join(oop_disease, by = "disease") |>
      left_join(consume_q,   by = "wealth_q") |>
      left_join(u5_q,        by = c("model_state_code", "wealth_q")) |>
      mutate(dalys_per_case = coalesce(dalys_per_case, 0))
    
    cat("\nStage 2 complete. Frame rows:", nrow(frame),
        "(expected", nrow(states_in) * 5 * length(scope_diseases), ").\n")
  }
  
  
  ## 3. STAGE 3 - ANTIGEN-SPECIFIC DELTA-COVERAGE (A1) OR FALLBACK (A2) ----
  # Each disease uses its antigen channel's NFHS-4 to NFHS-5 coverage 
  # change per state x quintile. Eliminates FIC-uniform double counting.
  # FIC change scaled by inverse antigen baseline gap,
  # dCov_d = dFIC * (1 - cov_d_baseline) / (1 - cov_FIC_baseline).
  
  if (TRUE) {
    
    if (grepl("^A1", approach)) {
      
      cat("\n===== Stage 3 — A1 antigen-specific Delta-coverage =====\n")
      
      
      ## A1 ingestion and pivot ----
      
      ac <- frp$R_antigen_coverage_quintile |>
        filter(!is.na(model_state_code)) |>
        pivot_wider(names_from = round, values_from = coverage,
                    id_cols = c(model_state_code, wealth_q, antigen_indicator),
                    values_fn = mean) |>
        rename(coverage_baseline = `NFHS-4`, coverage_post = `NFHS-5`) |>
        mutate(delta_cov = coalesce(coverage_post - coverage_baseline, 0))
      
      
      ## Map antigen indicator to disease ----
      
      cov_by_disease <- ac |>
        left_join(frp$R_antigen_disease_map, by = "antigen_indicator",
                  relationship = "many-to-many") |>
        filter(!is.na(disease)) |>
        select(model_state_code, wealth_q, disease,
               coverage_baseline, coverage_post, delta_cov)
      
      frame <- frame |>
        left_join(cov_by_disease,
                  by = c("model_state_code", "wealth_q", "disease")) |>
        mutate(across(c(coverage_baseline, coverage_post, delta_cov),
                      ~ coalesce(.x, 0)))
      
      cat("Mean delta_cov by disease:\n")
      print(frame |> group_by(disease) |>
              summarise(mean_dcov = round(mean(delta_cov, na.rm = TRUE), 3),
                        .groups   = "drop"))
      
    } else {
      
      cat("\n===== Stage 3 — A2 uncovered-share fallback =====\n")
      
      
      ## FIC delta and national antigen baselines ----
      
      fic <- frp$R_nfhs_coverage_quintile_FIC |>
        select(round, model_state_code, wealth_q, coverage) |>
        pivot_wider(names_from = round, values_from = coverage,
                    names_prefix = "fic_") |>
        mutate(delta_fic = coalesce(`fic_NFHS-5` - `fic_NFHS-4`, 0))
      
      natl_baseline <- c(tuberculosis = 0.955, diphtheria = 0.855,
                         pertussis    = 0.855, tetanus    = 0.855,
                         measles      = 0.879, FIC        = 0.765)
      
      frame <- frame |>
        left_join(fic |> select(model_state_code, wealth_q,
                                fic_baseline = `fic_NFHS-4`,
                                fic_post     = `fic_NFHS-5`, delta_fic),
                  by = c("model_state_code", "wealth_q")) |>
        mutate(coverage_baseline = natl_baseline[disease],
               coverage_post     = pmin(coverage_baseline +
                                          delta_fic * (1 - coverage_baseline) /
                                          (1 - natl_baseline["FIC"]), 0.99),
               delta_cov         = coverage_post - coverage_baseline)
    }
    
    cat("\nStage 3 complete.\n")
  }
  
  
  ## 4. STAGE 4 - CASES AND DALYs AVERTED (U5MR SCALAR ALLOCATION) ----
  # Quintile burden allocation:
  # state burden x (pop_q * u5mr_scalar) / sum_q(pop_q * u5mr_scalar).
  # Apportions burden to quintiles based on observed under-5 mortality risk
  # rather than naive population share alone.
  
  if (TRUE) {
    
    state_norm <- frame |>
      group_by(state_ut, disease) |>
      summarise(norm = sum(prop_q * u5mr_scalar, na.rm = TRUE),
                .groups = "drop")
    
    frame <- frame |>
      left_join(state_norm, by = c("state_ut", "disease")) |>
      mutate(burden_share_q = (prop_q * u5mr_scalar) / pmax(norm, 1e-9),
             cases_q        = state_incidence * burden_share_q,
             dalys_q        = state_dalys     * burden_share_q,
             cases_averted  = cases_q * ve * delta_cov,
             dalys_averted  = cases_averted * dalys_per_case)
    
    cat("\n===== Stage 4 — cases and DALYs averted =====\n")
    cat("Total cases averted (all diseases):",
        round(sum(frame$cases_averted, na.rm = TRUE)), "\n")
    cat("Total DALYs averted (all diseases):",
        round(sum(frame$dalys_averted, na.rm = TRUE)), "\n")
    
    cat("\nStage 4 complete.\n")
  }
  
  
  ## 5. STAGE 5 - PER-CASE OOP COMPOSITE BY DISEASE AND QUINTILE ----
  
  if (TRUE) {
    
    
    ### Quintile gradient from general OOP ----
    
    general_q <- frp$R_oop_quintile |>
      mutate(wealth_q = 1:5) |>
      select(wealth_q, mean_general = oop_total_mean_inr_2021)
    mean_overall <- mean(general_q$mean_general)
    
    
    ### Per-case OOP per disease x quintile ----
    
    oop_qd <- expand_grid(disease = scope_diseases, wealth_q = 1:5) |>
      left_join(general_q, by = "wealth_q") |>
      left_join(frp$R_disease_oop_anchor |>
                  select(disease, per_case_oop_national = per_case_oop_2020_21),
                by = "disease") |>
      left_join(frp$R_hh_oop_cv |> select(wealth_q, hh_oop_cv),
                by = "wealth_q") |>
      mutate(quintile_factor = mean_general / mean_overall,
             per_case_oop_qd = per_case_oop_national * quintile_factor)
    
    frame <- frame |>
      left_join(oop_qd |> select(disease, wealth_q,
                                 per_case_oop_qd, hh_oop_cv),
                by = c("disease", "wealth_q"))
    
    cat("\nStage 5 complete. Per-case OOP attached for", length(scope_diseases),
        "diseases x 5 quintiles.\n")
  }
  
  
  ## 6. STAGE 6 - CHE AND POVERTY VIA HOUSEHOLD LOGNORMAL MC ----
  # CHE counting integrates OOP and income jointly:
  # Pr(CHE | T) = Pr(OOP > T * income) over (income, OOP) pairs.
  # Pr(impoverish) = Pr(income > PL AND income - OOP < PL).
  # Both drawn from household-level lognormals; quintile-specific CVs derived
  # in inputs build script from NFHS data
  
  if (TRUE) {
    
    
    ### National-weighted annual poverty line ----
    
    pov_line_annual <- with(frp$R_poverty_lines_2020_21,
                            0.65 * pov_line_2020_21_inr_pcm[sector == "rural"] * params$hh_size * 12 +
                              0.35 * pov_line_2020_21_inr_pcm[sector == "urban"] * params$hh_size * 12)
    
    
    ### Quintile care-seeking from linear gradient ----
    # R_care_seeking_quintile in v4+ is a 5-row flat table keyed on wealth_q.
    # All diseases share the same fever/cough care-seeking proxy.
    
    care_q <- frp$R_care_seeking_quintile
    if (!is.null(care_q)) {
      frame <- frame |>
        left_join(care_q |> select(wealth_q, care_sought),
                  by = "wealth_q")
    } else {
      frame$care_sought <- params$care_natl
    }
    frame$care_sought <- coalesce(frame$care_sought, params$care_natl)
    
    
    ### Within-quintile income CV, national-weighted ----
    
    income_cv_q <- frp$R_within_q_income_cv |>
      pivot_wider(names_from = sector,
                  values_from = c(within_q_mean_relative, hh_income_cv)) |>
      mutate(hh_income_cv_national =
               0.65 * hh_income_cv_rural + 0.35 * hh_income_cv_urban) |>
      select(wealth_q, hh_income_cv_national)
    
    frame <- frame |> left_join(income_cv_q, by = "wealth_q")
    
    
    ### MC over (income, OOP) pairs per (disease, quintile) ----
    
    qd_keys <- frame |>
      distinct(disease, wealth_q, per_case_oop_qd, hh_oop_cv,
               annual_hh_consumption, hh_income_cv_national)
    
    mc_pr <- qd_keys |>
      rowwise() |>
      mutate(.tmp = list({
        ln_inc <- ln_mu_sigma(annual_hh_consumption, hh_income_cv_national)
        ln_oop <- ln_mu_sigma(per_case_oop_qd, hh_oop_cv)
        inc <- rlnorm(params$mc_n, meanlog = ln_inc$mu, sdlog = ln_inc$sigma)
        oop <- rlnorm(params$mc_n, meanlog = ln_oop$mu, sdlog = ln_oop$sigma)
        list(pr_che_low  = mean(oop > params$che_low  * inc),
             pr_che_mid  = mean(oop > params$che_mid  * inc),
             pr_che_high = mean(oop > params$che_high * inc),
             pr_pov      = mean(inc > pov_line_annual &
                                  (inc - oop) < pov_line_annual))
      })) |>
      mutate(pr_che_low  = .tmp$pr_che_low,
             pr_che_mid  = .tmp$pr_che_mid,
             pr_che_high = .tmp$pr_che_high,
             pr_pov      = .tmp$pr_pov) |>
      select(-.tmp) |>
      ungroup() |>
      select(disease, wealth_q, pr_che_low, pr_che_mid, pr_che_high, pr_pov)
    
    frame <- frame |>
      left_join(mc_pr, by = c("disease", "wealth_q")) |>
      mutate(oop_averted       = cases_averted * care_sought * per_case_oop_qd,
             che_low_averted   = cases_averted * care_sought * pr_che_low,
             che_mid_averted   = cases_averted * care_sought * pr_che_mid,
             che_high_averted  = cases_averted * care_sought * pr_che_high,
             pov_cases_averted = cases_averted * care_sought * pr_pov)
    
    cat("\nStage 6 complete. CHE and poverty probabilities computed for",
        nrow(qd_keys), "cells.\n")
  }
  
  
  ## 7. STAGE 7 - MONEY-METRIC VALUE OF INSURANCE (CRRA) ----
  # MMVI per case = (c - CE(c, X)) - E[X] under CRRA U(c) = c^(1-eta)/(1-eta).
  # CE solved by Monte Carlo over the household-level lognormal OOP. X capped
  # below c to keep utility well-defined.
  
  if (TRUE) {
    
    mmvi_per_case <- function(c, mean_oop, cv_oop, eta = params$crra, n = params$mc_n) {
      ln <- ln_mu_sigma(mean_oop, cv_oop)
      X  <- rlnorm(n, meanlog = ln$mu, sdlog = ln$sigma)
      X  <- pmin(X, c * 0.99)
      if (abs(eta - 1) < 1e-6) {
        ce <- exp(mean(log(c - X)))
      } else {
        ce <- mean((c - X)^(1 - eta))^(1 / (1 - eta))
      }
      (c - ce) - mean(X)
    }
    
    mmvi_qd <- qd_keys |>
      rowwise() |>
      mutate(mmvi_per_case = pmax(mmvi_per_case(annual_hh_consumption,
                                                per_case_oop_qd, hh_oop_cv), 0)) |>
      select(disease, wealth_q, mmvi_per_case) |>
      ungroup()
    
    frame <- frame |>
      left_join(mmvi_qd, by = c("disease", "wealth_q")) |>
      mutate(mmvi_averted = cases_averted * care_sought * mmvi_per_case)
    
    cat("\nStage 7 complete. Total MMVI:",
        round(sum(frame$mmvi_averted, na.rm = TRUE)), "INR.\n")
  }
  
  
  ## 8. STAGE 8 - FULL-PROGRAMME COST ALLOCATION, ICERs, NMB ----
  # Full-programme cost in v5: two arms summed at the state level.
  #   Routine arm: c_routine_per_FIC x FIC_attributable
  #   Campaign arm: c_IMI_per_dose x doses_per_child x cohort x exposure
  # State cost then apportioned to (quintile x disease) via uptake-weighted
  # allocation: state_cost x (dCov_q * pop_q) / sum_q(dCov_q * pop_q) / n_disease.
  # Falls back to naive pop-share if all dCov are non-positive for a state.
  
  if (TRUE) {
    
    
    ### Routine arm: per-FIC cost ----
    # FIC_attributable[s] = cohort[s] x (FIC_post[s] - FIC_pre[s])
    # NFHS-5 FIC and NFHS-4 FIC from R_nfhs_coverage_quintile_FIC, population-
    # weighted to the state level. Cohort proxied by pop_12_23_2021.
    
    fic_state <- frp$R_nfhs_coverage_quintile_FIC |>
      filter(!is.na(coverage)) |>
      group_by(state_ut, round) |>
      summarise(fic = mean(coverage, na.rm = TRUE),
                .groups = "drop") |>
      pivot_wider(names_from = round, values_from = fic,
                  names_prefix = "fic_") |>
      rename(fic_pre  = `fic_NFHS-4`,
             fic_post = `fic_NFHS-5`) |>
      mutate(delta_fic = pmax(fic_post - fic_pre, 0))
    
    cohort_state <- frp$R_treatment_intensity |>
      select(state_ut, pop_12_23_2021)
    
    routine_state <- frp$R_routine_cost_per_fic |>
      select(state_ut, c_routine = cost_per_fic_inr) |>
      left_join(fic_state,    by = "state_ut") |>
      left_join(cohort_state, by = "state_ut") |>
      mutate(fic_attributable = pop_12_23_2021 * delta_fic,
             cost_routine     = c_routine * fic_attributable) |>
      select(state_ut, c_routine, delta_fic, pop_12_23_2021,
             fic_attributable, cost_routine)
    
    
    ### Campaign arm: per-dose IMI cost ----
    # IMI cost[s] = c_IMI_per_dose[s] x doses_per_child x cohort[s] x
    #              imi_exposure_intensity[s]
    # Exposure intensity = cumulative district-rounds / total districts (from
    # capstone DiD). Zero in states with no documented IMI rounds.
    #
    # Per-state per-dose cost from the IMI cost source for the five sampled
    # states (UP, Bihar, Rajasthan, Assam, Maharashtra); remaining 24 states
    # carry the population-weighted national mean (INR 480, 2020-21).
    
    imi_state <- frp$R_imi_exposure |>
      select(state_ut, imi_exposure_intensity) |>
      left_join(frp$R_imi_costs |>
                  select(state_ut, c_imi = cost_per_dose_inr_2021_mean),
                by = "state_ut") |>
      left_join(cohort_state, by = "state_ut") |>
      mutate(c_imi               = coalesce(c_imi, 480),
             imi_doses_total     = imi_exposure_intensity *
               pop_12_23_2021 * params$imi_doses,
             cost_imi            = c_imi * imi_doses_total) |>
      select(state_ut, c_imi, imi_exposure_intensity,
             imi_doses_total, cost_imi)
    
    
    ### State-level total cost (routine + campaign) ----
    
    state_costs <- routine_state |>
      select(state_ut, cost_routine) |>
      left_join(imi_state |> select(state_ut, cost_imi), by = "state_ut") |>
      mutate(cost_imi         = coalesce(cost_imi, 0),
             state_cost_total = cost_routine + cost_imi)
    
    cat("\nFull-programme cost (national):\n")
    cat(sprintf("  Routine (per-FIC delivery): INR %s\n",
                format(round(sum(state_costs$cost_routine, na.rm = TRUE)),
                       big.mark = ",")))
    cat(sprintf("  Campaign (per-dose IMI, state-specific): INR %s\n",
                format(round(sum(state_costs$cost_imi, na.rm = TRUE)),
                       big.mark = ",")))
    cat(sprintf("  Total: INR %s\n",
                format(round(sum(state_costs$state_cost_total, na.rm = TRUE)),
                       big.mark = ",")))
    
    
    ### Uptake weights ----
    
    n_disease <- length(scope_diseases)
    weights <- frame |>
      mutate(weight_qd = pmax(delta_cov, 0) * prop_q)
    
    state_weight_total <- weights |>
      group_by(state_ut) |>
      summarise(state_weight_total = sum(weight_qd, na.rm = TRUE),
                .groups = "drop")
    
    frame <- weights |>
      left_join(state_weight_total, by = "state_ut") |>
      left_join(state_costs,        by = "state_ut") |>
      mutate(state_cost_qd =
               if_else(state_weight_total > 0,
                       state_cost_total * weight_qd / state_weight_total,
                       state_cost_total * prop_q / n_disease),
             net_cost_qd = state_cost_qd - oop_averted)
    
    
    ### Aggregate to (disease x quintile) and (quintile) ----
    
    results_qd <- frame |>
      group_by(disease, wealth_q) |>
      summarise(across(c(cases_averted, dalys_averted,
                         oop_averted,
                         che_low_averted, che_mid_averted, che_high_averted,
                         pov_cases_averted, mmvi_averted,
                         state_cost_qd, net_cost_qd),
                       ~ sum(.x, na.rm = TRUE)),
                .groups = "drop") |>
      mutate(icer_per_daly = if_else(dalys_averted > 0,
                                     net_cost_qd / dalys_averted, NA_real_),
             nmb_at_htain  = dalys_averted * params$wtp_htain - net_cost_qd,
             nmb_at_gdppc  = dalys_averted * params$wtp_gdppc - net_cost_qd)
    
    results_q_total <- results_qd |>
      group_by(wealth_q) |>
      summarise(across(c(cases_averted, dalys_averted,
                         oop_averted,
                         che_low_averted, che_mid_averted, che_high_averted,
                         pov_cases_averted, mmvi_averted,
                         state_cost_qd, net_cost_qd),
                       ~ sum(.x, na.rm = TRUE)),
                .groups = "drop") |>
      mutate(icer_per_daly = net_cost_qd / dalys_averted,
             nmb_at_htain  = dalys_averted * params$wtp_htain - net_cost_qd,
             nmb_at_gdppc  = dalys_averted * params$wtp_gdppc - net_cost_qd)
    
    cat("\nStage 8 complete. Aggregate net cost:",
        round(sum(results_q_total$net_cost_qd)), "INR.\n")
  }
  
  
  ## 9. STAGE 9 - WAGSTAFF BIVARIATE CONCENTRATION INDEX ----
  
  if (TRUE) {
    
    
    ### CI and pro-poor share helpers ----
    
    wagstaff_ci <- function(y, p = rep(0.2, 5)) {
      if (any(is.na(y))) return(NA_real_)
      R     <- cumsum(p) - 0.5 * p
      R_bar <- sum(p * R)
      mu    <- sum(p * y)
      if (mu <= 0) return(NA_real_)
      (2 / mu) * sum(p * (R - R_bar) * y)
    }
    
    pro_poor_share <- function(y) {
      if (sum(y, na.rm = TRUE) <= 0) return(NA_real_)
      sum(y[1:2], na.rm = TRUE) / sum(y, na.rm = TRUE)
    }
    
    
    ### Compute across outcomes ----
    
    ci_outcomes <- c("dalys_averted", "oop_averted",
                     "che_low_averted", "che_mid_averted", "che_high_averted",
                     "pov_cases_averted", "mmvi_averted")
    
    equity_summary <- tibble(
      outcome             = ci_outcomes,
      concentration_index = vapply(ci_outcomes, function(o)
        wagstaff_ci(results_q_total[[o]]), numeric(1)),
      pro_poor_share      = vapply(ci_outcomes, function(o)
        pro_poor_share(results_q_total[[o]]), numeric(1))
    )
    
    cat("\nStage 9 complete. Equity summary:\n")
    print(equity_summary |> mutate(across(where(is.numeric), ~ round(.x, 3))))
  }
  
  ## 10. STAGE 10 - UNCERTAINTY: OWSA, PSA, CEAC ----
  # OWSA over routine cost, IMI cost, CRRA, household OOP CV, and within-q
  # income CV. Each parameter is varied against the outcome it actually
  # moves: cost parameters against NMB, CRRA and OOP CV against MMVI, OOP CV
  # and income CV against CHE.
  
  if (TRUE) {
    
    
    ### NMB closure ----
    # Routine + IMI campaign cost scalable for OWSA.
    
    run_nmb <- function(c_routine = params$c_routine_FIC,
                        c_imi_mult = 1) {
      rs <- routine_state |>
        mutate(cost_routine = c_routine * fic_attributable) |>
        select(state_ut, cost_routine)
      is <- imi_state |>
        mutate(cost_imi = cost_imi * c_imi_mult) |>
        select(state_ut, cost_imi)
      sc <- rs |>
        left_join(is, by = "state_ut") |>
        mutate(cost_imi         = coalesce(cost_imi, 0),
               state_cost_total = cost_routine + cost_imi)
      f <- frame |>
        select(-any_of(c("state_cost_total", "state_cost_qd", "net_cost_qd",
                         "cost_routine", "cost_imi"))) |>
        left_join(sc, by = "state_ut") |>
        mutate(state_cost_qd =
                 if_else(state_weight_total > 0,
                         state_cost_total * weight_qd / state_weight_total,
                         state_cost_total * prop_q / n_disease),
               net_cost_qd = state_cost_qd - oop_averted)
      sum(f$dalys_averted, na.rm = TRUE) * params$wtp_gdppc -
        sum(f$net_cost_qd, na.rm = TRUE)
    }
    
    
    ### MMVI closure (CRRA, OOP CV multiplier) ----
    
    run_mmvi_total <- function(crra = params$crra, oop_cv_mult = 1) {
      m <- qd_keys |>
        rowwise() |>
        mutate(mmvi_per_case = pmax(mmvi_per_case(annual_hh_consumption,
                                                  per_case_oop_qd,
                                                  hh_oop_cv * oop_cv_mult,
                                                  eta = crra), 0)) |>
        select(disease, wealth_q, mmvi_per_case) |>
        ungroup()
      f <- frame |>
        select(-any_of("mmvi_per_case")) |>
        left_join(m, by = c("disease", "wealth_q")) |>
        mutate(mmvi_local = cases_averted * care_sought * mmvi_per_case)
      sum(f$mmvi_local, na.rm = TRUE)
    }
    
    
    ### CHE closure (OOP CV, income CV) ----
    
    run_che_total <- function(oop_cv_mult = 1, income_cv_mult = 1,
                              threshold = params$che_low) {
      qd <- qd_keys |>
        rowwise() |>
        mutate(.tmp = list({
          ln_inc <- ln_mu_sigma(annual_hh_consumption,
                                hh_income_cv_national * income_cv_mult)
          ln_oop <- ln_mu_sigma(per_case_oop_qd, hh_oop_cv * oop_cv_mult)
          inc <- rlnorm(params$mc_n, meanlog = ln_inc$mu, sdlog = ln_inc$sigma)
          oop <- rlnorm(params$mc_n, meanlog = ln_oop$mu, sdlog = ln_oop$sigma)
          list(pr_che = mean(oop > threshold * inc))
        })) |>
        mutate(pr_che = .tmp$pr_che) |>
        ungroup() |>
        select(disease, wealth_q, pr_che)
      f <- frame |>
        select(-any_of("pr_che")) |>
        left_join(qd, by = c("disease", "wealth_q")) |>
        mutate(che_local = cases_averted * care_sought * pr_che)
      sum(f$che_local, na.rm = TRUE)
    }
    
    
    ### Base values per outcome ----
    
    base_nmb   <- run_nmb()
    base_mmvi  <- run_mmvi_total()
    base_che10 <- run_che_total(threshold = params$che_low)
    
    cat("\n===== Stage 10 — uncertainty =====\n")
    cat("Base NMB at 1x GDPpc :", round(base_nmb), "INR\n")
    cat("Base MMVI total      :", round(base_mmvi), "INR\n")
    cat("Base CHE-10 averted  :", round(base_che10), "cases\n")
    
    
    ### OWSA per outcome ----
    
    owsa_nmb <- bind_rows(
      tibble(parameter     = "Routine cost per FIC (1800-3100)",
             lower         = params$c_routine_lower,
             upper         = params$c_routine_upper,
             outcome_lower = run_nmb(c_routine = params$c_routine_upper),
             outcome_upper = run_nmb(c_routine = params$c_routine_lower)),
      tibble(parameter     = "IMI cost per dose (x0.6 / x2.0)",
             lower         = 0.6, upper = 2.0,
             outcome_lower = run_nmb(c_imi_mult = 2.0),
             outcome_upper = run_nmb(c_imi_mult = 0.6))
    ) |>
      mutate(outcome = "NMB at 1x GDPpc", base = base_nmb,
             swing = abs(outcome_upper - outcome_lower))
    
    owsa_mmvi <- bind_rows(
      tibble(parameter = "CRRA eta",                lower = 1, upper = 5,
             outcome_lower = run_mmvi_total(crra = 1),
             outcome_upper = run_mmvi_total(crra = 5)),
      tibble(parameter = "Household OOP CV (x0.8/x2.0)", lower = 0.8, upper = 2.0,
             outcome_lower = run_mmvi_total(oop_cv_mult = 0.8),
             outcome_upper = run_mmvi_total(oop_cv_mult = 2.0))
    ) |>
      mutate(outcome = "Total MMVI", base = base_mmvi,
             swing = abs(outcome_upper - outcome_lower))
    
    owsa_che <- bind_rows(
      tibble(parameter = "Household OOP CV (x0.8/x2.0)", lower = 0.8, upper = 2.0,
             outcome_lower = run_che_total(oop_cv_mult = 0.8),
             outcome_upper = run_che_total(oop_cv_mult = 2.0)),
      tibble(parameter = "Within-q income CV (x0.5/x1.5)", lower = 0.5, upper = 1.5,
             outcome_lower = run_che_total(income_cv_mult = 0.5),
             outcome_upper = run_che_total(income_cv_mult = 1.5))
    ) |>
      mutate(outcome = "CHE-10 cases averted", base = base_che10,
             swing = abs(outcome_upper - outcome_lower))
    
    owsa <- bind_rows(owsa_nmb, owsa_mmvi, owsa_che) |>
      arrange(outcome, desc(swing))
    
    
    ### PSA: VE Beta + routine and IMI gamma ----
    # Tracks national totals and quintile-level DALYs/cost for distributional
    # CEAC. Each draw returns a 12-vector: 2 national + (5 q x 2 measures).
    
    psa_mat <- replicate(params$psa_n, {
      ve_draws <- ve |>
        mutate(draw = mapply(function(a, b) rbeta(1, a, b), ve_alpha, ve_beta)) |>
        select(disease, draw)
      c_routine_draw <- rgamma(1, shape = 25,
                               rate  = 25 / params$c_routine_FIC)
      f <- frame |>
        select(-any_of(c("state_cost_total", "cost_routine", "cost_imi"))) |>
        left_join(ve_draws, by = "disease") |>
        mutate(cases   = cases_q * draw * delta_cov,
               dalys   = cases * dalys_per_case,
               oop_avg = cases * care_sought * per_case_oop_qd)
      
      ## State cost with PSA-drawn routine + state-specific IMI gamma
      rs <- routine_state |>
        mutate(cost_routine = c_routine_draw * fic_attributable) |>
        select(state_ut, cost_routine)
      is <- frp$R_imi_costs |>
        rowwise() |>
        mutate(c_imi_draw = rgamma(1, shape = gamma_shape, rate = gamma_rate)) |>
        ungroup() |>
        select(state_ut, c_imi_draw) |>
        left_join(imi_state |> select(state_ut, imi_doses_total),
                  by = "state_ut") |>
        mutate(cost_imi = coalesce(c_imi_draw * imi_doses_total, 0)) |>
        select(state_ut, cost_imi)
      
      sc <- rs |>
        left_join(is, by = "state_ut") |>
        mutate(cost_imi         = coalesce(cost_imi, 0),
               state_cost_total = cost_routine + cost_imi) |>
        select(state_ut, state_cost_total)
      
      f <- f |> left_join(sc, by = "state_ut") |>
        mutate(state_cost_qd =
                 if_else(state_weight_total > 0,
                         state_cost_total * weight_qd / state_weight_total,
                         state_cost_total * prop_q / n_disease),
               net_cost_qd = state_cost_qd - oop_avg)
      
      # National
      out <- c(dalys = sum(f$dalys,        na.rm = TRUE),
               cost  = sum(f$net_cost_qd,  na.rm = TRUE))
      # Quintile-level
      q_summary <- f |>
        group_by(wealth_q) |>
        summarise(dalys = sum(dalys, na.rm = TRUE),
                  cost  = sum(net_cost_qd, na.rm = TRUE),
                  .groups = "drop") |>
        arrange(wealth_q)
      out_q <- c(setNames(q_summary$dalys, paste0("dalys_q", q_summary$wealth_q)),
                 setNames(q_summary$cost,  paste0("cost_q",  q_summary$wealth_q)))
      c(out, out_q)
    })
    psa <- as_tibble(t(psa_mat))
    
    
    ### CEAC across WTP grid (national) ----
    
    ceac <- tibble(wtp = params$wtp_grid) |>
      rowwise() |>
      mutate(p_ce = mean(psa$dalys * wtp - psa$cost > 0)) |>
      ungroup()
    
    
    ### Distributional CEAC across WTP grid (by quintile) ----
    # Tests whether the cost-effective conclusion holds at every level of the
    # equity distribution, not only average. P(NMB > 0) per quintile.
    
    dceac <- expand_grid(wtp = params$wtp_grid, wealth_q = 1:5) |>
      rowwise() |>
      mutate(p_ce = mean(psa[[paste0("dalys_q", wealth_q)]] * wtp -
                           psa[[paste0("cost_q",  wealth_q)]] > 0)) |>
      ungroup()
    
    cat("\nStage 10 complete.\n")
  }
  
  
  ## 11. STAGE 11 - SECTOR-STRATIFIED SECONDARY ANALYSIS ----
  # Computes a sector x quintile (rural / urban) view of the FRP outcomes.
  # Approach is "post-hoc layered" rather than refactoring the main engine
  # pipeline: the state-level cases/DALYs computed in Stages 4 are split to
  # sectors using the NFHS-derived state-sector population shares, and FRP
  # outcomes are recomputed against sector-specific consumption anchors, OOP
  # multipliers, poverty lines, and sector-specific care-seeking gradients.
  # The main results_q_total in Stage 8 remain the primary headline; the
  # sector view is a secondary deliverable shown in Table 2 and Figure 8.
  
  if (TRUE) {
    
    if (is.null(frp$R_antigen_coverage_sector_quintile) ||
        is.null(frp$R_care_seeking_sector_quintile)     ||
        is.null(frp$R_sector_pop_share)) {
      
      cat("\nStage 11 skipped: sector tables absent from input bundle.\n")
      cat("  Run AB_FRP_NFHS.R to produce nfhs_antigen_sector_quintile.csv,\n")
      cat("  then re-run 01_build_AB_FRP_inputs.R.\n")
      
      results_sq    <- NULL
      equity_sector <- NULL
      
    } else {
      
      cat("\n===== Stage 11 — sector-stratified secondary analysis =====\n")
      
      
      ### OOP sector multiplier ----
      
      oop_mult <- tibble(
        sector       = c("rural", "urban"),
        oop_mult     = c(scalar("oop_mult_rural"),
                         scalar("oop_mult_urban"))
      )
      
      
      ### Consumption per sector x quintile ----
      
      cons_sq <- frp$R_MPCE_quintile |>
        transmute(sector,
                  wealth_q,
                  annual_hh_consumption = mpce_2020_21_inr *
                    params$hh_size * 12)
      
      pov_sq <- frp$R_poverty_lines_2020_21 |>
        transmute(sector,
                  pov_line_2020_21 = pov_line_2020_21_inr_hh_annual)
      
      
      ### Sector pop shares (NFHS-5) ----
      # State x quintile x sector share, used to apportion state-level cases
      # to the sector dimension.
      
      sector_share <- frp$R_sector_pop_share |>
        filter(round == "NFHS-5") |>
        select(state_ut, wealth_q, share_rural, share_urban) |>
        pivot_longer(c(share_rural, share_urban),
                     names_to  = "sector",
                     values_to = "sector_share") |>
        mutate(sector = sub("share_", "", sector))
      
      
      ### Sector-stratified cell frame ----
      # Start from main frame (state x quintile x disease), expand to sector,
      # and propagate sector-specific consumption, poverty line, OOP, care-
      # seeking.
      
      frame_sq <- frame |>
        select(state_ut, wealth_q, disease,
               prop_q, cases_q, cases_averted, dalys_averted,
               per_case_oop_qd) |>
        left_join(sector_share, by = c("state_ut", "wealth_q"),
                  relationship = "many-to-many") |>
        left_join(oop_mult, by = "sector") |>
        left_join(cons_sq,  by = c("sector", "wealth_q")) |>
        left_join(pov_sq,   by = "sector") |>
        left_join(frp$R_care_seeking_sector_quintile |>
                    select(sector, wealth_q, care_sought_sec = care_sought),
                  by = c("sector", "wealth_q")) |>
        mutate(
          sector_share    = coalesce(sector_share, 0.5),
          cases_q_sec     = cases_q * sector_share,
          cases_avt_sec   = cases_averted * sector_share,
          dalys_avt_sec   = dalys_averted * sector_share,
          per_case_oop_sec = per_case_oop_qd * oop_mult,
          oop_averted_sec  = cases_avt_sec * care_sought_sec * per_case_oop_sec
        )
      
      
      ### Sector x quintile x disease CHE and poverty (Monte Carlo) ----
      # Mirrors Stage 6 main-analysis logic but with sector added to the key.
      # Per-case OOP varies by disease, so the MC keys on (disease, sector,
      # wealth_q). Lognormal income uses sector-specific within-quintile CV;
      # lognormal OOP uses quintile-level hh OOP CV with sector multiplier
      # already applied in per_case_oop_sec.
      
      sq_keys <- frame_sq |>
        filter(!is.na(per_case_oop_sec),
               per_case_oop_sec > 0,
               !is.na(annual_hh_consumption)) |>
        select(disease, sector, wealth_q,
               annual_hh_consumption, per_case_oop_sec,
               pov_line_2020_21) |>
        distinct() |>
        left_join(frp$R_hh_oop_cv |> select(wealth_q, hh_oop_cv),
                  by = "wealth_q") |>
        left_join(frp$R_within_q_income_cv |>
                    select(sector, wealth_q, hh_income_cv),
                  by = c("sector", "wealth_q"))
      
      mc_n <- params$mc_n
      sq_rates <- sq_keys |>
        rowwise() |>
        mutate(.tmp = list({
          ln_inc <- ln_mu_sigma(annual_hh_consumption, hh_income_cv)
          ln_oop <- ln_mu_sigma(per_case_oop_sec,      hh_oop_cv)
          inc <- rlnorm(mc_n, ln_inc$mu, ln_inc$sigma)
          oop <- rlnorm(mc_n, ln_oop$mu, ln_oop$sigma)
          list(pr_che_low  = mean(oop > params$che_low  * inc),
               pr_che_mid  = mean(oop > params$che_mid  * inc),
               pr_che_high = mean(oop > params$che_high * inc),
               pr_pov      = mean(inc > pov_line_2020_21 &
                                    (inc - oop) < pov_line_2020_21))
        })) |>
        mutate(pr_che_low  = .tmp$pr_che_low,
               pr_che_mid  = .tmp$pr_che_mid,
               pr_che_high = .tmp$pr_che_high,
               pr_pov      = .tmp$pr_pov) |>
        ungroup() |>
        select(disease, sector, wealth_q,
               pr_che_low, pr_che_mid, pr_che_high, pr_pov)
      
      
      ### Apply CHE / poverty rates to sector-stratified cases ----
      
      frame_sq <- frame_sq |>
        left_join(sq_rates, by = c("disease", "sector", "wealth_q")) |>
        mutate(
          che_low_sec  = cases_avt_sec * care_sought_sec * pr_che_low,
          che_mid_sec  = cases_avt_sec * care_sought_sec * pr_che_mid,
          che_high_sec = cases_avt_sec * care_sought_sec * pr_che_high,
          pov_sec      = cases_avt_sec * care_sought_sec * pr_pov
        )
      
      
      ### MMVI per disease x sector x quintile ----
      # Uses the global mmvi_per_case helper (Stage 7) with sector-specific
      # consumption and per-case OOP, ensuring identical certainty-equivalent
      # methodology across main and sector analyses.
      
      mmvi_sq <- sq_keys |>
        rowwise() |>
        mutate(mmvi_per_case = pmax(mmvi_per_case(annual_hh_consumption,
                                                  per_case_oop_sec,
                                                  hh_oop_cv), 0)) |>
        ungroup() |>
        select(disease, sector, wealth_q, mmvi_per_case)
      
      frame_sq <- frame_sq |>
        left_join(mmvi_sq, by = c("disease", "sector", "wealth_q")) |>
        mutate(mmvi_sec = cases_avt_sec * care_sought_sec * mmvi_per_case)
      
      
      ### Aggregate to sector x quintile totals ----
      
      results_sq <- frame_sq |>
        group_by(sector, wealth_q) |>
        summarise(cases_averted     = sum(cases_avt_sec,   na.rm = TRUE),
                  dalys_averted     = sum(dalys_avt_sec,   na.rm = TRUE),
                  oop_averted       = sum(oop_averted_sec, na.rm = TRUE),
                  che_low_averted   = sum(che_low_sec,     na.rm = TRUE),
                  che_mid_averted   = sum(che_mid_sec,     na.rm = TRUE),
                  che_high_averted  = sum(che_high_sec,    na.rm = TRUE),
                  pov_cases_averted = sum(pov_sec,         na.rm = TRUE),
                  mmvi_averted      = sum(mmvi_sec,        na.rm = TRUE),
                  .groups = "drop")
      
      cat("\nSector x quintile results:\n")
      print(results_sq |>
              mutate(across(where(is.numeric), ~ round(.x))))
      
      
      ### Wagstaff CIs computed within each sector ----
      
      compute_ci_within_sector <- function(df, outcome) {
        df |>
          group_by(sector) |>
          arrange(sector, wealth_q) |>
          mutate(p     = 0.2,
                 R     = cumsum(p) - 0.5 * p,
                 mu    = sum(p * !!sym(outcome)),
                 ci    = if_else(mu > 0,
                                 2 / mu * sum(p * (R - 0.5) *
                                                !!sym(outcome)),
                                 NA_real_)) |>
          summarise(concentration_index = first(ci), .groups = "drop") |>
          mutate(outcome = outcome)
      }
      
      equity_sector <- bind_rows(
        compute_ci_within_sector(results_sq, "dalys_averted"),
        compute_ci_within_sector(results_sq, "oop_averted"),
        compute_ci_within_sector(results_sq, "che_low_averted"),
        compute_ci_within_sector(results_sq, "che_mid_averted"),
        compute_ci_within_sector(results_sq, "che_high_averted"),
        compute_ci_within_sector(results_sq, "pov_cases_averted"),
        compute_ci_within_sector(results_sq, "mmvi_averted")
      ) |>
        select(outcome, sector, concentration_index)
      
      cat("\nSector-stratified Wagstaff CIs:\n")
      print(equity_sector |>
              mutate(concentration_index = round(concentration_index, 3)) |>
              pivot_wider(names_from = sector, values_from = concentration_index))
    }
    
    cat("\nStage 11 complete.\n")
  }
  
  
  ## 12. STAGE 12 - TABLES, FIGURES, SAVE ----
  
  if (TRUE) {
    
    
    ### Save consolidated results ----
    
    AB_FRP_results <- list(
      frame             = frame,
      results_qd        = results_qd,
      results_q_total   = results_q_total,
      equity_summary    = equity_summary,
      owsa              = owsa,
      psa               = psa,
      ceac              = ceac,
      dceac             = dceac,
      base_nmb_at_gdppc = base_nmb,
      base_mmvi         = base_mmvi,
      base_che10        = base_che10,
      results_sq        = results_sq,
      equity_sector     = equity_sector,
      metadata          = list(run_date          = Sys.Date(),
                               n_states          = nrow(states_in),
                               n_diseases        = length(scope_diseases),
                               coverage_approach = approach,
                               psa_iter          = params$psa_n,
                               mc_per_cell       = params$mc_n,
                               wtp_htain         = params$wtp_htain,
                               wtp_gdppc         = params$wtp_gdppc)
    )
    saveRDS(AB_FRP_results, file.path(out_dir, "AB_FRP_results.rds"))
    
    
    ### LIBRARIES ----
    
    library(gt)
    
    
    ### COLOUR PALETTE ----
    
    PRIMARY    <- "#002D72"
    SECONDARY  <- "#68ACE5"
    ACCENT     <- "#B30838"
    NEUTRAL    <- "#666666"
    LIGHT_GREY <- "#E5E5E5"
    GOLD       <- "#A28D5B"
    SABLE      <- "#31261D"
    
    palette_quintile <- c(PRIMARY, "#3A6BAB", SECONDARY, GOLD, "#7E6E40")
    
    
    ### Output directories ----
    
    fig_dir <- file.path(out_dir, "figures")
    tab_dir <- file.path(out_dir, "tables")
    if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)
    if (!dir.exists(tab_dir)) dir.create(tab_dir, recursive = TRUE)
    
    
    ### THEME ----
    
    theme_paper <- function(base_size = 11) {
      theme_minimal(base_size = base_size, base_family = "sans") +
        theme(
          panel.grid.minor    = element_blank(),
          panel.grid.major.x  = element_line(colour = LIGHT_GREY, linewidth = 0.3),
          panel.grid.major.y  = element_line(colour = LIGHT_GREY, linewidth = 0.3),
          axis.title          = element_text(face = "bold"),
          axis.text           = element_text(colour = "grey20"),
          plot.title          = element_text(face = "bold", size = base_size + 2,
                                             colour = PRIMARY),
          plot.subtitle       = element_text(colour = NEUTRAL, size = base_size - 1),
          plot.caption        = element_text(colour = NEUTRAL,
                                             size = base_size - 2, hjust = 0),
          legend.position     = "bottom",
          legend.title        = element_text(face = "bold", size = base_size - 1),
          legend.text         = element_text(size = base_size - 1),
          strip.text          = element_text(face = "bold", colour = PRIMARY)
        )
    }
    
    
    ### HELPERS ----
    # apply_table_style + save_table.
    # save_table writes HTML (inline CSS), PNG, DOCX, MD.
    
    apply_table_style <- function(gt_table, max_width_px = NULL) {
      base <- gt_table |>
        tab_options(
          table.font.names               = "Arial",
          table.font.size                = px(11),
          heading.title.font.size        = px(14),
          heading.subtitle.font.size     = px(11),
          heading.title.font.weight      = "bold",
          column_labels.font.weight      = "bold",
          column_labels.background.color = PRIMARY,
          column_labels.font.size        = px(11),
          row_group.background.color     = LIGHT_GREY,
          row_group.font.weight          = "bold",
          table.border.top.style         = "solid",
          table.border.top.color         = PRIMARY,
          table.border.top.width         = px(2),
          table.border.bottom.style      = "solid",
          table.border.bottom.color      = PRIMARY,
          table.border.bottom.width      = px(2),
          data_row.padding               = px(4)
        ) |>
        tab_style(
          style = cell_text(color = "white"),
          locations = cells_column_labels()
        )
      
      # Width: explicit pixel width if caller provided one (used by Table 7
      # parameter library which needs fixed sizing). Otherwise leave width
      # entirely to gt's natural auto-sizing without any cols_width directive.
      if (!is.null(max_width_px)) {
        base <- base |> tab_options(table.width = px(max_width_px))
      } else {
        base <- base |>
          tab_options(table.width = "auto",
                      container.width = "auto")
      }
      base
    }
    
    md_from_df <- function(df, title) {
      lines <- c(paste("##", title), "",
                 paste0("| ", paste(names(df), collapse = " | "), " |"),
                 paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|"))
      for (i in seq_len(nrow(df))) {
        cells <- vapply(seq_len(ncol(df)), function(j) {
          v <- df[[j]][i]
          if (is.numeric(v)) format(round(v, 3), big.mark = ",")
          else as.character(v)
        }, character(1))
        lines <- c(lines, paste0("| ", paste(cells, collapse = " | "), " |"))
      }
      lines
    }
    
    save_table <- function(gt_table, name, df_for_md = NULL, md_title = NULL) {
      
      # HTML
      tryCatch({
        gtsave(gt_table,
               filename   = file.path(tab_dir, paste0(name, ".html")),
               inline_css = TRUE)
      }, error = function(e) message("HTML export failed for ", name, ": ",
                                     conditionMessage(e)))
      
      # PNG
      tryCatch({
        gtsave(gt_table,
               filename = file.path(tab_dir, paste0(name, ".png")))
      }, error = function(e) message("PNG export failed for ", name,
                                     " (requires webshot2 + Chrome)"))
      
      # DOCX
      tryCatch({
        gtsave(gt_table,
               filename = file.path(tab_dir, paste0(name, ".docx")))
      }, error = function(e) message("DOCX export failed for ", name))
      
      # MD
      if (!is.null(df_for_md) && !is.null(md_title)) {
        writeLines(md_from_df(df_for_md, md_title),
                   file.path(tab_dir, paste0(name, ".md")))
      }
      
      invisible(gt_table)
    }
    
    save_fig <- function(plot, name, width = 7, height = 5) {
      ggsave(file.path(fig_dir, paste0(name, ".pdf")),
             plot = plot, width = width, height = height)
      ggsave(file.path(fig_dir, paste0(name, ".png")),
             plot = plot, width = width, height = height, dpi = 300)
      invisible(plot)
    }
    
    cat("\n===== Stage 12 — building tables and figures =====\n")
    
    
    ### TABLE 1 — Study population and scope (from aggregates) ----
    # Built from AB_FRP_inputs.rds aggregates.
    
    n_states_in    <- frp$R_metadata$scope_states
    n_states_drop  <- frp$R_metadata$scope_states_dropped
    diseases_str   <- paste(frp$R_metadata$scope_diseases, collapse = ", ")
    
    # Quintile population shares (NFHS-5, in-scope-only, population-weighted)
    pop_q_natl <- frp$R_nfhs_pop_quintile |>
      filter(round == "NFHS-5",
             model_state_code %in% states_in$model_state_code) |>
      group_by(wealth_q) |>
      summarise(prop_q_natl = sum(wt_sum_q, na.rm = TRUE) /
                  sum(wt_sum_q, na.rm = TRUE) * 5,
                .groups = "drop")
    
    pop_q_share <- frp$R_nfhs_pop_quintile |>
      filter(round == "NFHS-5",
             model_state_code %in% states_in$model_state_code) |>
      group_by(wealth_q) |>
      summarise(wt_total = sum(wt_sum_q, na.rm = TRUE), .groups = "drop") |>
      mutate(share = wt_total / sum(wt_total))
    
    # NFHS-5 FIC coverage by quintile (in-scope, weighted)
    fic_q <- frp$R_nfhs_coverage_quintile_FIC |>
      filter(round == "NFHS-5",
             model_state_code %in% states_in$model_state_code) |>
      group_by(wealth_q) |>
      summarise(fic_mean = mean(coverage, na.rm = TRUE), .groups = "drop")
    
    # Annual HH consumption by quintile, rural and urban
    cons_q <- frp$R_annual_consumption_quintile |>
      select(wealth_q, mpce_rural, mpce_urban, annual_hh_consumption)
    
    # u5mr scalar by quintile (in-scope NFHS-5)
    u5_q_natl <- frp$R_nfhs_u5mr_qs |>
      filter(round == "NFHS-5",
             model_state_code %in% states_in$model_state_code) |>
      group_by(wealth_q) |>
      summarise(u5mr_scalar = mean(u5mr_scalar, na.rm = TRUE), .groups = "drop")
    
    # Per-case OOP (national, before quintile gradient)
    oop_dis <- frp$R_disease_oop_anchor |>
      summarise(mean_oop = mean(per_case_oop_2020_21))
    
    # NFHS-5 quintile care-seeking
    cs_q <- frp$R_care_seeking_quintile
    
    # Assemble Table 1 as long-format
    t1_quintile <- pop_q_share |>
      left_join(fic_q,    by = "wealth_q") |>
      left_join(cons_q,   by = "wealth_q") |>
      left_join(u5_q_natl, by = "wealth_q") |>
      left_join(cs_q |> select(wealth_q, care_sought), by = "wealth_q") |>
      mutate(quintile  = paste0("Q", wealth_q),
             share_pct = round(100 * share, 1),
             fic_pct   = round(100 * fic_mean, 1)) |>
      select(wealth_q, quintile, share_pct, fic_pct, mpce_rural, mpce_urban,
             annual_hh_consumption, u5mr_scalar, care_sought)
    
    # NFHS-5 analytic sample N per quintile (FIC-status reported)
    n_per_q <- frp$R_antigen_coverage_quintile |>
      filter(round == "NFHS-5", antigen_indicator == "fic_cov") |>
      group_by(wealth_q) |>
      summarise(n_fic_sample = sum(n, na.rm = TRUE), .groups = "drop")
    
    nfhs_sample_total <- sum(n_per_q$n_fic_sample, na.rm = TRUE)
    
    # Total cohort (12-23m projection across in-scope states)
    cohort_total <- frp$R_treatment_intensity |>
      filter(state_ut %in% states_in$state_ut) |>
      summarise(n = sum(pop_12_23_2021, na.rm = TRUE)) |>
      pull(n)
    
    # Long, then pivot to (Feature | Q1 | Q2 | Q3 | Q4 | Q5 | Total/All)
    # Total column rules:
    #  - Population share, NFHS sample N : sum across quintiles
    #  - Rural/Urban MPCE, annual HH consumption: weighted by quintile share
    #  - FIC coverage, u5mr scalar, care-seeking: weighted mean by quintile share
    t1_long <- t1_quintile |>
      left_join(n_per_q, by = "wealth_q") |>
      select(wealth_q,
             `NFHS-5 analytic sample (n)` = n_fic_sample,
             `Population share (%)`       = share_pct,
             `FIC coverage NFHS-5 (%)`    = fic_pct,
             `Rural MPCE (INR/mo)`        = mpce_rural,
             `Urban MPCE (INR/mo)`        = mpce_urban,
             `Annual HH consumption (INR)` = annual_hh_consumption,
             `Under-5 mortality scalar`   = u5mr_scalar,
             `Care-seeking probability`   = care_sought)
    
    feature_levels <- c("NFHS-5 analytic sample (n)",
                        "Population share (%)",
                        "FIC coverage NFHS-5 (%)",
                        "Rural MPCE (INR/mo)",
                        "Urban MPCE (INR/mo)",
                        "Annual HH consumption (INR)",
                        "Under-5 mortality scalar",
                        "Care-seeking probability")
    
    # Quintile share weights for population-weighted aggregation
    q_w <- t1_quintile |>
      transmute(wealth_q, w = share_pct / sum(share_pct))
    
    sum_features <- c("NFHS-5 analytic sample (n)", "Population share (%)")
    
    t1_wide <- t1_long |>
      pivot_longer(-wealth_q, names_to = "Feature", values_to = "value") |>
      mutate(wealth_q = paste0("Q", wealth_q)) |>
      pivot_wider(names_from = wealth_q, values_from = value) |>
      rowwise() |>
      mutate(Total = if (Feature %in% sum_features) {
        sum(c_across(c(Q1, Q2, Q3, Q4, Q5)))
      } else {
        sum(c_across(c(Q1, Q2, Q3, Q4, Q5)) * q_w$w)
      }) |>
      ungroup() |>
      mutate(Feature = factor(Feature, levels = feature_levels)) |>
      arrange(Feature)
    
    # Round each row appropriately
    fmt_row <- function(x, fmt) {
      switch(fmt,
             int    = format(round(x), big.mark = ","),
             pct1   = sprintf("%.1f", x),
             dec2   = sprintf("%.2f", x),
             dec3   = sprintf("%.3f", x))
    }
    
    row_fmt <- c("NFHS-5 analytic sample (n)"   = "int",
                 "Population share (%)"         = "pct1",
                 "FIC coverage NFHS-5 (%)"      = "pct1",
                 "Rural MPCE (INR/mo)"          = "int",
                 "Urban MPCE (INR/mo)"          = "int",
                 "Annual HH consumption (INR)"  = "int",
                 "Under-5 mortality scalar"     = "dec2",
                 "Care-seeking probability"     = "dec3")
    
    t1_wide_disp <- t1_wide |>
      rowwise() |>
      mutate(across(c(Q1, Q2, Q3, Q4, Q5, Total),
                    \(x) fmt_row(x, row_fmt[as.character(Feature)]))) |>
      ungroup()
    
    gt_t1 <- gt(t1_wide_disp) |>
      tab_header(
        title    = md("**Table 1. Study population and analytic scope, by wealth quintile**"),
        subtitle = sprintf(
          "%d Indian states; %s NFHS-5 children aged 12-23 months (analytic sample); projected cohort %s million; 2020-21 INR.",
          n_states_in,
          format(nfhs_sample_total, big.mark = ","),
          format(round(cohort_total / 1e6, 1), nsmall = 1)
        )
      ) |>
      cols_align(align = "left",  columns = Feature) |>
      cols_align(align = "right", columns = c(Q1, Q2, Q3, Q4, Q5, Total)) |>
      tab_source_note(
        source_note = paste(
          "Care-seeking probability: probability that a child with a vaccine-",
          "preventable disease episode is brought to a formal provider. A linear-",
          "in-rank gradient was chosen as the most parsimonious functional form",
          "consistent with both reported moments (national mean and Wagstaff",
          "concentration index)."
        )
      ) |>
      apply_table_style(max_width_px = 600)
    
    save_table(gt_t1, "table1_study_population",
               df_for_md = t1_wide_disp,
               md_title  = "Table 1. Study population and analytic scope")
    
    
    ### TABLE 2 — National FRP-ECEA results, outcomes x wealth quintile ----
    # Transposed layout: each row is one FRP-ECEA outcome, columns are Q1-Q5
    # plus a Total. Easier to read across the wealth gradient than the
    # original outcomes-as-columns layout.
    
    tab2_wide <- results_q_total |>
      transmute(wealth_q,
                `Cases averted`         = cases_averted,
                `DALYs averted`         = dalys_averted,
                `OOP averted (INR)`     = oop_averted,
                `CHE-10 cases averted`  = che_low_averted,
                `CHE-25 cases averted`  = che_mid_averted,
                `CHE-40 cases averted`  = che_high_averted,
                `Poverty cases averted` = pov_cases_averted,
                `MMVI (INR)`            = mmvi_averted,
                `Net cost (INR)`        = net_cost_qd,
                `ICER per DALY (INR)`   = icer_per_daly,
                `NMB at 1x GDPpc (INR)` = nmb_at_gdppc)
    
    # Long, then pivot to (Outcome | Q1 | Q2 | Q3 | Q4 | Q5 | Total)
    # ICER is a ratio: Total ICER computed from total net cost / total DALYs
    # rather than summing per-quintile ICERs (which would be meaningless).
    tab2 <- tab2_wide |>
      tidyr::pivot_longer(-wealth_q,
                          names_to  = "Outcome",
                          values_to = "value") |>
      mutate(wealth_q = paste0("Q", wealth_q)) |>
      tidyr::pivot_wider(names_from  = wealth_q,
                         values_from = value) |>
      mutate(Total = case_when(
        Outcome == "ICER per DALY (INR)" ~
          sum(results_q_total$net_cost_qd) /
          sum(results_q_total$dalys_averted),
        TRUE ~ Q1 + Q2 + Q3 + Q4 + Q5
      )) |>
      mutate(across(c(Q1, Q2, Q3, Q4, Q5, Total), \(x) round(x))) |>
      mutate(Outcome = factor(Outcome, levels = c(
        "Cases averted", "DALYs averted", "OOP averted (INR)",
        "CHE-10 cases averted", "CHE-25 cases averted",
        "CHE-40 cases averted", "Poverty cases averted",
        "MMVI (INR)", "Net cost (INR)", "ICER per DALY (INR)",
        "NMB at 1x GDPpc (INR)"))) |>
      arrange(Outcome)
    
    gt_t2 <- gt(tab2) |>
      tab_header(
        title    = md("**Table 2. National FRP-ECEA results by wealth quintile**"),
        subtitle = sprintf("%d Indian states, %d diseases, 2020-21 INR. %s.",
                           n_states_in, length(scope_diseases), approach)
      ) |>
      cols_align(align = "left",  columns = Outcome) |>
      cols_align(align = "right", columns = c(Q1, Q2, Q3, Q4, Q5, Total)) |>
      fmt_number(columns = c(Q1, Q2, Q3, Q4, Q5, Total),
                 decimals = 0, sep_mark = ",") |>
      apply_table_style()
    
    save_table(gt_t2, "table2_quintile_results",
               df_for_md = tab2,
               md_title  = "Table 2. National FRP-ECEA results by wealth quintile")
    
    
    ### TABLE 2b — Sector x quintile FRP outcomes (secondary analysis) ----
    
    if (!is.null(results_sq)) {
      
      tab2b <- results_sq |>
        arrange(sector, wealth_q) |>
        transmute(`Sector`                = stringr::str_to_title(sector),
                  `Quintile`              = paste0("Q", wealth_q),
                  `Cases averted`         = round(cases_averted),
                  `DALYs averted`         = round(dalys_averted),
                  `OOP averted (INR)`     = round(oop_averted),
                  `CHE-10 cases averted`  = round(che_low_averted),
                  `CHE-25 cases averted`  = round(che_mid_averted),
                  `CHE-40 cases averted`  = round(che_high_averted),
                  `Poverty cases averted` = round(pov_cases_averted),
                  `MMVI (INR)`            = round(mmvi_averted))
      
      gt_t2b <- gt(tab2b, groupname_col = "Sector") |>
        tab_header(
          title    = md("**Table 2b. FRP outcomes by sector and wealth quintile**"),
          subtitle = "Secondary sector-stratified analysis. Sector-specific consumption, OOP, care-seeking, and poverty lines applied."
        ) |>
        fmt_number(columns = where(is.numeric), decimals = 0, sep_mark = ",") |>
        apply_table_style()
      
      save_table(gt_t2b, "table2b_sector_quintile_results",
                 df_for_md = tab2b,
                 md_title  = "Table 2b. FRP outcomes by sector and wealth quintile")
    }
    
    
    ### TABLE 3 — Cases and DALYs averted by disease and quintile ----
    
    tab3_long <- results_qd |>
      select(disease, wealth_q, cases_averted, dalys_averted) |>
      mutate(disease = stringr::str_to_title(disease))
    
    tab3_cases <- tab3_long |>
      select(disease, wealth_q, cases_averted) |>
      pivot_wider(names_from = wealth_q, values_from = cases_averted,
                  names_prefix = "Q") |>
      mutate(Total = round(Q1 + Q2 + Q3 + Q4 + Q5),
             across(starts_with("Q"), round))
    
    tab3_dalys <- tab3_long |>
      select(disease, wealth_q, dalys_averted) |>
      pivot_wider(names_from = wealth_q, values_from = dalys_averted,
                  names_prefix = "Q") |>
      mutate(Total = round(Q1 + Q2 + Q3 + Q4 + Q5),
             across(starts_with("Q"), round))
    
    tab3_disp <- bind_rows(
      tab3_cases |> mutate(metric = "Cases averted"),
      tab3_dalys |> mutate(metric = "DALYs averted")
    ) |>
      select(metric, disease, Q1, Q2, Q3, Q4, Q5, Total)
    
    gt_t3 <- gt(tab3_disp, groupname_col = "metric") |>
      tab_header(
        title    = md("**Table 3. Cases and DALYs averted, by disease and wealth quintile**"),
        subtitle = sprintf("%d Indian states; one cohort year at NFHS-5.",
                           n_states_in)
      ) |>
      fmt_number(columns = where(is.numeric), decimals = 0, sep_mark = ",") |>
      apply_table_style()
    
    save_table(gt_t3, "table3_disease_quintile",
               df_for_md = tab3_disp,
               md_title  = "Table 3. Cases and DALYs averted, by disease and quintile")
    
    
    ### TABLE 4 — Equity summary: Wagstaff concentration indices ----
    
    outcome_pretty <- c(
      dalys_averted     = "DALYs averted",
      oop_averted       = "OOP averted (INR)",
      che_low_averted   = "CHE-10 cases averted",
      che_mid_averted   = "CHE-25 cases averted",
      che_high_averted  = "CHE-40 cases averted",
      pov_cases_averted = "Poverty cases averted",
      mmvi_averted      = "MMVI (INR)"
    )
    
    tab4_disp <- equity_summary |>
      mutate(`Outcome`                    = outcome_pretty[outcome],
             `Wagstaff CI`                = round(concentration_index, 3),
             `Q1+Q2 share`                = round(pro_poor_share, 3),
             `Interpretation`             = case_when(
               concentration_index < -0.30 ~ "Strongly pro-poor",
               concentration_index < -0.10 ~ "Pro-poor",
               concentration_index <  0.10 ~ "Approximately equal",
               concentration_index <  0.30 ~ "Pro-rich",
               TRUE                        ~ "Strongly pro-rich"
             )) |>
      select(`Outcome`, `Wagstaff CI`, `Q1+Q2 share`, `Interpretation`)
    
    gt_t4 <- gt(tab4_disp) |>
      tab_header(
        title    = md("**Table 4. Equity summary: Wagstaff concentration indices**"),
        subtitle = "Negative CI = pro-poor; Q1+Q2 share = proportion accruing to poorest 40%."
      ) |>
      apply_table_style()
    
    save_table(gt_t4, "table4_equity_summary",
               df_for_md = tab4_disp,
               md_title  = "Table 4. Equity summary: Wagstaff concentration indices")
    
    
    ### Parameter library ----
    
    pl <- frp$R_parameters_long_FRP |>
      mutate(across(c(base, lower, upper), as.character))
    
    domain_labels <- c(
      decision_rule    = "A. Decision rules and WTP thresholds",
      epi              = "B. Epidemiological parameters and vaccine efficacy",
      cost             = "C. Cost parameters (OOP anchors)",
      behavioural      = "D. Behavioural parameters (CRRA, OOP CV)",
      equity           = "E. Equity parameters",
      model_structure  = "F. Model structure and methods"
    )
    
    t5_df <- pl |>
      filter(domain %in% names(domain_labels)) |>
      mutate(domain_section = factor(domain_labels[domain],
                                     levels = unname(domain_labels))) |>
      transmute(domain_section,
                `Parameter`        = parameter_label,
                `Base`             = base,
                `Lower`            = coalesce(lower, ""),
                `Upper`            = coalesce(upper, ""),
                `Units`            = coalesce(units, ""),
                `Distribution`     = distribution,
                `Sensitivity rule` = sensitivity_rule) |>
      arrange(domain_section)
    
    gt_t5 <- gt(t5_df, groupname_col = "domain_section") |>
      tab_header(
        title = md("**Table 5. Parameter library**"),
        subtitle = "All inputs, by domain. Lower / Upper define OWSA bounds where applicable."
      ) |>
      cols_width(
        `Parameter`        ~ px(260),
        `Base`             ~ px(80),
        `Lower`            ~ px(50),
        `Upper`            ~ px(50),
        `Units`            ~ px(110),
        `Distribution`     ~ px(80),
        `Sensitivity rule` ~ px(140)
      ) |>
      tab_style(style = cell_text(size = px(10)),
                locations = cells_body()) |>
      apply_table_style(max_width_px = 800)
    
    save_table(gt_t5, "table5_parameter_library",
               df_for_md = t5_df,
               md_title  = "Table 5. Parameter library")
    
    
    ### Figure 1 — FRP-ECEA outcomes by wealth quintile ----
    # 3x3 grid via patchwork. Three CHE panels share a common y-axis upper
    # bound (max across CHE-10/25/40) for direct threshold comparison. Row 3
    # uses two empty plots to flank MMVI so it keeps standard panel width
    # rather than stretching across the row.
    
    che_max <- max(results_q_total$che_low_averted,
                   results_q_total$che_mid_averted,
                   results_q_total$che_high_averted, na.rm = TRUE) * 1.15
    
    make_panel <- function(varname, title, y_max = NA,
                           y_label_fmt = scales::label_number(big.mark = ",")) {
      d <- results_q_total |>
        transmute(wealth_q = factor(wealth_q, levels = 1:5),
                  value    = !!sym(varname))
      
      # Headroom above bars so labels don't crop
      panel_max <- if (!is.na(y_max)) y_max
      else max(d$value, na.rm = TRUE) * 1.15
      
      p <- ggplot(d, aes(wealth_q, value, fill = wealth_q)) +
        geom_col() +
        geom_text(aes(label = y_label_fmt(value)),
                  vjust = -0.5, size = 3, colour = SABLE) +
        scale_fill_manual(values = palette_quintile, guide = "none") +
        scale_y_continuous(limits = c(0, panel_max),
                           labels = y_label_fmt) +
        labs(x = NULL, y = NULL, title = title) +
        theme_paper() +
        theme(plot.title = element_text(size = 11, face = "bold",
                                        colour = PRIMARY),
              axis.text  = element_text(size = 9))
      p
    }
    
    p_daly  <- make_panel("dalys_averted",     "DALYs averted")
    p_oop   <- make_panel("oop_averted",       "OOP averted (INR)",
                          y_label_fmt = scales::label_number(
                            scale = 1e-9, suffix = "B"))
    p_pov   <- make_panel("pov_cases_averted", "Poverty cases averted")
    p_che10 <- make_panel("che_low_averted",   "CHE-10 cases averted",
                          y_max = che_max)
    p_che25 <- make_panel("che_mid_averted",   "CHE-25 cases averted",
                          y_max = che_max)
    p_che40 <- make_panel("che_high_averted",  "CHE-40 cases averted",
                          y_max = che_max)
    p_mmvi  <- make_panel("mmvi_averted",      "MMVI (INR)",
                          y_label_fmt = scales::label_number(
                            scale = 1e-9, suffix = "B"))
    
    fig1 <- (p_daly  + p_oop   + p_pov +
               p_che10 + p_che25 + p_che40 +
               p_mmvi  + plot_spacer() + plot_spacer()) +
      plot_layout(ncol = 3) +
      plot_annotation(
        title    = "FRP-ECEA outcomes by wealth quintile",
        subtitle = sprintf(
          "%d Indian states; one cohort year; 2020-21 INR. CHE panels share y-scale.",
          n_states_in),
        theme = theme(plot.title    = element_text(face = "bold",
                                                   size  = 14,
                                                   colour = PRIMARY),
                      plot.subtitle = element_text(colour = NEUTRAL,
                                                   size   = 10))
      )
    
    # Apply common x-axis label only to bottom row of populated panels
    fig1 <- fig1 & labs(x = "Wealth quintile (1 = poorest, 5 = richest)")
    
    save_fig(fig1, "fig1_quintile_distribution", width = 10, height = 9)
    
    
    ### Figure 2a — OWSA tornado, by outcome ----
    
    fig2a <- ggplot(owsa, aes(x = reorder(parameter, swing))) +
      geom_segment(aes(xend = parameter, y = outcome_lower, yend = outcome_upper),
                   linewidth = 5, colour = PRIMARY) +
      geom_point(aes(y = base), colour = GOLD, size = 2) +
      coord_flip() +
      facet_wrap(~ outcome, scales = "free_x", ncol = 1) +
      labs(x = NULL, y = "Outcome value (INR or count)",
           title = "OWSA tornado, by outcome",
           subtitle = "Each bar shows the swing under low / high parameter values; gold dot = base case.") +
      theme_paper() +
      theme(legend.position = "none")
    
    
    ### Figure 2b — Cost-effectiveness acceptability curve ----
    
    fig2b <- ggplot(ceac, aes(wtp, p_ce)) +
      geom_line(linewidth = 1, colour = PRIMARY) +
      geom_vline(xintercept = params$wtp_htain, linetype = 3, colour = GOLD) +
      geom_vline(xintercept = params$wtp_gdppc, linetype = 3, colour = GOLD) +
      annotate("text", x = params$wtp_htain, y = 0.05, label = "HTAIn",
               colour = SABLE, angle = 90, vjust = -0.5, size = 3.2) +
      annotate("text", x = params$wtp_gdppc, y = 0.05, label = "1x GDPpc",
               colour = SABLE, angle = 90, vjust = -0.5, size = 3.2) +
      labs(x = "WTP (INR per DALY averted, 2020-21)",
           y = "Probability cost-effective",
           title = "Cost-effectiveness acceptability curve",
           subtitle = "P(NMB > 0) across the WTP grid; 1,000 PSA draws.") +
      theme_paper()
    
    fig2 <- fig2a / fig2b + plot_layout(heights = c(2, 1))
    save_fig(fig2, "fig2_owsa_ceac", width = 9, height = 9)
    
    
    ### Figure 3 — Cost-effectiveness acceptability plane (CEAP) ----
    # Each PSA draw is one point in (incremental DALYs, incremental cost) space.
    # Shaded WTP line at 1x GDPpc separates cost-effective (below line) from
    # not (above). Q4 quadrant (negative cost, positive DALYs) = dominant.
    
    psa_plot <- psa |>
      transmute(daly = dalys, cost = cost,
                pos_daly = daly > 0, neg_cost = cost < 0,
                quadrant = case_when(
                  pos_daly  & neg_cost ~ "Dominant (cost-saving + DALY gain)",
                  pos_daly  & !neg_cost ~ "Trade-off (more cost, more DALY)",
                  !pos_daly & neg_cost ~ "Trade-off (cost-saving, DALY loss)",
                  TRUE                  ~ "Dominated"
                ))
    
    wtp_line_slope <- params$wtp_gdppc
    daly_range     <- range(psa_plot$daly)
    
    fig3 <- ggplot(psa_plot, aes(daly, cost)) +
      geom_hline(yintercept = 0, colour = NEUTRAL, linewidth = 0.4) +
      geom_vline(xintercept = 0, colour = NEUTRAL, linewidth = 0.4) +
      geom_abline(slope = wtp_line_slope, intercept = 0,
                  linetype = 3, colour = GOLD, linewidth = 0.5) +
      geom_point(aes(colour = quadrant), alpha = 0.45, size = 1.5) +
      scale_colour_manual(values = c(
        "Dominant (cost-saving + DALY gain)"  = PRIMARY,
        "Trade-off (more cost, more DALY)"    = SECONDARY,
        "Trade-off (cost-saving, DALY loss)"  = ACCENT,
        "Dominated"                           = NEUTRAL
      )) +
      scale_y_continuous(labels = scales::label_number(scale = 1e-9,
                                                       suffix = "B")) +
      scale_x_continuous(labels = scales::label_number(big.mark = ",")) +
      labs(x       = "Incremental DALYs averted",
           y       = "Incremental net cost (INR, billions)",
           colour  = NULL,
           title   = "Cost-effectiveness acceptability plane",
           subtitle = sprintf(
             "%d PSA draws. Dashed line = 1x GDPpc WTP threshold (INR %s/DALY).",
             nrow(psa_plot),
             format(params$wtp_gdppc, big.mark = ","))) +
      theme_paper() +
      theme(legend.position = "bottom")
    
    save_fig(fig3, "fig3_ceap", width = 9, height = 6)
    
    
    ### Figure 4 — Distributional CEAC by wealth quintile ----
    # P(NMB > 0) by wealth quintile across the WTP grid. Tests whether the
    # cost-effective conclusion holds at every level of the equity distribution.
    
    dceac_plot <- dceac |>
      mutate(quintile = factor(paste0("Q", wealth_q),
                               levels = paste0("Q", 1:5)))
    
    fig4 <- ggplot(dceac_plot, aes(wtp, p_ce, colour = quintile)) +
      geom_line(linewidth = 1) +
      geom_vline(xintercept = params$wtp_htain, linetype = 3, colour = GOLD) +
      geom_vline(xintercept = params$wtp_gdppc, linetype = 3, colour = GOLD) +
      annotate("text", x = params$wtp_htain, y = 0.05, label = "HTAIn",
               colour = SABLE, angle = 90, vjust = -0.5, size = 3.2) +
      annotate("text", x = params$wtp_gdppc, y = 0.05, label = "1x GDPpc",
               colour = SABLE, angle = 90, vjust = -0.5, size = 3.2) +
      scale_colour_manual(values = palette_quintile,
                          name = "Wealth quintile") +
      scale_y_continuous(limits = c(0, 1),
                         breaks = seq(0, 1, 0.25)) +
      labs(x = "WTP (INR per DALY averted, 2020-21)",
           y = "Probability cost-effective",
           title = "Distributional cost-effectiveness acceptability curve",
           subtitle = sprintf(
             "P(NMB > 0) per quintile across the WTP grid; %d PSA draws.",
             params$psa_n)) +
      theme_paper()
    
    save_fig(fig4, "fig4_dceac", width = 9, height = 6)
    
    
    ### Figure 5 — Equity concentration indices ----
    # Bar chart of Wagstaff CIs across all seven outcomes. Single-glance view
    # of pro-poor vs pro-rich gradients. Colour-coded by sign and magnitude.
    
    eq_plot <- equity_summary |>
      mutate(outcome_label = factor(outcome_pretty[outcome],
                                    levels = rev(outcome_pretty[c(
                                      "dalys_averted",
                                      "oop_averted",
                                      "che_low_averted",
                                      "che_mid_averted",
                                      "che_high_averted",
                                      "pov_cases_averted",
                                      "mmvi_averted"
                                    )])),
             ci_sign = if_else(concentration_index < 0, "Pro-poor", "Pro-rich"))
    
    fig5 <- ggplot(eq_plot,
                   aes(x = outcome_label, y = concentration_index,
                       fill = ci_sign)) +
      geom_col() +
      geom_hline(yintercept = 0, colour = NEUTRAL, linewidth = 0.4) +
      geom_text(aes(label = sprintf("%.2f", concentration_index),
                    hjust = if_else(concentration_index < 0, 1.15, -0.15)),
                size = 3.5, colour = SABLE) +
      coord_flip() +
      scale_fill_manual(values = c("Pro-poor" = PRIMARY,
                                   "Pro-rich" = ACCENT),
                        name = NULL) +
      scale_y_continuous(limits = c(-1, 1),
                         breaks = seq(-1, 1, 0.25)) +
      labs(x = NULL, y = "Wagstaff concentration index",
           title    = "Concentration indices across FRP-ECEA outcomes",
           subtitle = paste("Negative = pro-poor; positive = pro-rich.",
                            "Range bounded at -1, +1.")) +
      theme_paper()
    
    save_fig(fig5, "fig5_concentration_indices", width = 9, height = 5)
    
    
    ### Figure 6 — Disease decomposition of cases and DALYs averted ----
    # Two-panel stacked bar: how each disease contributes to total cases
    # averted and total DALYs averted. Shows the pertussis/measles dominance
    # of the Indian VPD burden under HWC + IMI scale-up.
    
    decomp_data <- results_qd |>
      group_by(disease) |>
      summarise(`Cases averted` = sum(cases_averted, na.rm = TRUE),
                `DALYs averted` = sum(dalys_averted, na.rm = TRUE),
                .groups = "drop") |>
      mutate(disease = str_to_title(disease)) |>
      pivot_longer(c(`Cases averted`, `DALYs averted`),
                   names_to = "metric", values_to = "value") |>
      group_by(metric) |>
      mutate(share = value / sum(value)) |>
      ungroup()
    
    palette_disease <- c(PRIMARY, SECONDARY, GOLD, ACCENT, "#3A6BAB")
    names(palette_disease) <- sort(unique(decomp_data$disease))
    
    fig6 <- ggplot(decomp_data,
                   aes(x = metric, y = value, fill = disease)) +
      geom_col(position = "stack") +
      geom_text(aes(label = if_else(share > 0.05,
                                    sprintf("%s\n%.0f%%",
                                            disease, share * 100),
                                    "")),
                position = position_stack(vjust = 0.5),
                colour = "white", size = 3) +
      scale_fill_manual(values = palette_disease, name = "Disease") +
      scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
      labs(x = NULL, y = NULL,
           title    = "Disease decomposition of health gains",
           subtitle = "Stacked bars show absolute total; labels show share where > 5%.") +
      theme_paper() +
      theme(legend.position = "right")
    
    save_fig(fig6, "fig6_disease_decomposition", width = 9, height = 6)
    
    
    ### Figure 7 — Lorenz curves for FRP-ECEA outcomes ----
    # Cumulative share of each outcome (y-axis) against cumulative share of
    # population (x-axis), with quintiles sorted from poorest (Q1) to richest
    # (Q5). The 45-degree line represents perfect equality. Curves bowing
    # ABOVE the diagonal indicate pro-poor distributions; BELOW = pro-rich.
    # The horizontal distance from a Lorenz curve to the diagonal at any
    # cumulative population point equals the share by which that group's
    # outcome exceeds (above) or falls below (below) population share.
    
    lorenz_outcomes <- c(
      dalys_averted     = "DALYs averted",
      oop_averted       = "OOP averted",
      che_high_averted  = "CHE-40 cases averted",
      pov_cases_averted = "Poverty cases averted",
      mmvi_averted      = "MMVI"
    )
    
    lorenz_data <- results_q_total |>
      select(wealth_q,
             dalys_averted, oop_averted,
             che_high_averted, pov_cases_averted, mmvi_averted) |>
      arrange(wealth_q) |>
      pivot_longer(-wealth_q, names_to = "outcome",
                   values_to = "value") |>
      filter(outcome %in% names(lorenz_outcomes)) |>
      mutate(outcome = factor(lorenz_outcomes[outcome],
                              levels = unname(lorenz_outcomes))) |>
      group_by(outcome) |>
      mutate(cum_pop     = cumsum(rep(0.2, n())),
             cum_outcome = cumsum(value) / sum(value)) |>
      ungroup()
    
    # Prepend a (0,0) origin point per outcome
    lorenz_origin <- lorenz_data |>
      distinct(outcome) |>
      mutate(wealth_q = 0L, value = 0,
             cum_pop = 0, cum_outcome = 0)
    
    lorenz_full <- bind_rows(lorenz_origin, lorenz_data) |>
      arrange(outcome, cum_pop)
    
    palette_lorenz <- c(PRIMARY, SECONDARY, GOLD, ACCENT, "#3A6BAB")
    names(palette_lorenz) <- unname(lorenz_outcomes)
    
    fig7 <- ggplot(lorenz_full,
                   aes(x = cum_pop, y = cum_outcome,
                       colour = outcome, group = outcome)) +
      geom_abline(slope = 1, intercept = 0,
                  linetype = 2, colour = NEUTRAL, linewidth = 0.5) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      scale_colour_manual(values = palette_lorenz, name = NULL) +
      scale_x_continuous(limits = c(0, 1),
                         breaks = seq(0, 1, 0.2),
                         labels = scales::label_percent()) +
      scale_y_continuous(limits = c(0, 1),
                         breaks = seq(0, 1, 0.2),
                         labels = scales::label_percent()) +
      labs(x = "Cumulative population share (poorest to richest)",
           y = "Cumulative outcome share",
           title    = "Lorenz curves of FRP-ECEA outcomes",
           subtitle = paste("Above 45-degree line = pro-poor;",
                            "below = pro-rich. Dashed = perfect equality.")) +
      theme_paper() +
      theme(legend.position = "right")
    
    save_fig(fig7, "fig7_lorenz", width = 9, height = 7)
    
    
    ### Figure 8 — Sector x quintile results ----
    # Table 2b carries the full sector x quintile data
    
    if (!is.null(results_sq)) {
      
      ## Figure 8 — sector x quintile dodged bars ----
      
      fig8_data <- results_sq |>
        select(sector, wealth_q,
               `Cases averted`     = cases_averted,
               `DALYs averted`     = dalys_averted,
               `OOP averted (INR)` = oop_averted) |>
        pivot_longer(c(`Cases averted`, `DALYs averted`,
                       `OOP averted (INR)`),
                     names_to = "outcome", values_to = "value") |>
        mutate(outcome = factor(outcome,
                                levels = c("Cases averted",
                                           "DALYs averted",
                                           "OOP averted (INR)")),
               sector  = stringr::str_to_title(sector))
      
      # Ensure every (sector, wealth_q, outcome) cell exists
      fig8_data <- fig8_data |>
        tidyr::complete(sector  = c("Rural", "Urban"),
                        wealth_q = 1:5,
                        outcome  = levels(fig8_data$outcome),
                        fill = list(value = 0))
      
      fig8 <- ggplot(fig8_data,
                     aes(x = factor(wealth_q), y = value, fill = sector)) +
        geom_col(position = position_dodge2(preserve = "single",
                                            padding  = 0.1),
                 width = 0.9) +
        facet_wrap(~ outcome, scales = "free_y", ncol = 3) +
        scale_fill_manual(values = c(Rural = SECONDARY,
                                     Urban = PRIMARY),
                          name = "Sector") +
        scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
        labs(x = "Wealth quintile (1 = poorest, 5 = richest)",
             y = NULL,
             title    = "FRP-ECEA outcomes by sector and wealth quintile",
             subtitle = paste(
               "Rural and urban each take their sector's MPCE, poverty line,",
               "and sector-specific care-seeking gradient.",
               "Urban per-case OOP scaled by 1.6x."
             )) +
        theme_paper() +
        theme(legend.position = "top")
      
      save_fig(fig8, "fig8_sector_quintile", width = 11, height = 5)
    }
    
    
    ### Summary ----
    
    cat("\n===== Stage 12 — outputs written =====\n")
    cat(sprintf("Engine complete. %d states, %d diseases.\n",
                nrow(states_in), length(scope_diseases)))
    cat(sprintf("Coverage approach : %s\n", approach))
    cat(sprintf("PSA iterations    : %d\n", params$psa_n))
    cat(sprintf("MC draws per cell : %d\n", params$mc_n))
    cat(sprintf("Base NMB 1x GDPpc : INR %s\n",
                format(round(base_nmb), big.mark = ",")))
    cat("Output directories:\n")
    cat(sprintf("  %s/   (figures: PDF + PNG)\n", fig_dir))
    cat(sprintf("  %s/   (tables: HTML + PNG + DOCX + MD)\n", tab_dir))
    
    cat("\nEngine complete.\n")
  }
  
  
}  #