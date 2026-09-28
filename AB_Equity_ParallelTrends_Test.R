# AB EQUITY PARALLEL TRENDS TEST ----
#
#   Project : Equity in Childhood Vaccination Under Ayushman Bharat
#   Author  : Rohan Chitkara
#
# PURPOSE:
#   End to end pipeline that builds the five-round NFHS panel, fits
#   the event study with HWC density and IMI exposure as cross
#   sectional treatment intensities, and produces parallel-trends
#   diagnostics including coefficient plots and raw trajectory plots.
#
#   Stage 1. Pre-period extraction (NFHS-1, NFHS-2, NFHS-3)
#   Stage 2. Five-round panel (joins pre-period to NFHS-4 and NFHS-5)
#   Stage 3. Event-study regressions (HWC and IMI on FIC and CI)
#   Stage 4. Parallel-trends visualisation
#
# ALL OUTPUTS land in DHS_files/. Source the script from a working
# directory that contains the NFHS DHS .DTA files, kr_eligible.rds,
# and AB_Equity_Inputs_Final_v2.xlsx.


# PARALLEL TRENDS TEST ----

if (TRUE) {
  
  
  ## 1. STAGE 1 - PRE-PERIOD EXTRACTION (NFHS-1, NFHS-2, NFHS-3) ----
  
  
  # NFHS PRE-PERIOD EXTRACTION  NFHS-1, NFHS-2, NFHS-3 ----
  #
  #   Project : Equity in Childhood Vaccination Under Ayushman Bharat
  #   Author  : Rohan Chitkara
  #
  # PURPOSE:
  #   Build a pre-period coverage panel across NFHS-1 (1992-93), NFHS-2
  #   (1998-99), and NFHS-3 (2005-06) for parallel-trends testing on the
  #   NFHS-4 to NFHS-5 difference-in-differences identification.
  #
  # OUTPUTS in DHS_files/ :
  #   pre_period_NFHS{1,2,3}_kr.rds            eligible child level
  #   pre_period_NFHS{1,2,3}_coverage.rds      state mean FIC by round
  #   pre_period_NFHS{2,3}_ci_state.rds        state round CI
  #   pre_period_NFHS1_ci_state.rds            conditional on use_wi1
  #   pre_period_panel.rds                     combined coverage panel (raw)
  #   pre_period_ci_panel.rds                  combined CI panel (raw)
  #   pre_period_panel_harmonised.rds          coverage pooled to pre 2000 boundaries
  #   pre_period_ci_panel_harmonised.rds       CI pooled to pre 2000 boundaries
  #   pre_period_harmonisation_table.rds       reference table
  
  if (FALSE) {
    
    ### LIBRARIES ----
    
    library(haven)
    library(survey)
    library(dplyr)
    library(tidyr)
    library(stringr)
    library(tibble)
    
    
    ### FILE PATHS ----
    
    path_kr1 <- "1992-1993_India_DHS_Children's_Recode.DTA"
    path_kr2 <- "1998-1999_India_DHS_Children's_Recode.DTA"
    path_kr3 <- "2005-2006_India_DHS_Children's_Recode.DTA"
    
    path_wi1 <- "1992-1993_India_DHS_Wealth_Index.DTA"
    path_wi2 <- "1998-1999_India_DHS_Wealth_Index.DTA"
    
    # Set FALSE if the NFHS-1 retrospective wealth recode is not
    # acceptable for cross-round CI comparison.
    use_wi1  <- TRUE
    
    out_dir  <- "DHS_files"
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    
    
    ### Helper: Antigen Recode ----
    # DHS h-variable convention. Codes 1, 2, 3 (card date, mother's
    # report, card mark) are coded as received; 0 is coded as not
    # received; all other values become missing.
    
    vax_recode <- function(x) {
      case_when(
        x %in% c(1, 2, 3) ~ 1L,
        x == 0            ~ 0L,
        TRUE              ~ NA_integer_
      )
    }
    
    
    ### Helper: Safe Column Read ----
    
    safe_cols <- function(path, required, optional = character(0)) {
      actual <- names(read_dta(path, n_max = 0))
      missing_required <- setdiff(required, actual)
      if (length(missing_required) > 0) {
        message("  Required variables missing in ", basename(path), ": ",
                paste(missing_required, collapse = ", "))
      }
      present_required <- intersect(required, actual)
      present_optional <- intersect(optional, actual)
      unique(c(present_required, present_optional))
    }
    
    
    ### Helper: State Name from v024 Labels ----
    # DHS Stata files preserve v024 value labels. as_factor() returns
    # the state name without a hard coded code to name dictionary.
    # Falls back to the numeric code as a character string if labels
    # are absent.
    #
    # Cleaning steps applied here so downstream code receives canonical
    # state names:
    #   1. Lower case and trim whitespace.
    #   2. Strip bracketed two-letter code prefixes used by NFHS-3 for
    #      v024 labels (for example "[bh] bihar" becomes "bihar").
    #   3. Normalise spelling variants seen across rounds:
    #        "new delhi"          -> "delhi"
    #        "arunachalpradesh"   -> "arunachal pradesh"
    #        "jammu"              -> "jammu and kashmir"
    #        "uttranchal"         -> "uttaranchal"
    # Note: NFHS-1 surveyed only the Jammu region of Jammu and Kashmir
    # owing to fieldwork restrictions in Kashmir at the time. The
    # mapping above pools the NFHS-1 Jammu observation under the J&K
    # state label for harmonisation; the caveat should be carried in
    # the methods text.
    
    extract_state_name <- function(v024_vec) {
      out <- tryCatch(
        as.character(haven::as_factor(v024_vec)),
        error = function(e) as.character(as.integer(v024_vec))
      )
      out <- str_trim(tolower(out))
      out <- str_replace(out, "^\\s*\\[[^\\]]+\\]\\s*", "")
      out <- case_when(
        out == "new delhi"        ~ "delhi",
        out == "arunachalpradesh" ~ "arunachal pradesh",
        out == "jammu"            ~ "jammu and kashmir",
        out == "uttranchal"       ~ "uttaranchal",
        TRUE                      ~ out
      )
      out <- str_trim(out)
      out[out == "" | is.na(out)] <- NA_character_
      out
    }
    
    
    ### Helper: State Coverage with PSU/Strata Fallback ----
    # v021 (PSU) and v022 (strata) are not consistently populated in
    # older DHS rounds. When v021 is empty, the cluster v001 substitutes
    # for the PSU. When v022 is empty, an interaction of v024 (region)
    # with v025 (urban/rural) substitutes for the strata. The
    # substitution affects variance, not point estimates.
    
    state_coverage <- function(df, round_label) {
      options(survey.lonely.psu = "adjust")
      
      if (!"v021" %in% names(df) || all(is.na(df$v021))) {
        df$psu <- df$v001
        psu_src <- "v001 (fallback; v021 unavailable)"
      } else {
        df$psu <- df$v021
        psu_src <- "v021"
      }
      
      if (!"v022" %in% names(df) || all(is.na(df$v022))) {
        df$strata <- as.integer(interaction(df$v024, df$v025, drop = TRUE))
        str_src <- "v024 x v025 (fallback; v022 unavailable)"
      } else {
        df$strata <- df$v022
        str_src <- "v022"
      }
      
      df <- df |>
        filter(!is.na(psu), !is.na(strata), !is.na(wt)) |>
        mutate(fully_vaccinated = as.numeric(fully_vaccinated))
      
      cat("    [", round_label, "] PSU=", psu_src,
          "; strata=", str_src,
          "; n=", nrow(df), "\n", sep = "")
      
      name_lookup <- df |> distinct(state_code, state_name)
      
      svy <- svydesign(
        ids     = ~psu,
        strata  = ~strata,
        weights = ~wt,
        data    = df,
        nest    = TRUE
      )
      out <- svyby(~fully_vaccinated, ~state_code, svy,
                   svymean, na.rm = TRUE) |> as.data.frame()
      names(out) <- c("state_code", "coverage", "se")
      out$round    <- round_label
      out$ci_lower <- out$coverage - 1.96 * out$se
      out$ci_upper <- out$coverage + 1.96 * out$se
      
      out |>
        left_join(name_lookup, by = "state_code") |>
        select(state_code, state_name, round, coverage, se, ci_lower, ci_upper)
    }
    
    
    ### Helper: National Fractional Rank ----
    # Wagstaff (2002) fractional rank. Ranks are computed within round on
    # the full national sample so state CIs share a common wealth
    # ranking and are comparable across states.
    
    add_national_rank <- function(df) {
      df <- df |>
        filter(!is.na(fully_vaccinated), !is.na(wealth_cont), !is.na(wt)) |>
        arrange(wealth_cont)
      w <- df$wt; W <- cumsum(w); W_tot <- sum(w)
      df$rank_natl <- (W - w / 2) / W_tot
      df
    }
    
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
    
    state_ci <- function(kr_round, round_label) {
      kr_round |>
        group_by(state_code, state_name) |>
        group_modify(~ {
          ci_val <- compute_ci_natl(.x)
          se_val <- bootstrap_ci_se(.x, B = 500)
          bind_cols(ci_val, tibble(se_ci = se_val))
        }) |>
        ungroup() |>
        mutate(round = round_label,
               ci_lower = ci - 1.96 * se_ci,
               ci_upper = ci + 1.96 * se_ci)
    }
    
    
    ### Helper: Wealth Recode Merge (NFHS-1, NFHS-2) ----
    # The DHS retrospective wealth recode for NFHS-1 and NFHS-2 holds a
    # household identifier and the wealth index quintile and continuous
    # score. Three merge-key conventions appear across releases:
    #   1. whhid in WI plus caseid in KR. The DHS retrospective wealth
    #      recode for India NFHS-1 and NFHS-2 uses a 12-character whhid
    #      that matches the first 12 characters of the 15-character
    #      caseid. This is the primary path.
    #   2. hv001 + hv002 numeric keys (HR convention).
    #   3. v001  + v002  numeric keys (KR convention).
    
    merge_wealth_recode <- function(kr_df, wi_path, round_label) {
      if (!file.exists(wi_path)) {
        message("  Wealth recode not found at ", wi_path,
                " for ", round_label, "; proceeding without wealth merge.")
        kr_df$wealth_q    <- NA_integer_
        kr_df$wealth_cont <- NA_real_
        return(kr_df)
      }
      
      wi_raw   <- read_dta(wi_path)
      wi_names <- names(wi_raw)
      
      wq_candidates <- c("hv270", "wlthind5", "wlthind", "v190")
      wc_candidates <- c("hv271", "wlthindf", "wscore",  "v191")
      
      wq_var <- intersect(wq_candidates, wi_names)
      wc_var <- intersect(wc_candidates, wi_names)
      wq_var <- if (length(wq_var) > 0) wq_var[1] else NA_character_
      wc_var <- if (length(wc_var) > 0) wc_var[1] else NA_character_
      
      if (is.na(wq_var) && is.na(wc_var)) {
        message("  No wealth variables identified in ", basename(wi_path),
                ". First 30 names: ",
                paste(head(wi_names, 30), collapse = ", "))
        kr_df$wealth_q    <- NA_integer_
        kr_df$wealth_cont <- NA_real_
        return(kr_df)
      }
      
      keep_wi <- c(wq_var, wc_var)
      keep_wi <- keep_wi[!is.na(keep_wi)]
      
      rename_wealth <- function(df) {
        if (!is.na(wq_var)) {
          df <- df |> rename(wealth_q = !!wq_var)
        } else {
          df$wealth_q <- NA_integer_
        }
        if (!is.na(wc_var)) {
          df <- df |> rename(wealth_cont = !!wc_var)
        } else {
          df$wealth_cont <- NA_real_
        }
        df
      }
      
      if ("whhid" %in% wi_names && "caseid" %in% names(kr_df)) {
        
        wi_clean <- wi_raw |>
          select(whhid, all_of(keep_wi)) |>
          distinct(whhid, .keep_all = TRUE) |>
          rename_wealth()
        
        wi_width <- max(nchar(wi_raw$whhid), na.rm = TRUE)
        
        kr_df <- kr_df |>
          mutate(whhid_match = substr(caseid, 1, wi_width))
        
        cat("    Wealth merge for ", round_label,
            ": key=whhid (width=", wi_width,
            "), quintile=", ifelse(is.na(wq_var), "(none)", wq_var),
            ", score=",     ifelse(is.na(wc_var), "(none)", wc_var),
            ", WI rows=", nrow(wi_clean), "\n", sep = "")
        
        out <- kr_df |>
          left_join(wi_clean, by = c("whhid_match" = "whhid")) |>
          mutate(
            wealth_q    = as.integer(wealth_q),
            wealth_cont = as.numeric(wealth_cont)
          ) |>
          select(-whhid_match)
        
        return(out)
      }
      
      if (all(c("hv001", "hv002") %in% wi_names)) {
        wi_clean <- wi_raw |>
          select(hv001, hv002, all_of(keep_wi)) |>
          rename(v001 = hv001, v002 = hv002)
      } else if (all(c("v001", "v002") %in% wi_names)) {
        wi_clean <- wi_raw |>
          select(v001, v002, all_of(keep_wi))
      } else {
        message("  No supported merge keys (whhid+caseid, hv001+hv002, ",
                "or v001+v002) in ", basename(wi_path), ".")
        kr_df$wealth_q    <- NA_integer_
        kr_df$wealth_cont <- NA_real_
        return(kr_df)
      }
      
      wi_clean <- wi_clean |>
        rename_wealth() |>
        distinct(v001, v002, .keep_all = TRUE)
      
      cat("    Wealth merge for ", round_label,
          ": key=v001+v002, quintile=",
          ifelse(is.na(wq_var), "(none)", wq_var),
          ", score=", ifelse(is.na(wc_var), "(none)", wc_var),
          ", WI rows=", nrow(wi_clean), "\n", sep = "")
      
      kr_df |>
        left_join(wi_clean, by = c("v001", "v002")) |>
        mutate(
          wealth_q    = as.integer(wealth_q),
          wealth_cont = as.numeric(wealth_cont)
        )
    }
    
    
    ### Helper: Harmonise to Pre-2000 State Boundaries ----
    # Bihar split into Bihar + Jharkhand (Nov 2000); MP split into MP +
    # Chhattisgarh (Nov 2000); UP split into UP + Uttarakhand (Nov 2000);
    # AP split into AP + Telangana (June 2014). Pool the post-separation
    # child states to the undivided parent so NFHS-1, -2 (pre-2000) and
    # NFHS-3, -4, -5 (post-2000) panels share a common spatial unit.
    # Lower case state names are produced by extract_state_name().
    
    harmonise_state <- function(name_vec) {
      n <- str_trim(tolower(name_vec))
      case_when(
        n %in% c("bihar", "jharkhand")                      ~ "bihar (undivided)",
        n %in% c("madhya pradesh", "chhattisgarh")          ~ "mp (undivided)",
        n %in% c("uttar pradesh", "uttarakhand", "uttaranchal")
        ~ "up (undivided)",
        n %in% c("andhra pradesh", "telangana")             ~ "ap (undivided)",
        TRUE                                                ~ n
      )
    }
    
    harmonisation_table <- tribble(
      ~post_2000_state,    ~harm_parent_state,    ~note,
      "Bihar",              "Bihar (undivided)",  "Pre-2000",
      "Jharkhand",          "Bihar (undivided)",  "Separated Nov 2000",
      "Madhya Pradesh",     "MP (undivided)",     "Pre-2000",
      "Chhattisgarh",       "MP (undivided)",     "Separated Nov 2000",
      "Uttar Pradesh",      "UP (undivided)",     "Pre-2000",
      "Uttarakhand",        "UP (undivided)",     "Separated Nov 2000",
      "Andhra Pradesh",     "AP (undivided)",     "Pre-2014",
      "Telangana",          "AP (undivided)",     "Separated June 2014"
    )
    
    
    ### Round Processor ----
    # One pipeline serves all three rounds. inline_wealth = TRUE for
    # NFHS-3 (v190 / v191 in the KR file). inline_wealth = FALSE plus a
    # wi_path for NFHS-1 and NFHS-2.
    
    process_round <- function(kr_path,
                              round_label,
                              survey_year,
                              inline_wealth,
                              wi_path = NULL) {
      
      if (!file.exists(kr_path)) {
        message("  ", round_label, " KR file not found at ", kr_path,
                "; skipping.")
        return(list(kr = NULL, cov = NULL, ci = NULL))
      }
      
      cat("\n=== ", round_label, " (", survey_year, ") ===\n", sep = "")
      
      base_required <- c(
        "caseid", "v001", "v002", "v005",
        "v024", "v025",
        "b5", "h2", "h7", "h8", "h9", "hw1"
      )
      base_optional <- c("v003", "v021", "v022", "b4",
                         "h0", "h3", "h4", "h5", "h6")
      
      if (inline_wealth) {
        base_required <- c(base_required, "v190", "v191")
      }
      
      cols <- safe_cols(kr_path, base_required, base_optional)
      
      kr <- read_dta(kr_path, col_select = all_of(cols))
      
      state_name_vec <- extract_state_name(kr$v024)
      
      kr <- kr |>
        mutate(
          round       = round_label,
          survey_year = survey_year,
          state_code  = as.integer(v024),
          state_name  = state_name_vec,
          urban       = as.integer(v025 == 1),
          wt          = v005 / 1e6,
          bcg         = vax_recode(h2),
          dpt3        = vax_recode(h7),
          opv3        = vax_recode(h8),
          mcv1        = vax_recode(h9),
          age_months  = as.integer(hw1)
        ) |>
        mutate(
          fully_vaccinated = case_when(
            bcg == 1 & dpt3 == 1 & opv3 == 1 & mcv1 == 1 ~ 1L,
            bcg == 0 | dpt3 == 0 | opv3 == 0 | mcv1 == 0 ~ 0L,
            TRUE                                          ~ NA_integer_
          ),
          eligible = as.integer(age_months >= 12 & age_months <= 23 & b5 == 1)
        ) |>
        filter(eligible == 1)
      
      if (inline_wealth) {
        kr <- kr |>
          mutate(
            wealth_q    = as.integer(v190),
            wealth_cont = as.numeric(v191)
          )
      } else if (!is.null(wi_path)) {
        kr <- merge_wealth_recode(kr, wi_path, round_label)
      } else {
        kr$wealth_q    <- NA_integer_
        kr$wealth_cont <- NA_real_
      }
      
      cat("  Eligible children 12-23 months: ", nrow(kr), "\n", sep = "")
      cat("  Non missing FIC: ",
          sum(!is.na(kr$fully_vaccinated)), "\n", sep = "")
      cat("  Non missing wealth: ",
          sum(!is.na(kr$wealth_cont)), "\n", sep = "")
      cat("  Distinct state names: ",
          n_distinct(kr$state_name, na.rm = TRUE), "\n", sep = "")
      
      cov_round <- state_coverage(kr, round_label)
      cat("  State coverage cells: ", nrow(cov_round), "\n", sep = "")
      
      ci_round <- NULL
      if (sum(!is.na(kr$wealth_cont)) > 100) {
        kr_ranked <- add_national_rank(kr)
        ci_round  <- state_ci(kr_ranked, round_label)
        cat("  State CI cells: ", nrow(ci_round), "\n", sep = "")
        cat("  National CI (population weighted state mean): ",
            round(weighted.mean(ci_round$ci, ci_round$n, na.rm = TRUE), 4),
            "\n", sep = "")
      } else {
        message("  Insufficient wealth coverage; CI computation skipped.")
      }
      
      tag    <- gsub("-", "", round_label)
      rds_kr  <- file.path(out_dir, paste0("pre_period_", tag, "_kr.rds"))
      rds_cov <- file.path(out_dir, paste0("pre_period_", tag, "_coverage.rds"))
      csv_cov <- sub("\\.rds$", ".csv", rds_cov)
      
      saveRDS(kr,        rds_kr)
      saveRDS(cov_round, rds_cov)
      write.csv(cov_round, csv_cov, row.names = FALSE)
      
      if (!is.null(ci_round)) {
        rds_ci <- file.path(out_dir, paste0("pre_period_", tag, "_ci_state.rds"))
        csv_ci <- sub("\\.rds$", ".csv", rds_ci)
        saveRDS(ci_round, rds_ci)
        write.csv(ci_round, csv_ci, row.names = FALSE)
      }
      
      list(kr = kr, cov = cov_round, ci = ci_round)
    }
    
    
    ### Run Rounds ----
    
    res3 <- process_round(path_kr3, "NFHS-3", 2005,
                          inline_wealth = TRUE)
    
    res2 <- process_round(path_kr2, "NFHS-2", 1998,
                          inline_wealth = FALSE,
                          wi_path = path_wi2)
    
    res1 <- process_round(path_kr1, "NFHS-1", 1992,
                          inline_wealth = FALSE,
                          wi_path = if (use_wi1) path_wi1 else NULL)
    
    kr1 <- res1$kr; cov1 <- res1$cov; ci1 <- res1$ci
    kr2 <- res2$kr; cov2 <- res2$cov; ci2 <- res2$ci
    kr3 <- res3$kr; cov3 <- res3$cov; ci3 <- res3$ci
    
    
    ### State Harmonisation Reference ----
    
    cat("\nState harmonisation reference table:\n")
    print(harmonisation_table, width = Inf)
    
    saveRDS(harmonisation_table,
            file.path(out_dir, "pre_period_harmonisation_table.rds"))
    
    
    ### Combined Pre-Period Panels ----
    
    cat("\n=== COMBINED PRE-PERIOD PANELS ===\n")
    
    pre_period_panel <- bind_rows(
      if (!is.null(cov1)) cov1 |> mutate(state_basis = "pre-2000")  else NULL,
      if (!is.null(cov2)) cov2 |> mutate(state_basis = "pre-2000")  else NULL,
      if (!is.null(cov3)) cov3 |> mutate(state_basis = "post-2000") else NULL
    )
    
    pre_period_ci_panel <- bind_rows(
      if (!is.null(ci1)) ci1 |> mutate(state_basis = "pre-2000")  else NULL,
      if (!is.null(ci2)) ci2 |> mutate(state_basis = "pre-2000")  else NULL,
      if (!is.null(ci3)) ci3 |> mutate(state_basis = "post-2000") else NULL
    )
    
    if (!is.null(pre_period_panel) && nrow(pre_period_panel) > 0) {
      pre_period_panel <- pre_period_panel |>
        mutate(state_harmonised = harmonise_state(state_name))
      
      saveRDS(pre_period_panel,
              file.path(out_dir, "pre_period_panel.rds"))
      write.csv(pre_period_panel,
                file.path(out_dir, "pre_period_panel.csv"),
                row.names = FALSE)
      
      # Pool child states to undivided parents using inverse-variance
      # weights for the level FIC. Inverse-variance weighting downweights
      # state estimates with wide standard errors, which is appropriate
      # for the descriptive parallel-trends panel.
      pre_period_panel_harmonised <- pre_period_panel |>
        mutate(w_inv = 1 / pmax(se ^ 2, 1e-8)) |>
        group_by(round, state_harmonised) |>
        summarise(
          coverage   = weighted.mean(coverage, w_inv, na.rm = TRUE),
          n_subunits = n(),
          .groups    = "drop"
        )
      
      saveRDS(pre_period_panel_harmonised,
              file.path(out_dir, "pre_period_panel_harmonised.rds"))
      write.csv(pre_period_panel_harmonised,
                file.path(out_dir, "pre_period_panel_harmonised.csv"),
                row.names = FALSE)
      
      cat("\nCoverage panel: ", nrow(pre_period_panel),
          " state round cells across ",
          n_distinct(pre_period_panel$round), " rounds.\n", sep = "")
      
      cat("Coverage panel (harmonised): ",
          nrow(pre_period_panel_harmonised),
          " state round cells.\n", sep = "")
      
      cat("\nNational FIC trajectory (unweighted state mean):\n")
      print(pre_period_panel |>
              group_by(round) |>
              summarise(n_states = n(),
                        fic_mean = mean(coverage, na.rm = TRUE),
                        fic_min  = min(coverage,  na.rm = TRUE),
                        fic_max  = max(coverage,  na.rm = TRUE),
                        .groups  = "drop"),
            width = Inf)
    }
    
    if (!is.null(pre_period_ci_panel) && nrow(pre_period_ci_panel) > 0) {
      pre_period_ci_panel <- pre_period_ci_panel |>
        mutate(state_harmonised = harmonise_state(state_name))
      
      saveRDS(pre_period_ci_panel,
              file.path(out_dir, "pre_period_ci_panel.rds"))
      write.csv(pre_period_ci_panel,
                file.path(out_dir, "pre_period_ci_panel.csv"),
                row.names = FALSE)
      
      # Pool child-state CIs to undivided parents by sample-size weight.
      # The CI is non-linear in the data, so this is an approximation;
      # the full pooled CI would require recomputing on combined
      # microdata. For the descriptive parallel-trends panel the
      # approximation is acceptable.
      pre_period_ci_panel_harmonised <- pre_period_ci_panel |>
        group_by(round, state_harmonised) |>
        summarise(
          ci         = weighted.mean(ci, n, na.rm = TRUE),
          n          = sum(n,  na.rm = TRUE),
          n_subunits = dplyr::n(),
          .groups    = "drop"
        )
      
      saveRDS(pre_period_ci_panel_harmonised,
              file.path(out_dir, "pre_period_ci_panel_harmonised.rds"))
      write.csv(pre_period_ci_panel_harmonised,
                file.path(out_dir, "pre_period_ci_panel_harmonised.csv"),
                row.names = FALSE)
      
      cat("\nCI panel: ", nrow(pre_period_ci_panel),
          " state round cells with valid CI.\n", sep = "")
      
      cat("CI panel (harmonised): ",
          nrow(pre_period_ci_panel_harmonised),
          " state round cells.\n", sep = "")
      
      cat("\nNational CI trajectory (population weighted state mean):\n")
      print(pre_period_ci_panel |>
              group_by(round) |>
              summarise(n_states = n(),
                        ci_mean  = weighted.mean(ci, n, na.rm = TRUE),
                        ci_min   = min(ci, na.rm = TRUE),
                        ci_max   = max(ci, na.rm = TRUE),
                        .groups  = "drop"),
            width = Inf)
    }
    
  }
  
  
  
  
  ## 2. STAGE 2 - FIVE-ROUND PANEL (PRE-PERIOD + NFHS-4 + NFHS-5) ----
  
  
  # NFHS FIVE-ROUND HARMONISED PANEL ----
  #
  #   Project : Equity in Childhood Vaccination Under Ayushman Bharat
  #   Author  : Rohan Chitkara
  #
  # PURPOSE:
  #   Combine the pre-period extracts (NFHS-1, -2, -3) produced by
  #   AB_Equity_NFHS_PreTrends_Extraction.R with the current-period
  #   eligible file (kr_eligible.rds, NFHS-4 + NFHS-5) into a single
  #   five-round harmonised panel for the parallel-trends test.
  #
  #   Methodology is identical across all five rounds:
  #     - Survey-weighted state-mean FIC via svydesign()
  #     - Wagstaff (2002) bivariate CI using national-within-round
  #       fractional ranks of the wealth score
  #     - Bootstrap SE (B = 500) for the state CI
  #     - Harmonisation pools post-separation child states to
  #       pre-2000 / pre-2019 undivided parents
  #
  # OUTPUTS in DHS_files/ :
  #   five_round_panel.rds                coverage panel, all 5 rounds
  #   five_round_ci_panel.rds             CI panel, all 5 rounds
  #   five_round_panel_harmonised.rds     coverage at undivided-state resolution
  #   five_round_ci_panel_harmonised.rds  CI at undivided-state resolution
  
  if (FALSE) {
    
    ### LIBRARIES ----
    
    library(haven)
    library(survey)
    library(dplyr)
    library(tidyr)
    library(stringr)
    library(tibble)
    
    
    ### PATHS ----
    
    pre_dir   <- "DHS_files"
    kr45_path <- "DHS_files/kr_eligible.rds"
    out_dir   <- "DHS_files"
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    
    
    ### State Code Lookup (NFHS-5 Numbering) ----
    # kr_eligible.rds uses the NFHS-5 v024 numbering for both NFHS-4 and
    # NFHS-5 (NFHS-4 codes were recoded upstream). Code 26 is unused.
    
    state_code_lookup <- tribble(
      ~state_code, ~state_name,
      1L,  "jammu and kashmir",
      2L,  "himachal pradesh",
      3L,  "punjab",
      4L,  "chandigarh",
      5L,  "uttarakhand",
      6L,  "haryana",
      7L,  "delhi",
      8L,  "rajasthan",
      9L,  "uttar pradesh",
      10L, "bihar",
      11L, "sikkim",
      12L, "arunachal pradesh",
      13L, "nagaland",
      14L, "manipur",
      15L, "mizoram",
      16L, "tripura",
      17L, "meghalaya",
      18L, "assam",
      19L, "west bengal",
      20L, "jharkhand",
      21L, "odisha",
      22L, "chhattisgarh",
      23L, "madhya pradesh",
      24L, "gujarat",
      25L, "dadra and nagar haveli and daman and diu",
      27L, "maharashtra",
      28L, "andhra pradesh",
      29L, "karnataka",
      30L, "goa",
      31L, "lakshadweep",
      32L, "kerala",
      33L, "tamil nadu",
      34L, "puducherry",
      35L, "andaman and nicobar islands",
      36L, "telangana",
      37L, "ladakh"
    )
    
    
    ### Helper: State Coverage with PSU/Strata Fallback ----
    
    state_coverage <- function(df, round_label) {
      options(survey.lonely.psu = "adjust")
      
      if (!"v021" %in% names(df) || all(is.na(df$v021))) {
        df$psu <- df$v001
        psu_src <- "v001 (fallback; v021 unavailable)"
      } else {
        df$psu <- df$v021
        psu_src <- "v021"
      }
      
      if (!"v022" %in% names(df) || all(is.na(df$v022))) {
        df$strata <- as.integer(interaction(df$state_code, df$v025,
                                            drop = TRUE))
        str_src <- "state_code x v025 (fallback; v022 unavailable)"
      } else {
        df$strata <- df$v022
        str_src <- "v022"
      }
      
      df <- df |>
        filter(!is.na(psu), !is.na(strata), !is.na(wt)) |>
        mutate(fully_vaccinated = as.numeric(fully_vaccinated))
      
      cat("    [", round_label, "] PSU=", psu_src,
          "; strata=", str_src,
          "; n=", nrow(df), "\n", sep = "")
      
      name_lookup <- df |> distinct(state_code, state_name)
      
      svy <- svydesign(
        ids     = ~psu,
        strata  = ~strata,
        weights = ~wt,
        data    = df,
        nest    = TRUE
      )
      out <- svyby(~fully_vaccinated, ~state_code, svy,
                   svymean, na.rm = TRUE) |> as.data.frame()
      names(out) <- c("state_code", "coverage", "se")
      out$round    <- round_label
      out$ci_lower <- out$coverage - 1.96 * out$se
      out$ci_upper <- out$coverage + 1.96 * out$se
      
      out |>
        left_join(name_lookup, by = "state_code") |>
        select(state_code, state_name, round, coverage, se,
               ci_lower, ci_upper)
    }
    
    
    ### Helper: National Fractional Rank and CI ----
    
    add_national_rank <- function(df) {
      df <- df |>
        filter(!is.na(fully_vaccinated), !is.na(wealth_cont), !is.na(wt)) |>
        arrange(wealth_cont)
      w <- df$wt; W <- cumsum(w); W_tot <- sum(w)
      df$rank_natl <- (W - w / 2) / W_tot
      df
    }
    
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
    
    state_ci <- function(kr_round, round_label) {
      kr_round |>
        group_by(state_code, state_name) |>
        group_modify(~ {
          ci_val <- compute_ci_natl(.x)
          se_val <- bootstrap_ci_se(.x, B = 500)
          bind_cols(ci_val, tibble(se_ci = se_val))
        }) |>
        ungroup() |>
        mutate(round = round_label,
               ci_lower = ci - 1.96 * se_ci,
               ci_upper = ci + 1.96 * se_ci)
    }
    
    
    ### Helper: Harmonise to Pre-Separation Boundaries ----
    # Bihar split into Bihar + Jharkhand (Nov 2000); MP split into MP +
    # Chhattisgarh (Nov 2000); UP split into UP + Uttarakhand (Nov 2000);
    # AP split into AP + Telangana (June 2014); J&K split into J&K +
    # Ladakh (Oct 2019). Pool the post-separation child states to the
    # undivided parent so all five rounds share a common spatial unit.
    
    harmonise_state <- function(name_vec) {
      n <- str_trim(tolower(name_vec))
      case_when(
        n %in% c("bihar", "jharkhand")                          ~ "bihar (undivided)",
        n %in% c("madhya pradesh", "chhattisgarh")              ~ "mp (undivided)",
        n %in% c("uttar pradesh", "uttarakhand", "uttaranchal") ~ "up (undivided)",
        n %in% c("andhra pradesh", "telangana")                 ~ "ap (undivided)",
        n %in% c("jammu and kashmir", "ladakh")                 ~ "jk (undivided)",
        TRUE                                                    ~ n
      )
    }
    
    
    ### NFHS-4 and NFHS-5 Processing ----
    
    cat("\n=== NFHS-4 and NFHS-5 (kr_eligible.rds) ===\n")
    
    if (!file.exists(kr45_path)) {
      stop("kr_eligible.rds not found at ", kr45_path)
    }
    
    kr_45 <- readRDS(kr45_path) |>
      left_join(state_code_lookup, by = "state_code")
    
    unmapped <- kr_45 |>
      filter(is.na(state_name)) |>
      distinct(state_code) |>
      pull(state_code)
    if (length(unmapped) > 0) {
      message("  state_code values without a name in lookup: ",
              paste(unmapped, collapse = ", "))
    }
    
    kr4 <- kr_45 |> filter(round == "NFHS-4")
    kr5 <- kr_45 |> filter(round == "NFHS-5")
    
    cat("  NFHS-4 eligible: ", nrow(kr4),
        "; states: ", n_distinct(kr4$state_code), "\n", sep = "")
    cat("  NFHS-5 eligible: ", nrow(kr5),
        "; states: ", n_distinct(kr5$state_code), "\n", sep = "")
    
    # State-mean FIC.
    cov4 <- state_coverage(kr4, "NFHS-4")
    cov5 <- state_coverage(kr5, "NFHS-5")
    cat("  NFHS-4 state coverage cells: ", nrow(cov4), "\n", sep = "")
    cat("  NFHS-5 state coverage cells: ", nrow(cov5), "\n", sep = "")
    
    # State CI under national-within-round wealth ranks.
    kr4_ranked <- add_national_rank(kr4)
    kr5_ranked <- add_national_rank(kr5)
    
    ci4 <- state_ci(kr4_ranked, "NFHS-4")
    ci5 <- state_ci(kr5_ranked, "NFHS-5")
    cat("  NFHS-4 state CI cells: ", nrow(ci4), "\n", sep = "")
    cat("  NFHS-5 state CI cells: ", nrow(ci5), "\n", sep = "")
    
    cat("  NFHS-4 national CI (population-weighted): ",
        round(weighted.mean(ci4$ci, ci4$n, na.rm = TRUE), 4), "\n", sep = "")
    cat("  NFHS-5 national CI (population-weighted): ",
        round(weighted.mean(ci5$ci, ci5$n, na.rm = TRUE), 4), "\n", sep = "")
    
    # Save round-level outputs alongside the pre-period equivalents.
    saveRDS(cov4, file.path(out_dir, "post_period_NFHS4_coverage.rds"))
    saveRDS(cov5, file.path(out_dir, "post_period_NFHS5_coverage.rds"))
    saveRDS(ci4,  file.path(out_dir, "post_period_NFHS4_ci_state.rds"))
    saveRDS(ci5,  file.path(out_dir, "post_period_NFHS5_ci_state.rds"))
    
    
    ### Load Pre-Period Panels ----
    
    cat("\n=== Loading pre-period panels ===\n")
    
    pre_cov_path <- file.path(pre_dir, "pre_period_panel.rds")
    pre_ci_path  <- file.path(pre_dir, "pre_period_ci_panel.rds")
    
    if (!file.exists(pre_cov_path) || !file.exists(pre_ci_path)) {
      stop("Pre-period panels not found. Run ",
           "AB_Equity_NFHS_PreTrends_Extraction.R first.")
    }
    
    pre_cov <- readRDS(pre_cov_path)
    pre_ci  <- readRDS(pre_ci_path)
    
    cat("  Pre-period coverage: ", nrow(pre_cov), " cells across ",
        n_distinct(pre_cov$round), " rounds\n", sep = "")
    cat("  Pre-period CI:       ", nrow(pre_ci),  " cells across ",
        n_distinct(pre_ci$round),  " rounds\n", sep = "")
    
    
    ### Combine Five-Round Panels ----
    
    cat("\n=== COMBINED FIVE-ROUND PANELS ===\n")
    
    cov4 <- cov4 |> mutate(state_basis = "post-2000")
    cov5 <- cov5 |> mutate(state_basis = "post-2000")
    ci4  <- ci4  |> mutate(state_basis = "post-2000")
    ci5  <- ci5  |> mutate(state_basis = "post-2000")
    
    # Strip prior state_harmonised on pre-period panels and reapply with
    # the updated map (adds J&K + Ladakh case).
    pre_cov <- pre_cov |> select(-any_of("state_harmonised"))
    pre_ci  <- pre_ci  |> select(-any_of("state_harmonised"))
    
    five_round_panel <- bind_rows(pre_cov, cov4, cov5) |>
      mutate(state_harmonised = harmonise_state(state_name))
    
    five_round_ci_panel <- bind_rows(pre_ci, ci4, ci5) |>
      mutate(state_harmonised = harmonise_state(state_name))
    
    saveRDS(five_round_panel,
            file.path(out_dir, "five_round_panel.rds"))
    write.csv(five_round_panel,
              file.path(out_dir, "five_round_panel.csv"),
              row.names = FALSE)
    
    saveRDS(five_round_ci_panel,
            file.path(out_dir, "five_round_ci_panel.rds"))
    write.csv(five_round_ci_panel,
              file.path(out_dir, "five_round_ci_panel.csv"),
              row.names = FALSE)
    
    
    ### Harmonised Five-Round Panels ----
    
    five_round_panel_harm <- five_round_panel |>
      mutate(w_inv = 1 / pmax(se ^ 2, 1e-8)) |>
      group_by(round, state_harmonised) |>
      summarise(
        coverage   = weighted.mean(coverage, w_inv, na.rm = TRUE),
        n_subunits = n(),
        .groups    = "drop"
      )
    
    five_round_ci_panel_harm <- five_round_ci_panel |>
      group_by(round, state_harmonised) |>
      summarise(
        ci         = weighted.mean(ci, n, na.rm = TRUE),
        n          = sum(n,  na.rm = TRUE),
        n_subunits = dplyr::n(),
        .groups    = "drop"
      )
    
    saveRDS(five_round_panel_harm,
            file.path(out_dir, "five_round_panel_harmonised.rds"))
    write.csv(five_round_panel_harm,
              file.path(out_dir, "five_round_panel_harmonised.csv"),
              row.names = FALSE)
    
    saveRDS(five_round_ci_panel_harm,
            file.path(out_dir, "five_round_ci_panel_harmonised.rds"))
    write.csv(five_round_ci_panel_harm,
              file.path(out_dir, "five_round_ci_panel_harmonised.csv"),
              row.names = FALSE)
    
    
    ### Summary ----
    
    cat("\nFive-round panel: ", nrow(five_round_panel),
        " state round cells, ", n_distinct(five_round_panel$round),
        " rounds.\n", sep = "")
    cat("Five-round panel (harmonised): ", nrow(five_round_panel_harm),
        " cells.\n", sep = "")
    cat("Five-round CI panel: ", nrow(five_round_ci_panel),
        " cells.\n", sep = "")
    cat("Five-round CI panel (harmonised): ",
        nrow(five_round_ci_panel_harm), " cells.\n", sep = "")
    
    cat("\nNational FIC trajectory (unweighted state mean, raw panel):\n")
    print(five_round_panel |>
            group_by(round) |>
            summarise(n_states = n(),
                      fic_mean = mean(coverage, na.rm = TRUE),
                      fic_min  = min(coverage,  na.rm = TRUE),
                      fic_max  = max(coverage,  na.rm = TRUE),
                      .groups  = "drop"),
          width = Inf)
    
    cat("\nNational CI trajectory (population-weighted state mean, raw panel):\n")
    print(five_round_ci_panel |>
            group_by(round) |>
            summarise(n_states = n(),
                      ci_mean  = weighted.mean(ci, n, na.rm = TRUE),
                      ci_min   = min(ci, na.rm = TRUE),
                      ci_max   = max(ci, na.rm = TRUE),
                      .groups  = "drop"),
          width = Inf)
    
    cat("\nHarmonised cell counts by round:\n")
    print(five_round_panel_harm |>
            group_by(round) |>
            summarise(n_harm_cells = n(),
                      coverage_mean = mean(coverage, na.rm = TRUE),
                      .groups       = "drop"),
          width = Inf)
    
  }
  
  
  
  
  ## 3. STAGE 3 - EVENT STUDY REGRESSIONS ----
  
  
  # NFHS FIVE-ROUND EVENT STUDY ----
  #
  #   Project : Equity in Childhood Vaccination Under Ayushman Bharat
  #   Author  : Rohan Chitkara
  #
  # PURPOSE:
  #   Fit the five-round event study on the harmonised NFHS panel built
  #   by AB_Equity_NFHS_FiveRoundPanel.R. Two cross-sectional treatment
  #   intensities are loaded directly from AB_Equity_Inputs_Final_v2.xlsx
  #   and pooled to undivided-state resolution using state population
  #   weights:
  #     HWC density: xs_primary_feb2019_per_100k from R_treatment_intensity
  #     IMI exposure: imi_exposure_intensity from R_imi_exposure (skip = 5)
  #
  #   Specification:
  #     y_st = alpha_s + delta_t +
  #            sum_{r != NFHS-4} gamma_r [I(round=r) * x_s] + e_st
  #
  #   With NFHS-4 as the omitted reference round, gamma coefficients on
  #   NFHS-1, NFHS-2, NFHS-3 are leading-round (pre-trends) tests and
  #   gamma on NFHS-5 is the post-treatment effect. State fixed effects
  #   absorb time-invariant levels; round fixed effects absorb common
  #   trends. Standard errors are clustered at the harmonised state.
  
  if (TRUE) {
    
    ### LIBRARIES ----
    
    library(readxl)
    library(dplyr)
    library(tidyr)
    library(stringr)
    library(tibble)
    library(fixest)
    
    
    ### PATHS ----
    
    xlsx_path <- "AB_Equity_Inputs_Final_v2.xlsx"
    panel_dir <- "DHS_files"
    out_dir   <- "DHS_files"
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    
    
    ### Helper: State Name Canonicalisation and Harmonisation ----
    
    clean_state_name <- function(name_vec) {
      out <- str_trim(tolower(as.character(name_vec)))
      out <- str_replace(out, "^\\s*\\[[^\\]]+\\]\\s*", "")
      out <- str_replace_all(out, " & ", " and ")
      out <- case_when(
        out == "new delhi"            ~ "delhi",
        out == "nct of delhi"         ~ "delhi",
        out == "arunachalpradesh"     ~ "arunachal pradesh",
        out == "jammu"                ~ "jammu and kashmir",
        out == "uttranchal"           ~ "uttaranchal",
        out == "orissa"               ~ "odisha",
        out == "a and n islands"      ~ "andaman and nicobar islands",
        TRUE                          ~ out
      )
      out <- str_trim(out)
      out[out == "" | is.na(out)] <- NA_character_
      out
    }
    
    harmonise_state <- function(name_vec) {
      n <- str_trim(tolower(name_vec))
      case_when(
        n %in% c("bihar", "jharkhand")                          ~ "bihar (undivided)",
        n %in% c("madhya pradesh", "chhattisgarh")              ~ "mp (undivided)",
        n %in% c("uttar pradesh", "uttarakhand", "uttaranchal") ~ "up (undivided)",
        n %in% c("andhra pradesh", "telangana")                 ~ "ap (undivided)",
        n %in% c("jammu and kashmir", "ladakh")                 ~ "jk (undivided)",
        TRUE                                                    ~ n
      )
    }
    
    
    ### Load HWC Density ----
    
    cat("\n=== Loading HWC density ===\n")
    
    hwc_raw <- read_excel(xlsx_path, sheet = "R_treatment_intensity")
    
    hwc <- hwc_raw |>
      select(state_ut,
             hwc_count    = hwc_oper_feb2019,
             pop          = population_2021,
             hwc_per_100k = xs_primary_feb2019_per_100k) |>
      mutate(
        state_ut         = as.character(state_ut),
        state_clean      = clean_state_name(state_ut),
        state_harmonised = harmonise_state(state_clean),
        hwc_count        = as.numeric(hwc_count),
        pop              = as.numeric(pop),
        hwc_per_100k     = as.numeric(hwc_per_100k)
      ) |>
      filter(!is.na(state_harmonised), !is.na(hwc_per_100k), !is.na(pop))
    
    cat("  HWC rows loaded:        ", nrow(hwc), "\n", sep = "")
    cat("  Unique states (raw):    ",
        n_distinct(hwc$state_clean), "\n", sep = "")
    cat("  Unique harmonised:      ",
        n_distinct(hwc$state_harmonised), "\n", sep = "")
    
    
    ### Load IMI Exposure ----
    # The R_imi_exposure sheet has a metadata block at the top whose
    # height varies across versions of the workbook. Scan candidate
    # skip values until the read returns columns that include
    # state_ut and imi_exposure_intensity.
    
    cat("\n=== Loading IMI exposure ===\n")
    
    imi_raw       <- NULL
    skip_used_imi <- NA_integer_
    
    for (skp in c(5, 4, 6, 3, 2, 1, 0)) {
      tmp <- tryCatch(
        suppressMessages(
          read_excel(xlsx_path, sheet = "R_imi_exposure", skip = skp)),
        error = function(e) NULL
      )
      if (is.null(tmp)) next
      has_state    <- any(c("state_ut", "state", "state_name")
                          %in% names(tmp))
      has_exposure <- any(c("imi_exposure_intensity",
                            "exposure_intensity")
                          %in% names(tmp))
      if (has_state && has_exposure) {
        imi_raw       <- tmp
        skip_used_imi <- skp
        break
      }
    }
    
    if (is.null(imi_raw)) {
      stop("Could not locate header row in R_imi_exposure with ",
           "state_ut and imi_exposure_intensity. Inspect the sheet ",
           "manually and set skip in the loop above.")
    }
    
    cat("  Detected skip = ", skip_used_imi, "\n", sep = "")
    cat("  IMI sheet columns: ",
        paste(names(imi_raw), collapse = ", "), "\n", sep = "")
    
    state_col_imi <- intersect(c("state_ut", "state", "state_name"),
                               names(imi_raw))[1]
    exposure_col  <- intersect(c("imi_exposure_intensity",
                                 "exposure_intensity"),
                               names(imi_raw))[1]
    
    imi <- imi_raw |>
      select(state_ut      = !!state_col_imi,
             imi_intensity = !!exposure_col) |>
      mutate(
        state_ut         = as.character(state_ut),
        imi_intensity    = as.numeric(imi_intensity),
        state_clean      = clean_state_name(state_ut),
        state_harmonised = harmonise_state(state_clean)
      ) |>
      filter(!is.na(state_harmonised), !is.na(imi_intensity))
    
    cat("  IMI rows loaded:        ", nrow(imi), "\n", sep = "")
    cat("  Unique states (raw):    ",
        n_distinct(imi$state_clean), "\n", sep = "")
    cat("  Unique harmonised:      ",
        n_distinct(imi$state_harmonised), "\n", sep = "")
    
    
    ### Pool Treatments to Harmonised State ----
    
    hwc_pool <- hwc |>
      group_by(state_harmonised) |>
      summarise(
        hwc_per_100k = weighted.mean(hwc_per_100k, pop, na.rm = TRUE),
        pop_total    = sum(pop, na.rm = TRUE),
        n_subunits   = n(),
        .groups      = "drop"
      )
    
    imi_pool <- imi |>
      left_join(hwc |> select(state_clean, pop), by = "state_clean") |>
      group_by(state_harmonised) |>
      summarise(
        imi_intensity = weighted.mean(imi_intensity, pop, na.rm = TRUE),
        pop_total     = sum(pop, na.rm = TRUE),
        n_subunits    = n(),
        .groups       = "drop"
      )
    
    treat_panel <- hwc_pool |>
      select(state_harmonised, hwc_per_100k) |>
      full_join(imi_pool |> select(state_harmonised, imi_intensity),
                by = "state_harmonised")
    
    saveRDS(treat_panel,
            file.path(out_dir, "event_study_treatment_panel.rds"))
    
    cat("\nHarmonised treatment panel:\n")
    print(as.data.frame(treat_panel))
    
    
    ### Load Harmonised Five-Round Panels ----
    
    cat("\n=== Loading harmonised five-round panels ===\n")
    
    fic_panel_path <- file.path(panel_dir, "five_round_panel_harmonised.rds")
    ci_panel_path  <- file.path(panel_dir, "five_round_ci_panel_harmonised.rds")
    
    if (!file.exists(fic_panel_path) || !file.exists(ci_panel_path)) {
      stop("Five-round harmonised panels not found. Run ",
           "AB_Equity_NFHS_FiveRoundPanel.R first.")
    }
    
    fic_panel <- readRDS(fic_panel_path)
    ci_panel  <- readRDS(ci_panel_path)
    
    
    ### Join Treatments onto Panels ----
    
    fic_join <- fic_panel |>
      left_join(treat_panel, by = "state_harmonised") |>
      mutate(round = factor(round,
                            levels = c("NFHS-1", "NFHS-2", "NFHS-3",
                                       "NFHS-4", "NFHS-5")))
    
    ci_join <- ci_panel |>
      left_join(treat_panel, by = "state_harmonised") |>
      mutate(round = factor(round,
                            levels = c("NFHS-1", "NFHS-2", "NFHS-3",
                                       "NFHS-4", "NFHS-5")))
    
    cat("  FIC panel rows: ", nrow(fic_join),
        "; with HWC: ", sum(!is.na(fic_join$hwc_per_100k)),
        "; with IMI: ", sum(!is.na(fic_join$imi_intensity)),
        "\n", sep = "")
    cat("  CI  panel rows: ", nrow(ci_join),
        "; with HWC: ", sum(!is.na(ci_join$hwc_per_100k)),
        "; with IMI: ", sum(!is.na(ci_join$imi_intensity)),
        "\n", sep = "")
    
    
    ### Event Study Regressions ----
    
    cat("\n=== Event study: HWC density on level FIC ===\n")
    m_hwc_fic <- feols(
      coverage ~ i(round, hwc_per_100k, ref = "NFHS-4") |
        state_harmonised + round,
      data    = fic_join |> filter(!is.na(hwc_per_100k)),
      cluster = ~state_harmonised
    )
    print(summary(m_hwc_fic))
    
    cat("\n=== Event study: HWC density on state CI ===\n")
    m_hwc_ci <- feols(
      ci ~ i(round, hwc_per_100k, ref = "NFHS-4") |
        state_harmonised + round,
      data    = ci_join |> filter(!is.na(hwc_per_100k)),
      cluster = ~state_harmonised
    )
    print(summary(m_hwc_ci))
    
    cat("\n=== Event study: IMI exposure on level FIC ===\n")
    m_imi_fic <- feols(
      coverage ~ i(round, imi_intensity, ref = "NFHS-4") |
        state_harmonised + round,
      data    = fic_join |> filter(!is.na(imi_intensity)),
      cluster = ~state_harmonised
    )
    print(summary(m_imi_fic))
    
    cat("\n=== Event study: IMI exposure on state CI ===\n")
    m_imi_ci <- feols(
      ci ~ i(round, imi_intensity, ref = "NFHS-4") |
        state_harmonised + round,
      data    = ci_join |> filter(!is.na(imi_intensity)),
      cluster = ~state_harmonised
    )
    print(summary(m_imi_ci))
    
    
    ### Extract Coefficient Tables ----
    
    extract_coefs <- function(model, treat_name, outcome_name) {
      co <- as.data.frame(summary(model)$coeftable)
      co$term      <- rownames(co)
      rownames(co) <- NULL
      names(co)[1:4] <- c("estimate", "std_error", "t_value", "p_value")
      co$conf_low  <- co$estimate - 1.96 * co$std_error
      co$conf_high <- co$estimate + 1.96 * co$std_error
      co$treatment <- treat_name
      co$outcome   <- outcome_name
      co[, c("treatment", "outcome", "term", "estimate", "std_error",
             "conf_low", "conf_high", "p_value")]
    }
    
    coef_hwc_fic <- extract_coefs(m_hwc_fic, "HWC", "FIC")
    coef_hwc_ci  <- extract_coefs(m_hwc_ci,  "HWC", "CI")
    coef_imi_fic <- extract_coefs(m_imi_fic, "IMI", "FIC")
    coef_imi_ci  <- extract_coefs(m_imi_ci,  "IMI", "CI")
    
    saveRDS(coef_hwc_fic, file.path(out_dir, "event_study_hwc_fic.rds"))
    saveRDS(coef_hwc_ci,  file.path(out_dir, "event_study_hwc_ci.rds"))
    saveRDS(coef_imi_fic, file.path(out_dir, "event_study_imi_fic.rds"))
    saveRDS(coef_imi_ci,  file.path(out_dir, "event_study_imi_ci.rds"))
    
    
    ### Pre-Trends Test ----
    
    pre_trends_F <- function(model, label) {
      leading <- grep("NFHS-1|NFHS-2|NFHS-3",
                      names(coef(model)), value = TRUE)
      if (length(leading) == 0) {
        cat("  ", label, ": no leading-round terms found.\n", sep = "")
        return(invisible(NULL))
      }
      w <- wald(model, leading, print = FALSE)
      cat(sprintf("  %-30s F = %6.3f, df = %d, p = %.4f\n",
                  paste0(label, ":"), w$stat, w$df1, w$p))
    }
    
    cat("\n=== Pre-trends test (joint Wald on leading rounds) ===\n")
    pre_trends_F(m_hwc_fic, "HWC -> FIC")
    pre_trends_F(m_hwc_ci,  "HWC -> CI")
    pre_trends_F(m_imi_fic, "IMI -> FIC")
    pre_trends_F(m_imi_ci,  "IMI -> CI")
    
    
    ### Treatment-Effect Summary ----
    
    format_treatment_summary <- function(coef_df, label) {
      post <- coef_df |>
        filter(grepl("NFHS-5", term)) |>
        head(1)
      pre <- coef_df |>
        filter(grepl("NFHS-1|NFHS-2|NFHS-3", term)) |>
        mutate(reject = p_value < 0.05) |>
        summarise(any_reject = any(reject, na.rm = TRUE))
      cat(sprintf("  %-12s post (NFHS-5): est = %.5f, 95%% CI [%.5f, %.5f], p = %.4f\n",
                  paste0(label, ":"),
                  post$estimate, post$conf_low, post$conf_high, post$p_value))
      cat(sprintf("              any leading-round coef p < 0.05? %s\n",
                  ifelse(pre$any_reject, "YES (pre-trends violated)",
                         "no (pre-trends ok)")))
    }
    
    cat("\n=== Treatment-effect summary ===\n")
    format_treatment_summary(coef_hwc_fic, "HWC->FIC")
    format_treatment_summary(coef_hwc_ci,  "HWC->CI ")
    format_treatment_summary(coef_imi_fic, "IMI->FIC")
    format_treatment_summary(coef_imi_ci,  "IMI->CI ")
    
    
    cat("\nEvent study complete. Coefficient tables saved in ",
        out_dir, ".\n", sep = "")
    
  }
  
  
  
  
  ## 4. STAGE 4 - PARALLEL TRENDS VISUALISATION ----
  
  
  if (TRUE) {
    
    ### LIBRARIES ----
    
    library(ggplot2)
    library(patchwork)
    library(dplyr)
    library(tidyr)
    library(stringr)
    
    
    ### Event-Study Coefficient Plot ----
    # Combine the four coefficient tables from Stage 3, parse the round
    # label out of the fixest term name, and add a zero reference at
    # NFHS-4 (the omitted round in i(round, x_treat, ref = "NFHS-4")).
    
    cat("\n=== Stage 4: Parallel trends visualisation ===\n")
    
    round_levels <- c("NFHS-1", "NFHS-2", "NFHS-3", "NFHS-4", "NFHS-5")
    
    all_coefs <- bind_rows(
      coef_hwc_fic, coef_hwc_ci, coef_imi_fic, coef_imi_ci
    ) |>
      mutate(round = str_extract(term, "NFHS-[1-5]")) |>
      filter(!is.na(round)) |>
      mutate(round = factor(round, levels = round_levels))
    
    ref_rows <- tibble::tibble(
      treatment = rep(c("HWC", "IMI"), each = 2),
      outcome   = rep(c("FIC", "CI"), times = 2),
      round     = factor("NFHS-4", levels = round_levels),
      estimate  = 0,
      std_error = 0,
      conf_low  = 0,
      conf_high = 0,
      p_value   = 1,
      term      = "ref::NFHS-4"
    )
    
    plot_data <- bind_rows(all_coefs, ref_rows) |>
      mutate(
        panel    = paste0(treatment, " \u2192 ", outcome),
        panel    = factor(panel,
                          levels = c("HWC \u2192 FIC", "HWC \u2192 CI",
                                     "IMI \u2192 FIC", "IMI \u2192 CI")),
        is_ref   = term == "ref::NFHS-4",
        is_post  = round == "NFHS-5"
      )
    
    p_event <- ggplot(plot_data, aes(x = round, y = estimate)) +
      geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.4) +
      geom_errorbar(aes(ymin = conf_low, ymax = conf_high),
                    width = 0.15, colour = "grey30") +
      geom_point(aes(colour = is_ref, shape = is_ref), size = 2.4) +
      scale_colour_manual(values = c(`TRUE` = "grey40", `FALSE` = "black"),
                          guide = "none") +
      scale_shape_manual(values = c(`TRUE` = 1, `FALSE` = 16),
                         guide = "none") +
      facet_wrap(~ panel, scales = "free_y", ncol = 2) +
      labs(
        title    = "Event study: leading and lagging interaction coefficients",
        subtitle = paste0(
          "Reference round: NFHS-4 (open circle, fixed at zero). ",
          "Coefficients on NFHS-1, -2, -3 test parallel pre-trends."),
        x        = NULL,
        y        = "Round x treatment coefficient (95% CI)"
      ) +
      theme_bw(base_size = 11) +
      theme(
        plot.title       = element_text(face = "bold"),
        plot.subtitle    = element_text(colour = "grey30"),
        strip.background = element_rect(fill = "grey95", colour = NA),
        strip.text       = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )
    
    print(p_event)
    
    ggsave(file.path(out_dir, "parallel_trends_event_study.pdf"),
           p_event, width = 9, height = 6, units = "in")
    ggsave(file.path(out_dir, "parallel_trends_event_study.png"),
           p_event, width = 9, height = 6, units = "in", dpi = 200)
    
    
    ### Raw Trajectory Plot by Treatment Tercile ----
    # Cuts the harmonised states into terciles by HWC density and IMI
    # exposure, then plots state-level FIC and CI trajectories with
    # tercile means overlaid. Pre-period (NFHS-1, -2, -3) lines should
    # be roughly parallel across terciles for the DiD identifying
    # assumption to hold visually.
    
    treat_terciles <- treat_panel |>
      mutate(
        hwc_tercile = ntile(hwc_per_100k, 3),
        imi_tercile = ntile(imi_intensity, 3)
      )
    
    build_traj <- function(panel_df, outcome_col, tercile_col,
                           tercile_label) {
      panel_df |>
        left_join(treat_terciles |>
                    select(state_harmonised, all_of(tercile_col)),
                  by = "state_harmonised") |>
        filter(!is.na(.data[[tercile_col]])) |>
        mutate(
          round       = factor(round, levels = round_levels),
          round_num   = as.integer(round),
          tercile     = factor(.data[[tercile_col]],
                               levels = 1:3,
                               labels = c(paste0("Low ",  tercile_label),
                                          paste0("Mid ",  tercile_label),
                                          paste0("High ", tercile_label))),
          outcome_val = .data[[outcome_col]]
        )
    }
    
    fic_hwc <- build_traj(fic_panel, "coverage", "hwc_tercile", "HWC")
    ci_hwc  <- build_traj(ci_panel,  "ci",       "hwc_tercile", "HWC")
    fic_imi <- build_traj(fic_panel, "coverage", "imi_tercile", "IMI")
    ci_imi  <- build_traj(ci_panel,  "ci",       "imi_tercile", "IMI")
    
    traj_panel <- function(df, y_label, title_text) {
      means <- df |>
        group_by(tercile, round, round_num) |>
        summarise(mean_y = mean(outcome_val, na.rm = TRUE),
                  .groups = "drop")
      
      ggplot(df, aes(x = round_num, y = outcome_val)) +
        geom_line(aes(group = state_harmonised, colour = tercile),
                  alpha = 0.25, linewidth = 0.4) +
        geom_line(data = means,
                  aes(x = round_num, y = mean_y,
                      colour = tercile, group = tercile),
                  linewidth = 1.3) +
        geom_point(data = means,
                   aes(x = round_num, y = mean_y, colour = tercile),
                   size = 2.4) +
        scale_x_continuous(breaks = 1:5, labels = round_levels) +
        scale_colour_manual(values = c("#3b6ea5", "#888888", "#c1432a")) +
        labs(title = title_text, x = NULL, y = y_label, colour = NULL) +
        theme_bw(base_size = 11) +
        theme(
          plot.title       = element_text(face = "bold"),
          legend.position  = "bottom",
          panel.grid.minor = element_blank()
        )
    }
    
    p_fic_hwc <- traj_panel(fic_hwc, "Coverage (FIC)",
                            "FIC trajectory by HWC density tercile")
    p_ci_hwc  <- traj_panel(ci_hwc,  "Concentration index",
                            "CI trajectory by HWC density tercile")
    p_fic_imi <- traj_panel(fic_imi, "Coverage (FIC)",
                            "FIC trajectory by IMI exposure tercile")
    p_ci_imi  <- traj_panel(ci_imi,  "Concentration index",
                            "CI trajectory by IMI exposure tercile")
    
    p_traj <- (p_fic_hwc | p_ci_hwc) / (p_fic_imi | p_ci_imi) +
      plot_annotation(
        title    = "Raw trajectories by treatment tercile",
        subtitle = paste0(
          "Faint lines: state-level series. ",
          "Bold lines: tercile means."),
        theme    = theme(plot.title    = element_text(face = "bold"),
                         plot.subtitle = element_text(colour = "grey30"))
      )
    
    print(p_traj)
    
    ggsave(file.path(out_dir, "parallel_trends_trajectories.pdf"),
           p_traj, width = 11, height = 8, units = "in")
    ggsave(file.path(out_dir, "parallel_trends_trajectories.png"),
           p_traj, width = 11, height = 8, units = "in", dpi = 200)
    
    
    ### Textual Parallel-Trends Report ----
    # Restate the leading-round Wald p-values and the NFHS-5 effect
    # next to the figures so the diagnostic is auditable in the log.
    
    cat("\nParallel-trends diagnostic summary:\n")
    cat("\n  Joint Wald test on leading rounds (NFHS-1, -2, -3):\n")
    pre_trends_F(m_hwc_fic, "    HWC -> FIC")
    pre_trends_F(m_hwc_ci,  "    HWC -> CI")
    pre_trends_F(m_imi_fic, "    IMI -> FIC")
    pre_trends_F(m_imi_ci,  "    IMI -> CI")
    
    cat("\n  Post-treatment NFHS-5 effect:\n")
    format_treatment_summary(coef_hwc_fic, "    HWC->FIC")
    format_treatment_summary(coef_hwc_ci,  "    HWC->CI ")
    format_treatment_summary(coef_imi_fic, "    IMI->FIC")
    format_treatment_summary(coef_imi_ci,  "    IMI->CI ")
    
    cat("\nFigures saved:\n")
    cat("  ", file.path(out_dir, "parallel_trends_event_study.pdf"), "\n",
        sep = "")
    cat("  ", file.path(out_dir, "parallel_trends_event_study.png"), "\n",
        sep = "")
    cat("  ", file.path(out_dir, "parallel_trends_trajectories.pdf"), "\n",
        sep = "")
    cat("  ", file.path(out_dir, "parallel_trends_trajectories.png"), "\n",
        sep = "")
    
    cat("\nParallel-trends test complete.\n")
    
  }
  
}