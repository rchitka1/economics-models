# MPIE FINAL PROJECT — BURKINA FASO iCCM EVALUATION ----
#
#   Project : Methods for Planning and Implementing Evaluations of
#             Large-Scale Health Programs (MPIE), AY 2025-26
#   Author  : Rohan Chitkara
#
# Based on the Munos et al. 2016 difference-in-differences analysis of
# the Burkina Faso Rapid Scale-Up iCCM programme using the subset
# baseline (2010-11) and pooled baseline + endline (2013-14) datasets.
#
#   Section 1 — Setup, helpers, data loading and cleaning
#   Section 2 — Q1 Balance check between intervention and comparison areas
#   Section 3 — Q2 Baseline coverage point estimates
#   Section 4 — Q3 Longitudinal change in each evaluation area
#   Section 5 — Q4 Difference-in-differences estimation + table/figure data
#   Section 6 — Tables (Q1 balance, Q2 coverage, Q3 change, Q4 DiD)
#   Section 7 — Figures (Coverage by area, DiD in coverage)
#   Section 8 — Report output
#
# All calculations and data shaping live in Sections 1–5. Sections 6 and 7
# only build gt() tables and ggplot() figures from pre-built frames.
#
# Indicators: vitamin A supplementation (6-59 months) and ORS for diarrhea.
# Tables saved to tables/ as HTML, DOCX, PNG.
# Figures saved to figures/ as PDF, PNG.
# Quarto output saved to report/ as DOCX with associated files. Text to be 
# filled in by team as necessary
# Q1-Q4 are write-ups using this output.
# Q5-Q9 use output from R code and Annex2 for QoC tables.


# MPIE FINAL ----

if (TRUE) {
  
  
  ## 1. SETUP ----
  
  if (TRUE) {
    
    
    ### LIBRARIES ----
    
    library(haven)
    library(dplyr)
    library(tidyr)
    library(purrr)
    library(survey)
    library(knitr)
    library(broom)
    library(gt)
    library(ggplot2)
    library(scales)
    
    
    ### FILE PATHS ----
    
    data_dir <- "."
    tab_dir  <- "tables"
    if (!dir.exists(tab_dir)) dir.create(tab_dir, recursive = TRUE)
    fig_dir <- "figures"
    if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)
    
    
    ### SURVEY OPTIONS ----
    
    # Adjusts variance for any singleton stratum that may emerge after
    # subsetting on small denominators (Wilson Lecture 7, Step 3).
    options(survey.lonely.psu = "adjust")
    
    
    ### COLOUR PALETTE ----
    
    PRIMARY      <- "#002D72"
    SECONDARY    <- "#68ACE5"
    NEUTRAL      <- "#666666"
    LIGHT_GREY   <- "#E5E5E5"
    SECONDARY_50 <- "#B3D5F2"
    SECONDARY_75 <- "#8DBEE9"
    ACCENT       <- "#A89968"
    ACCENT_LIGHT <- "#F4EFDF"
    
    
    ### CONSTANTS ----
    
    indicator_levels <- c("Vitamin A supplementation (6\u201359 mo)",
                          "ORS for diarrhea")
    
    
    ### HELPERS ----
    
    # Translation to english. Baseline codes "no schooling" as
    # "Aucun niveau" etc., to unify across survey cycles.
    recode_education <- function(x) {
      x <- as.character(x)
      dplyr::case_when(
        is.na(x) | x == "NA"                                ~ NA_character_,
        x %in% c("Aucun niveau", "")                        ~ "None",
        x == "Primaire"                                     ~ "Primary",
        x %in% c("Secondaire 1er cycle",
                 "Secondaire 1ere cycle")                   ~ "Lower secondary",
        x %in% c("Secondaire 2ème cycle",
                 "Secondaire 2\u00c3\u00a8me cycle")        ~ "Upper secondary",
        x %in% c("Supérieur",
                 "Sup\u00c3\u00a9rieur")                    ~ "Higher",
        TRUE                                                ~ NA_character_
      )
    }
    
    recode_wealth <- function(x) {
      x <- as.character(x)
      dplyr::case_when(
        is.na(x) | x == "NA" | x == ""                      ~ NA_character_,
        x %in% c("Très Pauvres",
                 "Tr\u00c3\u00a8s Pauvres")                 ~ "Poorest",
        x == "Pauvres"                                      ~ "Poor",
        x == "Moyens"                                       ~ "Middle",
        x == "Riches"                                       ~ "Rich",
        x %in% c("Très Riches",
                 "Tr\u00c3\u00a8s Riches")                  ~ "Richest",
        TRUE                                                ~ NA_character_
      )
    }
    
    # Table style
    apply_table_style <- function(gt_table) {
      gt_table |>
        tab_options(
          table.font.names                  = "Calibri",
          table.font.size                   = px(10),
          heading.align                     = "left",
          heading.title.font.size           = px(12),
          heading.title.font.weight         = "bold",
          heading.subtitle.font.size        = px(10),
          heading.padding                   = px(4),
          heading.border.bottom.style       = "none",
          table.border.top.style            = "double",
          table.border.top.color            = PRIMARY,
          table.border.top.width            = px(3),
          table.border.bottom.style         = "solid",
          table.border.bottom.color         = PRIMARY,
          table.border.bottom.width         = px(1.5),
          table.border.left.style           = "none",
          table.border.right.style          = "none",
          column_labels.background.color    = SECONDARY,
          column_labels.font.weight         = "bold",
          column_labels.font.size           = px(10),
          column_labels.border.top.style    = "solid",
          column_labels.border.top.color    = PRIMARY,
          column_labels.border.top.width    = px(1),
          column_labels.border.bottom.style = "solid",
          column_labels.border.bottom.color = PRIMARY,
          column_labels.border.bottom.width = px(1),
          column_labels.padding             = px(4),
          table_body.hlines.style           = "none",
          table_body.vlines.style           = "none",
          table_body.border.top.style       = "none",
          table_body.border.bottom.style    = "none",
          row_group.background.color        = "white",
          row_group.font.weight             = "bold",
          row_group.padding                 = px(3),
          row_group.border.top.style        = "solid",
          row_group.border.top.color        = "#999999",
          row_group.border.top.width        = px(0.5),
          row_group.border.bottom.style     = "none",
          data_row.padding                  = px(3),
          source_notes.font.size            = px(8.5),
          source_notes.padding              = px(2),
          footnotes.font.size               = px(8.5),
          footnotes.padding                 = px(2)
        ) |>
        tab_style(
          style     = cell_text(color = PRIMARY, weight = "bold"),
          locations = cells_title(groups = "title")
        ) |>
        tab_style(
          style     = cell_text(color = NEUTRAL, style = "italic"),
          locations = cells_title(groups = "subtitle")
        ) |>
        tab_style(
          style     = cell_text(weight = "bold", color = "black"),
          locations = cells_column_labels()
        ) |>
        tab_style(
          style     = list(
            cell_text(weight = "bold", color = "black"),
            cell_borders(sides = "bottom", color = PRIMARY,
                         weight = px(0.5))
          ),
          locations = cells_column_spanners()
        ) |>
        tab_style(
          style     = cell_text(color = "black"),
          locations = cells_body()
        )
    }
    
    save_table <- function(gt_table, name) {
      gt::gtsave(gt_table, file.path(tab_dir, paste0(name, ".html")))
      gt::gtsave(gt_table, file.path(tab_dir, paste0(name, ".docx")))
      gt::gtsave(gt_table, file.path(tab_dir, paste0(name, ".png")))
      invisible(gt_table)
    }
    
    fmt_pct_ci <- function(p, lo, hi) {
      sprintf("%.1f%% (%.1f, %.1f)", p * 100, lo * 100, hi * 100)
    }
    
    fmt_num_ci <- function(m, lo, hi) {
      sprintf("%.1f (%.1f, %.1f)", m, lo, hi)
    }
    
    fmt_diff_pp <- function(d, lo, hi) {
      sprintf("%+.1f pp (%.1f, %.1f)", d, lo, hi)
    }
    
    fmt_or_ci <- function(or, lo, hi) {
      sprintf("%.2f (%.2f, %.2f)", or, lo, hi)
    }
    
    # Console print function
    print_console_table <- function(df, title) {
      cat("\n", strrep("=", 78), "\n", sep = "")
      cat(title, "\n", sep = "")
      cat(strrep("=", 78), "\n", sep = "")
      old <- options(width = 200)
      on.exit(options(old))
      out <- as.data.frame(df, stringsAsFactors = FALSE)
      rownames(out) <- NULL
      class(out) <- "data.frame"
      print(out, row.names = FALSE)
      cat("\n")
    }
    
    
    ### LOAD AND CLEAN DATA ----
    
    baseline_raw <- read_dta(file.path(data_dir, "BurkinaBaseline2026.dta"))
    pooled_raw   <- read_dta(file.path(data_dir, "BurkinaPooled2026.dta"),
                             encoding = "latin1")
    
    # Sample weights are already normalised in the BF codebook
    # ("Normalized child weights"). Set nwgt = sample_weight rather than
    # dividing by the mean.
    baseline <- baseline_raw |>
      mutate(nwgt = sample_weight,
             across(c(vitA, ors, carep, diar, ari, sex, age, Maternal_Age),
                    as.numeric),
             EvaluationArea  = factor(EvaluationArea,
                                      levels = c("Comparison", "Program")),
             education_clean = recode_education(education_level),
             any_education   = if_else(is.na(education_clean), NA_real_,
                                       if_else(education_clean == "None",
                                               0, 1)),
             wealth_clean    = recode_wealth(wealth),
             poor            = if_else(is.na(wealth_clean), NA_real_,
                                       if_else(wealth_clean %in%
                                                 c("Poorest", "Poor"), 1, 0)))
    
    # Cluster IDs and strata IDs are reused across rounds in the pooled file.
    # Treat baseline and endline EAs as independent PSUs.
    # The groups variable is for DiD svycontrast.
    pooled <- pooled_raw |>
      mutate(nwgt = sample_weight,
             across(c(vitA, ors, carep, diar, ari, sex, age, Maternal_Age),
                    as.numeric),
             EvaluationArea  = factor(EvaluationArea,
                                      levels = c("Comparison", "Program")),
             Survey          = factor(Survey, levels = c(1, 2),
                                      labels = c("Baseline", "Endline")),
             cluster_id      = paste0(Survey, "_", cluster),
             stratum_id      = paste0(Survey, "_", strata),
             education_clean = recode_education(education_level),
             any_education   = if_else(is.na(education_clean), NA_real_,
                                       if_else(education_clean == "None",
                                               0, 1)),
             wealth_clean    = recode_wealth(wealth),
             poor            = if_else(is.na(wealth_clean), NA_real_,
                                       if_else(wealth_clean %in%
                                                 c("Poorest", "Poor"), 1, 0)),
             groups = case_when(
               EvaluationArea == "Program"    & Survey == "Baseline" ~ 0,
               EvaluationArea == "Program"    & Survey == "Endline"  ~ 1,
               EvaluationArea == "Comparison" & Survey == "Baseline" ~ 2,
               EvaluationArea == "Comparison" & Survey == "Endline"  ~ 3
             ))
    
    
    ### SURVEY DESIGN OBJECTS ----
    
    svyset_baseline <- svydesign(
      id      = ~ cluster,
      data    = baseline,
      weight  = ~ nwgt,
      strata  = ~ strata,
      nest    = TRUE)
    
    svyset_pooled <- svydesign(
      id      = ~ cluster_id,
      data    = pooled,
      weight  = ~ nwgt,
      strata  = ~ stratum_id,
      nest    = TRUE)
    
    cat("Setup complete.\n")
    
  }
  
  
  ## 2. Q1 BALANCE CHECK ----
  
  if (TRUE) {
    
    
    ### CELL ESTIMATES ----
    
    # svyby with svyciprop for logit CIs for proportions; svymean
    # is used for continuous (Maternal_Age) since svyciprop expects 0/1.
    balance_estimate <- function(design, var, label, percent = TRUE) {
      if (percent) {
        est <- svyby(as.formula(paste0("~", var)), ~ Survey + EvaluationArea,
                     design, svyciprop, na.rm = TRUE,
                     vartype = c("se", "ci"), method = "logit")
      } else {
        est <- svyby(as.formula(paste0("~", var)), ~ Survey + EvaluationArea,
                     design, svymean, na.rm = TRUE,
                     vartype = c("se", "ci"))
      }
      ns <- aggregate(as.formula(paste0(var, " ~ Survey + EvaluationArea")),
                      data = design$variables,
                      FUN  = function(x) sum(!is.na(x)))
      names(ns)[3] <- "n"
      out <- merge(est, ns, by = c("Survey", "EvaluationArea"))
      names(out)[names(out) == var] <- "est"
      out$indicator <- label
      out$scale     <- if (percent) "%" else "yr"
      out
    }
    
    q1_results <- bind_rows(
      balance_estimate(svyset_pooled, "any_education",
                       "Mother has any education",   percent = TRUE),
      balance_estimate(svyset_pooled, "poor",
                       "Poorest 2 wealth quintiles", percent = TRUE),
      balance_estimate(svyset_pooled, "Maternal_Age",
                       "Maternal age, mean (years)", percent = FALSE),
      balance_estimate(svyset_pooled, "ari",
                       "ARI in last 2 weeks",        percent = TRUE)
    )
    
    
    ### BETWEEN-ARM DIFFERENCE ----
    
    # Programme - Comparison contrast at each round, fitted via svyglm with
    # identity link so the coefficient is on the proportion / mean scale.
    # Reported as supporting evidence to the descriptive balance comparison;
    # magnitude of imbalance used for the parallel-trends assumption.
    balance_diff <- function(design, var, label) {
      purrr::map_dfr(c("Baseline", "Endline"), function(rnd) {
        sub <- subset(design, Survey == rnd)
        fit <- svyglm(as.formula(paste0(var, " ~ EvaluationArea")),
                      design = sub, na.action = na.omit)
        ci  <- confint(fit)
        data.frame(indicator = label,
                   Survey    = rnd,
                   diff      = coef(fit)[["EvaluationAreaProgram"]],
                   ci_low    = ci["EvaluationAreaProgram", 1],
                   ci_high   = ci["EvaluationAreaProgram", 2],
                   p.value   = summary(fit)$coefficients[
                     "EvaluationAreaProgram", "Pr(>|t|)"])
      })
    }
    
    q1_diffs <- bind_rows(
      balance_diff(svyset_pooled, "any_education", "Mother has any education"),
      balance_diff(svyset_pooled, "poor",          "Poorest 2 wealth quintiles"),
      balance_diff(svyset_pooled, "Maternal_Age",  "Maternal age, mean (years)"),
      balance_diff(svyset_pooled, "ari",           "ARI in last 2 weeks")
    )
    
    
    ### TABLE DATA ----
    
    q1_table_data <- q1_results |>
      mutate(formatted = if_else(scale == "%",
                                 fmt_pct_ci(est, ci_l, ci_u),
                                 fmt_num_ci(est, ci_l, ci_u))) |>
      select(indicator, Survey, EvaluationArea, formatted) |>
      pivot_wider(names_from  = c(Survey, EvaluationArea),
                  values_from = formatted,
                  names_glue  = "{Survey}_{EvaluationArea}")
    
    cat("Q1 balance complete.\n")
    
  }
  
  
  ## 3. Q2 BASELINE COVERAGE ----
  
  if (TRUE) {
    
    
    ### CELL ESTIMATES ----
    
    # svyciprop with logit method gives bounded CIs appropriate for
    # proportions near 0 or 1.
    coverage_baseline <- function(design, indicator, label) {
      est <- svyby(as.formula(paste0("~", indicator)), ~ EvaluationArea,
                   design, svyciprop, na.rm = TRUE,
                   vartype = c("se", "ci"), method = "logit")
      ns <- aggregate(as.formula(paste0(indicator, " ~ EvaluationArea")),
                      data = design$variables,
                      FUN  = function(x) sum(!is.na(x)))
      names(ns)[2] <- "n"
      out <- merge(est, ns, by = "EvaluationArea")
      names(out)[names(out) == indicator] <- "coverage"
      out$indicator <- label
      out
    }
    
    q2_results <- bind_rows(
      coverage_baseline(svyset_baseline, "vitA",
                        "Vitamin A supplementation (6\u201359 mo)"),
      coverage_baseline(svyset_baseline, "ors",
                        "ORS for diarrhea")
    )
    
    
    ### TABLE DATA ----
    
    q2_table_data <- q2_results |>
      mutate(formatted = fmt_pct_ci(coverage, ci_l, ci_u)) |>
      select(indicator, EvaluationArea, n, formatted) |>
      pivot_wider(names_from  = EvaluationArea,
                  values_from = c(n, formatted),
                  names_glue  = "{.value}_{EvaluationArea}")
    
    cat("Q2 baseline coverage complete.\n")
    
  }
  
  
  ## 4. Q3 LONGITUDINAL CHANGE ----
  
  if (TRUE) {
    
    
    ### CELL ESTIMATES ----
    
    # Within-arm cell estimates per Survey round. Run for both arms to
    # produce a tidy long frame that downstream tables and figures reuse.
    cells_area <- function(design, indicator, label, area = "Program") {
      sub <- subset(design, EvaluationArea == area)
      est <- svyby(as.formula(paste0("~", indicator)), ~ Survey,
                   sub, svyciprop, na.rm = TRUE,
                   vartype = c("se", "ci"), method = "logit")
      ns <- aggregate(as.formula(paste0(indicator, " ~ Survey")),
                      data = sub$variables,
                      FUN  = function(x) sum(!is.na(x)))
      names(ns)[2] <- "n"
      out <- merge(est, ns, by = "Survey")
      names(out)[names(out) == indicator] <- "coverage"
      out$indicator      <- label
      out$EvaluationArea <- area
      out
    }
    
    q3_cells <- bind_rows(
      cells_area(svyset_pooled, "vitA",
                 "Vitamin A supplementation (6\u201359 mo)", area = "Program"),
      cells_area(svyset_pooled, "vitA",
                 "Vitamin A supplementation (6\u201359 mo)", area = "Comparison"),
      cells_area(svyset_pooled, "ors",
                 "ORS for diarrhea",                          area = "Program"),
      cells_area(svyset_pooled, "ors",
                 "ORS for diarrhea",                          area = "Comparison")
    )
    
    
    ### DIFFERENCE OF PROPORTIONS ----
    
    # svycontrast on the svyby object gives the percentage-point change
    # (Endline - Baseline) with normal-approximation CI.
    diff_area_pp <- function(design, indicator, label, area = "Program") {
      sub <- subset(design, EvaluationArea == area)
      est <- svyby(as.formula(paste0("~", indicator)), ~ Survey,
                   sub, svyciprop, na.rm = TRUE,
                   vartype = "se", method = "logit")
      diff <- svycontrast(est, quote(Endline - Baseline))
      ci   <- confint(diff)
      data.frame(indicator     = label,
                 difference_pp = as.numeric(coef(diff)) * 100,
                 ci_low_pp     = ci[1, 1] * 100,
                 ci_high_pp    = ci[1, 2] * 100)
    }
    
    q3_diffs_pp <- bind_rows(
      diff_area_pp(svyset_pooled, "vitA",
                   "Vitamin A supplementation (6\u201359 mo)"),
      diff_area_pp(svyset_pooled, "ors",
                   "ORS for diarrhea")
    )
    
    q3_diffs_comp_pp <- bind_rows(
      diff_area_pp(svyset_pooled, "vitA",
                   "Vitamin A supplementation (6\u201359 mo)",
                   area = "Comparison"),
      diff_area_pp(svyset_pooled, "ors",
                   "ORS for diarrhea",
                   area = "Comparison")
    )
    
    
    ### LOGISTIC REGRESSION (WITHIN-ARM OR) ----
    
    # svyglm with quasibinomial fitted on the arm-specific subset returns
    # the OR for SurveyEndline as the within-arm change in odds
    # (Endline vs Baseline). Used for both arms in Table 3.
    diff_area_or <- function(design, indicator, label, area = "Program") {
      sub <- subset(design, EvaluationArea == area)
      fit <- svyglm(as.formula(paste0(indicator, " ~ Survey")),
                    family = quasibinomial, design = sub,
                    na.action = na.omit)
      or  <- exp(coef(fit))[["SurveyEndline"]]
      ci  <- exp(confint(fit))["SurveyEndline", ]
      data.frame(indicator = label,
                 OR        = or,
                 ci_low    = ci[1],
                 ci_high   = ci[2])
    }
    
    q3_diffs_or <- bind_rows(
      diff_area_or(svyset_pooled, "vitA",
                   "Vitamin A supplementation (6\u201359 mo)"),
      diff_area_or(svyset_pooled, "ors",
                   "ORS for diarrhea")
    )
    
    q3_diffs_comp_or <- bind_rows(
      diff_area_or(svyset_pooled, "vitA",
                   "Vitamin A supplementation (6\u201359 mo)",
                   area = "Comparison"),
      diff_area_or(svyset_pooled, "ors",
                   "ORS for diarrhea",
                   area = "Comparison")
    )
    
    
    ### TABLE DATA ----
    
    q3_long_cells <- q3_cells |>
      mutate(EvaluationArea = if_else(EvaluationArea == "Program",
                                      "Programme", "Comparison"),
             metric         = as.character(Survey),
             n_fmt          = format(n, big.mark = ","),
             est_fmt        = fmt_pct_ci(coverage, ci_l, ci_u)) |>
      select(indicator, EvaluationArea, metric, n_fmt, est_fmt)
    
    q3_long_change <- bind_rows(
      q3_diffs_pp      |> mutate(EvaluationArea = "Programme"),
      q3_diffs_comp_pp |> mutate(EvaluationArea = "Comparison")
    ) |>
      mutate(metric  = "Longitudinal change",
             n_fmt   = "",
             est_fmt = fmt_diff_pp(difference_pp, ci_low_pp, ci_high_pp)) |>
      select(indicator, EvaluationArea, metric, n_fmt, est_fmt)
    
    q3_long_or <- bind_rows(
      q3_diffs_or      |> mutate(EvaluationArea = "Programme"),
      q3_diffs_comp_or |> mutate(EvaluationArea = "Comparison")
    ) |>
      mutate(metric  = "OR (Endline vs Baseline)",
             n_fmt   = "",
             est_fmt = fmt_or_ci(OR, ci_low, ci_high)) |>
      select(indicator, EvaluationArea, metric, n_fmt, est_fmt)
    
    q3_table_data <- bind_rows(q3_long_cells, q3_long_change, q3_long_or) |>
      mutate(
        indicator = factor(indicator, levels = indicator_levels),
        metric    = factor(metric, levels = c(
          "Baseline", "Endline", "Longitudinal change",
          "OR (Endline vs Baseline)"))
      ) |>
      arrange(indicator, metric, EvaluationArea) |>
      pivot_wider(names_from  = EvaluationArea,
                  values_from = c(n_fmt, est_fmt),
                  names_glue  = "{.value}_{EvaluationArea}")
    
    is_q3_change <- q3_table_data$metric %in% c("Longitudinal change",
                                                "OR (Endline vs Baseline)")
    
    cat("Q3 longitudinal change complete.\n")
    
  }
  
  
  ## 5. Q4 DIFFERENCE-IN-DIFFERENCES ----
  
  if (TRUE) {
    
    
    ### DIFFERENCE OF PROPORTIONS DiD ----
    
    # svycontrast on the svyby of ~groups computes
    # (Pgm_Endline - Pgm_Baseline) - (Comp_Endline - Comp_Baseline).
    did_estimate_pp <- function(design, indicator, label) {
      est <- svyby(as.formula(paste0("~", indicator)), ~ groups,
                   design, svyciprop, na.rm = TRUE,
                   vartype = "se", method = "logit")
      diff <- svycontrast(est, quote((`1` - `0`) - (`3` - `2`)))
      ci   <- confint(diff)
      data.frame(indicator  = label,
                 DiD_pp     = as.numeric(coef(diff)) * 100,
                 ci_low_pp  = ci[1, 1] * 100,
                 ci_high_pp = ci[1, 2] * 100)
    }
    
    q4_diffs_pp <- bind_rows(
      did_estimate_pp(svyset_pooled, "vitA",
                      "Vitamin A supplementation (6\u201359 mo)"),
      did_estimate_pp(svyset_pooled, "ors",
                      "ORS for diarrhea")
    )
    
    
    ### LOGISTIC REGRESSION DiD ----
    
    # svyglm with quasibinomial and Survey x EvaluationArea interaction term.
    # The interaction coefficient itself is the DiD on the log-odds scale;
    # exponentiate to get the DiD odds ratio.
    did_estimate_or <- function(design, indicator, label) {
      fit <- svyglm(as.formula(paste0(indicator,
                                      " ~ Survey * EvaluationArea")),
                    family = quasibinomial, design = design,
                    na.action = na.omit)
      lc  <- svycontrast(fit, quote(`SurveyEndline:EvaluationAreaProgram`))
      or  <- as.numeric(exp(coef(lc)))
      ci  <- exp(confint(lc))
      data.frame(indicator = label,
                 OR        = or,
                 ci_low    = ci[1, 1],
                 ci_high   = ci[1, 2])
    }
    
    q4_diffs_or <- bind_rows(
      did_estimate_or(svyset_pooled, "vitA",
                      "Vitamin A supplementation (6\u201359 mo)"),
      did_estimate_or(svyset_pooled, "ors",
                      "ORS for diarrhea")
    )
    
    
    ### TABLE DATA ----
    
    rows_cells <- q3_cells |>
      mutate(EvaluationArea = if_else(EvaluationArea == "Program",
                                      "Programme", "Comparison"),
             metric         = as.character(Survey),
             n_fmt          = format(n, big.mark = ","),
             est_fmt        = fmt_pct_ci(coverage, ci_l, ci_u)) |>
      select(indicator, EvaluationArea, metric, n_fmt, est_fmt)
    
    rows_change <- bind_rows(
      q3_diffs_pp      |> mutate(EvaluationArea = "Programme"),
      q3_diffs_comp_pp |> mutate(EvaluationArea = "Comparison")
    ) |>
      mutate(metric  = "Longitudinal change",
             n_fmt   = "",
             est_fmt = fmt_diff_pp(difference_pp, ci_low_pp, ci_high_pp)) |>
      select(indicator, EvaluationArea, metric, n_fmt, est_fmt)
    
    rows_did <- bind_rows(
      q4_diffs_pp |> mutate(metric  = "DiD pp",
                            n_fmt   = "",
                            est_fmt = fmt_diff_pp(DiD_pp, ci_low_pp,
                                                  ci_high_pp)) |>
        select(indicator, metric, n_fmt, est_fmt),
      q4_diffs_or |> mutate(metric  = "DiD OR",
                            n_fmt   = "",
                            est_fmt = fmt_or_ci(OR, ci_low, ci_high)) |>
        select(indicator, metric, n_fmt, est_fmt)
    ) |>
      mutate(EvaluationArea = "Difference-in-differences")
    
    rows_subgroup <- expand.grid(
      indicator      = indicator_levels,
      EvaluationArea = c("Comparison", "Programme",
                         "Difference-in-differences"),
      stringsAsFactors = FALSE
    ) |>
      mutate(metric  = "__SUBGROUP__",
             n_fmt   = "",
             est_fmt = "")
    
    metric_order <- c("__SUBGROUP__", "Baseline", "Endline",
                      "Longitudinal change", "DiD pp", "DiD OR")
    arm_order    <- c("Comparison", "Programme",
                      "Difference-in-differences")
    
    q4_table_data <- bind_rows(
      rows_cells, rows_change, rows_did, rows_subgroup
    ) |>
      mutate(
        indicator      = factor(indicator,      levels = indicator_levels),
        EvaluationArea = factor(EvaluationArea, levels = arm_order),
        metric         = factor(metric,         levels = metric_order),
        Timepoint      = if_else(metric == "__SUBGROUP__",
                                 as.character(EvaluationArea),
                                 as.character(metric))
      ) |>
      arrange(indicator, EvaluationArea, metric) |>
      select(indicator, Timepoint, n_fmt, est_fmt, metric, EvaluationArea)
    
    is_subgroup_arm <- q4_table_data$metric == "__SUBGROUP__" &
      q4_table_data$EvaluationArea %in% c("Comparison", "Programme")
    is_subgroup_did <- q4_table_data$metric == "__SUBGROUP__" &
      q4_table_data$EvaluationArea == "Difference-in-differences"
    is_did_data     <- q4_table_data$EvaluationArea ==
      "Difference-in-differences" &
      q4_table_data$metric != "__SUBGROUP__"
    is_change       <- q4_table_data$metric == "Longitudinal change"
    
    
    ### FIGURE DATA ----
    
    fig1_data <- q3_cells |>
      mutate(EvaluationArea = factor(EvaluationArea,
                                     levels = c("Comparison", "Program")),
             Survey         = factor(Survey,
                                     levels = c("Baseline", "Endline")))
    
    fig2_data <- q4_diffs_pp |>
      mutate(indicator = factor(indicator, levels = rev(unique(indicator))),
             label     = sprintf("%+.1f pp (%.1f, %.1f)",
                                 DiD_pp, ci_low_pp, ci_high_pp))
    
    cat("Q4 DiD complete.\n")
    
  }
  
  
  ## 6. TABLES ----
  
  if (TRUE) {
    
    
    ### Table — Q1 balance ----
    
    tab_balance <- q1_table_data |>
      gt() |>
      tab_header(
        title    = "Balance between intervention and comparison areas",
        subtitle = "Weighted estimates with 95% CI, pooled Burkina Faso dataset"
      ) |>
      tab_spanner(label   = "Baseline (2010-11)",
                  columns = c(Baseline_Comparison, Baseline_Program)) |>
      tab_spanner(label   = "Endline (2013-14)",
                  columns = c(Endline_Comparison, Endline_Program)) |>
      cols_label(
        indicator           = "Characteristic",
        Baseline_Comparison = "Comparison",
        Baseline_Program    = "Programme",
        Endline_Comparison  = "Comparison",
        Endline_Program     = "Programme"
      ) |>
      apply_table_style()
    
    save_table(tab_balance, "tab_T1_balance")
    print_console_table(q1_table_data,
                        "TABLE 1 \u2014 Q1 BALANCE CHECK")
    print_console_table(q1_diffs,
                        "TABLE 1b \u2014 Q1 BETWEEN-ARM DIFFERENCES")
    print(tab_balance)
    
    
    ### Table — Q2 baseline coverage ----
    
    tab_baseline <- q2_table_data |>
      gt() |>
      tab_header(
        title    = "Baseline coverage estimates",
        subtitle = "Weighted point estimates with 95% CI, baseline-only dataset"
      ) |>
      tab_spanner(label   = "Comparison area",
                  columns = c(n_Comparison, formatted_Comparison)) |>
      tab_spanner(label   = "Programme area",
                  columns = c(n_Program,    formatted_Program)) |>
      cols_label(
        indicator            = "Indicator",
        n_Comparison         = "n",
        formatted_Comparison = "Coverage",
        n_Program            = "n",
        formatted_Program    = "Coverage"
      ) |>
      apply_table_style()
    
    save_table(tab_baseline, "tab_T2_baseline_coverage")
    print_console_table(q2_table_data,
                        "TABLE 2 \u2014 Q2 BASELINE COVERAGE")
    print(tab_baseline)
    
    
    ### Table — Q3 longitudinal change ----
    
    tab_program_change <- q3_table_data |>
      gt(groupname_col = "indicator") |>
      tab_header(
        title    = "Longitudinal change in coverage",
        subtitle = "Endline minus baseline coverage with 95% CI, pooled dataset"
      ) |>
      tab_spanner(label   = "Programme area",
                  columns = c(n_fmt_Programme, est_fmt_Programme)) |>
      tab_spanner(label   = "Comparison area",
                  columns = c(n_fmt_Comparison, est_fmt_Comparison)) |>
      cols_label(
        metric             = "",
        n_fmt_Programme    = "n",
        est_fmt_Programme  = "Estimate (95% CI)",
        n_fmt_Comparison   = "n",
        est_fmt_Comparison = "Estimate (95% CI)"
      ) |>
      cols_align(align = "left",  columns = metric) |>
      cols_align(align = "right", columns = c(n_fmt_Programme, n_fmt_Comparison)) |>
      cols_align(align = "left",  columns = c(est_fmt_Programme, est_fmt_Comparison)) |>
      apply_table_style() |>
      tab_style(
        style     = list(
          cell_fill(color = PRIMARY),
          cell_text(color = "white", weight = "bold", size = px(11))
        ),
        locations = cells_row_groups()
      ) |>
      tab_style(
        style     = list(
          cell_fill(color = SECONDARY_50),
          cell_text(color = PRIMARY, weight = "bold")
        ),
        locations = cells_body(rows = is_q3_change)
      )
    
    save_table(tab_program_change, "tab_T3_longitudinal_change")
    print_console_table(q3_table_data,
                        "TABLE 3 \u2014 Q3 LONGITUDINAL CHANGE")
    print(tab_program_change)
    
    
    ### Table — Q4 difference-in-differences ----
    
    tab_did <- q4_table_data |>
      select(-metric, -EvaluationArea) |>
      gt(groupname_col = "indicator") |>
      tab_header(
        title    = "Difference-in-differences in coverage",
        subtitle = "Programme minus comparison change from baseline to endline, pooled dataset"
      ) |>
      cols_label(
        Timepoint = "",
        n_fmt     = "n",
        est_fmt   = "Estimate (95% CI)"
      ) |>
      cols_align(align = "left",  columns = Timepoint) |>
      cols_align(align = "right", columns = n_fmt) |>
      cols_align(align = "left",  columns = est_fmt) |>
      apply_table_style() |>
      tab_style(
        style     = list(
          cell_fill(color = PRIMARY),
          cell_text(color = "white", weight = "bold", size = px(11))
        ),
        locations = cells_row_groups()
      ) |>
      tab_style(
        style     = list(
          cell_fill(color = SECONDARY_50),
          cell_text(color = PRIMARY, weight = "bold")
        ),
        locations = cells_body(rows = is_subgroup_arm)
      ) |>
      tab_style(
        style     = list(
          cell_fill(color = SECONDARY_75),
          cell_text(color = PRIMARY, weight = "bold")
        ),
        locations = cells_body(rows = is_subgroup_did)
      ) |>
      tab_style(
        style     = list(
          cell_fill(color = ACCENT_LIGHT),
          cell_text(color = PRIMARY, weight = "bold")
        ),
        locations = cells_body(rows = is_did_data)
      ) |>
      tab_style(
        style     = cell_text(indent = px(18)),
        locations = cells_body(columns = Timepoint,
                               rows    = !(is_subgroup_arm | is_subgroup_did))
      ) |>
      tab_style(
        style     = cell_text(weight = "bold"),
        locations = cells_body(rows = is_change)
      )
    
    save_table(tab_did, "tab_T4_did")
    print_console_table(q4_table_data |> select(indicator, Timepoint,
                                                n_fmt, est_fmt),
                        "TABLE 4 \u2014 Q4 FULL RESULTS WITH DiD")
    print(tab_did)
    
    cat("Tables complete.\n")
    
  }
  
  
  ## 7. FIGURES ----
  
  if (TRUE) {
    
    
    ### THEME ----
    
    theme_paper <- function(base_size = 11) {
      theme_minimal(base_size = base_size, base_family = "sans") +
        theme(
          panel.grid.minor    = element_blank(),
          panel.grid.major.x  = element_blank(),
          panel.grid.major.y  = element_line(colour = LIGHT_GREY, linewidth = 0.3),
          axis.title          = element_text(face = "bold"),
          axis.text           = element_text(colour = "grey20"),
          plot.title          = element_text(face = "bold", size = base_size + 2,
                                             colour = PRIMARY),
          plot.subtitle       = element_text(colour = NEUTRAL,
                                             size = base_size - 1),
          plot.caption        = element_text(colour = NEUTRAL,
                                             size = base_size - 2, hjust = 0),
          legend.position     = "bottom",
          legend.title        = element_text(face = "bold", size = base_size - 1),
          legend.text         = element_text(size = base_size - 1),
          strip.text          = element_text(face = "bold", colour = PRIMARY,
                                             size = base_size)
        )
    }
    
    
    ### HELPERS ----
    
    save_fig <- function(plot, name, width = 7, height = 5) {
      ggsave(file.path(fig_dir, paste0(name, ".pdf")),
             plot = plot, width = width, height = height)
      ggsave(file.path(fig_dir, paste0(name, ".png")),
             plot = plot, width = width, height = height, dpi = 300)
      invisible(plot)
    }
    
    
    ### Figure 1 — Coverage by arm and timepoint ----
    
    # Paired baseline/endline bars per arm, faceted by indicator, with
    # 95% CI error bars.
    fig1 <- ggplot(fig1_data, aes(x = EvaluationArea, y = coverage * 100,
                                  fill = Survey)) +
      geom_col(position = position_dodge(width = 0.7), width = 0.6) +
      geom_errorbar(aes(ymin = ci_l * 100, ymax = ci_u * 100),
                    position = position_dodge(width = 0.7),
                    width = 0.2, colour = ACCENT, linewidth = 0.4) +
      facet_wrap(~ indicator, ncol = 2, scales = "free_y") +
      scale_fill_manual(values = c(Baseline = SECONDARY, Endline = PRIMARY)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
      labs(title    = "Coverage by evaluation area, baseline to endline",
           subtitle = "Weighted estimates with 95% CI, pooled Burkina Faso dataset",
           x        = NULL,
           y        = "Coverage (%)",
           fill     = NULL) +
      theme_paper()
    
    save_fig(fig1, "fig_F1_coverage", width = 9, height = 5)
    print(fig1)
    
    
    ### Figure 2 — Forest plot of DiDs ----
    
    # Single-panel forest plot of DiD point estimate and 95% CI for each
    # indicator. Dashed reference line at zero separates effects favouring
    # the programme (positive) from those favouring the comparison area
    # (negative).
    fig2 <- ggplot(fig2_data, aes(x = DiD_pp, y = indicator)) +
      geom_vline(xintercept = 0, linetype = "dashed", colour = NEUTRAL) +
      geom_errorbarh(aes(xmin = ci_low_pp, xmax = ci_high_pp),
                     height = 0.15, colour = PRIMARY, linewidth = 0.6) +
      geom_point(size = 3.5, colour = PRIMARY) +
      geom_text(aes(label = label),
                hjust = -0.15, size = 3.2, colour = "grey20") +
      scale_x_continuous(
        expand = expansion(mult = c(0.1, 0.4)),
        breaks = pretty(c(fig2_data$ci_low_pp, fig2_data$ci_high_pp))) +
      labs(title    = "Difference-in-differences in coverage",
           subtitle = "Programme minus comparison change, baseline to endline",
           x        = "Difference-in-differences (percentage points)",
           y        = NULL) +
      theme_paper() +
      theme(panel.grid.major.x = element_line(colour = LIGHT_GREY,
                                              linewidth = 0.3),
            panel.grid.major.y = element_blank())
    
    save_fig(fig2, "fig_F2_did_forest", width = 9, height = 3.5)
    print(fig2)
    
    cat("Figures complete.\n")
    
  }
  
  
  ## 8. REPORT OUTPUT ----
  
  if (TRUE) {
    
    
    ### LIBRARIES ----
    
    library(quarto)
    library(rmarkdown)
    
    
    ### FILE PATHS ----
    
    rep_dir <- "report"
    if (!dir.exists(rep_dir)) dir.create(rep_dir, recursive = TRUE)
    
    
    ### Persist objects ----
    
    save(tab_balance, tab_baseline, tab_program_change, tab_did,
         fig1, fig2,
         file = file.path(rep_dir, "report_objects.RData"))
    
    
    ### Read source script ----
    
    # Embed the full analysis script as a non-executed code block at the end
    # of the rendered Word document. eval=FALSE means the code displays with
    # syntax highlighting but does not run.
    script_lines <- readLines("MPIE_Final_Project.R", warn = FALSE,
                              encoding = "UTF-8")
    
    
    ### Build the .qmd report ----
    
    qmd_path <- file.path(rep_dir, "MPIE_Final_Report.qmd")
    
    qmd <- c(
      "---",
      "title: \"Burkina Faso iCCM Evaluation\"",
      "subtitle: \"MPIE Final Project, AY 2025-26\"",
      "author: \"Rohan Chitkara, Krithi Chakrapani, Advika Dani\"",
      "date: today",
      "format:",
      "  docx:",
      "    toc: true",
      "    toc-depth: 2",
      "    number-sections: true",
      "execute:",
      "  echo: false",
      "  warning: false",
      "  message: false",
      "---",
      "",
      "```{r setup}",
      "load(\"report_objects.RData\")",
      "library(gt)",
      "library(ggplot2)",
      "```",
      "",
      "# Q1 — Balance between intervention and comparison areas",
      "",
      "[Insert Q1 prose here ~500 words]",
      "",
      "![](../tables/tab_T1_balance.png)",
      "",
      "# Q2 — Baseline coverage estimates",
      "",
      "[Insert Q2 prose here ~short explanation]",
      "",
      "![](../tables/tab_T2_baseline_coverage.png)",
      "",
      "# Q3 — Longitudinal change in the programme area",
      "",
      "[Insert Q3 prose here ~short explanation]",
      "",
      "![](../tables/tab_T3_longitudinal_change.png)",
      "",
      "# Q4 — Difference-in-differences",
      "",
      "[Insert Q4 prose here ~short explanation]",
      "",
      "![](../tables/tab_T4_did.png)",
      "",
      "# Q5 — Methods",
      "",
      "[Insert Q5 prose here ~500 words]",
      "",
      "# Q6 — Results and inference",
      "",
      "[Insert Q6 prose here ~500 words]",
      "",
      "```{r fig1, fig.width=9, fig.height=5}",
      "fig1",
      "```",
      "",
      "```{r fig2, fig.width=9, fig.height=3.5}",
      "fig2",
      "```",
      "",
      "# Q7 — Limitations",
      "",
      "[Insert Q7 prose here, ~250 words]",
      "",
      "# Q8 — Quality-of-care measurement method",
      "",
      "[Insert Q8 prose here, ~500 words]",
      "",
      "## Indicator definition and questionnaire items",
      "",
      "[Indicator + survey items, uncounted]",
      "",
      "# Q9 — Interpreting quality-of-care data",
      "",
      "[Insert Q9 prose here, ~1000 words]",
      "",
      "{{< pagebreak >}}",
      "",
      "# Appendix — R analysis code",
      "",
      "The full source of `MPIE_Final_Project.R` is reproduced below for",
      "audit and reproducibility purposes. The block is rendered with",
      "syntax highlighting and is not executed during rendering.",
      "",
      "```{r audit, eval=FALSE, echo=TRUE}",
      script_lines,
      "```",
      ""
    )
    
    
    ### Write the .qmd template ----
    
    # If a .qmd file already exists, do not overwrite it. Delete manually
    # to regenerate by "file.remove("report/MPIE_Final_Report.qmd") in console".
    if (!file.exists(qmd_path)) {
      con <- file(qmd_path, open = "w", encoding = "UTF-8")
      writeLines(qmd, con)
      close(con)
      cat("Quarto report template written to:", qmd_path, "\n")
    } else {
      cat("Existing .qmd preserved at:", qmd_path,
          "(delete the file to regenerate the template)\n")
    }
    
    
    ### Render to DOCX ----
    
    rendered <- tryCatch({
      quarto::quarto_render(qmd_path, output_format = "docx")
      file.path(rep_dir, "MPIE_Final_Report.docx")
    }, error = function(e) {
      message("Quarto render failed: ", conditionMessage(e))
      message("Falling back to rmarkdown::render() ...")
      tryCatch({
        rmarkdown::render(qmd_path,
                          output_format = "word_document",
                          output_dir    = rep_dir,
                          quiet         = TRUE)
        file.path(rep_dir, "MPIE_Final_Report.docx")
      }, error = function(e2) {
        message("rmarkdown render also failed: ", conditionMessage(e2))
        NA_character_
      })
    })
    
    if (!is.na(rendered) && file.exists(rendered)) {
      cat("Word document rendered:", rendered, "\n")
    }
    
    cat("Report output complete.\n")
    
  }
}  # end

cat("\nFull script complete.\n")