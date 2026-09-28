# AB EQUITY REPORTING — FIGURES AND TABLES ----
#
#   Project : Equity in Childhood Vaccination Under Ayushman Bharat
#   Author  : Rohan Chitkara
#
# Loads saved RDS outputs from AB_Equity_ECEA_Wealth_Final.R and produces
# manuscript figures and tables.
#
#   Section 1 — Setup, palette, helpers, RDS loading
#   Section 2 — Figures (descriptive, results, sensitivity)
#   Section 3 — Tables (population, coefficients, ECEA, sensitivity)
#   Section 4 — Appendix exhibits
#   Section 5 — Schematic HTML to PNG
#
# Figures saved to figures/ as PDF + PNG.
# Tables saved to tables/ as HTML, RTF, DOCX, PNG.


# REPORTING ----

if (TRUE) {
  
  
  ## 1. SETUP ----
  
  if (TRUE) {
    
    
    ### LIBRARIES ----
    
    library(ggplot2)
    library(dplyr)
    library(tidyr)
    library(forcats)
    library(scales)
    library(patchwork)
    library(stringr)
    library(purrr)
    library(gt)
    library(sf)
    library(ggdag)
    library(dagitty)
    library(posterior)
    
    
    ### FILE PATHS ----
    
    path_shape <- "gadm41_IND_1.shp"
    out_dir    <- "ECEA_files"
    nss_dir    <- "NSS_files"
    fig_dir    <- "Figures"
    tab_dir    <- "tables"
    if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)
    if (!dir.exists(tab_dir)) dir.create(tab_dir, recursive = TRUE)
    
    
    ### COLOUR PALETTE ----
    
    PRIMARY    <- "#002D72"
    SECONDARY  <- "#68ACE5"
    ACCENT     <- "#B30838"
    NEUTRAL    <- "#666666"
    LIGHT_GREY <- "#E5E5E5"
    
    palette_cat <- c(PRIMARY, SECONDARY, ACCENT, NEUTRAL)
    palette_seq <- c("white", SECONDARY, PRIMARY)
    palette_div <- c(ACCENT, LIGHT_GREY, PRIMARY)
    
    
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
    
    pdf_device <- if (capabilities("cairo")) cairo_pdf else "pdf"
    
    save_fig <- function(plot, name, width = 7, height = 5) {
      ggsave(file.path(fig_dir, paste0(name, ".pdf")),
             plot = plot, width = width, height = height, device = pdf_device)
      ggsave(file.path(fig_dir, paste0(name, ".png")),
             plot = plot, width = width, height = height, dpi = 300)
      invisible(plot)
    }
    
    # gt theme — applies the project styling to a gt table.
    # PNG output is the priority; HTML uses inline CSS for portability.
    apply_table_style <- function(gt_table) {
      gt_table |>
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
    }
    
    
    save_table <- function(gt_table, name) {
      
      # HTML — fully self-contained with inline CSS
      tryCatch({
        gt::gtsave(gt_table,
                   filename   = file.path(tab_dir, paste0(name, ".html")),
                   inline_css = TRUE)
      }, error = function(e) message("HTML export failed for ", name, ": ",
                                     conditionMessage(e)))
      
      # PNG — primary format; uses webshot2 to capture the styled HTML
      tryCatch({
        gt::gtsave(gt_table,
                   filename = file.path(tab_dir, paste0(name, ".png")))
      }, error = function(e) message("PNG export failed for ", name,
                                     " (requires webshot2 + Chrome)"))
      
      # DOCX — best-effort; gt's Word output drops most styling
      tryCatch({
        gt::gtsave(gt_table,
                   filename = file.path(tab_dir, paste0(name, ".docx")))
      }, error = function(e) message("DOCX export failed for ", name))
      
      # RTF — best-effort; format limitations apply
      tryCatch({
        gt::gtsave(gt_table,
                   filename = file.path(tab_dir, paste0(name, ".rtf")))
      }, error = function(e) message("RTF export failed for ", name))
      
      invisible(gt_table)
    }
    
    closest_wtp <- function(tbl, target) {
      if (target %in% tbl$wtp) return(filter(tbl, wtp == target))
      filter(tbl, wtp == tbl$wtp[which.min(abs(tbl$wtp - target))])
    }
    
    
    ### LOAD RDS OUTPUTS ----
    
    stage2a              <- readRDS(file.path(out_dir, "stage2a_did_coverage.rds"))
    stage2b              <- readRDS(file.path(out_dir, "stage2b_brms_fit.rds"))
    ecea_base            <- readRDS(file.path(out_dir, "ecea_base.rds"))
    ecea_imi_base        <- readRDS(file.path(out_dir, "ecea_imi_base.rds"))
    ecea_equity_base     <- readRDS(file.path(out_dir, "ecea_equity_base.rds"))
    quintile_summary     <- readRDS(file.path(out_dir, "quintile_summary.rds"))
    joint_summary        <- readRDS(file.path(out_dir, "joint_summary.rds"))
    aggregate_quintiles  <- readRDS(file.path(out_dir, "aggregate_quintiles.rds"))
    owsa_nmb             <- readRDS(file.path(out_dir, "owsa_nmb.rds"))
    psa_equity           <- readRDS(file.path(out_dir, "psa_equity.rds"))
    ceac_equity          <- readRDS(file.path(out_dir, "ceac_equity.rds"))
    ceac_data            <- readRDS(file.path(out_dir, "ceac_data.rds"))
    state_descriptives   <- readRDS(file.path(out_dir, "state_descriptives.rds"))
    national_dynamics    <- readRDS(file.path(out_dir, "national_dynamics.rds"))
    quintile_dynamics    <- readRDS(file.path(out_dir, "quintile_dynamics.rds"))
    state_shifts         <- readRDS(file.path(out_dir, "state_shifts.rds"))
    population_summary   <- readRDS(file.path(out_dir, "population_summary.rds"))
    table1_demographics  <- readRDS(file.path(out_dir, "table1_demographics.rds"))
    eag_descriptives     <- readRDS(file.path(out_dir, "eag_descriptives.rds"))
    eag_lookup           <- readRDS(file.path(out_dir, "eag_lookup.rds"))
    eag_aggregate_ecea   <- readRDS(file.path(out_dir, "eag_aggregate_ecea.rds"))
    alpha_sensitivity    <- readRDS(file.path(out_dir, "alpha_sensitivity.rds"))
    concentration_curve  <- readRDS(file.path(out_dir, "concentration_curve_data.rds"))
    
    # MPCE quintile dollar ranges from NSS 75th Round, produced by Step 8
    # of NSSO75_ECEA_Extraction.R. Used in Table 1 to anchor wealth-quintile
    # rows in absolute INR/USD.
    t1_mpce_path <- file.path(nss_dir, "T1_quintile_mpce.rds")
    if (file.exists(t1_mpce_path)) {
      t1_quintile_mpce <- readRDS(t1_mpce_path)
    } else {
      message("T1_quintile_mpce.rds not found at ", t1_mpce_path,
              " — Table 1 MPCE column will display dashes. ",
              "Run Step 8 of NSSO75_ECEA_Extraction.R to generate it.")
      t1_quintile_mpce <- NULL
    }
    
    cat("Setup complete.\n")
    
  }
  
  
  ## 2. FIGURES ----
  
  if (TRUE) {
    
    
    ### India choropleth map ----
    
    india_sf <- sf::st_read(path_shape, quiet = TRUE) |>
      mutate(state_ut = case_when(
        NAME_1 == "NCT of Delhi"                              ~ "Delhi",
        NAME_1 == "Andaman and Nicobar"                       ~ "Andaman & Nicobar Islands",
        NAME_1 == "Jammu and Kashmir"                         ~ "Jammu & Kashmir",
        NAME_1 == "Dadra and Nagar Haveli and Daman and Diu"  ~ "Dadra & Nagar Haveli",
        TRUE ~ NAME_1
      ))
    
    india_data <- india_sf |>
      left_join(state_descriptives |>
                  select(state_ut, delta_FIC, delta_CI,
                         x_s_primary, imi_exposure_intensity),
                by = "state_ut")
    
    fig_india_map <- ggplot(india_data) +
      geom_sf(aes(fill = delta_CI), colour = "white", linewidth = 0.2) +
      scale_fill_gradient2(low = ACCENT, mid = LIGHT_GREY, high = PRIMARY,
                           midpoint = 0, na.value = "grey90",
                           name = "Δ Concentration index",
                           breaks = c(-0.10, -0.05, 0, 0.05, 0.10),
                           labels = c("−0.10", "−0.05", "0", "+0.05", "+0.10")) +
      labs(title = "Spatial distribution of equity change",
           subtitle = "Δ Concentration index between NFHS-4 and NFHS-5 across 32 study states",
           caption = "Negative (blue) values indicate pro-poor redistribution; grey states excluded from DiD panel.") +
      theme_paper() +
      theme(panel.grid = element_blank(),
            axis.text  = element_blank(),
            axis.title = element_blank(),
            panel.background = element_blank())
    
    save_fig(fig_india_map, "fig_M1_india_map", width = 9, height = 9)
    
    
    ### Treatment intensity scatter ----
    
    fig_treatment <- state_descriptives |>
      filter(!is.na(imi_exposure_intensity)) |>
      ggplot(aes(x = x_s_primary, y = imi_exposure_intensity)) +
      geom_point(aes(size = pop_12_23_2021, fill = nfhs5_phase),
                 shape = 21, colour = "white", alpha = 0.85) +
      geom_text(aes(label = state_ut), size = 3, colour = NEUTRAL,
                hjust = -0.15, check_overlap = TRUE) +
      scale_fill_manual(values = c("Phase 1" = PRIMARY, "Phase 2" = SECONDARY),
                        name = "NFHS-5 fieldwork phase") +
      scale_size_continuous(range = c(2, 12), labels = comma,
                            name = "12-23 month population") +
      labs(title = "Cross-state variation in treatment intensity",
           subtitle = "HWC density and IMI exposure across 32 Indian states",
           x = "HWC operational density (per 100,000 population, Feb 2019)",
           y = "IMI exposure intensity (district-rounds per district)") +
      theme_paper()
    
    save_fig(fig_treatment, "fig_M2_treatment_intensity", width = 9, height = 6)
    
    
    ### Identification timeline ----
    
    timeline <- tribble(
      ~event,                ~start,            ~end,              ~category,
      "NFHS-4 fieldwork",    "2015-01-01",      "2016-12-31",      "Survey",
      "NFHS-5 fieldwork",    "2019-06-01",      "2021-04-30",      "Survey",
      "MI Phase 1",          "2015-04-01",      "2015-07-31",      "MI baseline",
      "MI Phase 2",          "2015-10-01",      "2016-01-31",      "MI baseline",
      "MI Phase 3",          "2016-04-01",      "2016-07-31",      "MI baseline",
      "MI Phase 4",          "2017-02-01",      "2017-07-31",      "MI baseline",
      "IMI Phase 1",         "2017-10-01",      "2018-01-31",      "IMI exposure",
      "IMI 2.0",             "2019-12-01",      "2020-03-31",      "IMI exposure",
      "AB-HWC launch",       "2018-04-01",      "2018-04-15",      "HWC programme",
      "COVID-19 emergency",  "2020-03-25",      "2020-06-30",      "Confounder"
    ) |>
      mutate(across(c(start, end), as.Date)) |>
      mutate(event = factor(event, levels = rev(event)))
    
    fig_timeline <- ggplot(timeline) +
      geom_segment(aes(x = start, xend = end, y = event, yend = event,
                       colour = category),
                   linewidth = 6, lineend = "round") +
      scale_colour_manual(
        values = c("Survey" = NEUTRAL, "MI baseline" = LIGHT_GREY,
                   "IMI exposure" = PRIMARY, "HWC programme" = SECONDARY,
                   "Confounder" = ACCENT),
        name = NULL) +
      labs(title = "Identification timeline",
           subtitle = "Treatment exposure within the NFHS-4 to NFHS-5 inter-survey period",
           x = NULL, y = NULL) +
      theme_paper() +
      theme(panel.grid.major.y = element_blank())
    
    save_fig(fig_timeline, "fig_M3_timeline", width = 10, height = 5)
    
    
    ### DAG of joint identification ----
    
    dag <- dagitty::dagitty('dag {
      bb="0,0,1,1"
      StateFE     [pos="0.10,0.50"]
      Post        [pos="0.30,0.10"]
      HWC         [pos="0.40,0.40"]
      IMI         [pos="0.40,0.60"]
      Interaction [pos="0.55,0.50"]
      FIC         [outcome,pos="0.85,0.30"]
      CI          [outcome,pos="0.85,0.70"]
      Baseline    [pos="0.20,0.85"]
      COVID       [pos="0.55,0.90"]
      StateFE     -> FIC
      StateFE     -> CI
      Post        -> FIC
      Post        -> CI
      HWC         -> FIC
      HWC         -> CI
      IMI         -> FIC
      IMI         -> CI
      Interaction -> FIC
      Interaction -> CI
      Baseline    -> IMI
      Baseline    -> HWC
      COVID       -> FIC
      COVID       -> CI
    }')
    
    fig_dag <- ggdag::tidy_dagitty(dag) |>
      ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
      ggdag::geom_dag_edges(edge_colour = NEUTRAL) +
      ggdag::geom_dag_point(colour = PRIMARY, size = 18, alpha = 0.85) +
      ggdag::geom_dag_text(aes(label = name), colour = "white",
                           size = 2.6, fontface = "bold") +
      labs(title = "Augmented DiD identification structure",
           subtitle = "Causal DAG for the joint HWC + IMI specification",
           caption = "Solid arrows: causal pathways identified by the DiD. Baseline coverage drives IMI and HWC selection (endogeneity flag).") +
      ggdag::theme_dag() +
      theme(plot.title    = element_text(face = "bold", size = 13, colour = PRIMARY),
            plot.subtitle = element_text(colour = NEUTRAL, size = 10),
            plot.caption  = element_text(colour = NEUTRAL, size = 9, hjust = 0))
    
    save_fig(fig_dag, "fig_M4_dag", width = 10, height = 7)
    
    
    ### National coverage and equity dynamics ----
    
    national_data <- national_dynamics |>
      select(round, mean_FIC, mean_CI) |>
      pivot_longer(c(mean_FIC, mean_CI), names_to = "metric", values_to = "value") |>
      mutate(metric = ifelse(metric == "mean_FIC",
                             "Full immunisation coverage",
                             "Concentration index (wealth-ranked)"))
    
    fig_national <- ggplot(national_data, aes(x = round, y = value, fill = round)) +
      geom_col(width = 0.5) +
      geom_text(aes(label = sprintf("%.3f", value)),
                vjust = -0.5, size = 4, fontface = "bold", colour = PRIMARY) +
      scale_fill_manual(values = c("NFHS-4" = SECONDARY, "NFHS-5" = PRIMARY),
                        guide = "none") +
      facet_wrap(~ metric, scales = "free_y", nrow = 1) +
      labs(title = "National coverage and equity dynamics",
           subtitle = "Mean across 32 DiD states, NFHS-4 to NFHS-5",
           x = NULL, y = "Value") +
      theme_paper() +
      theme(panel.grid.major.x = element_blank())
    
    save_fig(fig_national, "fig_R1_national_dynamics", width = 8, height = 5)
    
    
    ### Quintile slope graph ----
    
    fig_quintile_slope <- quintile_dynamics |>
      mutate(wealth_q = factor(wealth_q, levels = 1:5,
                               labels = c("Q1\n(Poorest)", "Q2", "Q3", "Q4", "Q5\n(Richest)"))) |>
      ggplot(aes(x = round, y = mean_FIC, colour = wealth_q, group = wealth_q)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 4) +
      geom_text(data = ~filter(., round == "NFHS-5"),
                aes(label = sprintf("%.1f%%", mean_FIC * 100)),
                hjust = -0.4, fontface = "bold", size = 3.5) +
      scale_colour_manual(values = c(ACCENT, ACCENT, NEUTRAL, SECONDARY, PRIMARY),
                          name = "Wealth quintile") +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(title = "Quintile-level coverage trajectory",
           subtitle = "Mean full immunisation coverage by wealth quintile across DiD states",
           x = NULL, y = "Mean FIC") +
      theme_paper() +
      coord_cartesian(clip = "off") +
      theme(plot.margin = margin(10, 80, 10, 10))
    
    save_fig(fig_quintile_slope, "fig_R2_quintile_trajectory", width = 8, height = 5.5)
    
    
    ### State Δ FIC vs Δ CI scatter ----
    
    R3_data <- state_descriptives |>
      filter(!is.na(delta_FIC), !is.na(delta_CI), !is.na(imi_exposure_intensity))
    
    fig_dfic_dci <- ggplot(R3_data, aes(x = delta_FIC, y = delta_CI)) +
      geom_hline(yintercept = 0, colour = NEUTRAL, linetype = "dashed") +
      geom_vline(xintercept = 0, colour = NEUTRAL, linetype = "dashed") +
      geom_point(aes(size = imi_exposure_intensity, fill = nfhs5_phase),
                 shape = 21, colour = "white", alpha = 0.9) +
      geom_text(aes(label = state_ut), size = 3, colour = NEUTRAL,
                hjust = -0.15, check_overlap = TRUE) +
      scale_fill_manual(values = c("Phase 1" = PRIMARY, "Phase 2" = SECONDARY),
                        name = "NFHS-5 phase") +
      scale_size_continuous(range = c(2, 9), name = "IMI exposure") +
      scale_y_reverse() +
      labs(title = "State-level coverage and equity changes",
           subtitle = "Δ FIC vs Δ CI between NFHS-4 and NFHS-5 (y-axis reversed: down = pro-poor)",
           x = "Δ Full immunisation coverage (NFHS-5 − NFHS-4)",
           y = "Δ Concentration index (NFHS-5 − NFHS-4)") +
      theme_paper()
    
    save_fig(fig_dfic_dci, "fig_R3_delta_fic_vs_delta_ci", width = 9, height = 7)
    
    
    ### DiD coefficient forest plot ----
    
    R4_data <- bind_rows(
      tibble(coefficient = "HWC × post", model = "Coverage (frequentist)",
             est = stage2a$estimate,
             lo  = stage2a$estimate - 1.96 * stage2a$se,
             hi  = stage2a$estimate + 1.96 * stage2a$se),
      tibble(coefficient = "IMI × post", model = "Coverage (frequentist)",
             est = stage2a$beta_imi_estimate,
             lo  = stage2a$beta_imi_estimate - 1.96 * stage2a$beta_imi_se,
             hi  = stage2a$beta_imi_estimate + 1.96 * stage2a$beta_imi_se),
      tibble(coefficient = "HWC × IMI × post", model = "Coverage (frequentist)",
             est = stage2a$beta_inter_estimate,
             lo  = stage2a$beta_inter_estimate - 1.96 * stage2a$beta_inter_se,
             hi  = stage2a$beta_inter_estimate + 1.96 * stage2a$beta_inter_se),
      tibble(coefficient = "HWC × post", model = "Equity (Bayesian)",
             est = stage2b$posterior_median,
             lo  = stage2b$posterior_ci_lo,
             hi  = stage2b$posterior_ci_hi),
      tibble(coefficient = "IMI × post", model = "Equity (Bayesian)",
             est = stage2b$beta_imi_ci_median,
             lo  = stage2b$beta_imi_ci_q025,
             hi  = stage2b$beta_imi_ci_q975),
      tibble(coefficient = "HWC × IMI × post", model = "Equity (Bayesian)",
             est = stage2b$beta_inter_ci_median,
             lo  = stage2b$beta_inter_ci_q025,
             hi  = stage2b$beta_inter_ci_q975)
    ) |>
      mutate(coefficient = factor(coefficient,
                                  levels = c("HWC × IMI × post", "IMI × post", "HWC × post")))
    
    fig_coefficients <- ggplot(R4_data, aes(x = est, y = coefficient, colour = model)) +
      geom_vline(xintercept = 0, colour = NEUTRAL, linetype = "dashed") +
      geom_point(size = 4, position = position_dodge(width = 0.5)) +
      geom_errorbarh(aes(xmin = lo, xmax = hi),
                     height = 0.15, linewidth = 0.7,
                     position = position_dodge(width = 0.5)) +
      scale_colour_manual(values = c("Coverage (frequentist)" = PRIMARY,
                                     "Equity (Bayesian)"      = ACCENT),
                          name = "Outcome") +
      labs(title = "DiD coefficient estimates",
           subtitle = "Frequentist coverage DiD vs Bayesian equity DiD, augmented specification",
           x = "Coefficient estimate (95% CI / CrI)", y = NULL) +
      theme_paper()
    
    save_fig(fig_coefficients, "fig_R4_coefficient_forest", width = 9, height = 5)
    
    
    ### Bayesian posterior densities ----
    
    posterior_long <- tibble(
      HWC_x_post     = stage2b$beta3_ci_draws,
      IMI_x_post     = stage2b$beta_imi_ci_draws,
      HWCxIMI_x_post = stage2b$beta_inter_ci_draws
    ) |>
      pivot_longer(everything(), names_to = "coefficient", values_to = "value") |>
      mutate(coefficient = factor(coefficient,
                                  levels = c("HWC_x_post", "IMI_x_post", "HWCxIMI_x_post"),
                                  labels = c("HWC × post", "IMI × post",
                                             "HWC × IMI × post")))
    
    fig_posteriors <- ggplot(posterior_long, aes(x = value, fill = coefficient)) +
      geom_density(alpha = 0.7, colour = NA) +
      geom_vline(xintercept = 0, colour = NEUTRAL,
                 linetype = "dashed", linewidth = 0.5) +
      facet_wrap(~ coefficient, scales = "free", nrow = 1) +
      scale_fill_manual(values = palette_cat[1:3], guide = "none") +
      labs(title = "Bayesian posterior distributions",
           subtitle = "Stage 2b: equity DiD coefficients on the wealth-ranked concentration index",
           x = "Coefficient value", y = "Posterior density") +
      theme_paper()
    
    save_fig(fig_posteriors, "fig_R5_posterior_densities", width = 11, height = 4)
    
    
    ### State-level implied Q1 coverage gain ----
    
    R6_data <- ecea_equity_base$panel |>
      filter(wealth_q == 1) |>
      select(state_ut, delta_cov_q) |>
      arrange(desc(delta_cov_q)) |>
      mutate(state_ut  = factor(state_ut, levels = state_ut),
             direction = ifelse(delta_cov_q >= 0, "Q1 gain", "Q1 loss"))
    
    fig_q1_gain <- ggplot(R6_data, aes(x = delta_cov_q, y = fct_rev(state_ut),
                                       fill = direction)) +
      geom_col() +
      geom_vline(xintercept = 0, colour = NEUTRAL, linetype = "dashed") +
      scale_fill_manual(values = c("Q1 gain" = PRIMARY, "Q1 loss" = ACCENT),
                        guide = "none") +
      scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
      labs(title = "State-level implied Q1 coverage gain",
           subtitle = "Equity-anchored decomposition combining level and redistribution effects",
           x = "Implied Q1 coverage gain (percentage points)", y = NULL) +
      theme_paper()
    
    save_fig(fig_q1_gain, "fig_R6_q1_implied_gain", width = 8, height = 9)
    
    
    ### Cost-effectiveness plane ----
    
    psa_long <- psa_equity |>
      select(draw, B_q1, C_q1) |>
      mutate(quintile = "Q1") |>
      bind_rows(psa_equity |>
                  select(draw, B_q1 = B_q3, C_q1 = C_q3) |>
                  mutate(quintile = "Q3")) |>
      bind_rows(psa_equity |>
                  select(draw, B_q1 = B_q5, C_q1 = C_q5) |>
                  mutate(quintile = "Q5"))
    
    fig_ce_plane <- ggplot(psa_long, aes(x = B_q1, y = C_q1, colour = quintile)) +
      geom_hline(yintercept = 0, colour = NEUTRAL, linetype = "dashed") +
      geom_vline(xintercept = 0, colour = NEUTRAL, linetype = "dashed") +
      geom_abline(slope = 73500, intercept = 0,
                  colour = NEUTRAL, linetype = "dotted") +
      geom_abline(slope = 147000, intercept = 0,
                  colour = NEUTRAL, linetype = "dotted") +
      geom_point(alpha = 0.4, size = 0.8) +
      scale_colour_manual(values = c("Q1" = PRIMARY, "Q3" = NEUTRAL,
                                     "Q5" = ACCENT), name = "Quintile") +
      scale_x_continuous(labels = comma) +
      scale_y_continuous(labels = label_number(scale = 1e-9, suffix = "B")) +
      labs(title = "Cost-effectiveness plane (equity-anchored arm)",
           subtitle = "1,000 PSA draws by quintile",
           x = "Incremental DALYs averted",
           y = "Incremental cost (INR billions)",
           caption = "Dotted lines: WTP thresholds at 0.5× and 1× per-capita GDP (INR 73,500 and INR 147,000 per DALY).") +
      theme_paper()
    
    save_fig(fig_ce_plane, "fig_CE1_ce_plane", width = 9, height = 6)
    
    
    ### Equity-anchored CEAC by quintile (shows monotonic decrease) ----
    
    ceac_long <- ceac_equity |>
      pivot_longer(starts_with("ceac_"), names_to = "quintile", values_to = "ceac",
                   names_prefix = "ceac_") |>
      mutate(quintile = toupper(quintile))
    
    fig_ceac_equity <- ggplot(ceac_long, aes(x = wtp, y = ceac, colour = quintile)) +
      geom_line(linewidth = 1.1) +
      geom_vline(xintercept = c(73500, 147000),
                 colour = NEUTRAL, linetype = "dotted", linewidth = 0.5) +
      annotate("text", x = c(73500, 147000), y = 0.97,
               label = c("0.5× GDP", "1× GDP"),
               colour = NEUTRAL, size = 3, vjust = 1, hjust = -0.1) +
      scale_colour_manual(values = c("Q1" = PRIMARY, "Q2" = SECONDARY,
                                     "Q3" = NEUTRAL, "Q4" = "grey60",
                                     "Q5" = ACCENT), name = "Quintile") +
      scale_x_continuous(labels = label_number(scale = 1e-3, suffix = "K"),
                         limits = c(0, 200000)) +
      scale_y_continuous(labels = percent, limits = c(0, 1)) +
      labs(title = "Cost-effectiveness acceptability by wealth quintile",
           subtitle = "Equity-anchored arm; monotonic gradient confirms pro-poor cost-effectiveness",
           x = "WTP threshold (INR per DALY averted)",
           y = "Pr(NMB > 0)",
           caption = "Probability of cost-effectiveness decreases monotonically from Q1 (poorest) to Q5 (richest).") +
      theme_paper()
    
    save_fig(fig_ceac_equity, "fig_CE2_ceac_equity", width = 9, height = 6)
    
    
    ### CEAC overlay: equity vs level-coverage arms ----
    
    ceac_level_long <- ceac_data$ceac |>
      pivot_longer(starts_with("ceac_"), names_to = "quintile", values_to = "ceac",
                   names_prefix = "ceac_") |>
      mutate(quintile = toupper(quintile),
             arm = "Level-coverage")
    ceac_eq_long <- ceac_equity |>
      pivot_longer(starts_with("ceac_"), names_to = "quintile", values_to = "ceac",
                   names_prefix = "ceac_") |>
      mutate(quintile = toupper(quintile),
             arm = "Equity-anchored")
    
    ceac_combined <- bind_rows(ceac_level_long, ceac_eq_long) |>
      filter(quintile == "Q1")
    
    fig_ceac_overlay <- ggplot(ceac_combined, aes(x = wtp, y = ceac, colour = arm)) +
      geom_line(linewidth = 1.1) +
      geom_vline(xintercept = c(73500, 147000),
                 colour = NEUTRAL, linetype = "dotted", linewidth = 0.5) +
      annotate("text", x = c(73500, 147000), y = 0.95,
               label = c("0.5× GDP", "1× GDP"),
               colour = NEUTRAL, size = 3, vjust = 1, hjust = -0.1) +
      scale_colour_manual(values = c("Level-coverage"  = NEUTRAL,
                                     "Equity-anchored" = PRIMARY),
                          name = "Cost-effectiveness arm") +
      scale_x_continuous(labels = label_number(scale = 1e-3, suffix = "K"),
                         limits = c(0, 200000)) +
      scale_y_continuous(labels = percent, limits = c(0, 1)) +
      labs(title = "CEAC at Q1: equity-anchored vs level-coverage",
           subtitle = "Equity-anchored arm shows substantial probability of cost-effectiveness; level-coverage arm does not",
           x = "WTP threshold (INR per DALY averted)",
           y = "Pr(NMB > 0) at Q1") +
      theme_paper()
    
    save_fig(fig_ceac_overlay, "fig_CE3_ceac_overlay_q1", width = 9, height = 5.5)
    
    
    ### EAG vs non-EAG comparison ----
    
    eag_panels <- state_descriptives |>
      left_join(eag_lookup, by = "state_ut") |>
      filter(!is.na(delta_FIC), !is.na(delta_CI))
    
    p_eag_dfic <- ggplot(eag_panels, aes(x = eag_status, y = delta_FIC,
                                         fill = eag_status)) +
      geom_boxplot(alpha = 0.6, outlier.shape = NA, width = 0.5) +
      geom_jitter(width = 0.15, size = 2.5, alpha = 0.7,
                  shape = 21, colour = "white") +
      geom_hline(yintercept = 0, colour = NEUTRAL, linetype = "dashed") +
      scale_fill_manual(values = c("EAG" = PRIMARY, "non-EAG" = SECONDARY),
                        guide = "none") +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(title = "Δ FIC by state group",
           x = NULL, y = "Δ Full immunisation coverage") +
      theme_paper()
    
    p_eag_dci <- ggplot(eag_panels, aes(x = eag_status, y = delta_CI,
                                        fill = eag_status)) +
      geom_boxplot(alpha = 0.6, outlier.shape = NA, width = 0.5) +
      geom_jitter(width = 0.15, size = 2.5, alpha = 0.7,
                  shape = 21, colour = "white") +
      geom_hline(yintercept = 0, colour = NEUTRAL, linetype = "dashed") +
      scale_fill_manual(values = c("EAG" = PRIMARY, "non-EAG" = SECONDARY),
                        guide = "none") +
      scale_y_reverse() +
      labs(title = "Δ Concentration index by state group",
           x = NULL, y = "Δ CI (reversed: down = pro-poor)") +
      theme_paper()
    
    fig_eag_compare <- (p_eag_dfic | p_eag_dci) +
      plot_annotation(
        title = "EAG vs non-EAG state comparison",
        subtitle = "EAG states gained coverage and narrowed the equity gap; non-EAG states stagnated",
        caption = "EAG (Empowered Action Group) = UP, Bihar, Madhya Pradesh, Rajasthan, Chhattisgarh, Jharkhand, Odisha, Uttarakhand.",
        theme = theme_paper())
    
    save_fig(fig_eag_compare, "fig_R7_eag_compare", width = 11, height = 5.5)
    
    
    ### EAG ICER comparison by quintile ----
    
    eag_icer_data <- eag_aggregate_ecea |>
      filter(wealth_q %in% c(1, 2)) |>
      mutate(wealth_q = paste0("Q", wealth_q),
             ICER_finite = ifelse(is.finite(ICER_health_q) & ICER_health_q > 0,
                                  ICER_health_q, NA_real_))
    
    fig_eag_icer <- ggplot(eag_icer_data,
                           aes(x = wealth_q, y = ICER_finite, fill = eag_status)) +
      geom_col(position = "dodge", width = 0.6) +
      geom_hline(yintercept = c(73500, 147000, 441000),
                 colour = NEUTRAL, linetype = "dotted", linewidth = 0.5) +
      annotate("text", x = 0.55, y = c(73500, 147000, 441000),
               label = c("0.5× GDP", "1× GDP", "3× GDP"),
               colour = NEUTRAL, size = 3, hjust = 0, vjust = -0.3) +
      scale_fill_manual(values = c("EAG" = PRIMARY, "non-EAG" = SECONDARY),
                        name = "State group") +
      scale_y_continuous(labels = comma_format(prefix = "INR ")) +
      labs(title = "Quintile ICERs by EAG state group",
           subtitle = "Aggregate ICER under equity-anchored ECEA; only quintiles with positive benefit shown",
           x = "Wealth quintile", y = "ICER (INR per DALY averted)",
           caption = "EAG-state Q1 ICER ≈ 0.55× per-capita GDP (cost-effective). non-EAG states show no aggregate health gain at Q1 or Q2.") +
      theme_paper()
    
    save_fig(fig_eag_icer, "fig_CE4_eag_icer", width = 9, height = 6)
    
    
    ### Bottom 40% / Middle 40% / Top 20% aggregate ICER ----
    
    agg_data <- aggregate_quintiles |>
      mutate(group = factor(group,
                            levels = c("Bottom 40% (Q1+Q2)",
                                       "Middle 40% (Q3+Q4)",
                                       "Top 20% (Q5)")),
             NMB_M = NMB_q / 1e6)
    
    fig_aggregate <- ggplot(agg_data, aes(x = group, y = NMB_M, fill = programme)) +
      geom_col(position = "dodge", width = 0.7) +
      geom_hline(yintercept = 0, colour = NEUTRAL) +
      scale_fill_manual(values = c("HWC" = PRIMARY, "IMI" = ACCENT),
                        name = "Programme") +
      scale_y_continuous(labels = comma_format(prefix = "INR ", suffix = "M")) +
      labs(title = "Net monetary benefit by policy-relevant grouping",
           subtitle = "Aggregate NMB at base WTP across PMJAY-aligned groupings",
           x = NULL, y = "NMB (INR millions)",
           caption = "Bottom 40% maps to PMJAY beneficiary criteria; negative NMB reflects programme cost without sufficient benefit at base case.") +
      theme_paper()
    
    save_fig(fig_aggregate, "fig_R8_aggregate_groups", width = 10, height = 5.5)
    
    
    ### OWSA tornado ----
    
    R9_data <- owsa_nmb |>
      arrange(desc(range)) |>
      head(10) |>
      mutate(param = factor(param, levels = rev(param)))
    
    nmb_q1_base <- ecea_base$by_quintile$NMB_q[1]
    
    fig_owsa <- ggplot(R9_data) +
      geom_segment(aes(x = nmb_lo, xend = nmb_hi, y = param, yend = param),
                   colour = PRIMARY, linewidth = 6, lineend = "butt") +
      geom_point(aes(x = nmb_lo, y = param), colour = ACCENT, size = 3) +
      geom_point(aes(x = nmb_hi, y = param), colour = PRIMARY, size = 3) +
      geom_vline(xintercept = nmb_q1_base, colour = NEUTRAL, linetype = "dashed") +
      annotate("text", x = nmb_q1_base, y = 0.5,
               label = sprintf("Base NMB:\nINR %s",
                               format(round(nmb_q1_base / 1e9, 1), big.mark = ",")),
               colour = NEUTRAL, size = 3, vjust = -1) +
      scale_x_continuous(labels = label_number(scale = 1e-9, suffix = "B")) +
      labs(title = "One-way sensitivity analysis (NMB at Q1, base WTP)",
           subtitle = "Top 10 parameters by absolute NMB range",
           x = "NMB at Q1 (INR billions)", y = NULL,
           caption = "Brick = parameter low; Heritage = parameter high.") +
      theme_paper()
    
    save_fig(fig_owsa, "fig_S1_owsa_tornado", width = 9, height = 6)
    
    
    ### Equity dose-response by treatment ----
    
    R10_HWC <- state_descriptives |>
      filter(!is.na(delta_CI), !is.na(x_s_primary)) |>
      ggplot(aes(x = x_s_primary, y = delta_CI)) +
      geom_hline(yintercept = 0, colour = NEUTRAL, linetype = "dashed") +
      geom_point(size = 3, colour = PRIMARY, alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE,
                  colour = PRIMARY, fill = LIGHT_GREY) +
      scale_y_reverse() +
      labs(title = "HWC density vs Δ CI",
           x = "HWC density (per 100k)",
           y = "Δ CI (NFHS-5 − NFHS-4, reversed)") +
      theme_paper()
    
    R10_IMI <- state_descriptives |>
      filter(!is.na(delta_CI), !is.na(imi_exposure_intensity)) |>
      ggplot(aes(x = imi_exposure_intensity, y = delta_CI)) +
      geom_hline(yintercept = 0, colour = NEUTRAL, linetype = "dashed") +
      geom_point(size = 3, colour = ACCENT, alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE,
                  colour = ACCENT, fill = LIGHT_GREY) +
      scale_y_reverse() +
      labs(title = "IMI exposure vs Δ CI",
           x = "IMI exposure intensity",
           y = NULL) +
      theme_paper()
    
    fig_dose_response <- (R10_HWC | R10_IMI) +
      plot_annotation(
        title = "Equity impact dose-response by treatment",
        subtitle = "Negative slope = treatment associated with pro-poor redistribution",
        theme = theme_paper())
    
    save_fig(fig_dose_response, "fig_S2_equity_dose_response", width = 11, height = 5)
    
    
    ### Cost-allocation sensitivity (alpha grid from RDS) ----
    
    D1_data <- alpha_sensitivity |>
      filter(wealth_q == 1) |>
      select(scenario, alpha, B_health_q, C_net_q, ICER_health_q, NMB_q) |>
      mutate(NMB_b = NMB_q / 1e9,
             ICER_finite = ifelse(is.finite(ICER_health_q) & ICER_health_q > 0,
                                  ICER_health_q, NA_real_))
    
    fig_D1_A <- ggplot(D1_data, aes(x = alpha, y = NMB_b, colour = scenario)) +
      geom_hline(yintercept = 0, colour = NEUTRAL, linetype = "dashed") +
      geom_line(linewidth = 1) +
      geom_point(size = 3) +
      scale_colour_manual(values = c("Point estimate" = PRIMARY,
                                     "Upper 95% CI"   = ACCENT),
                          name = "Stage 2a coefficient") +
      scale_x_continuous(labels = percent_format(accuracy = 1),
                         breaks = c(0.01, 0.05, 0.10, 0.20, 0.30)) +
      labs(title = "Q1 net monetary benefit by allocation",
           x = "HWC immunisation allocation (alpha)",
           y = "Q1 NMB (INR billions)") +
      theme_paper()
    
    fig_D1_B <- D1_data |>
      filter(scenario == "Upper 95% CI", !is.na(ICER_finite)) |>
      ggplot(aes(x = alpha, y = ICER_finite)) +
      geom_hline(yintercept = c(73500, 147000, 441000),
                 colour = NEUTRAL, linetype = c("dashed", "dotted", "dotted"),
                 linewidth = 0.6) +
      annotate("text", x = 0.30, y = c(73500, 147000, 441000),
               label = c("0.5× GDP", "1× GDP", "3× GDP"),
               colour = NEUTRAL, size = 3, vjust = -0.4, hjust = 1) +
      geom_line(linewidth = 1, colour = ACCENT) +
      geom_point(size = 3, colour = ACCENT) +
      scale_x_continuous(labels = percent_format(accuracy = 1),
                         breaks = c(0.01, 0.05, 0.10, 0.20, 0.30)) +
      scale_y_log10(labels = comma_format(prefix = "INR ")) +
      labs(title = "Q1 ICER under upper-CI scenario",
           x = "HWC immunisation allocation (alpha)",
           y = "ICER at Q1 (INR per DALY, log scale)") +
      theme_paper()
    
    fig_alpha <- (fig_D1_A | fig_D1_B) +
      plot_annotation(
        title = "Cost-allocation sensitivity",
        subtitle = "How the Q1 cost-effectiveness conclusion depends on the HWC immunisation allocation",
        caption = "Base case alpha = 10%. ICER below 1× per-capita GDP across all alpha values under upper-CI scenario.",
        theme = theme_paper())
    
    save_fig(fig_alpha, "fig_S3_alpha_sensitivity", width = 12, height = 5.5)
    
    
    ### Bayesian trace plots (appendix) ----
    
    trace_data <- posterior::as_draws_df(stage2b$fit) |>
      select(.chain, .iteration,
             `HWC × post`        = `b_post:x_s`,
             `IMI × post`        = `b_post:imi_intensity`,
             `HWC × IMI × post`  = `b_post:x_s:imi_intensity`) |>
      pivot_longer(-c(.chain, .iteration),
                   names_to = "coefficient", values_to = "value") |>
      mutate(coefficient = factor(coefficient,
                                  levels = c("HWC × post", "IMI × post",
                                             "HWC × IMI × post")),
             chain = factor(.chain))
    
    fig_trace <- ggplot(trace_data, aes(x = .iteration, y = value, colour = chain)) +
      geom_line(linewidth = 0.3, alpha = 0.7) +
      facet_wrap(~ coefficient, ncol = 1, scales = "free_y") +
      scale_colour_manual(values = palette_cat, name = "Chain") +
      labs(title = "Bayesian posterior trace plots",
           subtitle = "Stage 2b coefficient chains across 4 chains × 2,000 post-warmup iterations",
           x = "Iteration (post-warmup)",
           y = "Coefficient value") +
      theme_paper() +
      theme(legend.position = "bottom")
    
    save_fig(fig_trace, "fig_A1_trace_plots", width = 10, height = 8)
    
    cat("Figures complete.\n")
    
    ### Lorenz curve of full immunisation coverage ----
    
    if (!is.null(concentration_curve)) {
      
      lorenz_curve_data <- concentration_curve |>
        mutate(
          round   = factor(round, levels = c("NFHS-4", "NFHS-5")),
          cum_pop = as.numeric(cum_pop),
          cum_y   = as.numeric(cum_y)
        ) |>
        filter(!is.na(round), !is.na(cum_pop), !is.na(cum_y)) |>
        bind_rows(
          tibble(round = factor(c("NFHS-4", "NFHS-5"),
                                levels = c("NFHS-4", "NFHS-5")),
                 cum_pop = 0,
                 cum_y   = 0),
          tibble(round = factor(c("NFHS-4", "NFHS-5"),
                                levels = c("NFHS-4", "NFHS-5")),
                 cum_pop = 1,
                 cum_y   = 1)
        ) |>
        distinct(round, cum_pop, cum_y) |>
        arrange(round, cum_pop, cum_y)
      
      fig_lorenz <- ggplot(lorenz_curve_data, aes(x = cum_pop, y = cum_y,
                                                  colour = round)) +
        geom_abline(slope = 1, intercept = 0,
                    colour = NEUTRAL, linetype = "dashed", linewidth = 0.6) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 1.4, alpha = 0.65) +
        scale_colour_manual(values = c("NFHS-4" = SECONDARY,
                                       "NFHS-5" = PRIMARY),
                            name = "Survey round") +
        scale_x_continuous(labels = percent_format(accuracy = 1),
                           limits = c(0, 1),
                           breaks = seq(0, 1, 0.2),
                           expand = expansion(mult = c(0, 0))) +
        scale_y_continuous(labels = percent_format(accuracy = 1),
                           limits = c(0, 1),
                           breaks = seq(0, 1, 0.2),
                           expand = expansion(mult = c(0, 0))) +
        coord_equal() +
        labs(
          title = "Lorenz curve of full immunisation coverage",
          subtitle = "Vaccination equity improved between NFHS-4 and NFHS-5",
          x = "Cumulative population share",
          y = "Cumulative share of fully immunised children",
          caption = "Full immunisation became less concentrated among richer households"
        ) +
        theme_paper() +
        theme(panel.grid.minor = element_blank())
      
      save_fig(fig_lorenz, "fig_CE_concentration_curve", width = 7, height = 7)
      
    }
  }
  
  
  ## 3. TABLES ----
  
  if (TRUE) {
    
    
    ### Table — Population characteristics (microdata, Table 1) ----
    
    pct       <- function(x) sprintf("%.1f%%", 100 * x)
    mean_only <- function(m) sprintf("%.1f", m)
    
    r4 <- table1_demographics |> filter(round == "NFHS-4")
    r5 <- table1_demographics |> filter(round == "NFHS-5")
    
    # Build wealth-quintile labels with MPCE interquartile range in brackets.
    # The IQR (P25 to P75) conveys the spread of consumption within each NFHS
    # asset-index quintile, which is the conventional reporting form for MPCE
    # strata. NSS 75th Round (2017-18) inflated to 2021 INR using CPI-IW.
    if (!is.null(t1_quintile_mpce)) {
      fmt_inr_short <- function(x) formatC(round(x), format = "d", big.mark = ",")
      mpce_brackets <- t1_quintile_mpce |>
        arrange(wealth_quintile) |>
        mutate(bracket = paste0(
          " [INR ", fmt_inr_short(mpce_p25_inr_2021),
          "–",      fmt_inr_short(mpce_p75_inr_2021), "]"
        )) |>
        pull(bracket)
      if (length(mpce_brackets) != 5) mpce_brackets <- rep("", 5)
    } else {
      mpce_brackets <- rep("", 5)
    }
    
    q_labels <- c(
      paste0("Q1 — Poorest (%)", mpce_brackets[1]),
      paste0("Q2 (%)",           mpce_brackets[2]),
      paste0("Q3 — Middle (%)",  mpce_brackets[3]),
      paste0("Q4 (%)",           mpce_brackets[4]),
      paste0("Q5 — Richest (%)", mpce_brackets[5])
    )
    
    pop_data <- tibble(
      Group = c(
        "Sample",
        "Child demographics", "Child demographics", "Child demographics",
        "Household / SES",
        "Wealth quintile", "Wealth quintile", "Wealth quintile",
        "Wealth quintile", "Wealth quintile",
        "Maternal education", "Maternal education", "Maternal education",
        "Maternal education",
        "National outcome"
      ),
      Variable = c(
        "Children 12-23 months (unweighted N)",
        "Female (%)", "Mean age (months)",
        "Scheduled Caste / Scheduled Tribe (%)",
        "Urban residence (%)",
        q_labels[1], q_labels[2], q_labels[3], q_labels[4], q_labels[5],
        "No schooling (%)", "Primary (%)", "Secondary (%)", "Higher (%)",
        "Full immunisation coverage (%)"
      ),
      NFHS_4 = c(
        format(r4$n_unweighted, big.mark = ","),
        pct(r4$pct_female), mean_only(r4$mean_age_months),
        pct(r4$pct_sc_st), pct(r4$pct_urban),
        pct(r4$pct_wealth_q1), pct(r4$pct_wealth_q2), pct(r4$pct_wealth_q3),
        pct(r4$pct_wealth_q4), pct(r4$pct_wealth_q5),
        pct(r4$pct_educ_none), pct(r4$pct_educ_primary),
        pct(r4$pct_educ_secondary), pct(r4$pct_educ_higher),
        pct(r4$fic_national)
      ),
      NFHS_5 = c(
        format(r5$n_unweighted, big.mark = ","),
        pct(r5$pct_female), mean_only(r5$mean_age_months),
        pct(r5$pct_sc_st), pct(r5$pct_urban),
        pct(r5$pct_wealth_q1), pct(r5$pct_wealth_q2), pct(r5$pct_wealth_q3),
        pct(r5$pct_wealth_q4), pct(r5$pct_wealth_q5),
        pct(r5$pct_educ_none), pct(r5$pct_educ_primary),
        pct(r5$pct_educ_secondary), pct(r5$pct_educ_higher),
        pct(r5$fic_national)
      )
    )
    
    tab_population <- pop_data |>
      gt(groupname_col = "Group") |>
      tab_header(
        title = "Population characteristics of children 12-23 months by NFHS round",
        subtitle = "Survey-weighted estimates from NFHS-4 (2015-16) and NFHS-5 (2019-21) microdata"
      ) |>
      cols_label(
        Variable = "Variable",
        NFHS_4   = "NFHS-4 (2015-16)",
        NFHS_5   = "NFHS-5 (2019-21)"
      ) |>
      cols_width(
        Variable ~ pct(56),
        NFHS_4   ~ pct(22),
        NFHS_5   ~ pct(22)
      ) |>
      tab_footnote(
        footnote = paste(
          "Wealth quintiles assigned from the NFHS DHS wealth index, an",
          "asset-and-amenity composite. Bracketed values show the",
          "interquartile range (P25 to P75) of monthly per-capita",
          "consumption expenditure (MPCE) within each quintile, derived",
          "from NSS 75th Round (2017-18) and inflated to 2021 INR using",
          "CPI-IW. The two stratifications are congruent but not",
          "identical; MPCE figures provide an absolute economic anchor."
        ),
        locations = cells_row_groups(groups = "Wealth quintile")
      ) |>
      sub_missing(missing_text = "—") |>
      apply_table_style() |>
      tab_options(table.width = px(450))
    
    save_table(tab_population, "tab_T1_population_characteristics")
    
    
    ### Table — State-level descriptives (alphabetical) ----
    
    state_data <- state_descriptives |>
      select(state_ut, nfhs5_phase, x_s_primary, imi_exposure_intensity,
             pop_12_23_2021, FIC_NFHS4, FIC_NFHS5, delta_FIC,
             CI_NFHS4, CI_NFHS5, delta_CI) |>
      arrange(state_ut) |>
      mutate(pop_12_23_2021 = pop_12_23_2021 / 1000)
    
    tab_states <- state_data |>
      gt(rowname_col = "state_ut") |>
      tab_header(
        title = "State-level characteristics across NFHS rounds",
        subtitle = "All 32 study states and union territories, alphabetical"
      ) |>
      tab_spanner(label = "Treatment intensity",
                  columns = c(x_s_primary, imi_exposure_intensity)) |>
      tab_spanner(label = "Coverage (FIC)",
                  columns = c(FIC_NFHS4, FIC_NFHS5, delta_FIC)) |>
      tab_spanner(label = "Equity (CI)",
                  columns = c(CI_NFHS4, CI_NFHS5, delta_CI)) |>
      cols_label(
        nfhs5_phase            = "NFHS-5 phase",
        x_s_primary            = "HWC density",
        imi_exposure_intensity = "IMI intensity",
        pop_12_23_2021         = "Pop 12-23m (000s)",
        FIC_NFHS4              = "NFHS-4",
        FIC_NFHS5              = "NFHS-5",
        delta_FIC              = "Δ",
        CI_NFHS4               = "NFHS-4",
        CI_NFHS5               = "NFHS-5",
        delta_CI               = "Δ"
      ) |>
      fmt_number(columns = pop_12_23_2021, decimals = 0, sep_mark = ",") |>
      fmt_number(columns = c(x_s_primary, imi_exposure_intensity),
                 decimals = 2) |>
      fmt_percent(columns = c(FIC_NFHS4, FIC_NFHS5, delta_FIC),
                  decimals = 1, scale_values = TRUE) |>
      fmt_number(columns = c(CI_NFHS4, CI_NFHS5, delta_CI), decimals = 3) |>
      sub_missing(missing_text = "—") |>
      apply_table_style()
    
    save_table(tab_states, "tab_T2_state_characteristics")
    
    
    ### Table — DiD coefficients ----
    
    coef_data <- tibble(
      Coefficient = c("HWC × post", "IMI × post",
                      "HWC × IMI × post complementarity"),
      freq_est   = c(stage2a$estimate, stage2a$beta_imi_estimate,
                     stage2a$beta_inter_estimate),
      freq_se    = c(stage2a$se, stage2a$beta_imi_se,
                     stage2a$beta_inter_se),
      freq_p     = c(stage2a$p_value, stage2a$beta_imi_p,
                     stage2a$beta_inter_p),
      freq_ci    = c(
        sprintf("(%.3f, %.3f)",
                stage2a$estimate - 1.96 * stage2a$se,
                stage2a$estimate + 1.96 * stage2a$se),
        sprintf("(%.3f, %.3f)",
                stage2a$beta_imi_estimate - 1.96 * stage2a$beta_imi_se,
                stage2a$beta_imi_estimate + 1.96 * stage2a$beta_imi_se),
        sprintf("(%.3f, %.3f)",
                stage2a$beta_inter_estimate - 1.96 * stage2a$beta_inter_se,
                stage2a$beta_inter_estimate + 1.96 * stage2a$beta_inter_se)
      ),
      bayes_med  = c(stage2b$posterior_median,
                     stage2b$beta_imi_ci_median,
                     stage2b$beta_inter_ci_median),
      bayes_ci   = c(
        sprintf("(%.4f, %.4f)",
                stage2b$posterior_ci_lo, stage2b$posterior_ci_hi),
        sprintf("(%.4f, %.4f)",
                stage2b$beta_imi_ci_q025, stage2b$beta_imi_ci_q975),
        sprintf("(%.4f, %.4f)",
                stage2b$beta_inter_ci_q025, stage2b$beta_inter_ci_q975)
      ),
      bayes_pdir = scales::percent(
        c(stage2b$prob_negative, stage2b$prob_imi_negative,
          stage2b$prob_inter_positive),
        accuracy = 0.1)
    )
    
    tab_coefficients <- coef_data |>
      gt() |>
      tab_header(
        title = "Augmented difference-in-differences coefficient estimates",
        subtitle = "Frequentist coverage DiD (Stage 2a) and Bayesian equity DiD (Stage 2b)"
      ) |>
      tab_spanner(label = "Frequentist (outcome: FIC)",
                  columns = c(freq_est, freq_se, freq_ci, freq_p)) |>
      tab_spanner(label = "Bayesian (outcome: CI)",
                  columns = c(bayes_med, bayes_ci, bayes_pdir)) |>
      cols_label(
        Coefficient = "Coefficient",
        freq_est    = "Estimate",
        freq_se     = "SE",
        freq_ci     = "95% CI",
        freq_p      = "p-value",
        bayes_med   = "Posterior median",
        bayes_ci    = "95% CrI",
        bayes_pdir  = "Pr(direction)"
      ) |>
      fmt_number(columns = c(freq_est, freq_se), decimals = 4) |>
      fmt_number(columns = bayes_med, decimals = 4) |>
      fmt_number(columns = freq_p, decimals = 3) |>
      apply_table_style()
    
    save_table(tab_coefficients, "tab_T3_did_coefficients")
    
    
    ### Table — Quintile-level ECEA results (equity-anchored at top) ----
    
    equity_quintile <- ecea_equity_base$by_quintile |>
      mutate(
        Programme = "Equity-anchored (HWC + IMI)",
        n_q          = round(n_q),
        B_health_q   = round(B_health_q),
        C_net_q_b    = C_net_q / 1e9,
        ICER_label   = ifelse(is.na(ICER_health_q) | !is.finite(ICER_health_q),
                              NA_character_,
                              format(round(ICER_health_q), big.mark = ",")),
        NMB_q_b      = NMB_q / 1e9
      ) |>
      select(Programme, wealth_q, n_q, B_health_q,
             C_net_q_b, ICER_label, NMB_q_b)
    
    single_quintile <- quintile_summary |>
      mutate(
        Programme    = ifelse(programme == "HWC",
                              "HWC only (level)",
                              "IMI only (level)"),
        n_q          = round(n_q),
        B_health_q   = round(B_health_q),
        C_net_q_b    = C_net_q / 1e9,
        ICER_label   = ifelse(is.na(ICER_health_q),
                              NA_character_,
                              format(round(ICER_health_q), big.mark = ",")),
        NMB_q_b      = NMB_q / 1e9
      ) |>
      select(Programme, wealth_q, n_q, B_health_q,
             C_net_q_b, ICER_label, NMB_q_b)
    
    ecea_data <- bind_rows(equity_quintile, single_quintile) |>
      mutate(Programme = factor(Programme,
                                levels = c("Equity-anchored (HWC + IMI)",
                                           "HWC only (level)",
                                           "IMI only (level)"))) |>
      arrange(Programme, wealth_q)
    
    tab_quintile_ecea <- ecea_data |>
      gt(groupname_col = "Programme") |>
      tab_header(
        title = "Quintile-level cost-effectiveness results",
        subtitle = "Equity-anchored ECEA (headline) and single-arm comparators"
      ) |>
      cols_label(
        wealth_q     = "Quintile",
        n_q          = "Children reached",
        B_health_q   = "DALYs averted",
        C_net_q_b    = "Net cost (INR B)",
        ICER_label   = "ICER (INR/DALY)",
        NMB_q_b      = "NMB (INR B)"
      ) |>
      fmt_number(columns = c(n_q, B_health_q),
                 decimals = 0, sep_mark = ",") |>
      fmt_number(columns = c(C_net_q_b, NMB_q_b),
                 decimals = 1, sep_mark = ",") |>
      sub_missing(missing_text = "—") |>
      apply_table_style()
    
    save_table(tab_quintile_ecea, "tab_T4_quintile_ecea")
    
    
    ### Table — Aggregate ICERs by policy grouping (equity-anchored at top) ----
    
    # Compute equity-anchored aggregate by Bottom 40 / Middle 40 / Top 20
    equity_aggregate <- ecea_equity_base$by_quintile |>
      mutate(group = case_when(
        wealth_q %in% 1:2 ~ "Bottom 40% (Q1+Q2)",
        wealth_q %in% 3:4 ~ "Middle 40% (Q3+Q4)",
        wealth_q == 5     ~ "Top 20% (Q5)"
      )) |>
      group_by(group) |>
      summarise(
        n_q          = sum(n_q,          na.rm = TRUE),
        B_health_q   = sum(B_health_q,   na.rm = TRUE),
        C_net_q      = sum(C_net_q,      na.rm = TRUE),
        NMB_q        = sum(NMB_q,        na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        programme = "Equity-anchored (HWC + IMI)",
        ICER_health = ifelse(B_health_q > 1e-6, C_net_q / B_health_q,
                             NA_real_)
      ) |>
      select(programme, group, n_q, B_health_q, C_net_q,
             ICER_health, NMB_q)
    
    # Combine with single-arm aggregates (HWC, IMI)
    aggregate_combined <- bind_rows(
      equity_aggregate,
      aggregate_quintiles |> select(programme, group, n_q, B_health_q,
                                    C_net_q, ICER_health, NMB_q)
    ) |>
      mutate(
        programme = factor(programme,
                           levels = c("Equity-anchored (HWC + IMI)", "HWC", "IMI")),
        n_q          = round(n_q),
        B_health_q   = round(B_health_q),
        C_net_q_b    = C_net_q / 1e9,
        ICER_label   = ifelse(is.na(ICER_health) | !is.finite(ICER_health),
                              NA_character_,
                              format(round(ICER_health), big.mark = ",")),
        NMB_q_b      = NMB_q / 1e9
      ) |>
      arrange(programme, group) |>
      select(programme, group, n_q, B_health_q, C_net_q_b,
             ICER_label, NMB_q_b)
    
    tab_aggregate <- aggregate_combined |>
      gt(groupname_col = "programme") |>
      tab_header(
        title = "Aggregate ECEA results by policy-relevant grouping",
        subtitle = "Equity-anchored ECEA (headline) and single-arm comparators across PMJAY-aligned categories"
      ) |>
      cols_label(
        group        = "Group",
        n_q          = "Children reached",
        B_health_q   = "DALYs averted",
        C_net_q_b    = "Net cost (INR B)",
        ICER_label   = "ICER (INR/DALY)",
        NMB_q_b      = "NMB (INR B)"
      ) |>
      fmt_number(columns = c(n_q, B_health_q),
                 decimals = 0, sep_mark = ",") |>
      fmt_number(columns = c(C_net_q_b, NMB_q_b),
                 decimals = 1, sep_mark = ",") |>
      sub_missing(missing_text = "—") |>
      apply_table_style()
    
    save_table(tab_aggregate, "tab_T5_aggregate_groups")
    
    
    ### Table — EAG vs non-EAG aggregate ECEA ----
    
    eag_data <- eag_aggregate_ecea |>
      mutate(
        n_q          = round(n_q),
        B_health_q   = round(B_health_q),
        C_net_q_b    = C_net_q / 1e9,
        ICER_label   = ifelse(is.na(ICER_health_q) | !is.finite(ICER_health_q),
                              NA_character_,
                              format(round(ICER_health_q), big.mark = ",")),
        NMB_q_b      = NMB_q / 1e9
      ) |>
      select(eag_status, wealth_q, n_q, B_health_q,
             C_net_q_b, ICER_label, NMB_q_b)
    
    tab_eag <- eag_data |>
      gt(groupname_col = "eag_status") |>
      tab_header(
        title = "EAG vs non-EAG state aggregate ECEA results",
        subtitle = "Equity-anchored arm; EAG = Empowered Action Group states"
      ) |>
      cols_label(
        wealth_q     = "Quintile",
        n_q          = "Children reached",
        B_health_q   = "DALYs averted",
        C_net_q_b    = "Net cost (INR B)",
        ICER_label   = "ICER (INR/DALY)",
        NMB_q_b      = "NMB (INR B)"
      ) |>
      fmt_number(columns = c(n_q, B_health_q),
                 decimals = 0, sep_mark = ",") |>
      fmt_number(columns = c(C_net_q_b, NMB_q_b),
                 decimals = 2, sep_mark = ",") |>
      sub_missing(missing_text = "—") |>
      apply_table_style()
    
    save_table(tab_eag, "tab_T6_eag_aggregate")
    
    
    ### Table — OWSA top 10 ----
    
    owsa_data <- owsa_nmb |>
      arrange(desc(range)) |>
      head(10) |>
      mutate(
        nmb_lo_b   = nmb_lo / 1e9,
        nmb_hi_b   = nmb_hi / 1e9,
        delta_lo_b = delta_lo / 1e9,
        delta_hi_b = delta_hi / 1e9,
        range_b    = range / 1e9
      ) |>
      select(param, value_lo, value_hi, nmb_lo_b, nmb_hi_b,
             delta_lo_b, delta_hi_b, range_b)
    
    tab_owsa <- owsa_data |>
      gt() |>
      tab_header(
        title = "One-way sensitivity analysis: top 10 parameters",
        subtitle = "NMB at Q1 at base WTP; ranked by absolute range"
      ) |>
      cols_label(
        param      = "Parameter",
        value_lo   = "Low value",
        value_hi   = "High value",
        nmb_lo_b   = "NMB low (B)",
        nmb_hi_b   = "NMB high (B)",
        delta_lo_b = "Δ low (B)",
        delta_hi_b = "Δ high (B)",
        range_b    = "Range (B)"
      ) |>
      fmt_number(columns = c(nmb_lo_b, nmb_hi_b, delta_lo_b,
                             delta_hi_b, range_b),
                 decimals = 1, sep_mark = ",") |>
      apply_table_style()
    
    save_table(tab_owsa, "tab_T7_owsa_top10")
    
    
    ### Table — Cost-allocation sensitivity grid (alpha) ----
    
    alpha_data <- alpha_sensitivity |>
      filter(wealth_q == 1) |>
      mutate(
        B_health_q   = round(B_health_q),
        C_net_q_M    = C_net_q / 1e6,
        ICER_label   = ifelse(is.na(ICER_health_q) | !is.finite(ICER_health_q),
                              NA_character_,
                              format(round(ICER_health_q), big.mark = ",")),
        NMB_q_M      = NMB_q / 1e6
      ) |>
      select(scenario, alpha, B_health_q, C_net_q_M, ICER_label, NMB_q_M)
    
    tab_alpha <- alpha_data |>
      gt(groupname_col = "scenario") |>
      tab_header(
        title = "Cost-allocation sensitivity at Q1",
        subtitle = "Q1 ICER and NMB across HWC immunisation allocations from 1% to 30%"
      ) |>
      cols_label(
        alpha        = "Allocation (α)",
        B_health_q   = "DALYs averted",
        C_net_q_M    = "Net cost (INR M)",
        ICER_label   = "ICER (INR/DALY)",
        NMB_q_M      = "NMB (INR M)"
      ) |>
      fmt_percent(columns = alpha, decimals = 0) |>
      fmt_number(columns = B_health_q, decimals = 0, sep_mark = ",") |>
      fmt_number(columns = c(C_net_q_M, NMB_q_M),
                 decimals = 0, sep_mark = ",") |>
      sub_missing(missing_text = "—") |>
      apply_table_style()
    
    save_table(tab_alpha, "tab_T8_alpha_sensitivity")
    
    
    ### Table — Cost-effectiveness anchor comparison ----
    
    anchor_data <- tibble(
      Reference = c("This analysis (HWC + IMI, Q1 equity-anchored)",
                    "This analysis (EAG-state Q1 only)",
                    "Clarke-Deelder 2024 (IMI base case)",
                    "Chatterjee 2018 (routine immunisation, India)",
                    "OHE 2024 (LMIC vaccination, average)",
                    "WHO threshold 1× per-capita GDP (India 2021)",
                    "WHO threshold 3× per-capita GDP (India 2021)"),
      Concept = c("Cost per DALY averted, equity-redistributive",
                  "Cost per DALY averted, EAG states only",
                  "Cost per DALY averted, IMI",
                  "Cost per fully immunised child",
                  "Cost per fully immunised child (LMIC mean)",
                  "Per-capita GDP based threshold",
                  "Per-capita GDP based threshold"),
      Estimate = c("INR 138,878 (~0.94× per-capita GDP)",
                   "INR 80,247 (~0.55× per-capita GDP)",
                   "USD 327 — dominated (95% UI)",
                   "USD 32 (national weighted)",
                   "USD 30-50 (range)",
                   "INR 147,000 (USD 1,927)",
                   "INR 441,000 (USD 5,800)"),
      Source = c("Stage 3d, ecea_equity_base",
                 "Stage 5, eag_aggregate_ecea",
                 "Clarke-Deelder et al. 2024 HPP",
                 "Chatterjee et al. 2018 BMJ GH",
                 "OHE 2024 cross-country analysis",
                 "World Bank 2021",
                 "WHO threshold (legacy)")
    )
    
    tab_anchors <- anchor_data |>
      gt() |>
      tab_header(
        title = "Cost-effectiveness anchor comparison",
        subtitle = "Reference values for interpreting the cost-effectiveness conclusions"
      ) |>
      apply_table_style()
    
    save_table(tab_anchors, "tab_T10_ce_anchors")
    
    cat("Tables complete.\n")
    
  }
  
  
  ## 4. APPENDIX EXHIBITS ----
  
  if (TRUE) {
    
    
    ### Table — Bayesian model diagnostics ----
    
    diag_data <- posterior::as_draws_df(stage2b$fit) |>
      posterior::subset_draws(variable = c("b_Intercept", "b_post", "b_x_s",
                                           "b_imi_intensity", "b_post:x_s",
                                           "b_post:imi_intensity",
                                           "b_post:x_s:imi_intensity",
                                           "sd_state_code__Intercept")) |>
      posterior::summarise_draws() |>
      mutate(variable = recode(variable,
                               "b_Intercept"                = "Intercept (baseline CI)",
                               "b_post"                     = "post (period shift)",
                               "b_x_s"                      = "HWC density (state-level)",
                               "b_imi_intensity"            = "IMI intensity (state-level)",
                               "b_post:x_s"                 = "HWC × post (key DiD coefficient)",
                               "b_post:imi_intensity"       = "IMI × post (key DiD coefficient)",
                               "b_post:x_s:imi_intensity"   = "HWC × IMI × post (interaction)",
                               "sd_state_code__Intercept"   = "SD of state-level random intercept"
      )) |>
      select(variable, mean, median, sd, q5, q95, rhat, ess_bulk, ess_tail)
    
    tab_diagnostics <- diag_data |>
      gt() |>
      tab_header(
        title = "Bayesian model diagnostics",
        subtitle = "Posterior summary and convergence statistics from the Stage 2b hierarchical model"
      ) |>
      cols_label(
        variable = "Parameter",
        mean     = "Mean",
        median   = "Median",
        sd       = "SD",
        q5       = "5%",
        q95      = "95%",
        rhat     = "R-hat",
        ess_bulk = "Bulk ESS",
        ess_tail = "Tail ESS"
      ) |>
      fmt_number(columns = c(mean, median, sd, q5, q95), decimals = 4) |>
      fmt_number(columns = rhat, decimals = 3) |>
      fmt_number(columns = c(ess_bulk, ess_tail),
                 decimals = 0, sep_mark = ",") |>
      apply_table_style() |>
      tab_style(
        style = cell_fill(color = LIGHT_GREY),
        locations = cells_body(columns = rhat, rows = rhat > 1.01)
      )
    
    save_table(tab_diagnostics, "tab_TA1_bayesian_diagnostics")
    
    
    ### Table — Data sources and variable derivations ----
    
    source_data <- tibble(
      Source = c(
        "NFHS-4 microdata (KR file)",
        "NFHS-5 microdata (KR file)",
        "GBD 2019 (under-5 DALYs)",
        "MoHFW HWC operational counts",
        "MoHFW IMI Phase 1 district list",
        "MoHFW IMI 2.0 Operational Guidelines",
        "Chatterjee et al. 2018 BMJ Global Health",
        "Chatterjee et al. 2021 Health Policy and Planning",
        "Singh et al. 2021 Health Policy and Planning",
        "Census of India 2011 + RGI projections",
        "Tendulkar Committee 2014 (poverty lines)",
        "NSS 75th Round (OOP healthcare expenditure)",
        "RBI 2021 Annual Report (USD-INR exchange rate)",
        "CPR India Budget Brief 2021-22",
        "PRS DFG analysis 2022-23"
      ),
      Year = c("2015-16", "2019-21", "2019",
               "Feb 2019, Mar 2020", "2017-18", "2019-20",
               "2018", "2021", "2021",
               "2011, 2021", "2011-12", "2017-18", "2020-21",
               "2021", "2022"),
      `Coverage / sample` = c(
        "699,686 children 0-59 months across 28 states",
        "232,920 children 0-59 months across 36 states/UTs",
        "Indian state-level under-5 DALYs by cause",
        "All 36 states/UTs, parliamentary records",
        "190 districts in 24 states (Gurnani 2018 sample)",
        "272 districts + 109 UP/Bihar block districts",
        "7 states, 24 districts, routine immunisation costs",
        "5 states, 40 districts, IMI incremental costs",
        "Multi-state HWC scale-up costing study",
        "All India + 36 states/UTs",
        "Rural and urban poverty lines",
        "All India OOP by wealth quintile",
        "Annual average exchange rate",
        "FY 2018-19 to 2021-22 HWC expenditure",
        "FY 2022-23 HWC budget allocation"
      ),
      `Variables derived` = c(
        "FIC, fully_vaccinated, wealth_q, ci by state",
        "FIC, fully_vaccinated, wealth_q, ci by state",
        "DALY rates by state x cause",
        "x_s_primary (HWC density per 100k)",
        "imi_phase_1_districts by state",
        "imi_2_0_districts by state",
        "Routine immunisation cost benchmark",
        "IMI cost per dose (state-level)",
        "HWC marginal cost per facility",
        "pop_12_23_2021, population_2021",
        "pov_line_rural_2018, pov_line_urban_2018",
        "oop_q (mean OOP by quintile)",
        "usd_inr_2021",
        "MoHFW HWC budget calibration anchor",
        "MoHFW HWC budget calibration anchor"
      )
    )
    
    tab_sources <- source_data |>
      gt() |>
      tab_header(
        title = "Data sources and variable derivations",
        subtitle = "Inputs for the distributional ECEA across NFHS-4 and NFHS-5"
      ) |>
      apply_table_style()
    
    save_table(tab_sources, "tab_TA2_data_sources")
    
    cat("Appendix exhibits complete.\n")
    
  }
  
  
  ## 5. SCHEMATIC HTML TO PNG ----
  
  if (TRUE) {
    
    schematic_html <- "AB_Equity_Fig_D4_Schematic.html"
    schematic_png  <- file.path(fig_dir, "fig_schematic.png")
    schematic_pdf  <- file.path(fig_dir, "fig_schematic.pdf")
    
    if (file.exists(schematic_html)) {
      tryCatch({
        if (!requireNamespace("webshot2", quietly = TRUE)) {
          message("webshot2 not installed; skipping schematic PNG render.")
        } else {
          webshot2::webshot(
            url     = paste0("file://", normalizePath(schematic_html)),
            file    = schematic_png,
            vwidth  = 1100,
            vheight = 800,
            zoom    = 2
          )
          cat("Schematic rendered to", schematic_png, "\n")
        }
      }, error = function(e) {
        message("Schematic PNG render failed: ", conditionMessage(e))
      })
      
      tryCatch({
        if (requireNamespace("webshot2", quietly = TRUE)) {
          webshot2::webshot(
            url    = paste0("file://", normalizePath(schematic_html)),
            file   = schematic_pdf,
            vwidth = 1100,
            vheight = 800
          )
          cat("Schematic rendered to", schematic_pdf, "\n")
        }
      }, error = function(e) {
        message("Schematic PDF render failed: ", conditionMessage(e))
      })
    } else {
      message("Schematic HTML not found at: ", schematic_html)
    }
    
  }
  
  
}  # end REPORTING

cat("\nReporting script complete.\n")