########### Typhoid TCV SVEIRD Cohort Model (South Asia) ##########
# From birth, 30-year horizon, healthcare-sector perspective
# Costs in 2025 USD, discounted at 3% annually
# Health outcome: DALYs discounted at 3% annually
#
####### YLL, YLD, and DALY Derivations #######
# YLD is incidence-based. Incident symptomatic episodes arise as E→I and E→IMR
# outflows.
# For each symptomatic episode, YLD = DW × episode duration in years.
# YLL = typhoid deaths in cycle × remaining life expectancy at age.
# Remaining life expectancy is derived from the embedded South Asia life-table
# qx vector.
# DALY per cycle = YLD + YLL. Total DALYs = discounted sum over all cycles.
#
####### Model Structure Summary #######
# Cohort state-transition model: S, V, E, I, IMR, R, D.
# S → E via exogenous symptomatic hazard; V → E at reduced hazard
# (vaccine efficacy).
# V → S via exponential waning; E → I (drug-susceptible) or E → IMR (AMR)
# within cycle.
# I/IMR → D (case fatality) or R (recovery) within cycle.
# R → S via natural immunity waning.
# Background mortality applied as competing risk to all living states via 
# age-specific qx.
# Epidemiologic rates from GBD 2019 South Asia tables.
# CFR calculated using total typhoid data, so applied equally across I and
# IMR states, insufficient data exists to stratify
#
####### Extended Study Options #######
# Modelling AMR across AMR subtypes, would need data for cellular marking to
# identify different types of AMR and use ordinary differential equation to
# model change over time
# Further costing data including budgetary constraints required
# Societal impact including effects of improving sanitation in slums
# Modelling could include costs of a slum based campaign
# Note difficulty in monitoring and data collection in slum setting
#
####### Interpretation Summary #######
#
# BASE CASE: TCV is cost-effective for the South Asian birth cohort at
# the population-weighted pooled WTP of $2,409/DALY (1x GDP per capita).
# The intervention averts DALYs primarily through prevention of
# typhoid mortality, with a positive incremental cost driven by the
# upfront vaccination programme expenditure.
#
# TORNADO DIAGRAM: Top 5 modifiers shown, but even at max CI, all remain 
# cost-effective.
#
# TWO-WAY SENSITIVITY: TCV remains cost-effective across most plausible
# combinations of efficacy and incidence; higher AMR prevalence strengthens
# the economic case for vaccination.
#
# CE PLANE: The majority of PSA iterations fall in the green zone,
# confirming robust cost-effectiveness at the average WTP threshold; the
# proportion in the red zone quantifies residual decision uncertainty
# for lower-income countries.
#
# CEAC: The probability of cost-effectiveness exceeds 50% at all country
# thresholds above second lowest WTP in region, with near-certainty for 
# higher-income South Asian nations.
#
# EVPI/EVPPI: Population EVPI identifies the maximum research investment
# justified to resolve remaining uncertainty; EVPPI directs that investment
# toward the most influential parameters.
#
# ELC/CERAC: TCV has lower expected monetary loss than no vaccination
# across most WTP values, showing it as the preferred strategy for
# both risk-neutral and risk-averse decision-makers.
#
# SCENARIO ANALYSIS: The base case conclusion is robust to shorter time
# horizons, rising or declining incidence, high AMR, and reduced coverage;
# only extreme parameter combinations reverse cost-effectiveness.
#
# BIA: TCV requires net additional investment over 5 years because the
# upfront vaccination cost exceeds short-run treatment savings from
# averted infections. This is expected for a prevention program with
# low baseline infection probability; however, the CEA confirms the investment
# represents good value per DALY averted thus this is a sound large scale
# investment.

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(janitor)
  library(glue)
  library(gt)
  library(cli)
  library(stringr)
  library(triangle)
  library(tibble)
  library(scales)
  library(patchwork)
})

# Graph Theme ----

graph_font <- "sans"

graph_style <- function(base_size = 11) {
  theme_classic(base_size = base_size, base_family = graph_font) %+replace%
    theme(
      plot.background    = element_rect(fill = "white", colour = NA),
      panel.background   = element_rect(fill = "white", colour = NA),
      panel.border       = element_rect(fill = NA, colour = "#4F81BD", linewidth = 0.4),
      panel.grid.major.y = element_line(colour = "#E8EDF4", linewidth = 0.25),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.line          = element_blank(),
      axis.ticks         = element_line(colour = "#4F81BD", linewidth = 0.3),
      axis.text          = element_text(colour = "#1F3A5F", size = rel(0.85)),
      axis.title         = element_text(colour = "#1F3A5F", size = rel(0.95)),
      plot.title         = element_text(face = "bold", size = rel(1.15), hjust = 0,
                                        colour = "#1F3A5F", margin = margin(b = 4)),
      plot.subtitle      = element_text(face = "plain", size = rel(0.85), hjust = 0,
                                        colour = "#4F81BD", margin = margin(b = 8)),
      plot.caption       = element_text(face = "italic", size = rel(0.7), hjust = 1,
                                        colour = "#7A8FAD"),
      legend.background  = element_rect(fill = "white", colour = NA),
      legend.key         = element_rect(fill = "white", colour = NA),
      legend.title       = element_text(face = "bold", size = rel(0.85), colour = "#1F3A5F"),
      legend.text        = element_text(size = rel(0.8), colour = "#1F3A5F"),
      legend.position    = "bottom",
      strip.background   = element_rect(fill = "#F2F6FB", colour = "#4F81BD", linewidth = 0.3),
      strip.text         = element_text(face = "bold", size = rel(0.9), colour = "#1F3A5F"),
      plot.margin        = margin(10, 12, 8, 8)
    )
}

# Output Table Setup ----
output_dir <- file.path(getwd(), "outputs")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

table_style <- function(gt_obj) {
  gt_obj %>%
    tab_options(
      table.background.color      = "white",
      heading.background.color    = "#F2F6FB",
      column_labels.background.color = "#F2F6FB",
      row_group.background.color  = "#F2F6FB",
      table.border.top.color      = "#4F81BD",
      table.border.bottom.color   = "#4F81BD",
      table.border.top.width      = px(2),
      table.border.bottom.width   = px(2),
      data_row.padding            = px(4),
      table.font.size             = px(11),
      heading.title.font.size     = px(14),
      heading.subtitle.font.size  = px(11)
    ) %>%
    cols_align(align = "left", columns = everything()) %>%
    tab_style(
      style     = cell_text(weight = "bold", color = "#1F3A5F"),
      locations = cells_column_labels(everything())
    ) %>%
    tab_style(
      style     = cell_text(weight = "bold", color = "#1F3A5F"),
      locations = cells_row_groups()
    )
}

# File Paths and Master Input Read ----
inputs_file_path      <- "SVEIRD_Typhoid_Inputs_v3.0.xlsx"
derivations_file_path <- "SVEIRD_Typhoid_Derivations_and_SourceData_v2.0.xlsx"

stopifnot(file.exists(inputs_file_path))
stopifnot(file.exists(derivations_file_path))

# Single read of inputs — reused everywhere
all_inputs_tbl <-
  read_excel(inputs_file_path, sheet = "All_inputs") %>%
  clean_names() %>%
  as_tibble()

# Scalar Parameter Table ----
scalar_params_tbl <-
  all_inputs_tbl %>%
  filter(!is.na(base), !is.na(symbol)) %>%
  transmute(
    parameter      = as.character(parameter),
    symbol         = as.character(symbol),
    base           = as.numeric(base),
    lower          = as.numeric(lower),
    upper          = as.numeric(upper),
    distribution   = as.character(distribution),
    units          = as.character(units),
    how_enters_model = as.character(how_it_enters_model),
    reference      = as.character(full_reference_harvard),
    notes          = as.character(notes)
  ) %>%
  arrange(symbol)

# Standardise distribution labels for PSA
standardise_distribution <- function(x) {
  x_clean <- x %>%
    str_trim() %>%
    str_to_lower() %>%
    str_replace_all("\\s+", "_") %>%
    str_replace_all("-", "_")
  
  case_when(
    x_clean %in% c("fixed", "deterministic")  ~ "fixed",
    x_clean %in% c("beta")                    ~ "beta",
    x_clean %in% c("gamma")                   ~ "gamma",
    x_clean %in% c("lognormal", "log_normal") ~ "lognormal",
    x_clean %in% c("normal", "gaussian")      ~ "normal",
    x_clean %in% c("uniform")                 ~ "uniform",
    x_clean %in% c("triangular", "triangle")  ~ "triangular",
    x_clean %in% c("derived")                 ~ "derived",
    TRUE ~ x_clean
  )
}

scalar_params_tbl <-
  scalar_params_tbl %>%
  mutate(distribution = standardise_distribution(distribution))

stopifnot(all(!is.na(scalar_params_tbl$symbol)))
stopifnot(all(!duplicated(scalar_params_tbl$symbol)))

# PSA Specification Table ----
psa_spec_tbl <-
  scalar_params_tbl %>%
  transmute(
    symbol, parameter, distribution, base, lower, upper,
    psa_eligible      = distribution %in% c("beta","gamma","lognormal","normal","uniform","triangular"),
    fixed_or_derived  = distribution %in% c("fixed","derived"),
    requires_bounds   = distribution %in% c("beta","uniform","triangular"),
    notes, reference
  ) %>%
  arrange(symbol)

# Validate PSA bounds
psa_missing_bounds <- psa_spec_tbl %>%
  filter(psa_eligible, (is.na(lower) | is.na(upper)))

if (nrow(psa_missing_bounds) > 0) {
  cli_abort(c(
    "PSA-eligible parameters with missing bounds:",
    "x" = paste(psa_missing_bounds$symbol, collapse = ", ")
  ))
}

psa_invalid_order <- psa_spec_tbl %>%
  filter(psa_eligible, !is.na(lower), !is.na(upper), lower > upper)

if (nrow(psa_invalid_order) > 0) {
  cli_abort(c(
    "Parameters with Lower > Upper:",
    "x" = paste(psa_invalid_order$symbol, collapse = ", ")
  ))
}

# Life Table Inputs ----
qx_tbl <- read_excel(derivations_file_path, sheet = "WPP_qx_derived_0_30") %>%
  clean_names() %>%
  transmute(age = as.integer(age), qx = as.numeric(qx)) %>%
  arrange(age)

ex_tbl <- read_excel(derivations_file_path, sheet = "WPP_ex_derived_0_30") %>%
  clean_names() %>%
  transmute(age = as.integer(age), ex = as.numeric(ex)) %>%
  arrange(age)

stopifnot(all(qx_tbl$age == 0:30))
stopifnot(all(ex_tbl$age == 0:30))

qx_by_age <- qx_tbl$qx
ex_by_age <- ex_tbl$ex

get_qx <- function(age) {
  idx <- age + 1L
  if (idx < 1 || idx > length(qx_by_age)) stop("Age outside qx vector.")
  qx_by_age[idx]
}

get_ex <- function(age) {
  idx <- age + 1L
  if (idx < 1 || idx > length(ex_by_age)) stop("Age outside ex vector.")
  ex_by_age[idx]
}

# Cross-check: derive ex from qx and compare to loaded values
build_ex_from_qx <- function(qx_vec) {
  n  <- length(qx_vec) - 1L
  lx <- dx <- Lx <- Tx <- ex_out <- numeric(n + 1)
  lx[1] <- 1
  for (a in 0:n) {
    dx[a + 1] <- lx[a + 1] * qx_vec[a + 1]
    if (a < n) lx[a + 2] <- lx[a + 1] - dx[a + 1]
    Lx[a + 1] <- lx[a + 1] - 0.5 * dx[a + 1]
  }
  Tx[n + 1] <- Lx[n + 1]
  if (n >= 1) for (a in (n - 1):0) Tx[a + 1] <- Lx[a + 1] + Tx[a + 2]
  for (a in 0:n) ex_out[a + 1] <- if (lx[a + 1] > 0) Tx[a + 1] / lx[a + 1] else 0
  ex_out
}

ex_check <- build_ex_from_qx(qx_by_age)
stopifnot(max(abs(ex_check - ex_by_age), na.rm = TRUE) < 1e-6)

# 1. TIME SETTINGS ----
cycle_length_years <- 1
time_horizon_years <- 30
n_cycles           <- time_horizon_years / cycle_length_years
stopifnot(n_cycles == as.integer(n_cycles))

discount_rate <- 0.03
disc <- function(t) 1 / ((1 + discount_rate)^t)

# 2. PARAMETER EXTRACTION HELPERS ----
get_scalar_base <- function(sym) {
  out <- scalar_params_tbl %>% filter(symbol == sym) %>% pull(base)
  if (length(out) != 1 || is.na(out)) stop(glue("Missing or non-unique base for: {sym}"))
  out
}

get_psa_spec <- function(sym) {
  out <- psa_spec_tbl %>% filter(symbol == sym)
  if (nrow(out) != 1) stop(glue("Missing or non-unique PSA spec for: {sym}"))
  out
}

# 3. EPIDEMIOLOGIC INPUTS ----
inc_per100k_2019     <- get_scalar_base("inc_per100k")
deaths_per100k_2019  <- get_scalar_base("deaths_per100k")
avg_age_death_gbd    <- get_scalar_base("age_death")
le_birth_years       <- ex_by_age[1]

# Case fatality fraction: δ = deaths rate / incidence rate
p_delta <- deaths_per100k_2019 / inc_per100k_2019

# 4. HEALTH USE INPUTS ----
p_phi                  <- get_scalar_base("phi")
p_psi                  <- get_scalar_base("psi")
vax_age                <- 0
n_doses                <- 1
dur_vax_years          <- get_scalar_base("dur_vax_years")
dur_nat_immunity_years <- get_scalar_base("dur_nat_immunity_years")

# AMR confirmation is post-admission; hospitalisation decision is identical at presentation
p_hosp_I   <- get_scalar_base("p_hosp")
p_hosp_IMR <- get_scalar_base("p_hosp")

# AMR share among symptomatic infections
pi_MR <- get_scalar_base("pi_mr")

# 5. COST INPUTS (inflated to 2025 USD) ----
inflator_2016_to_2025 <- get_scalar_base("inflator_2016_to_2025")

c_vaccine_procurement <- get_scalar_base("c_vax_2016")   * inflator_2016_to_2025
c_vaccine_admin       <- get_scalar_base("c_admin_2016") * inflator_2016_to_2025
c_no_hosp             <- get_scalar_base("c_no_hosp_2016") * inflator_2016_to_2025
c_hosp                <- get_scalar_base("c_hosp_2016")    * inflator_2016_to_2025

m_amr_episode <- get_scalar_base("m_amr_episode")
c_hosp_amr    <- c_hosp * m_amr_episode

# 6. DISABILITY WEIGHTS AND DURATIONS ----
DW_mod         <- get_scalar_base("DW_mod")
DW_sev         <- get_scalar_base("DW_sev")
DW_outpatient  <- DW_mod
DW_inpatient   <- DW_sev

dur_days       <- get_scalar_base("dur_days")
dur_care_years <- dur_days / 365

# 7. INCIDENCE PROBABILITIES ----
rate_to_prob <- function(rate, dt = 1) {
  if (rate <= 0) return(0)
  1 - exp(-rate * dt)
}

clamp01 <- function(x) pmax(0, pmin(1, x))

haz_sym <- inc_per100k_2019 / 1e5
p_inf_S <- 1 - exp(-haz_sym * cycle_length_years)
p_inf_V <- 1 - exp(-(1 - p_phi) * haz_sym * cycle_length_years)

# 8. STATES ----
states   <- c("S","V","E","I","IMR","R","D")
n_states <- length(states)

# 9. TRANSITION MATRIX ----

make_P <- function(t) {
  
  qx_bg    <- get_qx(t)
  p_wane_v <- rate_to_prob(1 / dur_vax_years, 1)
  p_wane_r <- rate_to_prob(1 / dur_nat_immunity_years, 1)
  
  P <- matrix(0, nrow = n_states, ncol = n_states,
              dimnames = list(states, states))
  
  # S → E or stay
  
  P["S","E"] <- p_inf_S
  P["S","S"] <- clamp01(1 - p_inf_S)
  
  # V: wane to S, then residual can infect
  P["V","S"] <- p_wane_v
  remV <- clamp01(1 - p_wane_v)
  P["V","E"] <- remV * p_inf_V
  P["V","V"] <- remV * clamp01(1 - p_inf_V)
  
  # E → I or IMR (within-cycle split)
  P["E","I"]   <- 1 - pi_MR
  P["E","IMR"] <- pi_MR
  
  # I → D or R
  P["I","D"] <- p_delta
  P["I","R"] <- 1 - p_delta
  
  # IMR → D or R  (same CFR; see audit note on differential AMR CFR)
  P["IMR","D"] <- p_delta
  P["IMR","R"] <- 1 - p_delta
  
  # R → S (natural immunity waning)
  P["R","S"] <- p_wane_r
  P["R","R"] <- clamp01(1 - p_wane_r)
  
  # D is absorbing
  P["D","D"] <- 1
  
  # Apply background mortality as competing risk
  living <- setdiff(states, "D")
  for (s in living) {
    P[s, ]   <- (1 - qx_bg) * P[s, ]
    P[s,"D"] <- P[s,"D"] + qx_bg
  }
  
  rs <- rowSums(P)
  if (any(abs(rs - 1) > 1e-10)) stop("Row sums ≠ 1 in transition matrix.")
  P
}

# 10. COSTS AND DALYS PER CYCLE ----
cycle_outcomes <- function(t, x, P) {
  
  inc_I   <- x["E"] * P["E","I"]
  inc_IMR <- x["E"] * P["E","IMR"]
  
  hosp_I   <- inc_I   * p_hosp_I
  out_I    <- inc_I   * (1 - p_hosp_I)
  hosp_IMR <- inc_IMR * p_hosp_IMR
  out_IMR  <- inc_IMR * (1 - p_hosp_IMR)
  
  c_tx <- out_I * c_no_hosp + hosp_I * c_hosp +
    out_IMR * c_no_hosp + hosp_IMR * c_hosp_amr
  
  yld <- (out_I + out_IMR) * DW_outpatient * dur_care_years +
    (hosp_I + hosp_IMR) * DW_inpatient * dur_care_years
  
  # Typhoid deaths
  deaths_ty <- x["I"] * P["I","D"] + x["IMR"] * P["IMR","D"]
  
  yll <- deaths_ty * get_ex(as.integer(t))
  
  list(cost = c_tx, yld = yld, yll = yll, daly = yld + yll)
}

# 11. RUN COHORT MODEL ----
run_model <- function(vaccination = TRUE, cohort = 1) {
  
  trace <- matrix(0, nrow = n_cycles + 1, ncol = n_states,
                  dimnames = list(0:n_cycles, states))
  
  if (vaccination) {
    trace[1, "V"] <- cohort * p_psi
    trace[1, "S"] <- cohort * (1 - p_psi)
  } else {
    trace[1, "S"] <- cohort
  }
  
  total_cost <- total_yld <- total_yll <- total_daly <- 0
  
  if (vaccination) {
    vax_unit_cost <- n_doses * (c_vaccine_procurement + c_vaccine_admin)
    total_cost    <- total_cost + (cohort * p_psi * vax_unit_cost) * disc(0)
  }
  
  for (t in 0:(n_cycles - 1)) {
    x   <- trace[t + 1, ]
    P   <- make_P(t)
    out <- cycle_outcomes(t, x, P)
    
    total_cost <- total_cost + out$cost * disc(t)
    total_yld  <- total_yld  + out$yld  * disc(t)
    total_yll  <- total_yll  + out$yll  * disc(t)
    total_daly <- total_daly + out$daly * disc(t)
    
    x_next <- as.numeric(x %*% P)
    names(x_next) <- states
    trace[t + 2, ] <- x_next
  }
  
  list(trace = as.data.frame(trace),
       cost  = total_cost,
       yld   = total_yld,
       yll   = total_yll,
       daly  = total_daly)
}

# 12. RESULTS AND ICER ----
res_vax <- run_model(vaccination = TRUE,  cohort = 1)
res_no  <- run_model(vaccination = FALSE, cohort = 1)

inc_cost     <- res_vax$cost - res_no$cost
inc_daly     <- res_vax$daly - res_no$daly
daly_averted <- -inc_daly

cat("\n══════════════════════════════════════════\n")
cat("BASE CASE RESULTS\n")
cat("══════════════════════════════════════════\n")
cat("Incidence rate per 100,000:   ", signif(inc_per100k_2019, 8), "\n")
cat("Deaths rate per 100,000:      ", signif(deaths_per100k_2019, 8), "\n")
cat("Case fatality fraction (\u03b4):   ", signif(p_delta, 8), "\n")
cat("Life expectancy at birth:     ", signif(le_birth_years, 10), "\n")
cat("Total cost (vaccination):     ", signif(res_vax$cost, 6), "\n")
cat("Total cost (no vaccination):  ", signif(res_no$cost, 6), "\n")
cat("Total DALYs (vaccination):    ", signif(res_vax$daly, 6), "\n")
cat("Total DALYs (no vaccination): ", signif(res_no$daly, 6), "\n")
cat("Incremental cost:             ", signif(inc_cost, 6), "\n")
cat("DALYs averted:                ", signif(daly_averted, 6), "\n")

if (abs(daly_averted) < 1e-12) {
  cat("ICER undefined (DALYs averted \u2248 0).\n")
  icer <- NA_real_
} else {
  icer <- inc_cost / daly_averted
  cat("ICER (USD per DALY averted):  ", signif(icer, 6), "\n")
}

# 12B. STATE OCCUPANCY PLOT ----
#
# Two-panel cohort trace showing the proportion of the birth cohort
# in each SVEIRD compartment over the 30-year horizon.
# Panel A: dominant states (S, V, R, D) on a 0-1 scale.
# Panel B: transient clinical states (E, I, IMR) on their own scale.
# Both arms (vaccination vs no vaccination) shown side by side.

trace_to_long <- function(trace_df, arm_label) {
  trace_df %>%
    mutate(year = as.numeric(rownames(.))) %>%
    pivot_longer(cols = all_of(states), names_to = "state", values_to = "proportion") %>%
    mutate(arm = arm_label)
}

trace_long <- bind_rows(
  trace_to_long(res_vax$trace, "TCV vaccination"),
  trace_to_long(res_no$trace,  "No vaccination")
)

# State display settings
state_colours <- c(
  "S" = "#2196F3", "V" = "#4CAF50", "E" = "#FF9800",
  "I" = "#F44336", "IMR" = "#9C27B0", "R" = "#607D8B", "D" = "#212121"
)
state_labels <- c(
  "S" = "Susceptible", "V" = "Vaccinated", "E" = "Exposed",
  "I" = "Infected", "IMR" = "Infected (AMR)", "R" = "Recovered", "D" = "Dead"
)

# Panel A: dominant states
dominant_states <- c("S", "V", "R", "D")
trace_dominant <- trace_long %>%
  filter(state %in% dominant_states) %>%
  mutate(state = factor(state, levels = dominant_states))

p_trace_A <- ggplot(trace_dominant, aes(x = year, y = proportion,
                                        colour = state, linetype = state)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ arm) +
  scale_colour_manual(values = state_colours, labels = state_labels) +
  scale_linetype_manual(values = c("S" = "solid", "V" = "longdash",
                                   "R" = "dotted", "D" = "twodash"),
                        labels = state_labels) +
  scale_x_continuous(breaks = seq(0, 30, 5)) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(
    title = "Panel A: Dominant State Occupancy",
    subtitle = "Proportion of cohort in S, V, R, and D over 30-year horizon",
    x = "Year", y = "Proportion of cohort",
    colour = "State", linetype = "State"
  ) +
  graph_style() +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 11))

# Panel B: transient clinical states
clinical_states <- c("E", "I", "IMR")
trace_clinical <- trace_long %>%
  filter(state %in% clinical_states) %>%
  mutate(state = factor(state, levels = clinical_states))

p_trace_B <- ggplot(trace_clinical, aes(x = year, y = proportion,
                                        colour = state, linetype = state)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ arm) +
  scale_colour_manual(values = state_colours, labels = state_labels) +
  scale_linetype_manual(values = c("E" = "solid", "I" = "longdash", "IMR" = "dotted"),
                        labels = state_labels) +
  scale_x_continuous(breaks = seq(0, 30, 5)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  labs(
    title = "Panel B: Transient Clinical State Occupancy",
    subtitle = "Proportion of cohort in E, I, and IMR",
    x = "Year", y = "Proportion of cohort",
    colour = "State", linetype = "State"
  ) +
  graph_style() +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 11))

# Combine panels
p_trace_combined <- p_trace_A / p_trace_B +
  plot_annotation(
    title = "Cohort State Occupancy: SVEIRD Typhoid Vaccine Model",
    subtitle = "Per-person cohort trace over 30-year horizon | Vaccination vs no vaccination",
    theme = theme(plot.title = element_text(face = "bold", size = 14,
                                            colour = "#1F3A5F", family = graph_font),
                  plot.subtitle = element_text(size = 11, colour = "#4F81BD",
                                               family = graph_font),
                  plot.background = element_rect(fill = "white", colour = NA))
  )

print(p_trace_combined)
ggsave(file.path(output_dir, "Fig_State_Occupancy.png"), p_trace_combined,
       width = 12, height = 10, dpi = 300, bg = "white")

# 13. REFERENCE KEY ----
make_abbrev_from_harvard <- function(x) {
  x <- as.character(x)
  if (is.na(x) || str_trim(x) == "") return(NA_character_)
  first_author <- str_extract(x, "^[^,]+") %>% str_trim()
  yr <- str_extract(x, "\\(\\d{4}\\)") %>% str_remove_all("[\\(\\)]")
  if (is.na(yr)) yr <- str_extract(x, "\\b\\d{4}\\b")
  if (is.na(first_author) && is.na(yr)) return(NA_character_)
  if (is.na(yr)) return(first_author)
  paste0(first_author, " et al. ", yr)
}

ref_key <- all_inputs_tbl %>%
  transmute(Full_Harvard = full_reference_harvard) %>%
  filter(!is.na(Full_Harvard), str_trim(Full_Harvard) != "") %>%
  distinct() %>%
  mutate(
    Ref_ID = paste0("REF", str_pad(row_number(), width = 3, pad = "0")),
    Abbrev = vapply(Full_Harvard, make_abbrev_from_harvard, character(1)),
    DOI    = str_extract(str_to_lower(Full_Harvard), "10\\.\\d{4,9}/[-._;()/:a-z0-9]+")
  ) %>%
  select(Ref_ID, Abbrev, Full_Harvard, DOI)

ref_abbrev_from_full <- function(full_ref) {
  out <- ref_key %>% filter(Full_Harvard == full_ref) %>% pull(Abbrev)
  if (length(out) == 0) return(NA_character_)
  out[1]
}

# Ref_ID lookup (for cross-referencing in tables)
ref_id_from_full <- function(full_ref) {
  if (is.na(full_ref) || str_trim(full_ref) == "") return(NA_character_)
  out <- ref_key %>% filter(Full_Harvard == full_ref) %>% pull(Ref_ID)
  if (length(out) == 0) return(NA_character_)
  out[1]
}

# Reference Key table
table_refs <- ref_key %>%
  transmute(`Ref ID` = Ref_ID, `Full Reference` = Full_Harvard) %>%
  gt() %>%
  tab_header(title = md("**Reference Key**")) %>%
  tab_options(table.layout = "auto", table.width = pct(70), data_row.padding = px(2)) %>%
  table_style()

print(table_refs)
gtsave(table_refs, file.path(output_dir, "Reference_Key.docx"))
gtsave(table_refs, file.path(output_dir, "Reference_Key.html"))
gtsave(table_refs, file.path(output_dir, "Reference_Key.png"))

# 14. UNCERTAINTY INPUTS ----
uncertainty_tbl <- all_inputs_tbl %>%
  transmute(
    parameter    = parameter,
    symbol       = str_to_lower(str_trim(as.character(symbol))),
    base         = as.numeric(base),
    lower        = as.numeric(lower),
    upper        = as.numeric(upper),
    units        = units,
    distribution = standardise_distribution(distribution),
    full_reference = full_reference_harvard
  ) %>%
  filter(!is.na(symbol), symbol != "") %>%
  mutate(
    source_abbrev = vapply(full_reference, ref_abbrev_from_full, character(1)),
    ref_id        = vapply(full_reference, ref_id_from_full, character(1))
  )

# Display Helpers ----
disp_symbol <- function(sym) {
  lookup <- c(
    "inc_per100k"           = "\u03bb\u2099",
    "deaths_per100k"        = "\u03bc\u209c\u2099",
    "delta"                 = "\u03b4",
    "phi"                   = "\u03d5",
    "psi"                   = "\u03c8",
    "pi_mr"                 = "\u03c0\u2098\u1d63",
    "dw_mod"                = "DW\u2098\u2092\u2091",
    "dw_sev"                = "DW\u209b\u2091\u1d65",
    "dur_days"              = "d\u209c\u2093",
    "dur_vax_years"         = "\u03c4\u1d65",
    "dur_nat_immunity_years"= "\u03c4\u2099",
    "p_hosp"                = "p\u2095\u2092\u209b\u209a",
    "c_vax_2016"            = "c\u1d65\u2090\u2093",
    "c_admin_2016"          = "c\u2090\u2091\u2098",
    "c_no_hosp_2016"        = "c\u2092\u209a",
    "c_hosp_2016"           = "c\u2095\u209a",
    "m_amr_episode"         = "m\u2090\u2098\u1d63",
    "inflator_2016_to_2025" = "CPI\u2082\u2080\u2082\u2085/CPI\u2082\u2080\u2081\u2086",
    "age_death"             = "\u0101\u2091",
    "le_rem"                = "e\u2093(\u0101)",
    "discount_rate"         = "r",
    "cycle_length_years"    = "\u0394t",
    "time_horizon_years"    = "T",
    "qx_by_age_0_to_30"    = "q\u2093"
  )
  sym_lower <- str_to_lower(str_trim(sym))
  out <- lookup[sym_lower]
  ifelse(is.na(out), sym, out)
}

# Format distribution label for display
disp_distribution <- function(dist) {
  lookup <- c(
    "beta"       = "Beta",
    "gamma"      = "Gamma",
    "lognormal"  = "Log-normal",
    "normal"     = "Normal",
    "uniform"    = "Uniform",
    "triangular" = "Triangular",
    "fixed"      = "Fixed",
    "derived"    = "Derived"
  )
  out <- lookup[str_to_lower(str_trim(dist))]
  ifelse(is.na(out), dist, out)
}

fmt_val_with_range <- function(x, low = NA, high = NA, digits = 6) {
  if (is.na(x)) return(NA_character_)
  x_fmt <- formatC(x, format = "f", digits = digits)
  if (is.na(low) || is.na(high)) return(x_fmt)
  lo_fmt <- formatC(low, format = "f", digits = digits)
  hi_fmt <- formatC(high, format = "f", digits = digits)
  paste0(x_fmt, " [", lo_fmt, ", ", hi_fmt, "]")
}

# 15. TABLE 1: MODEL INPUT PARAMETERS ----
# δ (case fatality fraction) is derived from deaths_per100k / inc_per100k.
table1_inputs_tbl <- uncertainty_tbl %>%
  mutate(
    `Base [low, high]` = mapply(fmt_val_with_range, base, lower, upper,
                                MoreArgs = list(digits = 4)) %>% as.character(),
    Symbol_disp  = vapply(symbol, disp_symbol, character(1)),
    Dist_disp    = vapply(distribution, disp_distribution, character(1)),
    Source       = ref_id
  ) %>%
  transmute(
    Category     = "Model Inputs",
    Parameter    = parameter,
    Symbol       = Symbol_disp,
    `Base [low, high]` = `Base [low, high]`,
    Units        = units,
    Distribution = Dist_disp,
    Source       = Source
  )

table_inputs <- table1_inputs_tbl %>%
  gt(groupname_col = "Category") %>%
  tab_header(
    title    = md("**Table 1. Model Input Parameters**"),
    subtitle = md("*SVEIRD typhoid conjugate vaccine cost\u2013effectiveness model, South Asia*")
  ) %>%
  cols_label(
    Parameter          = "Parameter",
    Symbol             = "Symbol",
    `Base [low, high]` = "Base case [low, high]",
    Units              = "Units",
    Distribution       = "Distribution",
    Source             = "Source"
  ) %>%
  tab_options(table.layout = "auto", table.width = pct(85), data_row.padding = px(2)) %>%
  table_style() %>%
  tab_source_note(source_note = md("*Source column shows Reference Key identifiers (e.g. REF001). See Reference Key table for full citations.*"))

print(table_inputs)
gtsave(table_inputs, file.path(output_dir, "Table1_Model_Inputs.docx"))
gtsave(table_inputs, file.path(output_dir, "Table1_Model_Inputs.html"))
gtsave(table_inputs, file.path(output_dir, "Table1_Model_Inputs.png"))

# 16. TABLE 2: TRANSITION PROBABILITIES ----

#
# Notes:
# Complement transitions (e.g. S→S = 1 − p(S→E)) are included for completeness.
# Background mortality (qx) is applied as a competing risk after disease transitions.
# CIs are propagated from input parameter bounds through derivation formulae.
# For derived quantities (e.g. δ = deaths/inc), bounds are computed using
# combinations of input bounds that yield the widest plausible interval.

# Helper: look up Ref_ID
get_ref_id <- function(sym) {
  sym <- str_to_lower(str_trim(sym))
  hit <- uncertainty_tbl %>% filter(symbol == sym)
  if (nrow(hit) != 1) return(NA_character_)
  hit$ref_id[[1]]
}

# Helper: retrieve lower/upper for a symbol from uncertainty_tbl
get_bounds <- function(sym) {
  sym <- str_to_lower(str_trim(sym))
  hit <- uncertainty_tbl %>% filter(symbol == sym)
  if (nrow(hit) != 1) return(list(lo = NA_real_, hi = NA_real_))
  list(lo = hit$lower[[1]], hi = hit$upper[[1]])
}

# ── Retrieve input bounds ──
bnd_inc     <- get_bounds("inc_per100k")       # λ
bnd_deaths  <- get_bounds("deaths_per100k")    # μ
bnd_phi     <- get_bounds("phi")               # φ
bnd_pi_mr   <- get_bounds("pi_mr")             # π_mr
bnd_delta   <- get_bounds("delta")             # δ (derived, has own bounds)
bnd_dur_vax <- get_bounds("dur_vax_years")     # τ_v
bnd_dur_nat <- get_bounds("dur_nat_immunity_years") # τ_n

# ── S→E: p = 1 − exp(−λ/10⁵) ──
p_inf_S_base <- 1 - exp(-inc_per100k_2019 / 1e5)
p_inf_S_lo   <- 1 - exp(-bnd_inc$lo / 1e5)
p_inf_S_hi   <- 1 - exp(-bnd_inc$hi / 1e5)

# ── S→S: complement ──
p_SS_base <- 1 - p_inf_S_base
p_SS_lo   <- 1 - p_inf_S_hi   # inverted
p_SS_hi   <- 1 - p_inf_S_lo

# ── V→S: p_wane_V = 1 − exp(−1/τ_v). Higher τ_v → lower waning ──
p_wane_V_base <- rate_to_prob(1 / dur_vax_years, 1)
p_wane_V_lo   <- rate_to_prob(1 / bnd_dur_vax$hi, 1)  # longer duration → less waning
p_wane_V_hi   <- rate_to_prob(1 / bnd_dur_vax$lo, 1)   # shorter duration → more waning

# ── V→E: (1 − p_wane_V) · [1 − exp(−(1−φ) · h)] ──
# Worst case (highest V→E): lowest φ, highest λ, lowest waning (highest τ_v)
# Best case (lowest V→E): highest φ, lowest λ, highest waning (lowest τ_v)
compute_VE <- function(inc, phi_val, dur_v) {
  wane <- rate_to_prob(1 / dur_v, 1)
  h <- inc / 1e5
  p_inf <- 1 - exp(-(1 - phi_val) * h)
  (1 - wane) * p_inf
}
p_VE_base <- compute_VE(inc_per100k_2019, p_phi, dur_vax_years)
p_VE_lo   <- compute_VE(bnd_inc$lo, bnd_phi$hi, bnd_dur_vax$lo)  # best case: low inc, high VE, short dur
p_VE_hi   <- compute_VE(bnd_inc$hi, bnd_phi$lo, bnd_dur_vax$hi)  # worst case: high inc, low VE, long dur

# ── V→V: residual = 1 − p_wane_V − p_V→E ──
p_VV_base <- 1 - p_wane_V_base - p_VE_base
p_VV_lo   <- 1 - p_wane_V_hi - p_VE_hi
p_VV_hi   <- 1 - p_wane_V_lo - p_VE_lo

# ── E→I: 1 − π_mr ──
p_EI_base <- 1 - pi_MR
p_EI_lo   <- 1 - bnd_pi_mr$hi
p_EI_hi   <- 1 - bnd_pi_mr$lo

# ── E→IMR: π_mr ──
p_EIMR_base <- pi_MR
p_EIMR_lo   <- bnd_pi_mr$lo
p_EIMR_hi   <- bnd_pi_mr$hi

# ── I→D and IMR→D: δ ──
p_ID_base  <- p_delta
p_ID_lo    <- bnd_delta$lo
p_ID_hi    <- bnd_delta$hi

# ── I→R and IMR→R: 1 − δ ──
p_IR_base <- 1 - p_delta
p_IR_lo   <- 1 - bnd_delta$hi
p_IR_hi   <- 1 - bnd_delta$lo

# ── R→S: p_wane_R = 1 − exp(−1/τ_n). τ_n is fixed so CI = base ──
p_wane_R_base <- rate_to_prob(1 / dur_nat_immunity_years, 1)
p_wane_R_lo   <- rate_to_prob(1 / bnd_dur_nat$hi, 1)
p_wane_R_hi   <- rate_to_prob(1 / bnd_dur_nat$lo, 1)

# ── R→R: complement ──
p_RR_base <- 1 - p_wane_R_base
p_RR_lo   <- 1 - p_wane_R_hi
p_RR_hi   <- 1 - p_wane_R_lo

# ── D→D: 1, no CI ──

# ── Build table ──
transition_prob_table <- tibble(
  Definition = c(
    "Symptomatic infection (susceptible)",
    "Remain susceptible",
    "Vaccine waning to susceptible",
    "Breakthrough infection (vaccinated)",
    "Remain vaccinated",
    "Progress to drug-sensitive infection",
    "Progress to AMR infection",
    "Typhoid death (drug-sensitive)",
    "Recovery (drug-sensitive)",
    "Typhoid death (AMR)",
    "Recovery (AMR)",
    "Natural immunity waning",
    "Remain recovered",
    "Absorbing death state"
  ),
  Symbol = c(
    "p\u1d62\u2099\u2082(S)",
    "1 \u2212 p\u1d62\u2099\u2082(S)",
    "p\u02b7\u1d65",
    "(1 \u2212 p\u02b7\u1d65) \u00b7 p\u1d62\u2099\u2082(V)",
    "(1 \u2212 p\u02b7\u1d65)(1 \u2212 p\u1d62\u2099\u2082(V))",
    "1 \u2212 \u03c0\u2098\u1d63",
    "\u03c0\u2098\u1d63",
    "\u03b4",
    "1 \u2212 \u03b4",
    "\u03b4",
    "1 \u2212 \u03b4",
    "p\u02b7\u1d63",
    "1 \u2212 p\u02b7\u1d63",
    "1"
  ),
  Transition = c(
    "S \u2192 E",
    "S \u2192 S",
    "V \u2192 S",
    "V \u2192 E",
    "V \u2192 V",
    "E \u2192 I",
    "E \u2192 I\u2098\u1d63",
    "I \u2192 D",
    "I \u2192 R",
    "I\u2098\u1d63 \u2192 D",
    "I\u2098\u1d63 \u2192 R",
    "R \u2192 S",
    "R \u2192 R",
    "D \u2192 D"
  ),
  Value_raw = c(
    p_inf_S_base, p_SS_base,
    p_wane_V_base, p_VE_base, p_VV_base,
    p_EI_base, p_EIMR_base,
    p_ID_base, p_IR_base,
    p_ID_base, p_IR_base,
    p_wane_R_base, p_RR_base,
    1
  ),
  Low_raw = c(
    p_inf_S_lo, p_SS_lo,
    p_wane_V_lo, p_VE_lo, p_VV_lo,
    p_EI_lo, p_EIMR_lo,
    p_ID_lo, p_IR_lo,
    p_ID_lo, p_IR_lo,
    p_wane_R_lo, p_RR_lo,
    NA_real_
  ),
  High_raw = c(
    p_inf_S_hi, p_SS_hi,
    p_wane_V_hi, p_VE_hi, p_VV_hi,
    p_EI_hi, p_EIMR_hi,
    p_ID_hi, p_IR_hi,
    p_ID_hi, p_IR_hi,
    p_wane_R_hi, p_RR_hi,
    NA_real_
  ),
  Derivation = c(
    "1 \u2212 exp(\u2212h \u00b7 \u0394t) where h = \u03bb/10\u2075",
    "Complement of p(S\u2192E)",
    "1 \u2212 exp(\u22121/\u03c4\u1d65)",
    "(1 \u2212 p\u02b7\u1d65) \u00b7 [1 \u2212 exp(\u2212(1\u2212\u03d5) \u00b7 h \u00b7 \u0394t)]",
    "Residual of V row",
    "Drug-sensitive share = 1 \u2212 \u03c0\u2098\u1d63",
    "AMR share (input)",
    paste0("\u03b4 = deaths per 100k / inc per 100k = ",
           formatC(deaths_per100k_2019, format = "f", digits = 4),
           " / ", formatC(inc_per100k_2019, format = "f", digits = 4)),
    "1 \u2212 \u03b4 (complement of CFR)",
    paste0("\u03b4 applied equally; same derivation as I\u2192D"),
    "1 \u2212 \u03b4 (complement of CFR)",
    "1 \u2212 exp(\u22121/\u03c4\u2099)",
    "Complement of p(R\u2192S)",
    "Absorbing state"
  ),
  Reference = c(
    get_ref_id("inc_per100k"),
    "\u2014",
    get_ref_id("dur_vax_years"),
    get_ref_id("phi"),
    "\u2014",
    get_ref_id("pi_mr"),
    get_ref_id("pi_mr"),
    paste0(get_ref_id("inc_per100k"), ", ", get_ref_id("deaths_per100k")),
    "\u2014",
    paste0(get_ref_id("inc_per100k"), ", ", get_ref_id("deaths_per100k")),
    "\u2014",
    get_ref_id("dur_nat_immunity_years"),
    "\u2014",
    "\u2014"
  )
) %>%
  mutate(
    `Value [low, high]` = mapply(fmt_val_with_range, Value_raw, Low_raw, High_raw,
                                 MoreArgs = list(digits = 6)) %>% as.character()
  ) %>%
  select(Definition, Symbol, Transition, `Value [low, high]`, Derivation, Reference)

table_transitions <- transition_prob_table %>%
  gt() %>%
  tab_header(
    title    = md("**Table 2. State-Transition Probabilities**"),
    subtitle = md("*All transitions in the cohort model q\u2093)*")
  ) %>%
  cols_label(
    Definition          = "Definition",
    Symbol              = "Symbol",
    Transition          = "Transition",
    `Value [low, high]` = "Base [low, high]",
    Derivation          = "Derivation",
    Reference           = "Reference"
  ) %>%
  tab_options(table.layout = "auto", table.width = pct(95), data_row.padding = px(2)) %>%
  table_style() %>%
  tab_source_note(
    source_note = md(paste0(
      "*CIs propagated from input parameter bounds through derivation formulae. ",
      "For compound transitions (e.g. V\u2192E), bounds use combinations of inputs yielding the widest plausible interval. ",
      "After disease transitions, age-specific background mortality q\u2093(age) is applied ",
      "as a competing risk: P[s,\u00b7] \u2190 (1 \u2212 q\u2093) \u00b7 P[s,\u00b7]; ",
      "P[s,D] \u2190 P[s,D] + q\u2093. ",
      "Reference column shows Reference Key identifiers; \u2014 = complement or model convention.*"
    ))
  )

print(table_transitions)
gtsave(table_transitions, file.path(output_dir, "Table2_Transition_Probabilities.docx"))
gtsave(table_transitions, file.path(output_dir, "Table2_Transition_Probabilities.html"))
gtsave(table_transitions, file.path(output_dir, "Table2_Transition_Probabilities.png"))

# 17. TABLE 3: DERIVATIONS AND IDENTITIES ----

deriv_table <- tribble(
  ~Quantity, ~Symbol, ~Definition,
  
  "Discount factor",
  "d(t)",
  "d(t) = (1 + r)\u207b\u1d57",
  
  "Symptomatic hazard (rate to probability input)",
  "h",
  "h = \u03bb / 100\u202f000",
  
  "Episode duration in years",
  "d\u209c\u2093\u02b8",
  "d\u209c\u2093\u02b8 = d\u209c\u2093 / 365",
  
  "Hospitalised inpatient cost (AMR)",
  "c\u2095\u209a\u1d43\u1d39\u1d3f",
  "c\u2095\u209a \u00d7 m\u2090\u2098\u1d63",
  
  "YLD per cycle (incidence-based)",
  "YLD",
  "YLD = \u03a3(outpatient episodes \u00d7 DW\u2098\u2092\u2091 + inpatient episodes \u00d7 DW\u209b\u2091\u1d65) \u00d7 d\u209c\u2093\u02b8",
  
  "YLL per cycle",
  "YLL",
  "YLL = \u03a3 deaths(t) \u00d7 e\u2093(age)",
  
  "DALY per cycle",
  "DALY",
  "DALY = YLD + YLL",
  
  "Total discounted cost",
  "C",
  "C = \u03a3\u209c c(t) \u00d7 d(t)",
  
  "Total discounted DALYs",
  "D",
  "D = \u03a3\u209c DALY(t) \u00d7 d(t)",
  
  "ICER",
  "ICER",
  "ICER = \u0394C / \u0394D (USD per DALY averted)"
)

table_deriv <- deriv_table %>%
  gt() %>%
  tab_header(
    title    = md("**Table 3. Non-Transition Derivations and Identities**"),
    subtitle = md("*Outcome, discounting, and cost derivations*")
  ) %>%
  cols_label(
    Quantity   = "Quantity",
    Symbol     = "Symbol",
    Definition = "Definition"
  ) %>%
  tab_options(table.layout = "auto", table.width = pct(80), data_row.padding = px(3)) %>%
  table_style()

print(table_deriv)
gtsave(table_deriv, file.path(output_dir, "Table3_Derivations.docx"))
gtsave(table_deriv, file.path(output_dir, "Table3_Derivations.html"))
gtsave(table_deriv, file.path(output_dir, "Table3_Derivations.png"))

# 18. TABLE 4: BASE CASE RESULTS ----
icer_table <- tribble(
  ~Arm, ~Total_cost, ~Total_YLD, ~Total_YLL, ~Total_DALY,
  ~Incremental_cost, ~Incremental_DALY, ~ICER,
  
  "TCV vaccination",  res_vax$cost, res_vax$yld, res_vax$yll, res_vax$daly,
  NA, NA, NA,
  
  "No vaccination",   res_no$cost,  res_no$yld,  res_no$yll,  res_no$daly,
  NA, NA, NA,
  
  "Incremental",      NA, NA, NA, NA,
  inc_cost, inc_daly, icer
)

table_icer <- icer_table %>%
  gt() %>%
  tab_header(
    title    = md("**Table 4. Base-Case Cost\u2013Effectiveness Results**"),
    subtitle = md("*Per-person cohort, 30-year horizon, 3% discounting, 2025 USD*")
  ) %>%
  fmt_currency(columns = c(Total_cost, Incremental_cost), currency = "USD", decimals = 2) %>%
  fmt_number(columns = c(Total_YLD, Total_YLL, Total_DALY, Incremental_DALY), decimals = 6) %>%
  fmt_currency(columns = ICER, currency = "USD", decimals = 2) %>%
  cols_label(
    Arm               = "Strategy",
    Total_cost        = "Total cost",
    Total_YLD         = "Total YLD",
    Total_YLL         = "Total YLL",
    Total_DALY        = "Total DALYs",
    Incremental_cost  = "\u0394 Cost",
    Incremental_DALY  = "\u0394 DALYs",
    ICER              = "ICER (USD/DALY averted)"
  ) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(rows = Arm == "Incremental")
  ) %>%
  tab_options(table.layout = "auto", table.width = pct(85), data_row.padding = px(2)) %>%
  table_style()

print(table_icer)
gtsave(table_icer, file.path(output_dir, "Table4_ICER_Results.docx"))
gtsave(table_icer, file.path(output_dir, "Table4_ICER_Results.html"))
gtsave(table_icer, file.path(output_dir, "Table4_ICER_Results.png"))

# 19. ONE-WAY SENSITIVITY ANALYSIS AND TORNADO DIAGRAM ----

# No additional packages required beyond those loaded in the header.
# PSA uses base R distribution functions (rbeta, rgamma, rlnorm, rnorm, runif).
# Parameterised model runner: pulls from scalar_params_tbl.
# Accepts a named list of overrides keyed by symbol.
# All base values are pulled from scalar_params_tbl.
# This function is used by OWSA, PSA, and scenario analysis.

# Build base parameters (symbol -> base value)
.base_params <- setNames(scalar_params_tbl$base, scalar_params_tbl$symbol)

# Accessor: returns override value if present, else base from Excel
.get_p <- function(sym, overrides) {
  if (!is.null(overrides[[sym]])) return(overrides[[sym]])
  .base_params[[sym]]
}

run_model_owsa <- function(overrides = list()) {
  
  # Pull every parameter from Excel base, applying overrides where supplied
  ov_inc      <- .get_p("inc_per100k", overrides)
  ov_deaths   <- .get_p("deaths_per100k", overrides)
  ov_phi      <- .get_p("phi", overrides)
  ov_psi      <- .get_p("psi", overrides)
  ov_dur_vax  <- .get_p("dur_vax_years", overrides)
  ov_dur_nat  <- .get_p("dur_nat_immunity_years", overrides)
  ov_pi_mr    <- .get_p("pi_mr", overrides)
  ov_p_hosp   <- .get_p("p_hosp", overrides)
  ov_DW_mod   <- .get_p("DW_mod", overrides)
  ov_DW_sev   <- .get_p("DW_sev", overrides)
  ov_dur_days <- .get_p("dur_days", overrides)
  ov_c_vax    <- .get_p("c_vax_2016", overrides)
  ov_c_admin  <- .get_p("c_admin_2016", overrides)
  ov_c_no_hp  <- .get_p("c_no_hosp_2016", overrides)
  ov_c_hp     <- .get_p("c_hosp_2016", overrides)
  ov_m_amr    <- .get_p("m_amr_episode", overrides)
  
  # Derived quantities
  loc_delta       <- ov_deaths / ov_inc
  loc_haz         <- ov_inc / 1e5
  loc_p_inf_S     <- 1 - exp(-loc_haz * cycle_length_years)
  loc_p_inf_V     <- 1 - exp(-(1 - ov_phi) * loc_haz * cycle_length_years)
  loc_p_wane_v    <- rate_to_prob(1 / ov_dur_vax, 1)
  loc_p_wane_r    <- rate_to_prob(1 / ov_dur_nat, 1)
  loc_dur_care_yr <- ov_dur_days / 365
  
  loc_c_vax_proc  <- ov_c_vax  * inflator_2016_to_2025
  loc_c_vax_adm   <- ov_c_admin * inflator_2016_to_2025
  loc_c_no_hosp   <- ov_c_no_hp * inflator_2016_to_2025
  loc_c_hosp      <- ov_c_hp   * inflator_2016_to_2025
  loc_c_hosp_amr  <- loc_c_hosp * ov_m_amr
  
  # Local transition matrix builder
  make_P_loc <- function(t) {
    age <- as.integer(t)
    qx_bg <- get_qx(age)
    P <- matrix(0, n_states, n_states, dimnames = list(states, states))
    P["S","E"] <- loc_p_inf_S
    P["S","S"] <- clamp01(1 - loc_p_inf_S)
    P["V","S"] <- loc_p_wane_v
    remV <- clamp01(1 - loc_p_wane_v)
    P["V","E"] <- remV * loc_p_inf_V
    P["V","V"] <- remV * clamp01(1 - loc_p_inf_V)
    P["E","I"]   <- 1 - ov_pi_mr
    P["E","IMR"] <- ov_pi_mr
    P["I","D"] <- loc_delta
    P["I","R"] <- 1 - loc_delta
    P["IMR","D"] <- loc_delta
    P["IMR","R"] <- 1 - loc_delta
    P["R","S"] <- loc_p_wane_r
    P["R","R"] <- clamp01(1 - loc_p_wane_r)
    P["D","D"] <- 1
    living <- setdiff(states, "D")
    for (s in living) {
      P[s, ]   <- (1 - qx_bg) * P[s, ]
      P[s,"D"] <- P[s,"D"] + qx_bg
    }
    P
  }
  
  # Local cycle outcomes
  cycle_out_loc <- function(t, x, P) {
    inc_I   <- x["E"] * P["E","I"]
    inc_IMR <- x["E"] * P["E","IMR"]
    hosp_I   <- inc_I   * ov_p_hosp
    out_I    <- inc_I   * (1 - ov_p_hosp)
    hosp_IMR <- inc_IMR * ov_p_hosp
    out_IMR  <- inc_IMR * (1 - ov_p_hosp)
    c_tx <- out_I * loc_c_no_hosp + hosp_I * loc_c_hosp +
      out_IMR * loc_c_no_hosp + hosp_IMR * loc_c_hosp_amr
    yld <- (out_I + out_IMR) * ov_DW_mod * loc_dur_care_yr +
      (hosp_I + hosp_IMR) * ov_DW_sev * loc_dur_care_yr
    deaths_ty <- x["I"] * P["I","D"] + x["IMR"] * P["IMR","D"]
    yll <- deaths_ty * get_ex(as.integer(t))
    list(cost = c_tx, yld = yld, yll = yll, daly = yld + yll)
  }
  
  # Local run model
  run_arm <- function(vaccination) {
    trace <- matrix(0, nrow = n_cycles + 1, ncol = n_states,
                    dimnames = list(0:n_cycles, states))
    if (vaccination) {
      trace[1, "V"] <- 1 * ov_psi
      trace[1, "S"] <- 1 * (1 - ov_psi)
    } else {
      trace[1, "S"] <- 1
    }
    tot_cost <- tot_daly <- 0
    if (vaccination) {
      vax_uc <- n_doses * (loc_c_vax_proc + loc_c_vax_adm)
      tot_cost <- tot_cost + (1 * ov_psi * vax_uc) * disc(0)
    }
    for (t in 0:(n_cycles - 1)) {
      x <- trace[t + 1, ]
      P <- make_P_loc(t)
      out <- cycle_out_loc(t, x, P)
      tot_cost <- tot_cost + out$cost * disc(t)
      tot_daly <- tot_daly + out$daly * disc(t)
      x_next <- as.numeric(x %*% P)
      names(x_next) <- states
      trace[t + 2, ] <- x_next
    }
    list(cost = tot_cost, daly = tot_daly)
  }
  
  rv <- run_arm(TRUE)
  rn <- run_arm(FALSE)
  d_cost <- rv$cost - rn$cost
  d_daly <- -(rv$daly - rn$daly)
  icer_val <- if (abs(d_daly) < 1e-12) NA_real_ else d_cost / d_daly
  list(inc_cost = d_cost, daly_averted = d_daly, icer = icer_val,
       cost_vax = rv$cost, cost_no = rn$cost, daly_vax = rv$daly, daly_no = rn$daly)
}

# Define OWSA parameter list from psa_spec_tbl
owsa_params <- psa_spec_tbl %>%
  filter(psa_eligible) %>%
  select(symbol, parameter, base, lower, upper)

# Run base case through parameterised runner
base_owsa <- run_model_owsa()
base_icer_owsa <- base_owsa$icer

cat("\n══════════════════════════════════════════\n")
cat("ONE-WAY SENSITIVITY ANALYSIS\n")
cat("══════════════════════════════════════════\n")
cat("Base case ICER: ", signif(base_icer_owsa, 6), "\n\n")

# Run OWSA
owsa_results <- tibble()

for (i in seq_len(nrow(owsa_params))) {
  sym  <- owsa_params$symbol[i]
  lbl  <- owsa_params$parameter[i]
  lo   <- owsa_params$lower[i]
  hi   <- owsa_params$upper[i]
  
  icer_lo <- run_model_owsa(setNames(list(lo), sym))$icer
  icer_hi <- run_model_owsa(setNames(list(hi), sym))$icer
  
  if (is.na(icer_lo) || is.na(icer_hi)) next
  
  owsa_results <- bind_rows(owsa_results, tibble(
    symbol    = sym,
    parameter = lbl,
    low_input = lo,
    high_input = hi,
    icer_low  = icer_lo,
    icer_high = icer_hi,
    icer_min  = min(icer_lo, icer_hi),
    icer_max  = max(icer_lo, icer_hi),
    icer_range = abs(icer_hi - icer_lo)
  ))
}

owsa_results <- owsa_results %>% arrange(desc(icer_range))

cat("OWSA complete:", nrow(owsa_results), "parameters tested\n")

# Read WTP thresholds
wtp_tbl <- read_excel(derivations_file_path, sheet = "wtp_table") %>%
  clean_names()

# Standardise column names
names(wtp_tbl) <- names(wtp_tbl) %>%
  str_replace_all("\\n", "_") %>%
  str_replace_all("[^a-z0-9_]", "") %>%
  str_to_lower()

# WTP THRESHOLD SETUP (population-weighted pooled South Asia) ----

wtp_all <- wtp_tbl %>%
  select(1:7) %>%
  setNames(c("country", "iso3", "gdp", "wtp_05x", "wtp_1x", "wtp_2x", "wtp_3x"))

# Population weights (World Bank 2024, millions)
sa_pop <- tibble(
  country = c("Afghanistan", "Pakistan", "Nepal", "India",
              "Bangladesh", "Sri Lanka", "Bhutan", "Maldives"),
  pop_millions = c(42.65, 251.27, 29.65, 1450.94,
                   173.56, 21.92, 0.79, 0.53),
  cbr_per1000  = c(27.0, 20.1, 15.8, 15.7, 14.7, 12.5, 14.2, 13.4)
)

# Annual births per country: population x crude birth rate / 1000
# Population: World Bank WDI 2024 (SP.POP.TOTL)
# CBR: UN WPP 2024, medium variant, 2024 projection
sa_pop <- sa_pop %>%
  mutate(annual_births = pop_millions * 1e6 * cbr_per1000 / 1000)

# Regional annual birth cohort (used for EVPI and BIA)
annual_births_sa <- sum(sa_pop$annual_births)

cat("South Asia annual birth cohort:", formatC(round(annual_births_sa), big.mark = ","), "\n")
cat("Source: Pop = World Bank WDI 2024; CBR = UN WPP 2024 medium variant\n")
wtp_all <- wtp_all %>% left_join(sa_pop, by = "country")

# Population-weighted pooled GDP per capita
wtp_pooled_gdp <- sum(wtp_all$gdp * wtp_all$pop_millions) / sum(wtp_all$pop_millions)
wtp_pooled_1x  <- round(wtp_pooled_gdp)
wtp_pooled_05x <- round(wtp_pooled_gdp * 0.5)
wtp_pooled_2x  <- round(wtp_pooled_gdp * 2)
wtp_pooled_3x  <- round(wtp_pooled_gdp * 3)

cat("Population-weighted pooled WTP (1x):", wtp_pooled_1x, "USD/DALY\n")

# CE classification thresholds:
# Green  = cost-effective at India 1x ($2,600) or any higher WTP
# Red    = not cost-effective at Pakistan 1x ($1,400) or any lower WTP
# Yellow = ambiguous zone between Pakistan and India thresholds
wtp_india_1x    <- as.numeric(wtp_all$wtp_1x[wtp_all$country == "India"])
wtp_pakistan_1x  <- as.numeric(wtp_all$wtp_1x[wtp_all$country == "Pakistan"])
if (is.na(wtp_india_1x))   wtp_india_1x   <- 2600
if (is.na(wtp_pakistan_1x)) wtp_pakistan_1x <- 1400

# Country colour palette (used across all plots)
country_colours_vec <- c(
  "Afghanistan" = "#8B0000", "Pakistan" = "#006400", "Nepal" = "#00008B",
  "India" = "#CC5500", "Bangladesh" = "#4B0082", "Sri Lanka" = "#004D40",
  "Bhutan" = "#5D3A1A", "Maldives" = "#8B008B"
)

# Create tornado_data from OWSA results
tornado_data <- owsa_results %>%
  mutate(parameter = factor(parameter, levels = rev(parameter)),
         sensitive = (icer_min <= wtp_pooled_1x & icer_max >= wtp_pooled_1x))

p_tornado <- ggplot(tornado_data, aes(y = parameter)) +
  geom_segment(aes(x = icer_min, xend = icer_max, yend = parameter,
                   colour = sensitive),
               linewidth = 7, alpha = 0.85) +
  scale_colour_manual(values = c("FALSE" = "steelblue", "TRUE" = "#E74C3C")) +
  
  # Base ICER vertical line
  geom_vline(xintercept = base_icer_owsa,
             linetype = "dashed",
             linewidth = 1,
             colour = "black") +
  
  annotate("text", x = base_icer_owsa, y = 0.5,
           label = paste0("Base ICER = $", formatC(round(base_icer_owsa), big.mark = ",")),
           hjust = -0.05, size = 3, fontface = "italic") +
  
  scale_x_continuous(labels = label_dollar()) +
  labs(
    title = "Tornado Diagram: One-Way Sensitivity Analysis",
    x = "ICER (USD per DALY averted)", y = NULL
  ) +
  graph_style(base_size = 10) +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "none"
  )

print(p_tornado)

ggsave(file.path(output_dir, "Fig_Tornado_OWSA.png"), p_tornado,
       width = 12, height = 7, dpi = 300, bg = "white")

# Two-way: phi vs incidence — with pooled + country contours ----
twoway_grid_phi_inc <- expand.grid(
  phi_val = seq(get_scalar_base("phi") * 0.5, min(get_scalar_base("phi") * 1.5, 0.99), length.out = 30),
  inc_val = seq(owsa_params$lower[owsa_params$symbol == "inc_per100k"],
                owsa_params$upper[owsa_params$symbol == "inc_per100k"], length.out = 30)
)
twoway_grid_phi_inc$icer <- mapply(function(ph, inc) {
  run_model_owsa(list(phi = ph, inc_per100k = inc))$icer
}, twoway_grid_phi_inc$phi_val, twoway_grid_phi_inc$inc_val)

p_twoway_phi <- ggplot(twoway_grid_phi_inc, aes(x = inc_val, y = phi_val, fill = icer)) +
  geom_tile() +
  geom_contour(aes(z = icer), breaks = wtp_pooled_1x, colour = "white", linewidth = 1.2) +
  geom_contour(aes(z = icer), breaks = unique(wtp_all$wtp_1x), colour = "grey80",
               linewidth = 0.4, linetype = "dashed") +
  scale_fill_gradient2(low = "#2ECC71", mid = "#F39C12", high = "#E74C3C",
                       midpoint = wtp_pooled_1x, name = "ICER\n(USD/DALY)",
                       labels = label_dollar()) +
  labs(title = "Two-Way Sensitivity: Vaccine Efficacy vs Incidence",
       subtitle = paste0("White = pooled SA WTP ($", formatC(wtp_pooled_1x, big.mark = ","),
                         ") | Grey dashed = country 1x WTP"),
       x = "Typhoid incidence per 100,000", y = "Vaccine efficacy (\u03c6)") +
  graph_style()

print(p_twoway_phi)
ggsave(file.path(output_dir, "Fig_TwoWay_Phi_Inc.png"), p_twoway_phi,
       width = 9, height = 7, dpi = 300, bg = "white")

# Two-way: AMR proportion vs incidence ----
twoway_grid_amr_inc <- expand.grid(
  amr_val = seq(owsa_params$lower[owsa_params$symbol == "pi_mr"],
                owsa_params$upper[owsa_params$symbol == "pi_mr"], length.out = 30),
  inc_val = seq(owsa_params$lower[owsa_params$symbol == "inc_per100k"],
                owsa_params$upper[owsa_params$symbol == "inc_per100k"], length.out = 30)
)
twoway_grid_amr_inc$icer <- mapply(function(amr, inc) {
  run_model_owsa(list(pi_mr = amr, inc_per100k = inc))$icer
}, twoway_grid_amr_inc$amr_val, twoway_grid_amr_inc$inc_val)

p_twoway_amr <- ggplot(twoway_grid_amr_inc, aes(x = inc_val, y = amr_val, fill = icer)) +
  geom_tile() +
  geom_contour(aes(z = icer), breaks = wtp_pooled_1x, colour = "white", linewidth = 1.2) +
  geom_contour(aes(z = icer), breaks = unique(wtp_all$wtp_1x), colour = "grey80",
               linewidth = 0.4, linetype = "dashed") +
  scale_fill_gradient2(low = "#2ECC71", mid = "#F39C12", high = "#E74C3C",
                       midpoint = wtp_pooled_1x, name = "ICER\n(USD/DALY)",
                       labels = label_dollar()) +
  labs(title = "Two-Way Sensitivity: AMR Proportion vs Incidence",
       subtitle = paste0("White = pooled SA WTP ($", formatC(wtp_pooled_1x, big.mark = ","),
                         ") | Grey dashed = country 1x WTP"),
       x = "Typhoid incidence per 100,000", y = "AMR proportion (\u03c0\u2098\u1d63)") +
  graph_style()

print(p_twoway_amr)
ggsave(file.path(output_dir, "Fig_TwoWay_AMR_Inc.png"), p_twoway_amr,
       width = 9, height = 7, dpi = 300, bg = "white")

# OWSA summary table ----
owsa_gt <- owsa_results %>%
  transmute(
    Parameter = parameter,
    `Low input` = formatC(low_input, format = "f", digits = 4),
    `High input` = formatC(high_input, format = "f", digits = 4),
    `ICER (low)` = dollar(round(icer_low)),
    `ICER (high)` = dollar(round(icer_high)),
    `ICER range` = dollar(round(icer_range))
  ) %>%
  gt() %>%
  tab_header(
    title = md("**Table 5. One-Way Sensitivity Analysis Results**"),
    subtitle = md(paste0("*Base case ICER: ", dollar(round(base_icer_owsa)),
                         " per DALY averted*"))
  ) %>%
  tab_options(table.layout = "auto", table.width = pct(90), data_row.padding = px(2)) %>%
  table_style()

print(owsa_gt)
ggsave_gt <- function(gt_obj, prefix) {
  gtsave(gt_obj, file.path(output_dir, paste0(prefix, ".docx")))
  gtsave(gt_obj, file.path(output_dir, paste0(prefix, ".html")))
  gtsave(gt_obj, file.path(output_dir, paste0(prefix, ".png")))
}
ggsave_gt(owsa_gt, "Table5_OWSA_Results")


# 20. WTP THRESHOLD TABLE ----

# Build GT table from wtp_tbl
wtp_display <- wtp_tbl %>%
  select(1:7) %>%
  setNames(c("Country", "ISO3", "GDP per capita (USD)",
             "0.5x", "1x", "2x", "3x"))

wtp_gt <- wtp_display %>%
  gt() %>%
  tab_header(
    title = md("**Table 6. Willingness-to-Pay Thresholds by South Asian Country**"),
    subtitle = md("*USD per DALY averted, based on GDP per capita (2024 nominal)*")
  ) %>%
  fmt_currency(columns = 3:7, currency = "USD", decimals = 0) %>%
  tab_spanner(label = "WTP Threshold (USD/DALY)", columns = 4:7) %>%
  tab_source_note(md("*GDP data: World Bank WDI 2024 (NY.GDP.PCAP.CD). Thresholds: 0.5x = Woods et al. (2016); 1x and 3x = WHO-CHOICE (2001); 2x = interpolation.*")) %>%
  tab_options(table.layout = "auto", table.width = pct(80), data_row.padding = px(3)) %>%
  table_style()

print(wtp_gt)
ggsave_gt(wtp_gt, "Table6_WTP_Thresholds")


# 21. PROBABILISTIC SENSITIVITY ANALYSIS ----

set.seed(2025)
n_psa <- 10000

# Helper: Beta parameters from mean and bounds
beta_params_from_bounds <- function(mu, lo, hi) {
  se <- (hi - lo) / (2 * 1.96)
  se <- max(se, mu * 0.001)
  var <- se^2
  if (var >= mu * (1 - mu)) var <- mu * (1 - mu) * 0.99
  alpha <- mu * (mu * (1 - mu) / var - 1)
  beta  <- alpha * (1 - mu) / mu
  list(alpha = max(alpha, 0.1), beta = max(beta, 0.1))
}

# Helper: Gamma parameters from mean and bounds
gamma_params_from_bounds <- function(mu, lo, hi) {
  se <- (hi - lo) / (2 * 1.96)
  se <- max(se, mu * 0.01)
  var <- se^2
  shape <- mu^2 / var
  rate  <- mu / var
  list(shape = max(shape, 0.1), rate = max(rate, 0.001))
}

# Helper: Lognormal parameters from mean and bounds
lnorm_params_from_bounds <- function(mu, lo, hi) {
  se <- (hi - lo) / (2 * 1.96)
  se <- max(se, mu * 0.01)
  sigma2 <- log(1 + (se / mu)^2)
  meanlog <- log(mu) - sigma2 / 2
  sdlog   <- sqrt(sigma2)
  list(meanlog = meanlog, sdlog = sdlog)
}

# Draw function for a single parameter
draw_param <- function(sym, base, lo, hi, dist) {
  switch(dist,
         beta = {
           bp <- beta_params_from_bounds(base, lo, hi)
           rbeta(1, bp$alpha, bp$beta)
         },
         gamma = {
           gp <- gamma_params_from_bounds(base, lo, hi)
           rgamma(1, shape = gp$shape, rate = gp$rate)
         },
         lognormal = {
           lp <- lnorm_params_from_bounds(base, lo, hi)
           rlnorm(1, meanlog = lp$meanlog, sdlog = lp$sdlog)
         },
         normal = {
           se <- (hi - lo) / (2 * 1.96)
           rnorm(1, mean = base, sd = max(se, base * 0.01))
         },
         triangular = {
           # Simple triangular: U = runif, then invert CDF
           u <- runif(1)
           Fc <- (base - lo) / (hi - lo)
           if (u < Fc) {
             lo + sqrt(u * (hi - lo) * (base - lo))
           } else {
             hi - sqrt((1 - u) * (hi - lo) * (hi - base))
           }
         },
         uniform = runif(1, min = lo, max = hi),
         base  # fallback: return base
  )
}

# PSA-eligible parameters
psa_params <- psa_spec_tbl %>% filter(psa_eligible)

cat("\n\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\n")
cat("PROBABILISTIC SENSITIVITY ANALYSIS\n")
cat("n = ", n_psa, " Monte Carlo draws\n")
cat("\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\n")

# Storage
psa_draws  <- matrix(NA_real_, nrow = n_psa, ncol = nrow(psa_params),
                     dimnames = list(NULL, psa_params$symbol))
psa_results <- tibble(
  iteration    = 1:n_psa,
  cost_vax     = NA_real_,
  cost_no      = NA_real_,
  daly_vax     = NA_real_,
  daly_no      = NA_real_,
  inc_cost     = NA_real_,
  daly_averted = NA_real_,
  icer         = NA_real_
)

pb <- txtProgressBar(min = 0, max = n_psa, style = 3)

for (i in 1:n_psa) {
  
  # Draw all PSA parameters
  draw_list <- list()
  for (j in seq_len(nrow(psa_params))) {
    val <- draw_param(
      psa_params$symbol[j],
      psa_params$base[j],
      psa_params$lower[j],
      psa_params$upper[j],
      psa_params$distribution[j]
    )
    psa_draws[i, j] <- val
    draw_list[[psa_params$symbol[j]]] <- val
  }
  
  # Run model with drawn parameters
  res_i <- tryCatch(
    run_model_owsa(draw_list),
    error = function(e) list(cost_vax = NA, cost_no = NA, daly_vax = NA, daly_no = NA,
                             inc_cost = NA, daly_averted = NA, icer = NA)
  )
  
  psa_results$cost_vax[i]     <- res_i$cost_vax
  psa_results$cost_no[i]      <- res_i$cost_no
  psa_results$daly_vax[i]     <- res_i$daly_vax
  psa_results$daly_no[i]      <- res_i$daly_no
  psa_results$inc_cost[i]     <- res_i$inc_cost
  psa_results$daly_averted[i] <- res_i$daly_averted
  psa_results$icer[i]         <- res_i$icer
  
  setTxtProgressBar(pb, i)
}
close(pb)

# Remove failed iterations
psa_valid <- psa_results %>% filter(!is.na(icer))
cat("\nValid iterations:", nrow(psa_valid), "of", n_psa, "\n")

# Quadrant classification and three-zone CE assessment ----
psa_valid <- psa_valid %>%
  mutate(
    quadrant = case_when(
      daly_averted > 0 & inc_cost > 0 ~ "NE: More costly, more effective",
      daly_averted > 0 & inc_cost <= 0 ~ "SE: Dominant (less costly, more effective)",
      daly_averted <= 0 & inc_cost > 0 ~ "NW: Dominated (more costly, less effective)",
      daly_averted <= 0 & inc_cost <= 0 ~ "SW: Less costly, less effective"
    ),
    # Three-zone classification:
    # Green  = CE at India WTP ($2,600) or higher
    # Red    = not CE at Pakistan WTP ($1,400) or lower
    # Yellow = ambiguous (between Pakistan and India thresholds)
    ce_zone = case_when(
      inc_cost <= wtp_india_1x * daly_averted   ~ "green",
      inc_cost > wtp_pakistan_1x * daly_averted  ~ "red",
      TRUE                                       ~ "yellow"
    )
  )

# PSA Summary Statistics
cat("\nIncremental Cost:  Mean = $", round(mean(psa_valid$inc_cost)),
    " | 95% CI [$", round(quantile(psa_valid$inc_cost, 0.025)),
    ", $", round(quantile(psa_valid$inc_cost, 0.975)), "]\n")
cat("DALYs averted:     Mean = ", signif(mean(psa_valid$daly_averted), 4),
    " | 95% CI [", signif(quantile(psa_valid$daly_averted, 0.025), 4),
    ", ", signif(quantile(psa_valid$daly_averted, 0.975), 4), "]\n")
cat("ICER:              Mean = $", round(mean(psa_valid$icer)),
    " | Median = $", round(median(psa_valid$icer)), "\n")
cat("Green zone (CE at India WTP):     ", round(100 * mean(psa_valid$ce_zone == "green"), 1), "%\n")
cat("Red zone (not CE at Pakistan WTP):", round(100 * mean(psa_valid$ce_zone == "red"), 1), "%\n")
cat("Yellow zone (ambiguous):          ", round(100 * mean(psa_valid$ce_zone == "yellow"), 1), "%\n")

# CE Plane with all country WTP lines ----
p_ceplane <- ggplot(psa_valid, aes(x = daly_averted, y = inc_cost)) +
  geom_point(aes(colour = ce_zone), alpha = 0.3, size = 1) +
  scale_colour_manual(
    values = c("green" = "#27AE60", "yellow" = "#F1C40F", "red" = "#E74C3C"),
    labels = c("green"  = paste0("CE at India WTP ($", formatC(wtp_india_1x, big.mark = ","), "+)"),
               "yellow" = "Ambiguous zone",
               "red"    = paste0("Not CE at Pakistan WTP ($", formatC(wtp_pakistan_1x, big.mark = ","), "-)")),
    name = "Cost-effectiveness"
  ) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  geom_vline(xintercept = 0, linewidth = 0.5)

# Add all country 1x and 0.5x WTP lines
for (k in seq_len(nrow(wtp_all))) {
  cname <- wtp_all$country[k]
  ccol  <- country_colours_vec[cname]
  # 1x WTP line (solid)
  p_ceplane <- p_ceplane +
    geom_abline(intercept = 0, slope = wtp_all$wtp_1x[k],
                linetype = "solid", colour = ccol, linewidth = 0.4, alpha = 0.6)
  # 0.5x WTP line (dashed)
  p_ceplane <- p_ceplane +
    geom_abline(intercept = 0, slope = wtp_all$wtp_05x[k],
                linetype = "dashed", colour = ccol, linewidth = 0.3, alpha = 0.4)
}

# Pooled WTP as prominent line
p_ceplane <- p_ceplane +
  geom_abline(intercept = 0, slope = wtp_pooled_1x, linetype = "longdash",
              colour = "#E67E22", linewidth = 0.9) +
  annotate("point", x = mean(psa_valid$daly_averted), y = mean(psa_valid$inc_cost),
           colour = "black", size = 4, shape = 18) +
  annotate("text", x = max(psa_valid$daly_averted) * 0.7,
           y = max(psa_valid$inc_cost) * 0.9,
           label = "NE: More costly,\nMore effective",
           size = 3, alpha = 0.6, fontface = "bold") +
  annotate("text", x = min(psa_valid$daly_averted) * 0.7,
           y = max(psa_valid$inc_cost) * 0.9,
           label = "NW: Dominated",
           size = 3, alpha = 0.6, colour = "red", fontface = "bold") +
  annotate("text", x = max(psa_valid$daly_averted) * 0.7,
           y = min(psa_valid$inc_cost) * 0.9,
           label = "SE: Dominant",
           size = 3, alpha = 0.6, colour = "darkgreen", fontface = "bold") +
  labs(
    title = "Cost-Effectiveness Plane",
    subtitle = sprintf("PSA: %s iterations | Solid = 1x WTP, Dashed = 0.5x WTP per country | Orange = pooled SA",
                       formatC(nrow(psa_valid), big.mark = ",")),
    x = "Incremental DALYs averted",
    y = "Incremental cost (USD)",
    caption = "Green = CE at India threshold or above | Red = not CE at Pakistan threshold or below | Yellow = ambiguous"
  ) +
  graph_style() +
  theme(legend.position = "bottom", plot.caption = element_text(size = 8, face = "italic"))

print(p_ceplane)
ggsave(file.path(output_dir, "Fig_CE_Plane.png"), p_ceplane,
       width = 11, height = 8, dpi = 300, bg = "white")

# Histogram of iNMB at pooled regional WTP ----
psa_valid <- psa_valid %>%
  mutate(inmb_pooled = wtp_pooled_1x * daly_averted - inc_cost)

p_hist_nmb <- ggplot(psa_valid, aes(x = inmb_pooled)) +
  geom_histogram(aes(fill = inmb_pooled >= 0), bins = 80, alpha = 0.8) +
  scale_fill_manual(values = c("TRUE" = "#27AE60", "FALSE" = "#E74C3C"),
                    labels = c("TRUE" = "NMB \u2265 0 (cost-effective)",
                               "FALSE" = "NMB < 0 (not cost-effective)"),
                    name = NULL) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.8) +
  scale_x_continuous(labels = label_dollar()) +
  labs(
    title = "Distribution of Incremental Net Monetary Benefit",
    subtitle = sprintf("Pooled South Asia WTP = $%s/DALY | P(CE) = %.1f%%",
                       formatC(wtp_pooled_1x, big.mark = ","),
                       100 * mean(psa_valid$inmb_pooled >= 0)),
    x = "Incremental NMB (USD)", y = "Frequency"
  ) +
  graph_style() +
  theme(legend.position = "bottom")

print(p_hist_nmb)
ggsave(file.path(output_dir, "Fig_Histogram_iNMB.png"), p_hist_nmb,
       width = 10, height = 6, dpi = 300, bg = "white")

# Bar chart of quadrant proportions by country ----
# For each country, classify iterations at that country's 1x WTP
quad_by_country <- tibble()
for (k in seq_len(nrow(wtp_all))) {
  cname <- wtp_all$country[k]
  c_wtp <- wtp_all$wtp_1x[k]
  temp <- psa_valid %>%
    mutate(
      ce_this = (inc_cost <= c_wtp * daly_averted),
      quad_this = case_when(
        daly_averted > 0 & ce_this  ~ "CE (NE below WTP or SE)",
        daly_averted > 0 & !ce_this ~ "Not CE (NE above WTP)",
        daly_averted <= 0 & inc_cost > 0 ~ "Dominated (NW)",
        TRUE ~ "SW"
      )
    ) %>%
    count(quad_this) %>%
    mutate(country = cname, pct = n / sum(n) * 100)
  quad_by_country <- bind_rows(quad_by_country, temp)
}

# Also add pooled
temp_pooled <- psa_valid %>%
  mutate(
    ce_this = (inc_cost <= wtp_pooled_1x * daly_averted),
    quad_this = case_when(
      daly_averted > 0 & ce_this  ~ "CE (NE below WTP or SE)",
      daly_averted > 0 & !ce_this ~ "Not CE (NE above WTP)",
      daly_averted <= 0 & inc_cost > 0 ~ "Dominated (NW)",
      TRUE ~ "SW"
    )
  ) %>%
  count(quad_this) %>%
  mutate(country = "Pooled South Asia", pct = n / sum(n) * 100)
quad_by_country <- bind_rows(quad_by_country, temp_pooled)

# Order countries by GDP (low to high), pooled last
country_order <- c(wtp_all$country[order(wtp_all$gdp)], "Pooled South Asia")
quad_by_country <- quad_by_country %>%
  mutate(country = factor(country, levels = country_order))

quad_fill <- c(
  "CE (NE below WTP or SE)" = "#27AE60",
  "Not CE (NE above WTP)"   = "#E74C3C",
  "Dominated (NW)"          = "#95A5A6",
  "SW"                       = "#F39C12"
)

p_quad_bar <- ggplot(quad_by_country, aes(x = country, y = pct, fill = quad_this)) +
  geom_col(position = "stack", alpha = 0.85) +
  scale_fill_manual(values = quad_fill, name = "Classification") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Cost-Effectiveness Classification by Country",
    subtitle = sprintf("At each country's 1x GDP/capita WTP | n = %s PSA iterations",
                       formatC(nrow(psa_valid), big.mark = ",")),
    x = NULL, y = "Proportion of iterations (%)"
  ) +
  graph_style() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 9),
        legend.position = "bottom")

print(p_quad_bar)
ggsave(file.path(output_dir, "Fig_Quadrant_Bar_ByCountry.png"), p_quad_bar,
       width = 12, height = 7, dpi = 300, bg = "white")



# 22. COST-EFFECTIVENESS ACCEPTABILITY CURVE ----

# Country colour palette for CEAC: three shades per country
country_shades <- tibble(
  country = c("Afghanistan", "Pakistan", "Nepal", "India",
              "Bangladesh", "Sri Lanka", "Bhutan", "Maldives"),
  dark    = c("#8B0000", "#006400", "#00008B", "#CC5500",
              "#4B0082", "#004D40", "#5D3A1A", "#8B008B"),
  medium  = c("#CD5C5C", "#3CB371", "#4169E1", "#FF8C00",
              "#8A2BE2", "#009688", "#A0522D", "#DA70D6"),
  light   = c("#F08080", "#90EE90", "#87CEEB", "#FFB347",
              "#DDA0DD", "#80CBC4", "#D2B48C", "#FFB6C1")
)

# Build WTP lines data
wtp_lines <- wtp_all %>%
  select(country, iso3, gdp, wtp_1x, wtp_2x, wtp_3x) %>%
  left_join(country_shades, by = "country") %>%
  pivot_longer(cols = c(wtp_1x, wtp_2x, wtp_3x),
               names_to = "threshold_type", values_to = "wtp_value") %>%
  mutate(
    colour = case_when(
      threshold_type == "wtp_1x" ~ dark,
      threshold_type == "wtp_2x" ~ medium,
      threshold_type == "wtp_3x" ~ light
    ),
    linetype = case_when(
      threshold_type == "wtp_1x" ~ "solid",
      threshold_type == "wtp_2x" ~ "dashed",
      threshold_type == "wtp_3x" ~ "dotted"
    ),
    label = paste0(country, " ", str_remove(threshold_type, "wtp_"), " GDP")
  )

# Compute CEAC
wtp_max <- max(wtp_lines$wtp_value, na.rm = TRUE) * 1.1
wtp_grid <- seq(0, wtp_max, by = 50)
ceac_prob <- numeric(length(wtp_grid))

for (j in seq_along(wtp_grid)) {
  ceac_prob[j] <- mean(psa_valid$inc_cost <= wtp_grid[j] * psa_valid$daly_averted)
}

ceac_data <- tibble(wtp = wtp_grid, prob_ce = ceac_prob)

# Base CEAC plot
p_ceac <- ggplot(ceac_data, aes(x = wtp, y = prob_ce)) +
  geom_line(linewidth = 1.5, colour = "black") +
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey50", alpha = 0.6)

# Add country WTP lines
for (k in seq_len(nrow(wtp_lines))) {
  p_ceac <- p_ceac +
    geom_vline(xintercept = wtp_lines$wtp_value[k],
               colour = wtp_lines$colour[k],
               linetype = wtp_lines$linetype[k],
               linewidth = 0.6, alpha = 0.8)
}

# Add pooled WTP line
p_ceac <- p_ceac +
  geom_vline(xintercept = wtp_pooled_1x, colour = "#E67E22",
             linetype = "longdash", linewidth = 1) +
  annotate("text", x = wtp_pooled_1x * 1.02, y = 0.05,
           label = paste0("Pooled SA = $", formatC(wtp_pooled_1x, big.mark = ",")),
           colour = "#E67E22", size = 3, hjust = 0, fontface = "bold")

# Country name labels at top for 1x thresholds only
wtp_1x_only <- wtp_lines %>% filter(threshold_type == "wtp_1x")
for (k in seq_len(nrow(wtp_1x_only))) {
  p_ceac <- p_ceac +
    annotate("text", x = wtp_1x_only$wtp_value[k], y = 1.02,
             label = wtp_1x_only$country[k],
             colour = wtp_1x_only$dark[k], size = 2.5, angle = 45,
             hjust = 0, fontface = "bold")
}

p_ceac <- p_ceac +
  scale_x_continuous(labels = label_dollar(),
                     breaks = pretty(c(0, wtp_max), n = 10)) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1.05)) +
  labs(
    title = "Cost-Effectiveness Acceptability Curve",
    subtitle = "Probability TCV is cost-effective vs no vaccination across WTP thresholds",
    x = "Willingness-to-pay threshold (USD per DALY averted)",
    y = "Probability cost-effective",
    caption = paste0("Vertical lines: country-specific WTP thresholds ",
                     "(solid = 1x GDP, dashed = 2x, dotted = 3x). ",
                     "Orange = pooled SA WTP. n = ",
                     formatC(nrow(psa_valid), big.mark = ","), " PSA iterations.")
  ) +
  graph_style() +
  theme(plot.caption = element_text(size = 8, face = "italic"))

# Add line type legend
p_ceac <- p_ceac +
  annotate("segment", x = wtp_max * 0.7, xend = wtp_max * 0.75,
           y = 0.30, yend = 0.30, linewidth = 0.8, linetype = "solid") +
  annotate("text", x = wtp_max * 0.76, y = 0.30, label = "1x GDP/capita",
           size = 3, hjust = 0) +
  annotate("segment", x = wtp_max * 0.7, xend = wtp_max * 0.75,
           y = 0.25, yend = 0.25, linewidth = 0.8, linetype = "dashed") +
  annotate("text", x = wtp_max * 0.76, y = 0.25, label = "2x GDP/capita",
           size = 3, hjust = 0) +
  annotate("segment", x = wtp_max * 0.7, xend = wtp_max * 0.75,
           y = 0.20, yend = 0.20, linewidth = 0.8, linetype = "dotted") +
  annotate("text", x = wtp_max * 0.76, y = 0.20, label = "3x GDP/capita",
           size = 3, hjust = 0)

print(p_ceac)
ggsave(file.path(output_dir, "Fig_CEAC.png"), p_ceac,
       width = 14, height = 8, dpi = 300, bg = "white")

# CEAC probability at key thresholds table
ceac_at_thresholds <- wtp_lines %>%
  rowwise() %>%
  mutate(
    prob_ce = mean(psa_valid$inc_cost <= wtp_value * psa_valid$daly_averted)
  ) %>%
  ungroup() %>%
  select(country, threshold_type, wtp_value, prob_ce) %>%
  mutate(threshold_type = str_remove(threshold_type, "wtp_"),
         prob_ce = paste0(round(prob_ce * 100, 1), "%"),
         wtp_value = dollar(wtp_value))

ceac_prob_gt <- ceac_at_thresholds %>%
  pivot_wider(names_from = threshold_type, values_from = c(wtp_value, prob_ce)) %>%
  gt() %>%
  tab_header(
    title = md("**Table 7. Probability of Cost-Effectiveness at Country-Specific WTP Thresholds**")
  ) %>%
  tab_options(table.layout = "auto", table.width = pct(90), data_row.padding = px(2)) %>%
  table_style()

print(ceac_prob_gt)
ggsave_gt(ceac_prob_gt, "Table7_CEAC_Probabilities")


# 23. VALUE OF INFORMATION ANALYSIS ----

# 23A. EVPI (per-person and population)

# NMB for each iteration and each strategy
compute_evpi <- function(wtp_val, psa_df) {
  nmb_vax <- wtp_val * (-psa_df$daly_vax) - psa_df$cost_vax
  nmb_no  <- wtp_val * (-psa_df$daly_no)  - psa_df$cost_no
  
  # Expected NMB with current info: choose strategy with higher mean NMB
  e_nmb_vax <- mean(nmb_vax)
  e_nmb_no  <- mean(nmb_no)
  enb_current <- max(e_nmb_vax, e_nmb_no)
  
  # Expected NMB with perfect info: for each iteration choose max
  enb_perfect <- mean(pmax(nmb_vax, nmb_no))
  
  enb_perfect - enb_current
}

evpi_grid <- seq(0, wtp_max, by = 100)
evpi_values <- sapply(evpi_grid, compute_evpi, psa_df = psa_valid)

evpi_data <- tibble(wtp = evpi_grid, evpi_per_person = evpi_values)

# Population EVPI assumptions
# annual_births_sa already computed in WTP section from sa_pop (CBR x population)
tech_lifetime    <- 10      # years
pop_multiplier   <- sum(annual_births_sa / (1 + discount_rate)^(0:(tech_lifetime - 1)))

evpi_data <- evpi_data %>%
  mutate(evpi_population = evpi_per_person * pop_multiplier)

# Per-person EVPI plot
p_evpi <- ggplot(evpi_data, aes(x = wtp, y = evpi_per_person)) +
  geom_line(linewidth = 1.2, colour = "#2C3E50") +
  geom_vline(xintercept = wtp_pooled_1x, linetype = "dashed", colour = "#E67E22") +
  annotate("text", x = wtp_pooled_1x * 1.05, y = max(evpi_data$evpi_per_person) * 0.9,
           label = paste0("Pooled SA = $", formatC(wtp_pooled_1x, big.mark = ",")),
           colour = "#E67E22", size = 3, hjust = 0) +
  scale_x_continuous(labels = label_dollar()) +
  scale_y_continuous(labels = label_dollar()) +
  labs(
    title = "Expected Value of Perfect Information (per person)",
    subtitle = "Upper bound on value of eliminating all parameter uncertainty",
    x = "Willingness-to-pay threshold (USD per DALY averted)",
    y = "EVPI per person (USD)"
  ) +
  graph_style()

print(p_evpi)
ggsave(file.path(output_dir, "Fig_EVPI_PerPerson.png"), p_evpi,
       width = 10, height = 6, dpi = 300, bg = "white")

# Population EVPI plot
p_evpi_pop <- ggplot(evpi_data, aes(x = wtp, y = evpi_population)) +
  geom_line(linewidth = 1.2, colour = "#8E44AD") +
  geom_vline(xintercept = wtp_pooled_1x, linetype = "dashed", colour = "#E67E22") +
  scale_x_continuous(labels = label_dollar()) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M", prefix = "$")) +
  labs(
    title = "Population EVPI",
    subtitle = sprintf("Annual birth cohort: %sM | Intervention: %d years | Discount: %d%%",
                       formatC(annual_births_sa / 1e6, format = "f", digits = 0),
                       tech_lifetime, discount_rate * 100),
    x = "Willingness-to-pay threshold (USD per DALY averted)",
    y = "Population EVPI (USD millions)"
  ) +
  graph_style()

print(p_evpi_pop)
ggsave(file.path(output_dir, "Fig_EVPI_Population.png"), p_evpi_pop,
       width = 10, height = 6, dpi = 300, bg = "white")

# 23B. EVPPI (partial, for top 5 tornado parameters)

# Use nonparametric regression approach: partition draws into deciles
# and compute conditional EVPI within each decile

top5_params <- owsa_results$symbol[1:min(5, nrow(owsa_results))]

compute_evppi <- function(param_sym, wtp_val, psa_df, draws_mat) {
  nmb_vax <- wtp_val * (-psa_df$daly_vax) - psa_df$cost_vax
  nmb_no  <- wtp_val * (-psa_df$daly_no)  - psa_df$cost_no
  
  param_draws <- draws_mat[1:nrow(psa_df), param_sym]
  deciles <- cut(param_draws, breaks = quantile(param_draws, probs = seq(0, 1, 0.1)),
                 include.lowest = TRUE, labels = FALSE)
  
  # Within each decile: compute conditional expected NMB
  inner_max <- numeric(10)
  for (d in 1:10) {
    idx <- which(deciles == d)
    if (length(idx) < 2) { inner_max[d] <- NA; next }
    e_vax_d <- mean(nmb_vax[idx])
    e_no_d  <- mean(nmb_no[idx])
    inner_max[d] <- max(e_vax_d, e_no_d)
  }
  
  evppi <- mean(inner_max, na.rm = TRUE) - max(mean(nmb_vax), mean(nmb_no))
  max(evppi, 0)
}

evppi_results <- tibble()
for (sym in top5_params) {
  evppi_at_india <- compute_evppi(sym, wtp_pooled_1x, psa_valid, psa_draws)
  lbl <- psa_params$parameter[psa_params$symbol == sym]
  evppi_results <- bind_rows(evppi_results, tibble(
    symbol = sym, parameter = lbl, evppi = evppi_at_india
  ))
}

evppi_results <- evppi_results %>% arrange(desc(evppi))

cat("\n\u2550\u2550 EVPPI at Pooled SA WTP (\u2550\u2550\n")
print(evppi_results)

# EVPPI across WTP grid for top 5
evppi_grid_data <- tibble()
for (sym in top5_params) {
  lbl <- psa_params$parameter[psa_params$symbol == sym]
  for (w in seq(0, wtp_max, by = 500)) {
    ev <- compute_evppi(sym, w, psa_valid, psa_draws)
    evppi_grid_data <- bind_rows(evppi_grid_data, tibble(
      wtp = w, symbol = sym, parameter = lbl, evppi = ev
    ))
  }
}

p_evppi <- ggplot(evppi_grid_data, aes(x = wtp, y = evppi, colour = parameter)) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = wtp_pooled_1x, linetype = "dashed", colour = "#E67E22") +
  scale_x_continuous(labels = label_dollar()) +
  scale_y_continuous(labels = label_dollar()) +
  labs(
    title = "Expected Value of Perfect Parameter Information (EVPPI)",
    subtitle = "Top 5 parameters identified with OWSA",
    x = "Willingness-to-pay threshold (USD per DALY averted)",
    y = "EVPPI per person (USD)",
    colour = "Parameter"
  ) +
  graph_style() +
  theme(legend.position = "bottom", legend.text = element_text(size = 8))

print(p_evppi)
ggsave(file.path(output_dir, "Fig_EVPPI.png"), p_evppi,
       width = 12, height = 7, dpi = 300, bg = "white")


# 24. SCENARIO ANALYSIS ----

# Scenarios address structural uncertainty
# Each scenario modifies one or more structural assumptions and re-runs the model

scenarios <- list(
  list(name = "Base case",           overrides = list()),
  list(name = "Short horizon (10y)", overrides = list(.n_cycles = 10)),
  list(name = "High AMR (2x base)",  overrides = list(pi_mr = pi_MR * 2)),
  list(name = "Rising incidence (+50%)",  overrides = list(inc_per100k = inc_per100k_2019 * 1.5)),
  list(name = "Declining incidence (-50%)", overrides = list(inc_per100k = inc_per100k_2019 * 0.5)),
  list(name = "Two-dose schedule",   overrides = list(.n_doses = 2)),
  list(name = "Lower coverage (50%)", overrides = list(psi = 0.50))
)

# Extended model runner for scenario analysis (handles structural changes)
run_scenario <- function(sc) {
  ov <- sc$overrides
  
  # Handle structural overrides that aren't simple parameter swaps
  saved_n_cycles <- n_cycles
  saved_n_doses  <- n_doses
  
  if (!is.null(ov$.n_cycles)) {
    assign("n_cycles", ov$.n_cycles, envir = .GlobalEnv)
    ov$.n_cycles <- NULL
  }
  if (!is.null(ov$.n_doses)) {
    assign("n_doses", ov$.n_doses, envir = .GlobalEnv)
    ov$.n_doses <- NULL
  }
  
  res <- tryCatch(run_model_owsa(ov), error = function(e) {
    list(inc_cost = NA, daly_averted = NA, icer = NA,
         cost_vax = NA, cost_no = NA, daly_vax = NA, daly_no = NA)
  })
  
  # Restore
  assign("n_cycles", saved_n_cycles, envir = .GlobalEnv)
  assign("n_doses",  saved_n_doses,  envir = .GlobalEnv)
  
  tibble(
    Scenario      = sc$name,
    Inc_cost      = res$inc_cost,
    DALY_averted  = res$daly_averted,
    ICER          = res$icer
  )
}

scenario_results <- bind_rows(lapply(scenarios, run_scenario))

scenario_gt <- scenario_results %>%
  mutate(
    Inc_cost     = dollar(round(Inc_cost, 2)),
    DALY_averted = formatC(DALY_averted, format = "f", digits = 6),
    ICER         = dollar(round(as.numeric(ICER)))
  ) %>%
  gt() %>%
  tab_header(
    title = md("**Table 8. Scenario Analysis Results**"),
    subtitle = md("*ICER (USD per DALY averted) under alternative structural assumptions*")
  ) %>%
  cols_label(
    Scenario     = "Scenario",
    Inc_cost     = "\u0394 Cost",
    DALY_averted = "\u0394 DALYs averted",
    ICER         = "ICER (USD/DALY)"
  ) %>%
  tab_options(table.layout = "auto", table.width = pct(80), data_row.padding = px(3)) %>%
  table_style()

print(scenario_gt)
ggsave_gt(scenario_gt, "Table8_Scenario_Analysis")


# 25. EXPECTED LOSS CURVES ----

# For each WTP, the expected loss for strategy j is:
# EL(j, lambda) = E[ max_k NMB(k,theta) - NMB(j,theta) ]

compute_expected_loss <- function(wtp_val, psa_df) {
  nmb_vax <- wtp_val * (-psa_df$daly_vax) - psa_df$cost_vax
  nmb_no  <- wtp_val * (-psa_df$daly_no)  - psa_df$cost_no
  
  max_nmb <- pmax(nmb_vax, nmb_no)
  
  el_vax <- mean(max_nmb - nmb_vax)
  el_no  <- mean(max_nmb - nmb_no)
  
  tibble(wtp = wtp_val,
         strategy = c("TCV vaccination", "No vaccination"),
         expected_loss = c(el_vax, el_no))
}

elc_data <- bind_rows(lapply(evpi_grid, compute_expected_loss, psa_df = psa_valid))

p_elc <- ggplot(elc_data, aes(x = wtp, y = expected_loss, colour = strategy)) +
  geom_line(linewidth = 1.2) +
  scale_colour_manual(values = c("TCV vaccination" = "#27AE60", "No vaccination" = "#E74C3C")) +
  geom_vline(xintercept = wtp_pooled_1x, linetype = "dashed", colour = "#E67E22") +
  annotate("text", x = wtp_pooled_1x * 1.02, y = max(elc_data$expected_loss) * 0.9,
           label = paste0("Pooled SA = $", formatC(wtp_pooled_1x, big.mark = ",")),
           colour = "#E67E22", size = 3, hjust = 0) +
  scale_x_continuous(labels = label_dollar()) +
  scale_y_continuous(labels = label_dollar()) +
  labs(
    title = "Expected Loss Curves",
    subtitle = "Strategy with lowest expected loss is optimal at each WTP",
    x = "Willingness-to-pay threshold (USD per DALY averted)",
    y = "Expected monetary loss (USD per person)",
    colour = "Strategy"
  ) +
  graph_style() +
  theme(legend.position = "bottom")

print(p_elc)
ggsave(file.path(output_dir, "Fig_ELC.png"), p_elc,
       width = 10, height = 7, dpi = 300, bg = "white")


# 26. COST-EFFECTIVENESS RISK-AVERSION CURVE (CERAC) ----

# CERAC penalises expected NMB for downside risk
# NBRR(j, lambda) = E[NMB(j)] / SemiSD(j)
# where SemiSD = sqrt of mean of squared deviations below the mean

compute_cerac <- function(wtp_val, psa_df) {
  nmb_vax <- wtp_val * (-psa_df$daly_vax) - psa_df$cost_vax
  nmb_no  <- wtp_val * (-psa_df$daly_no)  - psa_df$cost_no
  
  calc_nbrr <- function(nmb) {
    e_nmb <- mean(nmb)
    below <- nmb[nmb < e_nmb]
    if (length(below) < 2) return(NA_real_)
    semi_var <- mean((below - e_nmb)^2)
    semi_sd  <- sqrt(semi_var)
    if (semi_sd < 1e-10) return(NA_real_)
    e_nmb / semi_sd
  }
  
  tibble(
    wtp = wtp_val,
    strategy = c("TCV vaccination", "No vaccination"),
    nbrr = c(calc_nbrr(nmb_vax), calc_nbrr(nmb_no))
  )
}

cerac_data <- bind_rows(lapply(evpi_grid, compute_cerac, psa_df = psa_valid))

# Filter out extreme NBRR values
cerac_data <- cerac_data %>%
  filter(!is.na(nbrr), is.finite(nbrr), abs(nbrr) < quantile(abs(cerac_data$nbrr), 0.99, na.rm = TRUE))

p_cerac <- ggplot(cerac_data, aes(x = wtp, y = nbrr, colour = strategy)) +
  geom_line(linewidth = 1.2) +
  scale_colour_manual(values = c("TCV vaccination" = "#27AE60", "No vaccination" = "#E74C3C")) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey50") +
  geom_vline(xintercept = wtp_pooled_1x, linetype = "dashed", colour = "#E67E22") +
  annotate("text", x = wtp_pooled_1x * 1.02, y = max(cerac_data$nbrr, na.rm = TRUE) * 0.9,
           label = paste0("Pooled SA = $", formatC(wtp_pooled_1x, big.mark = ",")),
           colour = "#E67E22", size = 3, hjust = 0) +
  scale_x_continuous(labels = label_dollar()) +
  labs(
    title = "Cost-Effectiveness Risk-Aversion Curve (CERAC)",
    subtitle = "Strategy with highest NBRR is preferred by risk-averse decision makers",
    x = "Willingness-to-pay threshold (USD per DALY averted)",
    y = "Net Benefit-to-Risk Ratio (NBRR)",
    colour = "Strategy"
  ) +
  graph_style() +
  theme(legend.position = "bottom")

print(p_cerac)
ggsave(file.path(output_dir, "Fig_CERAC.png"), p_cerac,
       width = 10, height = 7, dpi = 300, bg = "white")



# 27. BUDGET IMPACT ANALYSIS ----

# Time horizon: 5 years, aligned to a government election and
# budget planning cycle.

cat("\n══════════════════════════════════════════\n")
cat("BUDGET IMPACT ANALYSIS (5-YEAR HORIZON)\n")
cat("══════════════════════════════════════════\n")

# Save original model settings
saved_n_cycles <- n_cycles

# Re-run model with 5-year horizon
bia_horizon <- 5
assign("n_cycles", bia_horizon, envir = .GlobalEnv)

res_vax_5y <- run_model(vaccination = TRUE,  cohort = 1)
res_no_5y  <- run_model(vaccination = FALSE, cohort = 1)

# Restore original settings
assign("n_cycles", saved_n_cycles, envir = .GlobalEnv)

# Per-person 5-year costs
pp_cost_vax_5y <- res_vax_5y$cost
pp_cost_no_5y  <- res_no_5y$cost

# Vaccination arm
pp_vax_programme_5y <- p_psi * n_doses * (c_vaccine_procurement + c_vaccine_admin) * disc(0)
pp_treatment_vax_5y <- pp_cost_vax_5y - pp_vax_programme_5y

# Population: annual birth cohort computed from sa_pop
bia_cohort <- annual_births_sa

# Scale to population over 5-year budget window

bia_cost_current_5y <- bia_cohort * pp_cost_no_5y
bia_cost_new_5y     <- bia_cohort * pp_cost_vax_5y
bia_total_5y        <- bia_cost_new_5y - bia_cost_current_5y

bia_vax_programme_5y  <- bia_cohort * pp_vax_programme_5y
bia_treatment_new_5y  <- bia_cohort * pp_treatment_vax_5y
bia_treatment_curr_5y <- bia_cost_current_5y
bia_treatment_savings_5y <- bia_treatment_curr_5y - bia_treatment_new_5y

cat(sprintf("BIA time horizon:             %d years\n", bia_horizon))
cat(sprintf("Annual birth cohort:          %s\n",
            formatC(round(bia_cohort), big.mark = ",", format = "d")))
cat(sprintf("Vaccine coverage:             %.0f%%\n", p_psi * 100))
cat(sprintf("Vaccine efficacy:             %.1f%%\n", p_phi * 100))
cat("\nPer-person costs (5-year discounted, 2025 USD):\n")
cat(sprintf("  No vaccination:             $%.4f\n", pp_cost_no_5y))
cat(sprintf("  With vaccination:           $%.4f\n", pp_cost_vax_5y))
cat(sprintf("    of which programme cost:  $%.4f\n", pp_vax_programme_5y))
cat(sprintf("    of which treatment cost:  $%.4f\n", pp_treatment_vax_5y))
cat("\nPopulation-level budget impact (one cohort, 2025 USD millions):\n")
cat(sprintf("  Current scenario (no TCV):  $%.2fM\n", bia_cost_current_5y / 1e6))
cat(sprintf("  New scenario (with TCV):    $%.2fM\n", bia_cost_new_5y / 1e6))
cat(sprintf("    Vaccination programme:    $%.2fM\n", bia_vax_programme_5y / 1e6))
cat(sprintf("    Treatment costs:          $%.2fM\n", bia_treatment_new_5y / 1e6))
cat(sprintf("  Treatment savings:          $%.2fM\n", bia_treatment_savings_5y / 1e6))
cat(sprintf("  NET BUDGET IMPACT:          $%.2fM\n", bia_total_5y / 1e6))

if (bia_total_5y < 0) {
  cat("  Interpretation: TCV programme is COST-SAVING within 5 years.\n")
} else {
  cat(sprintf("  Interpretation: TCV programme requires additional $%.2fM over 5 years.\n",
              bia_total_5y / 1e6))
  cat(sprintf("  Per-person net cost: $%.4f\n", bia_total_5y / bia_cohort))
}

# BIA by country (proportional to birth cohort share)
bia_by_country <- sa_pop %>%
  mutate(
    birth_share = annual_births / sum(annual_births),
    bia_current  = birth_share * bia_cost_current_5y,
    bia_new      = birth_share * bia_cost_new_5y,
    bia_vax_prog = birth_share * bia_vax_programme_5y,
    bia_tx_save  = birth_share * bia_treatment_savings_5y,
    bia_net      = birth_share * bia_total_5y
  )

bia_gt <- bia_by_country %>%
  transmute(
    Country            = country,
    `Annual births (M)` = round(annual_births / 1e6, 2),
    `Birth share`      = paste0(round(birth_share * 100, 1), "%"),
    `Current (no TCV)` = round(bia_current / 1e6, 2),
    `Vaccine programme` = round(bia_vax_prog / 1e6, 2),
    `Treatment savings` = round(bia_tx_save / 1e6, 2),
    `Net BIA`          = round(bia_net / 1e6, 2)
  ) %>%
  gt() %>%
  tab_header(
    title = md("**Table 9. Budget Impact Analysis by Country (5-Year Horizon)**"),
  ) %>%
  fmt_number(columns = c(`Annual births (M)`), decimals = 2) %>%
  fmt_currency(columns = c(`Current (no TCV)`, `Vaccine programme`,
                           `Treatment savings`, `Net BIA`),
               currency = "USD", decimals = 2) %>%
  cols_label(
    `Current (no TCV)` = "Current (no TCV) $M",
    `Vaccine programme` = "Vaccine programme $M",
    `Treatment savings` = "Treatment savings $M",
    `Net BIA`          = "Net BIA $M"
  ) %>%
  tab_source_note(
    md(paste0("*Population: World Bank WDI 2024. CBR: UN WPP 2024 medium variant. ",
              "Per-person costs from model re-run at 5-year horizon. ",
              "Sullivan, S.D. et al. (2014) Budget impact analysis: ",
              "principles of good practice. Value in Health, 17(2), pp.187-194.*"))
  ) %>%
  tab_options(table.layout = "auto", table.width = pct(95), data_row.padding = px(2)) %>%
  table_style()

print(bia_gt)
ggsave_gt(bia_gt, "Table9_BIA_ByCountry")

# BIA waterfall chart
wf_current   <- bia_cost_current_5y
wf_vax_cost  <- bia_vax_programme_5y
wf_savings   <- -bia_treatment_savings_5y
wf_net       <- bia_total_5y

waterfall_data <- tibble(
  component = c("Current treatment\ncosts (5y)", "Vaccination\nprogramme cost",
                "Treatment\nsavings", "Net budget\nimpact"),
  value = c(wf_current, wf_vax_cost, wf_savings, wf_net),
  type  = c("current", "cost_add", "cost_save", "net")
)

waterfall_data <- waterfall_data %>%
  mutate(
    end = cumsum(c(wf_current, wf_vax_cost, wf_savings, 0)),
    start = lag(end, default = 0),
    fill = case_when(
      type == "current"   ~ "#3498DB",
      type == "cost_add"  ~ "#E74C3C",
      type == "cost_save" ~ "#27AE60",
      type == "net"       ~ if_else(wf_net < 0, "#27AE60", "#E74C3C")
    )
  )

waterfall_data$start[waterfall_data$type == "net"] <- 0
waterfall_data$end[waterfall_data$type == "net"]   <- wf_net

p_bia_waterfall <- ggplot(waterfall_data,
                          aes(x = factor(component, levels = component))) +
  geom_rect(aes(xmin = as.numeric(factor(component, levels = component)) - 0.4,
                xmax = as.numeric(factor(component, levels = component)) + 0.4,
                ymin = pmin(start, end), ymax = pmax(start, end), fill = fill)) +
  scale_fill_identity() +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M", prefix = "$")) +
  labs(
    title = "Budget Impact Analysis: Waterfall Chart (5-Year Horizon)",
    subtitle = sprintf("One annual birth cohort",
                       formatC(round(bia_cohort), big.mark = ",")),
    x = NULL,
    y = "Budget impact (USD millions)"
  ) +
  graph_style() +
  theme(axis.text.x = element_text(size = 9))

print(p_bia_waterfall)
ggsave(file.path(output_dir, "Fig_BIA_Waterfall.png"), p_bia_waterfall,
       width = 10, height = 7, dpi = 300, bg = "white")


# 28. COMBINED FIGURES ----

# 28A. Value of Information (VOI) ----
# EVPI per person, population EVPI, and EVPPI at equal panel sizes.

p_voi_combined <- (p_evpi | p_evpi_pop) / p_evppi +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(
    title    = "Value of information analysis",
    subtitle = "A. EVPI per person; B. Population EVPI; C. EVPPI by parameter",
    tag_levels = "A",
    theme = theme(
      plot.title    = element_text(face = "bold", size = 16, colour = "#1F3A5F",
                                   family = graph_font),
      plot.subtitle = element_text(size = 10, colour = "#4F81BD",
                                   family = graph_font),
      plot.background = element_rect(fill = "white", colour = NA)
    )
  )

print(p_voi_combined)
ggsave(file.path(output_dir, "Fig_VOI_Combined.png"), p_voi_combined,
       width = 16, height = 12, dpi = 300, bg = "white")

# 28B. Probabilistic Sensitivity Analysis (PSA) Results ----
# CE plane, iNMB histogram, and CE classification by country.

p_psa_combined <- p_ceplane / (p_hist_nmb | p_quad_bar) +
  plot_layout(heights = c(3, 2)) +
  plot_annotation(
    title    = "Probabilistic sensitivity analysis results",
    subtitle = "A. Cost-effectiveness plane; B. Incremental NMB distribution; C. CE classification by country",
    tag_levels = "A",
    theme = theme(
      plot.title    = element_text(face = "bold", size = 16, colour = "#1F3A5F",
                                   family = graph_font),
      plot.subtitle = element_text(size = 10, colour = "#4F81BD",
                                   family = graph_font),
      plot.background = element_rect(fill = "white", colour = NA)
    )
  )

print(p_psa_combined)
ggsave(file.path(output_dir, "Fig_PSA_Combined.png"), p_psa_combined,
       width = 16, height = 14, dpi = 300, bg = "white")

# 28C. Decision Robustness Analysis ----
# CEAC, BIA waterfall, ELC, and CERAC.

p_decision_combined <- (p_ceac | p_bia_waterfall) / (p_elc | p_cerac) +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(
    title    = "Decision robustness analysis",
    subtitle = "A. CEAC; B. Budget impact analysis; C. Expected loss curves; D. CERAC",
    tag_levels = "A",
    theme = theme(
      plot.title    = element_text(face = "bold", size = 16, colour = "#1F3A5F",
                                   family = graph_font),
      plot.subtitle = element_text(size = 10, colour = "#4F81BD",
                                   family = graph_font),
      plot.background = element_rect(fill = "white", colour = NA)
    )
  )

print(p_decision_combined)
ggsave(file.path(output_dir, "Fig_Decision_Robustness_Combined.png"), p_decision_combined,
       width = 16, height = 14, dpi = 300, bg = "white")

# 28D. Deterministic Sensitivity Analysis ----
# Tornado diagram and both two-way sensitivity heatmaps.

p_dsa_combined <- p_tornado / (p_twoway_phi | p_twoway_amr) +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(
    title    = "Deterministic sensitivity analysis",
    subtitle = "A. Tornado diagram (OWSA); B. Two-way: vaccine efficacy vs incidence; C. Two-way: AMR proportion vs incidence",
    tag_levels = "A",
    theme = theme(
      plot.title    = element_text(face = "bold", size = 16, colour = "#1F3A5F",
                                   family = graph_font),
      plot.subtitle = element_text(size = 10, colour = "#4F81BD",
                                   family = graph_font),
      plot.background = element_rect(fill = "white", colour = NA)
    )
  )

print(p_dsa_combined)
ggsave(file.path(output_dir, "Fig_DSA_Combined.png"), p_dsa_combined,
       width = 16, height = 14, dpi = 300, bg = "white")


# 29. SUMMARY OF ALL OUTPUTS ----

cat("\n══════════════════════════════════════════\n")
cat("ALL ANALYSES COMPLETE\n")
cat("══════════════════════════════════════════\n")
cat("Figures saved:\n")
cat("  Fig_State_Occupancy.png\n")
cat("  Fig_Tornado_OWSA.png\n")
cat("  Fig_TwoWay_Phi_Inc.png\n")
cat("  Fig_TwoWay_AMR_Inc.png\n")
cat("  Fig_CE_Plane.png\n")
cat("  Fig_Histogram_iNMB.png\n")
cat("  Fig_Quadrant_Bar_ByCountry.png\n")
cat("  Fig_CEAC.png\n")
cat("  Fig_EVPI_PerPerson.png\n")
cat("  Fig_EVPI_Population.png\n")
cat("  Fig_EVPPI.png\n")
cat("  Fig_ELC.png\n")
cat("  Fig_CERAC.png\n")
cat("  Fig_BIA_Waterfall.png\n")
cat("\nCombined figures:\n")
cat("  Fig_VOI_Combined.png\n")
cat("  Fig_PSA_Combined.png\n")
cat("  Fig_Decision_Robustness_Combined.png\n")
cat("  Fig_DSA_Combined.png\n")
cat("\nTables saved:\n")
cat("  Table5_OWSA_Results\n")
cat("  Table6_WTP_Thresholds\n")
cat("  Table7_CEAC_Probabilities\n")
cat("  Table8_Scenario_Analysis\n")
cat("  Table9_BIA_ByCountry\n")
cat("\nAll outputs in:", output_dir, "\n")