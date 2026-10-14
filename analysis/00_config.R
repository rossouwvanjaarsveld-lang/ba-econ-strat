# =============================================================================
#  analysis/00_config.R -- settings shared by every analysis step
#
#  Run from the package root. Every later script sources this file first.
# =============================================================================
suppressMessages(library(baeconstrat))

CFG <- list(
  raw        = "data-raw",
  out        = "output",
  sample_from = as.Date("2008-06-30"),   # first quarter with a q/q growth rate
  date0      = as.Date("2026-06-30"),    # jump-off: last BA900 quarter
  horizon    = 12L,                      # projection quarters (3 years)
  seed       = ngfb_canonical()$seed,    # 20260622
  lags       = 0:4,                      # admissible driver lags (quarters)
  max_k      = 3L,                       # max macro drivers per satellite
  h_oos      = 4L,                       # dynamic OOS horizon for selection
  min_train  = 28L,                      # first OOS origin after 7 years
  ic_window  = 4L,                       # intercept correction on ratio targets
  ic_families = c("pnl", "credit", "pricing"),
  ic_mode    = "level",                  # shift the projected path (see 08_model_checks.R)
  tax        = NA_real_,                 # NA = sector effective rate (set in 04)
  payout     = 0.50,
  cet1_min   = 0.115,                    # management floors for the optimiser:
  lcr_min    = 1.20,                     # buffers above the 100% LCR minimum
  ## Published sector liquidity ratios at the calibration month (June 2025):
  ## Prudential Authority, "Selected South African banking sector trends",
  ## June 2025 -- LCR 149.59%, NSFR 117.29%, CET1 13.25%.
  lcr_sector  = 1.4959,
  nsfr_sector = 1.1729,
  cet1_sector = 0.1325,
  ## Baseline policy and inflation paths, quarterly averages from 2026Q3.
  ## Sources: SARB MPC statements (repo 7.00% after May 2026, +25 bps to
  ## 7.25% on 24 Sep 2026; inflation expected above 5% around the turn of the
  ## year and back at the 3% target by late 2027); 3% target with a 1pp band
  ## announced 12 Nov 2025. The easing path after mid-2027 is an assumption.
  repo_path  = c(7.02, 7.25, 7.25, 7.25, 7.00, 6.75, 6.50, 6.50, 6.25, 6.25, 6.25, 6.25),
  cpi_path   = c(4.40, 5.00, 5.10, 4.60, 3.90, 3.30, 3.10, 3.00, 3.00, 3.00, 3.00, 3.00),
  anchors    = list(repo = 6.25, gov10y = 9.25, cpi_yy = 3.0, gdp_yy = 1.6, unemp = 32.0,
                    hpi_yy = 4.0, zar_yy = 2.0, alsi_yy = 9.0),
  ## judgemental scenario probabilities, skewed to the downside; used only
  ## for the probability-weighted (robust) positioning
  scenario_weights = c(baseline = 0.50, upside = 0.20, downside = 0.30),
  ## economic profit of share strategies: capital held at a 12.5% CET1
  ## target (11.5% management floor plus a 1pp buffer) and charged at a
  ## 14.5% cost of equity (the 10-year bond near 9-9.5% plus an equity risk
  ## premium of about 5%); both are assumptions, not estimates
  cet1_target = 0.125,
  coe = 0.145,
  frontier_q = 0.80,                      # "frontier" = a top-quintile large bank
  ## monthly models (09_monthly.R): admissible lags in months, the dynamic
  ## pseudo-out-of-sample horizon, the first origin, the out-of-sample
  ## screen, and the polynomial-lag length
  m_lags = c(0L, 1L, 2L, 3L, 6L, 9L, 12L),
  m_h_oos = 12L,
  m_min_train = 84L,
  m_oos_top = 300L,
  m_pdl_L = 12L
)
dir.create(CFG$out, showWarnings = FALSE, recursive = TRUE)
out_path <- function(f) file.path(CFG$out, f)
