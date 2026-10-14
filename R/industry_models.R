# =============================================================================
#  industry_models.R -- the Stage 1 target registry and fitting loop
#
#  Each target names its dependent variable, the candidate macro drivers and
#  the a-priori sign of each driver. The signs encode economics, not
#  convenience; where theory is ambiguous the sign is left free (0) and the
#  report says so.
#
#    volumes   : annualised q/q log growth of sector balances (BA900 TOTAL)
#    pricing   : sector lending spread over prime/JIBAR and deposit spread
#                under repo (BA120/BA100 implied rates)
#    credit    : credit-loss ratio (impairment charge / average loans)
#    other P&L : non-interest revenue and operating cost growth
# =============================================================================

#' Macro-driver groups (one per group per specification)
#' @return Named character vector driver -> group.
#' @export
driver_groups <- function() {
  c(gdp_yy = "activity", coinc_yy = "activity", lead_yy = "activity", gdp_neg = "activity_kink",
    repo = "rates", d_repo = "rates", d4_repo = "rates", real_repo = "rates",
    repo_pos = "rates_up", repo_neg = "rates_down", d_repo_pos = "rates_change_up",
    d_repo_neg = "rates_change_down",
    slope = "curve", gov10y = "curve", d_gov10y = "curve",
    cpi_yy = "prices", unemp = "labour", d4_unemp = "labour",
    hpi_yy = "housing", zar_yy = "fx", alsi_yy = "equity")
}

#' Stage 1 target registry
#'
#' @return Named list; each element has `target`, `label`, `candidates`,
#'   `signs` and `family` ("volume", "pricing", "credit", "pnl").
#' @export
stage1_targets <- function() {
  v <- function(target, label, signs, family = "volume")
    list(target = target, label = label, candidates = names(signs), signs = signs, family = family)
  list(
    g_mortgages_res = v("g_mortgages_res", "Residential mortgages",
                        c(gdp_yy = 1, hpi_yy = 1, repo = -1, d_repo = -1, d4_repo = -1,
                          real_repo = -1, d4_unemp = -1, cpi_yy = 1)),
    g_mortgages_com = v("g_mortgages_com", "Commercial mortgages",
                        c(gdp_yy = 1, hpi_yy = 1, repo = -1, d_repo = -1, d4_repo = -1,
                          real_repo = -1, alsi_yy = 1, cpi_yy = 1)),
    g_vaf = v("g_vaf", "Vehicle and asset finance",
              c(gdp_yy = 1, repo = -1, d_repo = -1, d4_repo = -1, real_repo = -1, d4_unemp = -1,
                cpi_yy = 1, zar_yy = 0)),
    g_cards = v("g_cards", "Credit cards",
                c(gdp_yy = 1, cpi_yy = 1, repo = 0, d_repo = 0, d4_repo = 0, d4_unemp = -1,
                  alsi_yy = 0)),
    g_personal_other = v("g_personal_other", "Personal and other retail loans",
                         c(gdp_yy = 1, d4_unemp = -1, repo = -1, d_repo = -1, d4_repo = -1,
                           real_repo = -1, cpi_yy = 1)),
    g_corp_all = v("g_corp_all", "Corporate loans and overdrafts",
                   c(gdp_yy = 1, repo = 0, d_repo = 0, d4_repo = 0, slope = 0, zar_yy = 1,
                     alsi_yy = 0, cpi_yy = 1)),
    g_nmd = v("g_nmd", "Non-maturity deposits (operational, savings, notice)",
              c(gdp_yy = 1, repo = 0, d_repo = 0, d4_repo = 0, real_repo = 0, cpi_yy = 1,
                alsi_yy = 0)),
    g_beh_term = v("g_beh_term", "Term deposits (over 1 month)",
                   c(gdp_yy = 1, repo = 1, d_repo = 1, d4_repo = 1, real_repo = 1, cpi_yy = 1,
                     slope = 0, alsi_yy = -1))
  )
}

#' Fit every Stage 1 satellite
#'
#' @param panel Quarterly panel holding targets and macro drivers.
#' @param targets Target registry (default [stage1_targets()]).
#' @param controls Control dummies always included (COVID-quarter dummies
#'   apply to volume targets only: the 2020 credit-loss spike is macro news).
#' @param impulse_k Threshold (robust SDs) for automatic impulse dummies on
#'   each target; `Inf` disables them.
#' @param ... Passed to [satellite_search()].
#' @return Named list of `baeconstrat_satellite` objects.
#' @export
fit_stage1 <- function(panel, targets = stage1_targets(), controls = character(0),
                       impulse_k = 4, ...) {
  out <- list()
  for (nm in names(targets)) {
    tg <- targets[[nm]]
    if (!tg$target %in% names(panel)) next
    pz <- panel
    ctl <- controls
    if (is.finite(impulse_k)) {
      ok <- is.finite(pz[[tg$target]])
      imp <- impulse_dummies(pz[[tg$target]], pz$date, k = impulse_k, prefix = paste0("imp_", nm))
      # COVID quarters are handled by the explicit controls
      drop <- grepl("202006$|202009$", names(imp)) & any(c("covid_q2", "covid_q3") %in% ctl)
      imp <- imp[, !drop, drop = FALSE]
      if (ncol(imp)) { pz <- cbind(pz, imp); ctl <- c(ctl, names(imp)) }
    }
    if (!identical(tg$family, "volume")) ctl <- setdiff(ctl, c("covid_q2", "covid_q3"))
    out[[nm]] <- satellite_search(pz, tg$target, tg$candidates, tg$signs,
                                  groups = tg$groups %||% driver_groups(), controls = ctl,
                                  required = tg$required %||% character(0), ...)
    out[[nm]]$label <- tg$label; out[[nm]]$family <- tg$family
    out[[nm]]$impulses <- setdiff(ctl, controls)
  }
  out
}

#' Tidy summary of fitted satellites
#' @param sats Named list of satellites.
#' @return Data frame: target, label, specification, n, R2, OOS RMSE, AR RMSE,
#'   skill.
#' @export
stage1_summary <- function(sats) {
  do.call(rbind, lapply(names(sats), function(nm) {
    s <- sats[[nm]]
    data.frame(target = nm, label = s$label %||% nm,
               spec = if (length(s$spec$d)) paste(sprintf("%s[L%d]", s$spec$d, s$spec$l), collapse = " + ")
                      else "AR(1) only",
               n = s$fit$n, r2 = s$fit$r2, rmse_oos = s$oos$rmse,
               rmse_ar = s$benchmark_rmse, skill = s$skill,
               n_valid = s$n_valid %||% 0L,
               n_specs = s$n_specs, stringsAsFactors = FALSE)
  }))
}
