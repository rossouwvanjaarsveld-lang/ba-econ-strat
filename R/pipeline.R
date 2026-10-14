# =============================================================================
#  pipeline.R -- glue: raw caches -> panels -> models -> NGFB -> scenarios
#
#  Each step is a plain function returning a list, so analysis/ scripts can
#  run them in sequence, cache the results, and the report can read a single
#  results object.
# =============================================================================

#' Composite product lines used for modelling
#'
#' BA900's 1 day-1 month bucket swings with December window-dressing, so the
#' three non-maturity categories are modelled together (NMD); overdrafts are
#' mostly corporate and are modelled with corporate loans.
#' @param x Panel with BA900 lines.
#' @return `x` with `nmd`, `corp_all` (and `loans_total`) added.
#' @export
add_composites <- function(x) {
  z <- function(v) if (v %in% names(x)) { y <- x[[v]]; y[is.na(y)] <- 0; y } else 0
  x$nmd <- z("beh_operational") + z("beh_savings") + z("beh_notice")
  x$corp_all <- z("corporate_loans") + z("overdrafts")
  x$loans_total <- z("mortgages_res") + z("mortgages_com") + z("vaf") + z("cards") +
    z("personal_other") + z("corporate_loans") + z("overdrafts")
  x
}

#' Products carried through Stage 1 and Stage 2
#' @return Character vector.
#' @export
model_products <- function() {
  c("mortgages_res", "mortgages_com", "vaf", "cards", "personal_other", "corp_all",
    "nmd", "beh_term")
}

#' Engine line -> modelled product (whose growth drives it)
#' @return Named character vector.
#' @export
line_driver_map <- function() {
  c(mortgages_res = "mortgages_res", mortgages_com = "mortgages_com", vaf = "vaf",
    cards = "cards", personal_other = "personal_other", corporate_loans = "corp_all",
    overdrafts = "corp_all", beh_operational = "nmd", beh_savings = "nmd",
    beh_notice = "nmd", beh_term = "beh_term")
}

#' Robust impulse dummies for outlying quarters
#'
#' Flags quarters whose AR(1) innovation is more than `k` robust standard
#' deviations (MAD) from the median -- in BA900 these are reclassifications
#' and corporate events (e.g. the African Bank good-bank/bad-bank split in
#' 2016Q2), not macro news.
#' @param y Numeric series.
#' @param dates Matching dates.
#' @param k Threshold in robust standard deviations.
#' @param prefix Column-name prefix.
#' @return Data frame of 0/1 columns (possibly with zero columns).
#' @export
impulse_dummies <- function(y, dates, k = 4, prefix = "imp") {
  # outliers are judged on AR(1) innovations, not on levels: a high deposit
  # cost in 2008 is a high-rate regime, not an error
  yl <- lag_vec(y, 1L)
  ok <- is.finite(y) & is.finite(yl)
  e <- rep(NA_real_, length(y))
  if (sum(ok) > 10L) {
    b <- stats::lm.fit(cbind(1, yl[ok]), y[ok])$coefficients
    e[ok] <- y[ok] - b[1] - b[2] * yl[ok]
  }
  med <- stats::median(e, na.rm = TRUE)
  s <- stats::mad(e, na.rm = TRUE)
  idx <- which(is.finite(e) & abs(e - med) > k * s)
  out <- as.data.frame(matrix(0, length(y), length(idx)))
  if (length(idx)) {
    names(out) <- paste0(prefix, "_", format(dates[idx], "%Y%m"))
    for (j in seq_along(idx)) out[idx[j], j] <- 1
  }
  out
}

#' Build the quarterly Stage 1 panel (macro drivers + sector targets)
#'
#' @param gp Group panel (with a "Sector" entity).
#' @param macro Macro panel ([build_macro_panel()]).
#' @param spnl Sector P&L panel ([sector_pnl_panel()]).
#' @param end Last quarter of the estimation sample.
#' @return Data frame by quarter.
#' @export
stage1_panel <- function(gp, macro, spnl, end = NULL) {
  gp <- add_composites(gp)
  sec <- gp[gp$entity == "Sector", ]
  sec <- sec[order(sec$date), ]
  out <- data.frame(date = sec$date)
  for (p in c(model_products(), "total_assets", "loans_total", "dep_retail", "dep_corporate",
              "dep_financial", "overdrafts", "corporate_loans"))
    out[[paste0("g_", p)]] <- growth_qq(sec[[p]])
  out <- merge(out, macro, by = "date", all.x = TRUE)
  keep <- c("date", "nim", "clr", "nir_assets", "opex_assets", "cost_to_income", "roe", "tax_rate",
            "y_homeloans", "y_commercial", "y_vaf", "y_cards", "y_overdrafts", "y_term_loans",
            "y_loans", "y_st_sec", "y_gov_sec", "c_current", "c_savings", "c_call", "c_fixed_notice",
            "c_ncd", "c_deposits", "c_term_debt", "leverage_iea")
  out <- merge(out, spnl[, intersect(keep, names(spnl))], by = "date", all.x = TRUE)
  ## spreads to reference rates (percentage points)
  out$s_homeloans <- out$y_homeloans - out$prime
  out$s_commercial <- out$y_commercial - out$prime
  out$s_vaf <- out$y_vaf - out$prime
  out$s_cards <- out$y_cards - out$prime
  out$s_overdrafts <- out$y_overdrafts - out$prime
  out$s_term_loans <- out$y_term_loans - out$jibar3m
  out$covid_q2 <- as.numeric(out$date == as.Date("2020-06-30"))
  out$covid_q3 <- as.numeric(out$date == as.Date("2020-09-30"))
  if (!is.null(end)) out <- out[out$date <= as.Date(end), ]
  out[order(out$date), ]
}

#' Stage 1 targets for pricing, credit and other P&L lines
#'
#' Deposit costs are anchored to the repo rate (it enters every
#' specification). Two refinements of the deposit-cost equations are tested
#' in `analysis/08_model_checks.R` and set here:
#'
#' * `deposit_dynamics = "ardl11"` lets the repo LEVEL and its quarterly
#'   CHANGE enter together, i.e. an ARDL(1,1) whose impact and long-run
#'   pass-through can differ. `"restricted"` is the original registry, in
#'   which the level and the change belong to one driver group and never
#'   enter together.
#' * for the deposit categories listed in `asymmetric` the repo level is
#'   split into partial sums of increases and decreases (Shin, Yu and
#'   Greenwood-Nimmo, 2014), so hikes and cuts can pass through at different
#'   speeds and to different long-run levels; for the loan spreads listed,
#'   the quarterly change in the repo rate is split into rises and falls.
#'
#' `convex_credit = TRUE` adds GDP growth below zero (`gdp_neg`) as a
#' candidate driver of the credit-loss ratio, so losses can respond more
#' steeply in a contraction than they fall in an expansion. It is not part of
#' the core (it improves the pseudo-out-of-sample fit but not the back-test)
#' and is used as a scenario sensitivity.
#' @param asymmetric Pricing targets (deposit costs or loan spreads) with
#'   asymmetric repo pass-through; `character(0)` gives the symmetric registry.
#' @param deposit_dynamics `"ardl11"` or `"restricted"` (see above).
#' @param convex_credit Add the contraction kink to the credit-loss ratio.
#' @return Target registry in the format of [stage1_targets()]; elements may
#'   carry their own `groups` (driver -> group map).
#' @export
stage1_pnl_targets <- function(asymmetric = c("c_savings", "s_cards"),
                               deposit_dynamics = c("ardl11", "restricted"),
                               convex_credit = FALSE) {
  deposit_dynamics <- match.arg(deposit_dynamics)
  v <- function(target, label, signs, family, required = character(0))
    list(target = target, label = label, candidates = names(signs), signs = signs, family = family,
         required = required)
  grp <- driver_groups()
  if (deposit_dynamics == "ardl11") grp[["d_repo"]] <- "rates_change"
  dep <- function(target, label, extra = c()) {
    tg <- if (target %in% asymmetric)
      v(target, label, c(repo_pos = 1, repo_neg = 1, d_repo = 0, extra), "pricing",
        required = c("repo_pos", "repo_neg"))
    else v(target, label, c(repo = 1, d_repo = 0, extra), "pricing", required = "repo")
    tg$groups <- grp
    tg
  }
  spr <- function(target, label, signs) {
    if (target %in% asymmetric && "d_repo" %in% names(signs)) {
      sg <- signs[["d_repo"]]
      signs <- c(signs[names(signs) != "d_repo"], d_repo_pos = sg, d_repo_neg = sg)
    }
    v(target, label, signs, "pricing")
  }
  clr_signs <- c(d4_unemp = 1, unemp = 1, gdp_yy = -1, repo = 1, d_repo = 1, real_repo = 1,
                 hpi_yy = -1, alsi_yy = -1)
  if (convex_credit) clr_signs <- c(clr_signs, gdp_neg = -1)
  list(
    clr = v("clr", "Credit-loss ratio (% of average loans)", clr_signs, "credit"),
    nir_assets = v("nir_assets", "Non-interest revenue (% of average assets)",
                   c(gdp_yy = 1, alsi_yy = 1, cpi_yy = 1, zar_yy = 0, d4_unemp = -1), "pnl"),
    opex_assets = v("opex_assets", "Operating costs incl. indirect tax (% of average assets)",
                    c(cpi_yy = 1, gdp_yy = 0, zar_yy = 0), "pnl"),
    s_homeloans = spr("s_homeloans", "Home-loan yield less prime (pp)",
                      c(d_repo = 0, d4_unemp = 1, gdp_yy = 0, hpi_yy = 0)),
    s_commercial = spr("s_commercial", "Commercial-mortgage yield less prime (pp)",
                       c(d_repo = 0, d4_unemp = 1, gdp_yy = 0)),
    s_vaf = spr("s_vaf", "VAF yield less prime (pp)",
                c(d_repo = -1, repo = -1, d4_unemp = 1, gdp_yy = 0)),
    s_cards = spr("s_cards", "Card yield less prime (pp)",
                  c(d_repo = 0, repo = 0, d4_unemp = 1, gdp_yy = 0)),
    s_overdrafts = spr("s_overdrafts", "Overdraft yield less prime (pp)",
                       c(d_repo = 0, d4_unemp = 1, gdp_yy = 0)),
    s_term_loans = spr("s_term_loans", "Term-loan yield less JIBAR (pp)",
                       c(d_repo = 0, d4_unemp = 1, gdp_yy = 0, slope = 0)),
    c_current = dep("c_current", "Cost of current accounts (%)"),
    c_savings = dep("c_savings", "Cost of savings deposits (%)"),
    c_call = dep("c_call", "Cost of call deposits (%)"),
    c_fixed_notice = dep("c_fixed_notice", "Cost of fixed and notice deposits (%)", c(slope = 0)),
    c_ncd = dep("c_ncd", "Cost of NCDs (%)", c(slope = 0))
  )
}
