# =============================================================================
#  benchmark.R -- the 3-month floating benchmark through JIBAR's cessation
#
#  JIBAR is published for the last time on 31 December 2026 (SARB Market
#  Practitioners Group, announced 3 December 2025). New JIBAR-linked contracts
#  have been prohibited since 1 May 2026, apart from trades that reduce
#  existing JIBAR exposure; cleared JIBAR swaps are converted into ZARONIA
#  overnight-index swaps at the clearing houses on 20-21 November 2026; and
#  contracts that still reference 3-month JIBAR after the cessation fall back
#  to ZARONIA compounded in arrears plus a fixed credit adjustment spread
#  (CAS) of 16.19 bp -- the five-year median of 3-month JIBAR less compounded
#  ZARONIA, fixed on 3 December 2025.
#
#  The engine's floating reference ("float3m") is therefore 3-month JIBAR up
#  to 2026Q4 and compounded ZARONIA + CAS from 2027Q1, for floating-rate
#  assets, floating funding and the floating leg of the structural hedge.
#  Quarterly averages stand in for both: the average of daily ZARONIA over a
#  quarter is its compounded rate over the quarter up to compounding
#  convexity (well under 1 bp at 7%).
#
#  Because the CAS is a long-run median of the JIBAR-ZARONIA basis, the
#  switch is close to level-neutral on average. What it changes is the risk:
#  the basis no longer floats (JIBAR's credit and term premium widened in
#  stress; the CAS does not), and borrowers may negotiate the CAS away when
#  loans are re-papered.
# =============================================================================

#' JIBAR cessation and the ZARONIA fallback
#'
#' @return List: `last_fixing` (date of the final 3-month JIBAR publication),
#'   `cas` (3-month credit adjustment spread, percentage points),
#'   `announced`, `new_contracts_banned` and `ccp_conversion` (dates).
#' @export
jibar_transition <- function() {
  list(last_fixing = as.Date("2026-12-31"), cas = 0.1619,
       announced = as.Date("2025-12-03"), new_contracts_banned = as.Date("2026-05-01"),
       ccp_conversion = as.Date(c("2026-11-20", "2026-11-21")))
}

#' Spreads of the floating benchmarks over the policy rate
#'
#' JIBAR's spread is measured on the macro panel's own series (the one the
#' models were estimated on). ZARONIA's is measured on daily data where the
#' panel carries rbond's policy rate (`repo_rb`, averaged over the same days
#' as ZARONIA), because the panel's quarterly policy rate averages month-end
#' values and runs ahead of the daily average in a quarter with a rate change.
#'
#' @param hist Quarterly macro panel with `repo`, `jibar3m` and, where
#'   available, `zaronia` and `repo_rb`.
#' @param n Number of most recent quarters for the medians.
#' @return Named numeric (percentage points): `jibar` (3-month JIBAR less
#'   repo), `zaronia` (ZARONIA less repo), `cas`.
#' @export
benchmark_spreads <- function(hist, n = 8L) {
  h <- utils::tail(hist[is.finite(hist$repo), , drop = FALSE], n)
  jb <- stats::median(h$jibar3m - h$repo, na.rm = TRUE)
  if (!is.finite(jb)) jb <- 0.35
  zr <- NA_real_
  if ("zaronia" %in% names(hist)) {
    rp <- if ("repo_rb" %in% names(hist)) hist$repo_rb else hist$repo
    ok <- is.finite(hist$zaronia) & is.finite(rp)
    if (any(ok)) zr <- stats::median(utils::tail(hist$zaronia[ok] - rp[ok], n))
  }
  if (!is.finite(zr)) zr <- -0.13
  c(jibar = jb, zaronia = zr, cas = jibar_transition()$cas)
}

#' The 3-month floating benchmark along a path of policy rates
#'
#' @param date Quarter-end dates.
#' @param repo Policy rate (percent), quarterly averages.
#' @param spreads Output of [benchmark_spreads()].
#' @return Data frame: `zaronia` (overnight, quarterly average), `float3m`
#'   (3-month JIBAR up to the last fixing, then compounded ZARONIA + CAS) and
#'   `benchmark` (which of the two).
#' @export
float_benchmark <- function(date, repo, spreads) {
  post <- as.Date(date) > jibar_transition()$last_fixing
  z <- repo + spreads[["zaronia"]]
  data.frame(zaronia = z, float3m = ifelse(post, z + spreads[["cas"]], repo + spreads[["jibar"]]),
             benchmark = ifelse(post, "ZARONIA + CAS", "JIBAR"), stringsAsFactors = FALSE)
}

#' Value of a frame's 3-month floating benchmark
#'
#' `float3m` where the frame carries it; otherwise 3-month JIBAR (`jibar3m`,
#' then `jibar`), which is what the benchmark was over the whole history.
#' @param x Data frame or list of rates.
#' @return Numeric vector (percent), or `NULL` when there is none.
#' @export
float_rate <- function(x) x[["float3m"]] %||% x[["jibar3m"]] %||% x[["jibar"]]
