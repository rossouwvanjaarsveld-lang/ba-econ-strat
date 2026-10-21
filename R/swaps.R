# =============================================================================
#  swaps.R -- the ZAR swap curve: rbond.co.za daily fixings -> quarterly
#             panel; term spreads over 3-month JIBAR (the relationship the
#             scenarios use to turn a policy path into swap rates); the
#             fixed rate of the structural hedge book at the jump-off
#
#  The swap data are JIBAR interest-rate swaps (SASW), the only curve with a
#  history. After JIBAR's last fixing on 31 December 2026 a new receive-fixed
#  hedge is a ZARONIA overnight-index swap; with the JIBAR-ZARONIA basis at
#  the 16.19 bp fallback spread, its rate is the JIBAR swap rate less the
#  spread and it pays ZARONIA, so its carry equals that of a JIBAR swap
#  against ZARONIA + 16.19 bp. The scenarios therefore quote swap rates on
#  the JIBAR-equivalent basis -- the floating benchmark (benchmark.R) plus
#  the term spread -- and the engine pays the same benchmark on the floating
#  leg, so both legs sit on one basis before and after the cessation.
#
#  Earlier versions priced the hedge off a proxy inside the government curve,
#  JIBAR + 0.55 x (10-year bond - JIBAR). South African government yields
#  carry a large fiscal premium over swaps (the 5-year bond traded 0.5-1.6pp
#  above the 5-year swap in 2023-26), so that proxy overstated swap rates by
#  0.5-2pp and with them the carry of the hedge.
# =============================================================================

#' Read rbond.co.za rate files into quarterly averages
#'
#' @param dir Folder with the CSV files written by `data-raw/04_pull_swaps.R`
#'   (one file per identifier, columns `date` and `yield_pct`).
#' @param min_days Minimum number of daily fixings for a period's average;
#'   partial periods at either end of the download are dropped.
#' @param freq "quarter" (dates are quarter ends) or "month" (month starts).
#' @return Data frame: `date` and, in percent, `swap1y`,
#'   `swap2y`, `swap3y`, `swap5y`, `swap10y` (JIBAR interest-rate swaps),
#'   `jibar3m_rb`, `zaronia`, `repo_rb` (the policy rate averaged over the same
#'   days), `fra3x6`, and generic government yields `gov3y`, `gov5y`,
#'   `gov10y_rb`. `NULL` when the folder holds none of the files.
#' @export
read_rbond_cache <- function(dir, min_days = NULL, freq = c("quarter", "month")) {
  freq <- match.arg(freq)
  if (is.null(min_days)) min_days <- if (freq == "quarter") 20L else 10L
  map <- c(SASW1 = "swap1y", SASW2 = "swap2y", SASW3 = "swap3y", SASW5 = "swap5y", SASW10 = "swap10y",
           JIBAR3M = "jibar3m_rb", ZARONIA = "zaronia", REPO = "repo_rb", FRA3X6 = "fra3x6", ZARGB3 = "gov3y",
           ZARGB5 = "gov5y", ZARGB10 = "gov10y_rb")
  parts <- list()
  for (id in names(map)) {
    f <- file.path(dir, paste0(id, ".csv"))
    if (!file.exists(f)) next
    x <- utils::read.csv(f, stringsAsFactors = FALSE)
    if (!nrow(x) || !all(c("date", "yield_pct") %in% names(x))) next
    d <- as.Date(substr(x$date, 1, 10)); v <- suppressWarnings(as.numeric(x$yield_pct))
    ok <- is.finite(v) & !is.na(d)
    if (!any(ok)) next
    q <- if (freq == "quarter") quarter_end(d[ok]) else month_start(d[ok])
    a <- stats::aggregate(list(v = v[ok]), list(date = q), mean)
    n <- stats::aggregate(list(n = v[ok]), list(date = q), length)
    a <- a[n$n >= min_days, , drop = FALSE]
    names(a)[2] <- map[[id]]
    parts[[id]] <- a
  }
  if (!length(parts)) return(NULL)
  out <- Reduce(function(a, b) merge(a, b, by = "date", all = TRUE), parts)
  out[order(out$date), , drop = FALSE]
}

#' Swap tenors used by the engine
#' @return Named numeric vector: column name -> tenor in years.
#' @export
swap_tenors <- function() c(swap1y = 1, swap2y = 2, swap3y = 3, swap5y = 5, swap10y = 10)

#' Term spreads of the swap curve over 3-month JIBAR
#'
#' The relationship the scenarios use to turn a policy path into swap
#' rates: swap(T) = benchmark + s(T), with s(T) the average spread of the
#' JIBAR swap over 3-month JIBAR in the quarters with swap data. A scenario
#' fixes the path of the floating benchmark (perfect foresight), and with it
#' the whole curve. Richer forms were tested in
#' `analysis/12_audit.R`: the government curve's slope helps only at 10 years
#' and the coming year's policy path only at one year, neither at the 3-7
#' year points where the hedge is struck.
#'
#' @param hist Quarterly macro panel with `jibar3m` and swap columns.
#' @param min_n Minimum number of quarters with both series.
#' @return Data frame: `col`, `tenor` (years), `spread` (mean, pp), `sd`,
#'   `min`, `max`, `n`, `from`, `to`; `NULL` when there are no swap data.
#' @export
swap_term_spreads <- function(hist, min_n = 4L) {
  ten <- swap_tenors()
  rows <- lapply(names(ten), function(k) {
    if (!k %in% names(hist) || !"jibar3m" %in% names(hist)) return(NULL)
    ok <- is.finite(hist[[k]]) & is.finite(hist$jibar3m)
    if (sum(ok) < min_n) return(NULL)
    s <- hist[[k]][ok] - hist$jibar3m[ok]
    data.frame(col = k, tenor = ten[[k]], spread = mean(s), sd = stats::sd(s), min = min(s), max = max(s),
               n = sum(ok), from = min(hist$date[ok]), to = max(hist$date[ok]), stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(NULL)
  do.call(rbind, rows)
}

#' Add a swap curve to a scenario (or any frame with a floating benchmark)
#'
#' Swap rates at 1, 2, 3, 5 and 10 years from the term spreads over the
#' 3-month floating benchmark ([float_rate()]: `float3m`, else `jibar3m`),
#' and 7 years interpolated linearly between 5 and 10. The rates are on the
#' JIBAR-equivalent basis (see the note at the top of swaps.R). Without swap
#' data in `hist` the 5-year rate falls back to the old proxy inside the
#' government curve (benchmark + 0.55 x slope).
#' @param s Data frame with `float3m` or `jibar3m` (and `gov10y` for the
#'   fallback).
#' @param hist Macro history holding the swap columns.
#' @return `s` with swap columns added.
#' @export
add_swap_curve <- function(s, hist) {
  fl <- float_rate(s)
  sp <- swap_term_spreads(hist)
  if (is.null(sp) || !"swap5y" %in% sp$col) {
    s$swap5y <- fl + 0.55 * (s$gov10y - fl)
    return(s)
  }
  for (k in seq_len(nrow(sp))) s[[sp$col[k]]] <- fl + sp$spread[k]
  if (all(c("swap5y", "swap10y") %in% names(s))) s$swap7y <- s$swap5y + 0.4 * (s$swap10y - s$swap5y)
  s
}

#' Fixed rate of a rolling receive-fixed book at the jump-off
#'
#' A boxcar of `tenor_q` equal quarterly tranches of swaps of the same tenor:
#' its fixed rate is the average swap rate over the last `tenor_q` quarters.
#' Quarters before the swap data start are back-cast from the government
#' yield of the same tenor less the average swap spread to it over the
#' quarters where both exist (the government curve is published from 2018).
#'
#' @param hist Quarterly macro panel up to the jump-off.
#' @param tenor_q Book length in quarters (20 = a 5-year boxcar).
#' @param swap,gov Swap and government-yield columns of the same tenor.
#' @return List: `fixed0` (percent), `quarters` (date, rate, source),
#'   `gov_spread` (mean swap - government yield over the overlap), `n_actual`,
#'   `n_backcast`, `proxy` (the old government-curve proxy over the same
#'   quarters, for comparison).
#' @export
hedge_fixed0 <- function(hist, tenor_q = 20L, swap = "swap5y", gov = "gov5y") {
  h <- hist[is.finite(hist$jibar3m), , drop = FALSE]
  h <- utils::tail(h[order(h$date), , drop = FALSE], tenor_q)
  sw <- if (swap %in% names(h)) h[[swap]] else rep(NA_real_, nrow(h))
  gv <- if (gov %in% names(h)) h[[gov]] else rep(NA_real_, nrow(h))
  both <- is.finite(hist[[swap]] %||% NA) & is.finite(hist[[gov]] %||% NA)
  gs <- if (any(both)) mean(hist[[swap]][both] - hist[[gov]][both]) else NA_real_
  rate <- sw; src <- ifelse(is.finite(sw), "swap", NA_character_)
  bc <- !is.finite(sw) & is.finite(gv) & is.finite(gs)
  rate[bc] <- gv[bc] + gs; src[bc] <- "back-cast"
  proxy <- h$jibar3m + 0.55 * (h$gov10y - h$jibar3m)
  miss <- !is.finite(rate)
  rate[miss] <- proxy[miss]; src[miss] <- "proxy"
  list(fixed0 = mean(rate), quarters = data.frame(date = h$date, rate = rate, source = src, proxy = proxy,
                                                  stringsAsFactors = FALSE),
       gov_spread = gs, n_actual = sum(src == "swap"), n_backcast = sum(src == "back-cast"),
       n_proxy = sum(src == "proxy"), proxy = mean(proxy))
}

#' Merge quarterly rbond rates into the macro panel
#' @param macro Quarterly macro panel ([build_macro_panel()]).
#' @param rb Output of [read_rbond_cache()] (or `NULL`).
#' @return `macro` with the swap, ZARONIA, FRA and generic government columns
#'   added (attributes kept).
#' @export
add_rbond <- function(macro, rb) {
  if (is.null(rb) || !nrow(rb)) return(macro)
  at <- attributes(macro)[c("sources")]
  keep <- setdiff(names(rb), names(macro))
  m <- merge(macro, rb[, c("date", keep), drop = FALSE], by = "date", all.x = TRUE, sort = TRUE)
  src <- at$sources %||% character(0)
  src[keep] <- "rbond.co.za (CC BY 4.0)"
  attr(m, "sources") <- src
  m
}
