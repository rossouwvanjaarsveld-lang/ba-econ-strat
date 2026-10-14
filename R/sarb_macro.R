# =============================================================================
#  sarb_macro.R -- SARB KBP time series from the MonthlyIndicatorsAll service
#
#  The "release of selected data" route is not dashboard-shaped: each DataType
#  carries a slice of the KBP time-series database, some series back to 1946
#  (inventory built in caterpillar-3-4/sarb_inventory.R). Field names vary a
#  little between payloads, so columns are matched from candidate lists.
# =============================================================================

.kbp_fields <- list(
  code = c("TimeSeriesCode", "TimeseriesCode", "TsCode", "Code"),
  desc = c("Description", "Name", "SubTitle", "Indicator"),
  per  = c("Period", "Date", "ValueDate"),
  val  = c("Value", "TheValue")
)

.pick_field <- function(d, cand) {
  i <- intersect(cand, names(d))
  if (length(i)) i[1] else NA_character_
}

#' Convert a raw MonthlyIndicatorsAll payload into a tidy long frame
#'
#' Dates are re-keyed to month-start (SARB stamps month-end). Duplicated
#' (series, date) pairs keep the first occurrence.
#' @param d Raw data frame from [sarb_monthly_indicators()].
#' @param source Label recorded in the output (e.g. the DataType).
#' @return Data frame `series, date, value, description, source`.
#' @export
tidy_kbp_payload <- function(d, source = NA_character_) {
  if (is.null(d) || !nrow(d)) return(NULL)
  f <- lapply(.kbp_fields, function(cand) .pick_field(d, cand))
  if (is.na(f$code) || is.na(f$per) || is.na(f$val)) return(NULL)
  out <- data.frame(
    series      = as.character(d[[f$code]]),
    date        = month_start(d[[f$per]]),
    value       = sarb_num(d[[f$val]]),
    description = if (is.na(f$desc)) NA_character_ else
      gsub("\\s+", " ", trimws(as.character(d[[f$desc]]))),
    source      = source,
    stringsAsFactors = FALSE)
  out <- out[!is.na(out$date) & !is.na(out$value), , drop = FALSE]
  out <- out[!duplicated(out[, c("series", "date")]), , drop = FALSE]
  out[order(out$series, out$date), , drop = FALSE]
}

#' Read every cached MonthlyIndicatorsAll payload into one long frame
#' @param dir Directory holding `sub_MonthlyIndicatorsAll_*.rds` files (the
#'   caterpillar cache layout) and/or `kbp_*.rds` files (this package's layout).
#' @return Data frame `series, date, value, description, source`.
#' @export
read_kbp_cache <- function(dir) {
  fs <- list.files(dir, pattern = "^(sub_MonthlyIndicatorsAll_|kbp_).*\\.rds$",
                   full.names = TRUE)
  if (!length(fs)) return(NULL)
  lst <- lapply(fs, function(f) {
    src <- sub("^(sub_MonthlyIndicatorsAll_|kbp_)", "", sub("\\.rds$", "", basename(f)))
    tidy_kbp_payload(readRDS(f), source = src)
  })
  d <- rbind_fill(lst)
  if (is.null(d)) return(NULL)
  # the same code can sit in several DataTypes; keep the longest history
  n_by <- stats::aggregate(list(n = d$value), list(series = d$series, source = d$source), length)
  n_by <- n_by[order(n_by$series, -n_by$n), ]
  best <- n_by[!duplicated(n_by$series), c("series", "source")]
  d <- merge(d, best, by = c("series", "source"))
  d[order(d$series, d$date), c("series", "date", "value", "description", "source")]
}

#' Extract one or more series (wide, by date) from a tidy KBP frame
#' @param kbp Output of [read_kbp_cache()].
#' @param codes Character vector of series codes; names become column names.
#' @return Data frame with `date` plus one column per code.
#' @export
kbp_wide <- function(kbp, codes) {
  nm <- if (is.null(names(codes))) codes else ifelse(nzchar(names(codes)), names(codes), codes)
  out <- NULL
  for (i in seq_along(codes)) {
    s <- kbp[kbp$series == codes[[i]], c("date", "value")]
    names(s)[2] <- nm[i]
    out <- if (is.null(out)) s else merge(out, s, by = "date", all = TRUE)
  }
  out[order(out$date), , drop = FALSE]
}
