# =============================================================================
#  utils.R -- small shared helpers: logging, null-coalescing, dates, guards
#
#  Every other file in R/ may depend on these. They are base R only so that
#  data-raw/01_pull_data.R can source() the package files on a Windows machine
#  where the package itself is not installed.
# =============================================================================

#' Null / NA coalescing operator
#'
#' Returns `b` when `a` is `NULL`, zero-length, or a single `NA`.
#' @param a,b Objects.
#' @return `a` or `b`.
#' @name null-coalesce
#' @export
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0L || (length(a) == 1L && is.na(a))) b else a
}

#' Write a timestamped log line to the console and (optionally) a log file
#'
#' The log file is taken from `getOption("baeconstrat.logfile")`; when unset,
#' messages go to the console only.
#' @param fmt A [sprintf()] format string.
#' @param ... Arguments for `fmt`.
#' @return Invisibly, the formatted line.
#' @export
ngfb_log <- function(fmt, ...) {
  line <- sprintf("[%s] %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                  sprintf(fmt, ...))
  cat(line, "\n", sep = "")
  lf <- getOption("baeconstrat.logfile")
  if (!is.null(lf) && nzchar(lf)) {
    try(cat(line, "\n", sep = "", file = lf, append = TRUE), silent = TRUE)
  }
  invisible(line)
}

#' First day of the month for a date-like vector
#' @param x Date or character coercible with [as.Date()].
#' @return Date vector.
#' @export
month_start <- function(x) as.Date(format(as.Date(substr(as.character(x), 1, 10)), "%Y-%m-01"))

#' Calendar quarter label ("2024Q3") for a date-like vector
#' @param x Date or character.
#' @return Character vector.
#' @export
quarter_label <- function(x) {
  d <- as.Date(substr(as.character(x), 1, 10))
  sprintf("%s Q%d", format(d, "%Y"), (as.integer(format(d, "%m")) - 1L) %/% 3L + 1L)
}

#' Last day of the calendar quarter containing each date
#' @param x Date or character.
#' @return Date vector of quarter-end dates.
#' @export
quarter_end <- function(x) {
  d  <- as.Date(substr(as.character(x), 1, 10))
  y  <- as.integer(format(d, "%Y"))
  qe <- ((as.integer(format(d, "%m")) - 1L) %/% 3L + 1L) * 3L
  nxt <- as.Date(sprintf("%04d-%02d-01", ifelse(qe == 12L, y + 1L, y),
                         ifelse(qe == 12L, 1L, qe + 1L)))
  nxt - 1L
}

#' Is a date the last month of a calendar quarter?
#' @param x Date or character.
#' @return Logical vector.
#' @export
is_quarter_end_month <- function(x) {
  as.integer(format(as.Date(substr(as.character(x), 1, 10)), "%m")) %in% c(3L, 6L, 9L, 12L)
}

#' Stop with a formatted message unless a condition holds
#'
#' A `stopifnot()` with a human-readable message. Used as the render-failing
#' guard on every published number, per the house rule that drift must fail
#' loudly rather than print a wrong figure.
#' @param cond Logical scalar.
#' @param fmt,... Message passed to [sprintf()].
#' @return Invisibly `TRUE`.
#' @export
guard <- function(cond, fmt, ...) {
  if (!isTRUE(all(cond))) stop("guard failed: ", sprintf(fmt, ...), call. = FALSE)
  invisible(TRUE)
}

#' Safe division (returns NA where the denominator is zero or non-finite)
#' @param a,b Numeric vectors.
#' @return Numeric vector.
#' @export
safe_div <- function(a, b) {
  out <- a / b
  out[!is.finite(out)] <- NA_real_
  out
}

#' Row-bind a list of data frames whose columns may differ
#' @param lst List of data frames (NULL entries are dropped).
#' @return A data frame (or `NULL` when nothing remains).
#' @export
rbind_fill <- function(lst) {
  lst <- Filter(function(d) !is.null(d) && NROW(d) > 0L, lst)
  if (!length(lst)) return(NULL)
  cols <- Reduce(union, lapply(lst, names))
  out <- lapply(lst, function(d) {
    for (m in setdiff(cols, names(d))) d[[m]] <- NA
    d[, cols, drop = FALSE]
  })
  do.call(rbind, out)
}

#' Lag a vector by k positions (padding with NA)
#' @param x Vector.
#' @param k Non-negative integer lag.
#' @return Vector of the same length.
#' @export
lag_vec <- function(x, k = 1L) {
  if (k == 0L) return(x)
  n <- length(x)
  if (k >= n) return(rep(NA, n))
  c(rep(NA, k), x[seq_len(n - k)])
}

#' Create a directory (recursively, quietly) and return its path
#' @param path Directory path.
#' @return `path`, invisibly.
#' @export
ensure_dir <- function(path) {
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  invisible(path)
}

#' Format a number as basis points text
#' @param x Numeric in decimal (0.0125 = 125 bps).
#' @param digits Digits after the decimal point.
#' @return Character.
#' @export
fmt_bps <- function(x, digits = 0) sprintf(paste0("%+.", digits, "f bps"), 1e4 * x)

#' Format a decimal as a percentage string
#' @param x Numeric decimal.
#' @param digits Digits.
#' @return Character.
#' @export
fmt_pct <- function(x, digits = 1) sprintf(paste0("%.", digits, "f%%"), 100 * x)

#' Number of days in the month of each date
#' @param d Dates.
#' @return Integer vector.
#' @export
days_in_month <- function(d) {
  d <- month_start(d)
  y <- as.integer(format(d, "%Y")); mo <- as.integer(format(d, "%m"))
  nxt <- as.Date(sprintf("%04d-%02d-01", y + (mo == 12L), mo %% 12L + 1L))
  as.integer(nxt - d)
}
