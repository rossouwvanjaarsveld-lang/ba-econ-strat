# =============================================================================
#  fred.R -- macro series from FRED (OECD MEI / BIS / IMF re-publications)
#
#  FRED fills the gaps the SARB monthly-indicator service does not cover at
#  usable length: quarterly real GDP, the unemployment rate and residential
#  property prices. Each code is optional; the panel builder uses whatever
#  arrived and the report states what was missing.
# =============================================================================

#' Default FRED series used by the macro panel
#'
#' Names are the panel's variable names; values are FRED codes. Several
#' alternatives are listed for the same concept because OECD/BIS codes on FRED
#' are occasionally discontinued.
#' @return Named character vector.
#' @export
fred_default_series <- function() {
  c(gdp_real         = "NGDPRSAXDCZAQ",     # real GDP, s.a., quarterly (IMF IFS)
    gdp_real_nsa     = "NGDPRNSAXDCZAQ",    # real GDP, n.s.a. (IMF IFS)
    gdp_real_oecd_qq = "NAEXKP01ZAQ657S",   # real GDP growth q/q (OECD MEI)
    gdp_nominal      = "ZAFGDPNQDSMEI",     # nominal GDP, s.a. (OECD MEI)
    unemp_rate       = "LRUNTTTTZAQ156S",   # unemployment rate, s.a. (OECD)
    house_px_nominal = "QZAN628BIS",        # residential property prices (BIS)
    house_px_real    = "QZAR628BIS",        # real residential property prices
    gov10y           = "IRLTLT01ZAM156N",   # 10y government bond yield
    jibar3m          = "IR3TIB01ZAM156N",   # 3m interbank rate
    cpi_index        = "ZAFCPIALLMINMEI",   # CPI all items, index
    share_px         = "SPASTT01ZAM661N",   # share prices, index
    hh_debt_gdp      = "HDTGPDZAQ163N",     # household debt, % GDP (BIS)
    pnfs_credit_gdp  = "QZAPAM770A",        # credit to private non-fin sector, % GDP
    usdzar           = "EXSFUS",            # ZAR per USD, monthly average
    brent            = "POILBREUSDM",       # Brent, USD/bbl
    us10y            = "GS10",              # US 10y Treasury
    vix              = "VIXCLS")            # CBOE VIX (daily)
}

#' Download one FRED series as a data frame
#' @param code FRED series id.
#' @return Data frame `date, value` or `NULL`.
#' @export
fred_fetch <- function(code) {
  url <- sprintf("https://fred.stlouisfed.org/graph/fredgraph.csv?id=%s", code)
  r <- http_get(url, accept = "text/csv", tries = 3L, timeout = 60, pause = 0.2)
  if (!identical(r$status, 200L) || is.null(r$body)) return(NULL)
  parse_fred_csv(r$body)
}

#' Parse the text of a fredgraph.csv response
#'
#' Handles both the old `DATE` and the newer `observation_date` header, and
#' FRED's "." missing-value marker.
#' @param txt CSV text.
#' @return Data frame `date, value` or `NULL`.
#' @export
parse_fred_csv <- function(txt) {
  d <- tryCatch(utils::read.csv(text = txt, stringsAsFactors = FALSE,
                                na.strings = c(".", "", "NA")),
                error = function(e) NULL)
  if (is.null(d) || ncol(d) < 2L || !nrow(d)) return(NULL)
  out <- data.frame(date = as.Date(d[[1]]), value = suppressWarnings(as.numeric(d[[2]])))
  out <- out[!is.na(out$date) & !is.na(out$value), , drop = FALSE]
  if (!nrow(out)) NULL else out
}

#' Read a directory of cached FRED CSVs (one file per code)
#' @param dir Directory with `<code>.csv` files written by the pull.
#' @param series Named vector mapping panel names to codes.
#' @return Long data frame `name, code, date, value`.
#' @export
read_fred_cache <- function(dir, series = fred_default_series()) {
  lst <- lapply(seq_along(series), function(i) {
    f <- file.path(dir, paste0(series[[i]], ".csv"))
    if (!file.exists(f)) return(NULL)
    d <- parse_fred_csv(paste(readLines(f, warn = FALSE), collapse = "\n"))
    if (is.null(d)) return(NULL)
    data.frame(name = names(series)[i], code = series[[i]], d, stringsAsFactors = FALSE)
  })
  rbind_fill(lst)
}
