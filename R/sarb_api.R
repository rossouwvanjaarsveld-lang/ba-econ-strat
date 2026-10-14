# =============================================================================
#  sarb_api.R -- transport layer for the SARB Web API
#
#  Endpoints that are confirmed to work (see website-code/caterpillar-3-4 and
#  nmd-dynamic, June-August 2026):
#
#    SarbData/IFData/GetAllTypes                          PA return catalogue
#    SarbData/IFData/GetPeriods/{form}                    periods for a form
#    SarbData/IFData/GetInstitutions/{form}/{period}      Id, Name per period
#    SarbData/IFData/GetInstitutionData/{form}/{period}/{id}
#                                                         JSON wrapper around
#                                                         an XMLData string
#    WebIndicators/ReleaseOfSelectedData                  DataType listing
#    WebIndicators/ReleaseOfSelectedData/MonthlyIndicatorsAll/{DataType}
#                                                         KBP time series
#
#  BA900 is reachable through GetInstitutionData even though GetAllTypes does
#  not list it (it lists the Prudential Authority BA100-BA700 family). Its
#  periods are month-END dates ("2015-06-30") and the sector aggregate has
#  Id "TOTAL"; BA1xx periods are month-START dates ("2013-01-01") and the
#  aggregate is the institution whose Name contains "Total".
#
#  Everything here needs internet access to custom.resbank.co.za, i.e. it is
#  run on the analyst's machine (data-raw/01_pull_data.R), never inside the
#  report render.
# =============================================================================

#' Base URL of the SARB Web API
#' @return Character scalar.
#' @export
sarb_base_url <- function() getOption("baeconstrat.sarb_base",
                                      "https://custom.resbank.co.za/SarbWebApi")

#' HTTP GET returning status and body text
#'
#' Uses the `curl` package when installed and falls back to base [url()].
#' Retries with linear back-off. Never throws: failures come back as
#' `status = NA` with `body = NULL`.
#' @param url Full URL.
#' @param accept Accept header.
#' @param tries Number of attempts.
#' @param timeout Seconds per attempt.
#' @param pause Seconds to sleep after a successful call (politeness).
#' @return `list(status = <int>, body = <chr or NULL>)`.
#' @export
http_get <- function(url, accept = "application/json", tries = 3L,
                     timeout = 120, pause = getOption("baeconstrat.pause", 0.3)) {
  have_curl <- requireNamespace("curl", quietly = TRUE)
  for (k in seq_len(tries)) {
    res <- tryCatch({
      if (have_curl) {
        h <- curl::new_handle(followlocation = TRUE, timeout = timeout,
                              connecttimeout = 30)
        curl::handle_setheaders(h, Accept = accept,
                                `User-Agent` = "R/baeconstrat (NGFB research)")
        r <- curl::curl_fetch_memory(url, handle = h)
        list(status = as.integer(r$status_code),
             body = rawToChar(r$content))
      } else {
        old <- options(timeout = timeout); on.exit(options(old), add = TRUE)
        con <- url(url, open = "rb", headers = c(Accept = accept))
        on.exit(try(close(con), silent = TRUE), add = TRUE)
        txt <- paste(readLines(con, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
        list(status = 200L, body = txt)
      }
    }, error = function(e) list(status = NA_integer_, body = NULL,
                                error = conditionMessage(e)))
    if (identical(res$status, 200L) && !is.null(res$body) && nzchar(res$body)) {
      if (pause > 0) Sys.sleep(pause)
      return(res)
    }
    if (!is.na(res$status) && res$status %in% c(400L, 404L)) return(res)  # no point retrying
    if (k < tries) Sys.sleep(1.5 * k)
  }
  res
}

#' GET a SARB Web API path and parse the JSON
#' @param path Path below [sarb_base_url()].
#' @param simplify Passed to [jsonlite::fromJSON()] as `simplifyDataFrame`.
#' @param quiet Suppress the failure message.
#' @return Parsed JSON or `NULL`.
#' @export
sarb_get_json <- function(path, simplify = TRUE, quiet = FALSE) {
  if (!requireNamespace("jsonlite", quietly = TRUE))
    stop("package 'jsonlite' is required for SARB downloads")
  url <- paste0(sarb_base_url(), "/", sub("^/", "", path))
  r <- http_get(url)
  if (!identical(r$status, 200L) || is.null(r$body)) {
    if (!quiet) ngfb_log("  ! %s (status %s)", path, r$status %||% "NA")
    return(NULL)
  }
  out <- tryCatch(jsonlite::fromJSON(r$body, simplifyDataFrame = simplify),
                  error = function(e) NULL)
  if (is.null(out) && !quiet) ngfb_log("  ! unparseable JSON: %s", path)
  out
}

#' Catalogue of IFData return types (BA100, BA120, ...)
#' @return Data frame with at least `Id`, `Description`, `Frequency`.
#' @export
sarb_if_types <- function() {
  x <- sarb_get_json("SarbData/IFData/GetAllTypes")
  if (is.null(x)) return(NULL)
  as.data.frame(x, stringsAsFactors = FALSE)
}

#' Reporting periods available for an IFData form
#' @param form Form code, e.g. `"BA900"`.
#' @return Sorted character vector of `YYYY-MM-DD` dates.
#' @export
sarb_if_periods <- function(form) {
  x <- sarb_get_json(sprintf("SarbData/IFData/GetPeriods/%s", form), quiet = TRUE)
  if (is.null(x)) return(character(0))
  v <- if (is.data.frame(x)) x[[1]] else unlist(x)
  v <- substr(as.character(v), 1, 10)
  sort(unique(v[grepl("^\\d{4}-\\d{2}-\\d{2}$", v)]))
}

#' Institutions reporting a form in a period
#' @param form Form code.
#' @param period Period string as returned by [sarb_if_periods()].
#' @return Data frame with `Id` and `Name` (character), or `NULL`.
#' @export
sarb_if_institutions <- function(form, period) {
  x <- sarb_get_json(sprintf("SarbData/IFData/GetInstitutions/%s/%s", form, period),
                     quiet = TRUE)
  if (is.null(x)) return(NULL)
  d <- as.data.frame(x, stringsAsFactors = FALSE)
  if (!nrow(d)) return(NULL)
  if (!"Id" %in% names(d))   names(d)[1] <- "Id"
  if (!"Name" %in% names(d)) names(d)[min(2L, ncol(d))] <- "Name"
  d$Id <- as.character(d$Id); d$Name <- trimws(as.character(d$Name))
  d
}

#' Raw XML string of one institution's return for one period
#' @param form,period,iid Form code, period and institution Id.
#' @return Character scalar of XML, or `NULL`.
#' @export
sarb_if_institution_xml <- function(form, period, iid) {
  raw <- sarb_get_json(sprintf("SarbData/IFData/GetInstitutionData/%s/%s/%s",
                               form, period, utils::URLencode(iid, reserved = TRUE)),
                       quiet = TRUE)
  if (is.null(raw)) return(NULL)
  xs <- if (is.data.frame(raw)) raw$XMLData[1] else (raw$XMLData %||% raw[[1]]$XMLData)
  if (is.null(xs) || is.na(xs) || !nzchar(xs)) return(NULL)
  as.character(xs)
}

#' DataType codes offered by the "release of selected data" service
#' @return Character vector of DataType codes (e.g. `"MRDMA"`).
#' @export
sarb_release_datatypes <- function() {
  x <- sarb_get_json("WebIndicators/ReleaseOfSelectedData")
  if (is.null(x)) return(character(0))
  d <- as.data.frame(x, stringsAsFactors = FALSE)
  unique(as.character(d$DataType %||% d[[1]]))
}

#' Monthly indicator payload (KBP time series) for one DataType
#' @param datatype A code from [sarb_release_datatypes()].
#' @return Data frame of raw records, or `NULL`.
#' @export
sarb_monthly_indicators <- function(datatype) {
  x <- sarb_get_json(sprintf("WebIndicators/ReleaseOfSelectedData/MonthlyIndicatorsAll/%s",
                             utils::URLencode(datatype, reserved = TRUE)), quiet = TRUE)
  if (is.null(x)) return(NULL)
  d <- tryCatch(as.data.frame(x, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(d) || !nrow(d)) NULL else d
}
