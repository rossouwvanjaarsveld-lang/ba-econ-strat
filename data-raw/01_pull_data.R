# =============================================================================
#  data-raw/01_pull_data.R -- download every raw input the package needs
#
#  RUN ON YOUR OWN MACHINE (the SARB API and FRED must be reachable):
#
#     double-click  data-raw\00_pull_data.bat
#  or, in R/RStudio with the working directory at the package root:
#     source("data-raw/01_pull_data.R")
#
#  The package does not need to be installed: this script sources R/*.R.
#  Everything lands under data-raw/ (sarb/, macro/, fred/, manifest.csv) and a
#  log is written to data-raw/logs/. Interrupt and re-run at will: every BA
#  return is cached per (form, period, institution) and is never re-fetched.
#
#  Expected run time: 25-45 minutes, dominated by ~1,300 BA900 calls.
# =============================================================================

## -- locate the package root ---------------------------------------------------
if (!file.exists("DESCRIPTION")) {
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grepl("^--file=", args)])
  if (length(f)) setwd(normalizePath(file.path(dirname(f[1]), "..")))
}
if (!file.exists("DESCRIPTION") || !dir.exists("R"))
  stop("Run this from the BA-econ-strat package root (the folder holding DESCRIPTION).")

## -- dependencies (only what the download needs) ------------------------------
need <- c("jsonlite", "xml2", "curl")
miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss)) {
  message("installing: ", paste(miss, collapse = ", "))
  utils::install.packages(miss, repos = "https://cloud.r-project.org")
}

## -- source the package functions --------------------------------------------
for (f in c("utils.R", "sarb_api.R", "sarb_forms.R", "sarb_macro.R", "fred.R", "pull.R"))
  sys.source(file.path("R", f), envir = globalenv())

dir.create("data-raw/logs", showWarnings = FALSE, recursive = TRUE)
logfile <- file.path("data-raw/logs", format(Sys.time(), "pull_%Y%m%d_%H%M%S.log"))
options(baeconstrat.logfile = logfile, baeconstrat.pause = 0.3)
file.copy(logfile, "data-raw/logs/pull_latest.log", overwrite = TRUE)

ngfb_log("R %s on %s | wd %s", getRversion(), R.version$platform, getwd())

## -- connectivity check (fail fast with a clear message) ------------------------
chk <- http_get(paste0(sarb_base_url(), "/WebIndicators/HomePageRates"), tries = 2L)
if (!identical(chk$status, 200L)) {
  ngfb_log("! cannot reach the SARB API (status %s). Check the internet connection / proxy.",
           chk$status %||% "NA")
  stop("SARB API unreachable")
}
ngfb_log("SARB API reachable")

res <- tryCatch(pull_all(root = "data-raw", start = "2008-01-01"),
                error = function(e) { ngfb_log("! pull stopped: %s", conditionMessage(e)); NULL })
if (!is.null(res)) print(res, row.names = FALSE)
file.copy(logfile, "data-raw/logs/pull_latest.log", overwrite = TRUE)
ngfb_log("log: %s", logfile)
