# =============================================================================
#  data-raw/03_pull_monthly.R -- add the months between quarter ends to BA900
#
#  RUN ON YOUR OWN MACHINE (the SARB API must be reachable):
#
#     double-click  data-raw\03_pull_monthly.bat
#  or, in R/RStudio with the working directory at the package root:
#     source("data-raw/03_pull_monthly.R")
#
#  The quarterly pull fetched BA900 for quarter-end months only. This fetches
#  every other month since 2008 for the same institutions, skipping cells that
#  are already cached (so it can be interrupted and re-run), and writes
#  data-raw/sarb/BA900m_long.rds: every month, only the items the package maps.
#  It first pulls the sector balance sheet (BA100, one institution) for every
#  month, written to data-raw/sarb/BA100m_long.rds, so that monthly yields and
#  deposit costs can be formed. The quarterly files are not touched.
#
#  Expected run time: 60-100 minutes (about 3,300 new BA900 calls).
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
for (f in c("utils.R", "sarb_api.R", "sarb_forms.R", "sarb_macro.R", "fred.R", "pull.R",
            "map_ba900.R"))
  sys.source(file.path("R", f), envir = globalenv())

dir.create("data-raw/logs", showWarnings = FALSE, recursive = TRUE)
logfile <- file.path("data-raw/logs", format(Sys.time(), "monthly_%Y%m%d_%H%M%S.log"))
options(baeconstrat.logfile = logfile, baeconstrat.pause = 0.3)

ngfb_log("R %s on %s | wd %s", getRversion(), R.version$platform, getwd())

## -- connectivity check (fail fast with a clear message) ------------------------
chk <- http_get(paste0(sarb_base_url(), "/WebIndicators/HomePageRates"), tries = 2L)
if (!identical(chk$status, 200L)) {
  ngfb_log("! cannot reach the SARB API (status %s). Check the internet connection / proxy.",
           chk$status %||% "NA")
  stop("SARB API unreachable")
}
ngfb_log("SARB API reachable")

res <- tryCatch(pull_ba900_monthly(root = "data-raw", start = "2008-01-01"),
                error = function(e) { ngfb_log("! monthly pull stopped: %s", conditionMessage(e)); NULL })
file.copy(logfile, "data-raw/logs/monthly_latest.log", overwrite = TRUE)
if (!is.null(res)) writeLines(format(Sys.time()), "data-raw/logs/MONTHLY_DONE.txt")
ngfb_log("log: %s", logfile)
