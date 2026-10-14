# =============================================================================
#  data-raw/02_pull_supplement.R -- top up FRED series missing from a pull
#
#  The first pull showed that the OECD real-GDP codes for South Africa are no
#  longer on FRED (HTTP 404). This fetches every code in fred_default_series()
#  that is not yet cached, plus fallbacks, without touching the SARB cache.
#
#     source("data-raw/02_pull_supplement.R")   # from the package root
# =============================================================================
if (!file.exists("DESCRIPTION")) {
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grepl("^--file=", args)])
  if (length(f)) setwd(normalizePath(file.path(dirname(f[1]), "..")))
}
stopifnot(file.exists("DESCRIPTION"))
for (f in c("utils.R", "sarb_api.R", "sarb_forms.R", "sarb_macro.R", "fred.R", "pull.R"))
  sys.source(file.path("R", f), envir = globalenv())
dir.create("data-raw/logs", showWarnings = FALSE, recursive = TRUE)
options(baeconstrat.logfile = file.path("data-raw/logs",
                                        format(Sys.time(), "supplement_%Y%m%d_%H%M%S.log")))
froot <- "data-raw/fred"; dir.create(froot, showWarnings = FALSE, recursive = TRUE)

extra <- c(fred_default_series(),
           gdp_real_fallback1 = "NAEXKP01ZAQ652S",
           gdp_real_fallback2 = "NAEXKP01ZAQ661S",
           gdp_real_fallback3 = "CLVMNACSAB1GQZA",
           gdp_deflator       = "ZAFGDPDEFQISMEI")
for (nm in names(extra)) {
  code <- extra[[nm]]
  dest <- file.path(froot, paste0(code, ".csv"))
  if (file.exists(dest)) { ngfb_log("  %-20s %-16s cached", nm, code); next }
  r <- http_get(sprintf("https://fred.stlouisfed.org/graph/fredgraph.csv?id=%s", code),
                accept = "text/csv", tries = 2L, timeout = 60, pause = 0.2)
  ok <- identical(r$status, 200L) && !is.null(parse_fred_csv(r$body %||% ""))
  if (ok) writeLines(r$body, dest)
  ngfb_log("  %-20s %-16s %s", nm, code, if (ok) "ok" else paste("failed", r$status %||% ""))
}
## BA900 top-up: institutions added to the pattern list after the first pull
## (Imperial Bank, absorbed by Nedbank; assigned pro-forma to its acquirer).
pv <- sarb_if_periods("BA900"); pv <- pv[pv >= "2008-01-01"]
pv <- unique(c(pv[is_quarter_end_month(pv)], max(pv)))
pull_form("BA900", pv, patterns = default_ba900_institutions()["IMPERIAL"], root = "data-raw/sarb")
d <- combine_form_cells("BA900", root = "data-raw/sarb", months = "quarter_end")
if (!is.null(d)) saveRDS(d, "data-raw/sarb/BA900_long.rds")
ngfb_log("supplement done")
writeLines(format(Sys.time()), "data-raw/logs/SUPPLEMENT_DONE.txt")
