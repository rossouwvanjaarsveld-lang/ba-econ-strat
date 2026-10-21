# =============================================================================
#  data-raw/04_pull_swaps.R -- ZAR swap and money-market rates
#
#  RUN ON YOUR OWN MACHINE (rbond.co.za must be reachable):
#     double-click  data-raw\04_pull_swaps.bat
#  or, in R/RStudio with the working directory at the package root:
#     source("data-raw/04_pull_swaps.R")
#
#  Daily JIBAR interest-rate swap fixings (1, 2, 3, 5 and 10 years, NACQ,
#  3-month JIBAR floating leg), 3-month JIBAR, ZARONIA, the repo rate and
#  generic government bond yields since 2018, written to data-raw/rbond/.
#  The structural hedge needs real swap rates: the book's fixed rate is the
#  average 5-year swap rate at which it was struck.
#  Data: rbond.co.za (CC BY 4.0).
# =============================================================================
if (!file.exists("DESCRIPTION")) {
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grepl("^--file=", args)])
  if (length(f)) setwd(normalizePath(file.path(dirname(f[1]), "..")))
}
if (!file.exists("DESCRIPTION") || !dir.exists("R"))
  stop("Run this from the BA-econ-strat package root (the folder holding DESCRIPTION).")
dir.create("data-raw/rbond", showWarnings = FALSE, recursive = TRUE)
ids <- c("SASW1", "SASW2", "SASW3", "SASW5", "SASW10", "JIBAR3M", "ZARONIA", "REPO", "ZARGB3", "ZARGB5", "ZARGB10",
         "FRA3X6")
from <- "2018-01-01"; to <- format(Sys.Date())
log <- character(0)
for (id in ids) {
  url <- sprintf("https://rbond.co.za/api/v1/rates/%s?from=%s&to=%s&fmt=csv", id, from, to)
  f <- file.path("data-raw/rbond", paste0(id, ".csv"))
  ok <- tryCatch({ utils::download.file(url, f, mode = "wb", quiet = TRUE); TRUE },
                 error = function(e) { message(id, ": ", conditionMessage(e)); FALSE })
  n <- if (ok && file.exists(f)) length(readLines(f, warn = FALSE)) else 0L
  msg <- sprintf("%s: %s (%d lines)", id, if (ok) "ok" else "FAILED", n)
  message(msg); log <- c(log, msg)
  Sys.sleep(1.2)                          # the anonymous limit is 60 requests a minute
}
writeLines(c(format(Sys.time()), "Data: rbond.co.za (CC BY 4.0)", log), "data-raw/rbond/PULL_LOG.txt")
message("done: data-raw/rbond/")
