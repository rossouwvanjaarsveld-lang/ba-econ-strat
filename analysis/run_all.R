# =============================================================================
#  analysis/run_all.R -- run the whole pipeline in order
#
#  From the package root, after the data pull and `install.packages(".",
#  repos = NULL, type = "source")`:
#      source("analysis/run_all.R")
#  Writes output/*.rds; the report reads them.
# =============================================================================
steps <- c("01_build_panels.R", "02_stage1.R", "03_stage2.R", "04_ngfb.R",
           "05_scenarios.R", "06_optimise.R", "07_backtest.R", "08_model_checks.R",
           "09_monthly.R", "09b_dynamics.R", "10_share_strategy.R", "11_performance.R")
t0 <- Sys.time()
for (s in steps) {
  message(sprintf("== %s ==", s))
  sys.source(file.path("analysis", s), envir = new.env())
}
message(sprintf("pipeline finished in %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
