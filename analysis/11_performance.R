# =============================================================================
#  analysis/11_performance.R -- macro-adjusted performance measurement
#
#  1. Scenario scorecards: in each scenario and product, NGFB's growth split
#     into the market (macro, Stage 1) and its share effect (Stage 2), with
#     the share effect compared with the large-bank population: par (the
#     typical bank), the frontier (a top-quintile bank) and NGFB's
#     percentile. "Could NGFB have done better?" is the gap to the frontier.
#  2. Against the baseline: how much of each scenario's change in NGFB's
#     growth is the market (no bank controls it) and how much is share.
#  3. History: the industry's growth against what the macro environment
#     implied (each Stage 1 volume model simulated four quarters ahead along
#     the realised macro path) -- macro par for the industry as a whole.
# =============================================================================
source("analysis/00_config.R")
P  <- readRDS(out_path("panels.rds"))
S1 <- readRDS(out_path("stage1.rds"))
S2 <- readRDS(out_path("stage2.rds"))
N  <- readRDS(out_path("ngfb.rds"))
SC <- readRDS(out_path("scenarios.rds"))
ST <- readRDS(out_path("strategy.rds"))
prods <- model_products()

## ---- 1. scorecards --------------------------------------------------------------
cards <- do.call(rbind, lapply(names(SC$results), function(s) {
  r <- SC$results[[s]]$ngfb
  sc <- scenario_scorecard(N$share_params, S2$population_hyper, r$regimes$regime,
                           r$proj[match(r$regimes$date, r$proj$date), ], S2$mean_g, prods,
                           frontier_q = CFG$frontier_q)
  sc$scenario <- s
  sc
}))
guard(all(is.finite(cards$percentile)), "a scorecard percentile is missing")
guard(all(abs(cards$ngfb_growth - cards$market_growth - cards$share_effect) < 1e-10),
      "scorecard growth does not add up")
## consistency with the engine: NGFB's book growth in the scenario runs is the
## market plus the same share effect
chk <- do.call(rbind, lapply(names(SC$results), function(s) {
  r <- SC$results[[s]]$ngfb; g <- r$inputs$growth
  data.frame(scenario = s, product = "mortgages_res",
             engine = mean(400 * log(g$mortgages_res)),
             card = cards$ngfb_growth[cards$scenario == s & cards$product == "mortgages_res"])
}))
guard(max(abs(chk$engine - chk$card)) < 1e-8, "scorecard and engine growth differ")

## ---- 2. against the baseline -------------------------------------------------------
b <- cards[cards$scenario == "baseline", c("product", "market_growth", "share_effect", "ngfb_growth")]
vs_base <- do.call(rbind, lapply(setdiff(names(SC$results), "baseline"), function(s) {
  x <- cards[cards$scenario == s, ]
  x <- merge(x, b, by = "product", suffixes = c("", ".base"))
  data.frame(scenario = s, product = x$product,
             d_growth = x$ngfb_growth - x$ngfb_growth.base,
             d_market = x$market_growth - x$market_growth.base,
             d_share = x$share_effect - x$share_effect.base,
             percentile = x$percentile, verdict = x$verdict, stringsAsFactors = FALSE)
}))
guard(all(abs(vs_base$d_growth - vs_base$d_market - vs_base$d_share) < 1e-10), "attribution does not add up")

## ---- 3. the industry against macro par ------------------------------------------------
vol <- names(S1$sats)[vapply(S1$sats, function(s) identical(s$family, "volume"), TRUE)]
hist_par <- do.call(rbind, lapply(vol, function(nm) {
  s <- S1$sats[[nm]]
  m <- macro_implied_growth(s, satellite_panel(s, P$s1pan), window = 4L)
  if (is.null(m)) return(NULL)
  m$target <- nm; m$gap <- m$actual - m$implied
  m
}))
guard(nrow(hist_par) > 100, "too few macro-par windows")
recent <- stats::aggregate(cbind(actual, implied, gap) ~ target,
                           hist_par[hist_par$date > CFG$date0 - 3 * 365, ], mean)

saveRDS(list(scorecards = cards, vs_baseline = vs_base, history = hist_par, recent = recent,
             frontier_q = CFG$frontier_q), out_path("performance.rds"))
ngfb_log("performance: %d scorecard rows; %d macro-par windows", nrow(cards), nrow(hist_par))
