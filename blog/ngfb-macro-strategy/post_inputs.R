# Inputs for "The Tide and the Swimming" (and its long version, "Sixty Histories and a Risk Budget").
#
# post_artefacts.rds is written by blog/build_post_snapshot.R in the
# baeconstrat package, after analysis/run_all.R. It carries results (scenario
# outcomes, positioning, levers, the strategy lab, the risk-budgeted optimiser)
# and the public market data behind the figures. It carries nothing from the
# industry models' internals, which this post deliberately leaves out.
#
# validate_post() fails the render if a rebuild moves a number far enough to
# change a sentence in the post.

load_post <- function() {
  for (p in c("post_artefacts.rds", file.path("artefacts", "post_artefacts.rds")))
    if (file.exists(p)) return(suppressWarnings(readRDS(p)))
  stop("post_artefacts.rds not found: copy it from baeconstrat/blog/ngfb-macro-strategy/")
}

`%||%` <- function(a, b) if (is.null(a)) b else a

## ---- formatting -------------------------------------------------------------
## Money in the snapshot is in R'000, as in the BA returns.
pct  <- function(x, d = 1) sprintf(paste0("%.", d, "f%%"), 100 * x)
bps  <- function(x, d = 0) { v <- round(1e4 * x, d); v[v == 0] <- 0; sprintf(paste0("%+.", d, "f bps"), v) }
ubps <- function(x, d = 0) sprintf(paste0("%.", d, "f bps"), abs(1e4 * x))
num  <- function(x, d = 1) formatC(x, format = "f", digits = d, big.mark = ",")
rbn  <- function(x, d = 0) paste0("R", formatC(abs(x) / 1e6, format = "f", digits = d, big.mark = ","), "bn")
srbn <- function(x, d = 1) { v <- round(x / 1e6, d); v[v == 0] <- 0
  paste0(ifelse(v < 0, "-R", "+R"), formatC(abs(v), format = "f", digits = d, big.mark = ","), "bn") }
rmn  <- function(x) paste0("R", formatC(round(abs(x) / 1e3), format = "d", big.mark = ","), "m")
srmn <- function(x) { v <- round(x / 1e3); v[v == 0] <- 0
  paste0(ifelse(v < 0, "-R", "+R"), formatC(abs(v), format = "d", big.mark = ","), "m") }
pp   <- function(x, d = 1) sprintf(paste0("%.", d, "f pp"), abs(x))        # x already in points

## ---- labels -------------------------------------------------------------------
SC_LAB <- c(baseline = "Baseline", upside = "Upside", downside = "Downside",
            downside_cuts = "Downside with cuts")
PROD_LAB <- c(mortgages_res = "Home loans", mortgages_com = "Commercial mortgages",
              vaf = "Vehicle and asset finance", cards = "Credit cards",
              personal_other = "Personal loans", corp_all = "Corporate loans",
              nmd = "Non-maturity deposits", beh_term = "Term deposits",
              dep_retail = "Household deposits")

## ---- the harness ----------------------------------------------------------------
## Every qualitative claim in the post, checked against the snapshot.
validate_post <- function(S) {
  st <- S$scen$sumtab; rownames(st) <- st$scenario
  at <- S$scen$attribution
  a <- function(s, e = "NGFB", ch = "total") at[at$scenario == s & at$entity == e, ch]
  se <- S$scen$sens; rownames(se) <- se$shock
  sn <- S$scen$sens_nohedge; rownames(sn) <- sn$shock
  ps <- S$pos$positions; g <- S$pos$gain
  ld <- S$lab$dist[S$lab$dist$phi == 0, ]; rownames(ld) <- ld$strategy
  rb <- S$rb; cen <- rb$variants[rb$variants$variant == "Every setting", ]
  rg <- rb$regime$all; rownames(rg) <- rg$regime
  cy <- rb$carry; cyt <- cy$spread_frontier[is.finite(cy$spread_frontier$spread_step), ]
  lv <- S$levers$summary
  lvs <- function(l, s) lv[lv$lever == l & lv$setting == s, ]
  pb <- S$share$playbook
  sc <- S$share$scorecards
  pr <- S$audit$proxy
  stopifnot(
    ## the bank and the scenarios
    "NGFB earns less than the sector, the hedge helps, losses are higher" =
      st["baseline", "ngfb"] < st["baseline", "sector"] && st["baseline", "ngfb_nohedge"] < st["baseline", "ngfb"] &&
      st["baseline", "clr"] > st["baseline", "sector_clr"],
    "upside: NGFB gains through credit first, the sector loses its endowment" =
      a("upside") > 0 && a("upside", "Sector") < 0 && a("upside", "Sector", "pricing") < 0 &&
      a("upside", "NGFB", "credit") > a("upside", "NGFB", "pricing") && a("upside", "NGFB", "pricing") > 0,
    "downside: NGFB loses through credit first and pricing second; the sector's endowment offsets its losses" =
      a("downside") < 0 && a("downside", "NGFB", "credit") < a("downside", "NGFB", "pricing") &&
      a("downside", "NGFB", "pricing") < 0 && a("downside", "Sector", "pricing") > 0 &&
      a("downside", "Sector", "credit") < 0 && a("downside", "Sector") > 0,
    "downside with cuts: the hedge protects NGFB and the sector loses more" =
      a("downside_cuts", "NGFB", "pricing") > 0 && a("downside_cuts", "Sector", "pricing") < 0 &&
      a("downside_cuts") > a("downside") && a("downside_cuts", "Sector") < a("downside_cuts"),
    "capital and liquidity do not bind in the scenarios" =
      min(st$cet1_min) > S$meta$cet1_min + 0.005 && min(st$lcr_min) > 1.6,
    ## sensitivities
    "hedged NGFB is liability-sensitive, unhedged asset-sensitive" =
      se["repo_up_100", "total"] < 0 && sn["repo_up_100", "total"] > 0 && se["repo_down_100", "total"] > 0,
    "growth is the largest single exposure, almost all credit" =
      abs(se["gdp_down_1", "total"]) == max(abs(se$total)) &&
      abs(se["gdp_down_1", "credit"]) > 0.9 * abs(se["gdp_down_1", "total"]),
    "no unemployment channel" = se["unemp_up_2", "credit"] == 0 && abs(se["unemp_up_2", "total"]) < 5e-4,
    ## positioning
    "downside positioning directions" =
      ps["downside", "hedge"] < 0 && ps["downside", "into_cards"] < 0 && ps["downside", "into_personal"] < 0 &&
      ps["downside", "fund_savings"] > 0 && ps["downside", "fund_operational"] > 0,
    "upside positioning directions" =
      ps["upside", "hedge"] > 0 && ps["upside", "into_cards"] > 0 && ps["upside", "into_personal"] > 0 &&
      ps["upside", "fund_savings"] < 0 && ps["upside", "fund_operational"] < 0,
    "gain if right, cost if wrong" =
      g["downside", "downside"] > 0 && g["downside", "baseline"] < 0 && g["downside", "upside"] < 0 &&
      g["upside", "upside"] > 0 && g["upside", "baseline"] < 0 && g["upside", "downside"] < 0,
    "being wrong costs more than being right earns" =
      -g["downside", "upside"] > 2 * g["downside", "downside"] && -g["upside", "downside"] > 3 * g["upside", "upside"],
    "positioning recovers a small part of the downside" = g["downside", "downside"] < abs(a("downside")) / 10,
    "the hedge flips with the policy response; unsecured cuts do not" =
      S$pos$cuts[["hedge"]] > 0 && S$pos$cuts[["into_personal"]] < 0 && S$pos$cuts[["into_cards"]] < 0,
    "doubling the moves wipes out the gain" =
      abs(S$pos$size$gain[S$pos$size$alpha == 2 & S$pos$size$realised == "downside"]) < 2e-4,
    ## share strategy and scorecards
    "the push cells are easing-cycle cards, corporate loans and VAF" =
      setequal(paste(pb$product, pb$regime)[pb$action == "Push"], c("cards easing", "corp_all easing", "vaf easing")),
    "cards in a contraction are the guard case" = pb$action[pb$product == "cards" & pb$regime == "contraction"] == "Guard",
    "downside scorecards: cards near the frontier, home loans below par" =
      sc$percentile[sc$scenario == "downside" & sc$product == "cards"] > 0.7 &&
      sc$percentile[sc$scenario == "downside" & sc$product == "mortgages_res"] < 0.5,
    "upside: below par where the growth is" =
      all(sc$percentile[sc$scenario == "upside" & sc$product %in% c("cards", "corp_all", "vaf")] < 0.5),
    ## the audit
    "the proxy overstated the swap rate in every quarter" = all(pr$error > 0),
    "the swap correction is large" = S$audit$correction$d_roe[S$audit$correction$scenario == "baseline"] < -0.02,
    "unhedged asset-sensitive above the disclosure, hedged liability-sensitive" =
      S$audit$nii_sens$up_pct[2] > S$audit$disclosure[["sens_bn"]] / S$audit$disclosure[["nii_bn"]] &&
      S$audit$nii_sens$up_pct[1] < 0,
    "the switch to ZARONIA is level-neutral" = abs(S$audit$transition$switch_effect$d_roe[1]) < 5e-4,
    ## the levers
    "ten-year reinvestment and the spread levers pay in every scenario" =
      all(lvs("Reinvestment tenor", "10 years")[, c("d_roe_baseline", "d_roe_upside", "d_roe_downside", "d_roe_cuts")] > 0.005) &&
      all(S$levers$asset_swap$d_roe > 0.003) && all(S$levers$buffer$d_roe > 0.002),
    "the hedge ratio is a rate view" =
      lvs("Hedge ratio", "80%")$d_roe_baseline > 0 && lvs("Hedge ratio", "80%")$d_roe_downside < 0 &&
      lvs("Hedge ratio", "0%")$d_roe_downside > 0,
    "the asset swap's OCI risk is a fraction of the ten-year book's" =
      max(abs(S$levers$asset_swap$d_cet1_oci_min)) < max(abs(S$levers$hqla$d_cet1_oci_min)) / 3,
    "the combined levers fit only without fair-value marks" =
      all(S$levers$combo$cet1_min > S$meta$cet1_min) && min(S$levers$combo$cet1_min_fvoci) < S$meta$cet1_min,
    ## the strategy lab
    "the plan earns its cost of equity in one history" = round(ld["Plan", "coe_hit"] * S$rb$n_episodes) == 1,
    "R1 adds nothing; R6, R7 and the static spread trades beat the plan every time" =
      abs(ld["R1 hedge ratio with the policy rate", "d_roe_mean"]) < 1e-3 &&
      ld["R6 asset swap with the bond-swap spread", "win"] == 1 && ld["R7 liquidity buffer with stress", "win"] == 1 &&
      ld["25% of HQLA in asset-swapped 5-year bonds", "win"] == 1 && ld["Buffer at an LCR of 170%", "win"] == 1,
    "R3 earns most on its own and protects the capital trough better than a fixed holding" =
      ld["R3 HQLA duration with the curve's slope", "d_roe_mean"] > ld["25% of HQLA in 10-year bonds", "d_roe_mean"] &&
      ld["R3 HQLA duration with the curve's slope", "cet1_min_fv"] > ld["25% of HQLA in 10-year bonds", "cet1_min_fv"],
    "ten-year reinvestment beats the plan in every history" = ld["50% at 10 years", "win"] == 1,
    "all seven rules: big average, big EVE, thin capital" =
      ld["All seven rules", "d_roe_mean"] > 0.025 && ld["All seven rules", "d_eve200"] > 0.15 &&
      ld["All seven rules", "cet1_min_fv"] < 0.12,
    ## the risk-budgeted optimiser
    "the central portfolio" =
      identical(unname(rb$central$all), c("37%", "7 years", "12.5%", "50%", "LCR 160%", "R4")) &&
      cen$win == 1 && cen$cet1_min >= S$meta$cet1_min && cen$eve_max <= rb$b_central + 1e-3,
    "the spread trades are about two thirds of the gain and the same in every regime" =
      (rg["All episodes", "as"] + rg["All episodes", "buf"]) / rg["All episodes", "total"] > 0.6 &&
      (rg["All episodes", "as"] + rg["All episodes", "buf"]) / rg["All episodes", "total"] < 0.75 &&
      diff(range(rg$as)) < 5e-4 && diff(range(rg$buf)) < 5e-4,
    "the hedge and the bonds offset across the rate cycle" =
      rg["Rates rising", "hedge"] > 0 && rg["Rates falling", "hedge"] < 0 &&
      rg["Rates falling", "dur"] > rg["Rates rising", "dur"],
    "two limits bind: EVE and the liquidity-book cap" =
      rb$limits_used$value[5] > 0.9 * rb$b_central && rb$limits_used$value[6] > 0.88,
    "the surrogate is exact enough" = max(abs(rb$verify$roe_err)) < 5e-6,
    ## the carry book
    "the downside caps the carry book at R50bn; leverage never binds" =
      cy$recommended$carry == "R50bn" && !cyt$ok_scen[cyt$carry == "R75bn"] &&
      all(pmin(cyt$lev_min, cyt$lev_scen_min) > cy$lr_floor) && all(cyt$cet1_min > S$meta$cet1_min),
    "each R25bn of carry adds about 10 bps" = abs(cy$per_step[["roe"]] - 1e-3) < 2e-4,
    "the carry earns the same in every regime" = diff(range(cy$regime$carry)) < 3e-4,
    "history holds spread widenings as large as the downside's" =
      S$rb$carry_extra$widen_hist > S$rb$carry_extra$widen_down,
    "at R75bn the downside's CET1 trough breaks the floor" =
      cyt$cet1_downside[cyt$carry == "R75bn"] < S$meta$cet1_min,
    ## further claims in the text
    "NGFB earns less than the sector in every scenario" = all(st$ngfb < st$sector),
    "full-conviction positioning loses on average at every size" = all(S$pos$size$exp_gain < 0),
    "the probability-weighted positioning barely moves anything but the hedge" =
      max(abs(S$pos$robust_x[names(S$pos$robust_x) != "hedge"])) < 0.1,
    "the asset swap: a seventh of the OCI risk for about 40% of the return" = {
      r1 <- max(abs(S$levers$asset_swap$d_cet1_oci_min)) / max(abs(S$levers$hqla$d_cet1_oci_min))
      r2 <- lvs("Asset swap", "A quarter of HQLA in asset-swapped 5-year bonds")$d_roe_weighted /
        lvs("HQLA duration", "A quarter of HQLA in 10-year bonds")$d_roe_weighted
      r1 > 0.12 && r1 < 0.17 && r2 > 0.35 && r2 < 0.45 },
    "the buffer saves roughly nine bps per R10bn" = {
      b <- S$levers$buffer[S$levers$buffer$scenario == "baseline", ]
      all(1e4 * b$d_roe / (b$release / 1e7) > 8 & 1e4 * b$d_roe / (b$release / 1e7) < 10) },
    "R3 earns the most of any single rule" =
      which.max(ld[grepl("^R[1-7] ", rownames(ld)), "d_roe_mean"]) == which(grepl("^R3 ", rownames(ld)[grepl("^R[1-7] ", rownames(ld))])),
    "R4 raises the capital trough above the plan's" =
      ld["R4 payout with growth and capital", "cet1_min_fv"] > ld["Plan", "cet1_min_fv"],
    "R1 still adds nothing when the curve prices half the path" =
      abs(S$lab$dist$d_roe_mean[S$lab$dist$phi == 0.5 & S$lab$dist$strategy == "R1 hedge ratio with the policy rate"]) < 1e-3,
    "beyond 10% of CET1 the tail gives" = {
      f <- rb$frontier[!rb$frontier$static_only, ]
      f$roe_p5[abs(f$budget - 0.3) < 1e-9] < f$roe_p5[abs(f$budget - 0.1) < 1e-9] },
    "the portfolio lifts ROE in every scenario, tightest in the downside" =
      all(rb$scenarios$roe_central > rb$scenarios$roe_plan) &&
      rb$scenarios$scenario[which.min(rb$scenarios$cet1_central)] == "downside",
    "R4 adds least where growth is weak" = rownames(rg)[which.min(rg$pay)] == "Growth below 1%",
    "the liquidity book carries the result" =
      rb$variants$d_roe[rb$variants$variant == "Liquidity book only"] > 10 * rb$variants$d_roe[rb$variants$variant == "Hedge and payout only"]
  )
  ## the numbers typed into the summary shortcode
  plan <- rb$plan_stats$all[["roe"]]
  stopifnot(
    "summary: plan and portfolio ROE" = round(100 * plan, 1) == 12.8 && round(100 * (plan + cen$d_roe), 1) == 15.0,
    "summary: positioning" = round(1e4 * g["downside", "downside"]) == 21 && round(-1e4 * a("downside")) == 253 &&
      round(-1e4 * g["downside", "upside"]) == 48,
    "summary: the carry book" = round(1e4 * cy$recommended$d_roe_vs_central) == 20,
    "summary: gains" = round(1e4 * cen$d_roe) == 213 && round(1e4 * cen$d_p5) == 198,
    "summary: scale" = S$rb$n_combos == 25200 && S$rb$n_episodes == 61 && S$meta$n_sector_models == 22,
    "summary: the surrogate's error" = round(1e4 * max(abs(rb$verify$roe_err)), 2) <= 0.03,
    "summary: the carry book's price and carry" =
      round(1e4 * cy$per_step[["roe"]] / cy$per_step[["csrbb"]] * 0.01) == 11 && round(cy$carry_jo, 2) == 0.58
  )
  invisible(TRUE)
}

## words for small counts, ordinals for percentiles
nw <- function(n) { w <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve")
  ifelse(n >= 1 & n <= 12, w[pmax(1, pmin(12, n))], as.character(n)) }
ord <- function(x) { n <- round(100 * x)
  suf <- ifelse(n %% 100 %in% 11:13, "th", c("th", "st", "nd", "rd", rep("th", 6))[n %% 10 + 1]); paste0(n, suf) }
