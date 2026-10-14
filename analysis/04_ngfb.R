# =============================================================================
#  analysis/04_ngfb.R -- build NGFB, embed it in the sector, calibrate
# =============================================================================
source("analysis/00_config.R")
P  <- readRDS(out_path("panels.rds"))
S2 <- readRDS(out_path("stage2.rds"))
canon <- ngfb_canonical()

## 1. composition: business-line draw from the six (seed 20260622)
A  <- ngfb_draw_assets(P$bp, date0 = CFG$date0, seed = canon$seed)
bal <- ngfb_balance_sheet(P$bp, CFG$date0, A$draw)

## 2. embed in the sector history (added to the industry, Post 1 convention)
prods <- model_products()
emb_lines <- intersect(c(names(line_driver_map()), "dep_retail", "total_assets"), names(P$gp))
gp_n <- add_composites(P$gp)
## NGFB's book for embedding: modelled products at the jump-off
nb <- data.frame(line = c(prods, "dep_retail"), balance = NA_real_)
B0 <- stats::setNames(bal$balance, bal$line)
nb$balance[nb$line == "mortgages_res"] <- B0[["mortgages_res"]]
nb$balance[nb$line == "mortgages_com"] <- B0[["mortgages_com"]]
nb$balance[nb$line == "vaf"] <- B0[["vaf"]]
nb$balance[nb$line == "cards"] <- B0[["cards"]]
nb$balance[nb$line == "personal_other"] <- B0[["personal_other"]]
nb$balance[nb$line == "corp_all"] <- B0[["corporate_loans"]] + B0[["overdrafts"]]
nb$balance[nb$line == "nmd"] <- B0[["beh_operational"]] + B0[["beh_savings"]] + B0[["beh_notice"]]
nb$balance[nb$line == "beh_term"] <- B0[["beh_term"]]
## retail deposits: NGFB's NMD at the population's average retail share of NMD
six <- P$bp[P$bp$key %in% ngfb_population_keys() & P$bp$date == CFG$date0, ]
nb$balance[nb$line == "dep_retail"] <- nb$balance[nb$line == "nmd"] * sum(six$dep_retail) / sum(six$nmd)
attr(nb, "date0") <- CFG$date0
par <- S2$ngfb_share$params
par_nodrift <- par; par_nodrift[, "(Intercept)"] <- 0
par_emb <- rbind(par_nodrift, dep_retail = par_nodrift["nmd", ])
gp_emb <- ngfb_embed(gp_n, nb, par_emb, S2$mean_g, S2$regimes, products = c(prods, "dep_retail"))

## 3. regime share analysis with NGFB in the mix
sh <- market_shares(gp_emb[gp_emb$date >= as.Date("2008-03-31"), c("date", "entity", c(prods, "dep_retail"))],
                    c(prods, "dep_retail"))
sh <- merge(sh, S2$regimes, by = "date")
sh <- sh[order(sh$entity, sh$product, sh$date), ]
sh$ds <- stats::ave(sh$share, sh$entity, sh$product, FUN = function(x) c(NA, diff(x)))
reg_share <- stats::aggregate(list(ds_pp_yr = 400 * sh$ds), list(entity = sh$entity, product = sh$product,
                              regime = sh$regime), mean, na.rm = TRUE)
flows <- list()
for (p in unique(reg_share$product)) for (r in c("contraction", "hiking", "easing", "neutral")) {
  x <- reg_share[reg_share$product == p & reg_share$regime == r, ]
  v <- stats::setNames(x$ds_pp_yr, x$entity)
  flows[[paste(p, r)]] <- share_flow_matrix(v)
}

## 4. calibration to the sector (credit, capital, tax, interest levels, LCR,
##    NSFR); quarters the Stage 1 impulse dummies flag as one-off outliers are
##    neutralised in the calibration window
S1 <- readRDS(out_path("stage1.rds"))
flagged <- lapply(S1$sats, function(s) {
  m <- regmatches(s$impulses, regexpr("[0-9]{6}$", s$impulses))
  if (!length(m)) return(as.Date(character(0)))
  quarter_end(as.Date(paste0(m, "01"), "%Y%m%d"))
})
names(flagged) <- vapply(S1$sats, function(s) s$target, "")
cal <- calibrate_to_sector(P, lcr_target = CFG$lcr_sector, nsfr_target = CFG$nsfr_sector,
                           flagged = flagged)
guard(abs(cal$liq$check[["lcr"]] / CFG$lcr_sector - 1) < 1e-8, "sector LCR calibration failed")
guard(abs(cal$liq$check[["nsfr"]] / CFG$nsfr_sector - 1) < 1e-8, "sector NSFR calibration failed")
guard(abs(cal$checks$value[grepl("NII vs BA120 after", cal$checks$item)] - 1) < 1e-6,
      "sector NII calibration failed")
pricing <- line_pricing_map()
pricing$hqla$scale <- cal$hqla_scale
pricing$wholesale_other$spread_fixed <- cal$wholesale_spread
## NGFB: the six-bank pool's depositor mix inside each behavioural category
chars_ngfb   <- entity_chars(cal, P$depmat, "Population", CFG$date0)
chars_sector <- entity_chars(cal, P$depmat, "Sector", CFG$date0)
ebs <- engine_balance_sheet(bal, chars_ngfb, pricing)
nmd0 <- sum(B0[c("beh_operational", "beh_savings", "beh_notice")])

## 5. committed artefact (the NGFB identity, reproducible across sessions)
art <- list(seed = canon$seed, date0 = CFG$date0, size = attr(bal, "size"),
            asset_mix = A$draw, line_draw = A$line_draw, line_mean = A$line_mean,
            within = A$within, distance = A$distance, radius = A$radius,
            deposit_mix = canon$deposit_mix, leverage_iea_equity = canon$leverage_iea_equity,
            all_dep_iea = canon$all_dep_iea, share_params = par,
            population_hyper = S2$population_hyper, provenance = canon$provenance,
            created = format(Sys.time()))
dir.create("inst/artefacts", showWarnings = FALSE, recursive = TRUE)
saveRDS(art, "inst/artefacts/ngfb_canonical.rds")

saveRDS(list(asset_draw = A[c("draw", "line_draw", "line_mean", "within", "distance", "radius")],
             balance = bal, engine_bs = ebs, nmd0 = nmd0, embedded = gp_emb, shares = sh,
             regime_share = reg_share, flows = flows, calibration = cal, pricing = pricing,
             chars_ngfb = chars_ngfb, chars_sector = chars_sector,
             share_params = par), out_path("ngfb.rds"))
ngfb_log("NGFB: R%.0f bn total assets (%.1f%% of sector), draw distance %.2f (radius %.2f)",
         attr(bal, "size") / 1e6, 100 * attr(bal, "size") /
           P$gp$total_assets[P$gp$entity == "Sector" & P$gp$date == CFG$date0], A$distance, A$radius)
