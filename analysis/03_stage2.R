# =============================================================================
#  analysis/03_stage2.R -- Stage 2: banks and peer groups relative to the sector
#    * macro regimes (contraction / hiking / easing / neutral)
#    * market-share dynamics for the published peer groups (k >= 4)
#    * the same model for the six reference banks -> population
#      hyperparameters -> NGFB's posterior-predictive draw (seed 20260622)
# =============================================================================
source("analysis/00_config.R")
P <- readRDS(out_path("panels.rds"))
bp <- P$bp; gp <- P$gp; macro <- P$macro

reg <- data.frame(date = macro$date, regime = classify_regimes(macro))
reg <- reg[reg$date >= as.Date("2008-03-31") & reg$date <= CFG$date0, ]
prods <- c(model_products(), "dep_retail", "total_assets")
sg <- sector_growth(gp, prods)
mean_g <- vapply(prods, function(p) mean(sg$growth[sg$product == p & sg$date >= CFG$sample_from],
                                         na.rm = TRUE), 0)

grp_sh <- market_shares(gp[, c("date", "entity", prods)], prods)
cf_grp <- fit_share_models(grp_sh, sg, reg)

six <- bp[bp$key %in% ngfb_population_keys(), ]; six$entity <- six$key
sec <- gp[gp$entity == "Sector", ]
six_sh <- market_shares(rbind(six[, c("date", "entity", prods)], sec[, c("date", "entity", prods)]), prods)
cf_six <- fit_share_models(six_sh, sg, reg)            # internal: never published bank by bank
ngfb_par <- draw_ngfb_share_params(cf_six[cf_six$product %in% model_products(), ],
                                   ngfb_population_keys(), seed = CFG$seed)

guard(all(ngfb_par$pools$k >= ngfb_canonical()$k_anon), "population pool below k-anonymity")
saveRDS(list(regimes = reg, sector_growth = sg, mean_g = mean_g, group_shares = grp_sh,
             coef_groups = cf_grp, ngfb_share = ngfb_par,
             population_hyper = ngfb_par$pools), out_path("stage2.rds"))
ngfb_log("stage 2: %d group-product models; NGFB share parameters for %d products",
         length(unique(paste(cf_grp$entity, cf_grp$product))), nrow(ngfb_par$params))
