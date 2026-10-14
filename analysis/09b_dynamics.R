# =============================================================================
#  analysis/09b_dynamics.R -- how the satellites are built and what they say
#
#  1. What the sign restrictions do: the quarterly search re-run without them
#     (how much model weight goes to economically wrong signs, and what that
#     does to out-of-sample accuracy), and how many specifications each
#     restriction removes.
#  2. Lag profiles: the model-averaged lag of each driver, quarterly against
#     monthly.
#  3. Dynamic multipliers of rate and GDP shocks, quarterly and monthly, with
#     parameter bands, and a local-projection check on the dynamics.
# =============================================================================
source("analysis/00_config.R")
P  <- readRDS(out_path("panels.rds"))
S1 <- readRDS(out_path("stage1.rds"))
MM <- readRDS(out_path("monthly.rds"))
pan_m <- MM$panel_m; sats_m <- MM$sats_m; m_args <- MM$m_args
vol_targets <- MM$vol_targets
sats_q <- c(if (MM$vol_source == "KBP") MM$sats_qv else S1$sats[vol_targets],
            S1$sats[names(stage1_pnl_targets())])
pan_q_of <- function(nm) if (nm %in% vol_targets) MM$panel_qv else P$s1pan
targets <- c(stage1_targets(), stage1_pnl_targets())[names(sats_m)]

## ---- 1. what the sign restrictions do (quarterly production registry) --------------
free_signs <- function(tl) lapply(tl, function(t) { t$signs[] <- 0; t })
sats_free <- c(do.call(fit_stage1, c(list(P$s1pan, free_signs(stage1_targets()), controls = c("covid_q2", "covid_q3")), S1$args)),
               do.call(fit_stage1, c(list(P$s1pan, free_signs(stage1_pnl_targets()), controls = c("covid_q2", "covid_q3")), S1$args)))
reg_all <- c(stage1_targets(), stage1_pnl_targets())
sign_check <- do.call(rbind, lapply(names(sats_free), function(nm) {
  sr <- S1$sats[[nm]]; sf <- sats_free[[nm]]
  if (is.null(sr) || is.null(sf$bma)) return(NULL)
  tg <- reg_all[[nm]]
  wrong <- vapply(seq_along(sf$bma$specs), function(i) {
    sp <- sf$bma$specs[[i]]; cf <- sf$bma$coefs[[i]]
    if (!length(sp$d)) return(FALSE)
    sg <- tg$signs[sp$d]; sg[is.na(sg)] <- 0
    b <- cf[sprintf("%s_L%d", sp$d, sp$l)]
    any(sg != 0 & sign(b) != sg)
  }, TRUE)
  pr <- satellite_panel(sr, P$s1pan); pf <- satellite_panel(sf, P$s1pan)
  ok <- which(is.finite(pr[[nm]])); o <- ok[ok >= ok[CFG$min_train] & ok <= max(ok) - 1L]
  act <- data.frame(date = pr$date, value = pr[[nm]])
  er <- quarterly_errors(oos_forecasts(satellite_designs(sr, pr, 20L), o, 4L), pr$date[o], act)
  ef <- quarterly_errors(oos_forecasts(satellite_designs(sf, pf, 20L), o, 4L), pf$date[o], act)
  data.frame(target = nm, family = sr$family %||% "", n_specs = sr$n_specs,
             rejected_sign = sr$rejected[["sign"]], rejected_t = sr$rejected[["t"]],
             rejected_rho = sr$rejected[["rho"]], sign_valid = sr$n_sign_valid,
             wrong_weight_free = sum(sf$bma$weights[wrong]),
             rmse_restricted = sqrt(mean(er$error^2)), rmse_free = sqrt(mean(ef$error^2)))
}))

## ---- 3. dynamic multipliers -----------------------------------------------------------
## the production (quarterly, BA900) satellites, the monthly ones and, when
## the monthly volumes come from the credit statistics, the quarterly models
## of those same series (the like-for-like comparison of timing)
shocks <- list(repo_up = list(repo = 1), repo_down = list(repo = -1), gdp_down = list(gdp_yy = -1))
asym <- c("c_savings", "s_cards", "c_current", "c_fixed_notice", "g_mortgages_res", "g_cards")
sets <- list(production = list(sats = S1$sats, pan = function(nm) P$s1pan, freq = "quarter", draws = 200L),
             monthly = list(sats = sats_m, pan = function(nm) pan_m, freq = "month", draws = 100L))
if (MM$vol_source == "KBP")
  sets$quarterly_kbp <- list(sats = MM$sats_qv, pan = function(nm) MM$panel_qv, freq = "quarter", draws = 0L)
mult <- list()
for (st in names(sets)) for (nm in names(sets[[st]]$sats)) for (sk in names(shocks)) {
  if (sk == "repo_down" && !(nm %in% asym)) next
  s <- sets[[st]]$sats[[nm]]; fq <- sets[[st]]$freq
  n <- if (fq == "quarter") 12L else 36L
  m <- satellite_dynamic_multiplier(s, satellite_panel(s, sets[[st]]$pan(nm)), shocks[[sk]], n = n, freq = fq,
                                    cumulate = grepl("^g_", nm), n_draws = sets[[st]]$draws)
  m$quarters <- if (fq == "quarter") m$h else m$h / 3
  m$target <- nm; m$shock <- sk; m$freq <- fq; m$set <- st
  mult[[length(mult) + 1L]] <- m
}
mult <- rbind_fill(mult)

## local projections: a model-free check on the dynamics of the repo pass-through
lp_targets <- c("c_savings", "c_current", "c_fixed_notice", "s_vaf", "g_mortgages_res", "g_vaf", "g_cards")
lp <- list()
for (nm in lp_targets) for (fq in c("quarter", "month")) {
  if (fq == "month" && grepl("^g_", nm) && MM$vol_source != "BA900") next
  pan <- if (fq == "quarter") P$s1pan else pan_m
  s <- if (fq == "quarter") S1$sats[[nm]] else sats_m[[nm]]
  if (is.null(s)) next
  Hh <- if (fq == "quarter") 12L else 36L
  d <- pan[pan$date >= (if (fq == "quarter") S1$args$sample_from else m_args$sample_from), ]
  ## for growth targets the response of the level: cumulate the growth rate
  if (grepl("^g_", nm)) { d$resp <- cumsum(ifelse(is.finite(d[[nm]]), d[[nm]], 0)) / (if (fq == "quarter") 4 else 12)
                          d$resp[!is.finite(d[[nm]])] <- NA } else d$resp <- d[[nm]]
  ctl <- c("gdp_yy", "cpi_yy", if (fq == "month") sprintf("sm%02d", 2:12))
  r_y <- local_projection(d, "resp", "repo", controls = ctl, H = Hh, p = if (fq == "quarter") 2L else 3L)
  r_r <- local_projection(d, "repo", "repo", controls = ctl, H = Hh, p = if (fq == "quarter") 2L else 3L)
  ## the ARDL's response along the same (LP-implied) repo path: the
  ## projection's first period is the impulse period (LP horizon 0)
  path <- r_r$beta[order(r_r$h)]
  am <- satellite_dynamic_multiplier(s, satellite_panel(s, pan), list(repo = path), n = length(path), freq = fq,
                                     cumulate = grepl("^g_", nm))
  lp[[length(lp) + 1L]] <- data.frame(target = nm, freq = fq, h = r_y$h, lp = r_y$beta, lo = r_y$lo, hi = r_y$hi,
                                      repo_path = r_r$beta[match(r_y$h, r_r$h)], ardl = am$effect[r_y$h + 1L])
}
lp <- do.call(rbind, lp)

## lag profiles: BMA-weighted mean lag of each driver, quarterly vs monthly (in quarters)
lag_profile <- do.call(rbind, lapply(names(targets), function(nm) {
  one <- function(s, per) {
    if (is.null(s$bma)) return(NULL)
    d <- do.call(rbind, lapply(seq_along(s$bma$specs), function(i)
      if (length(s$bma$specs[[i]]$d)) data.frame(driver = s$bma$specs[[i]]$d, lag = s$bma$specs[[i]]$l / per, w = s$bma$weights[i])))
    if (is.null(d)) return(NULL)
    a <- stats::aggregate(list(lag = d$lag * d$w, w = d$w), list(driver = d$driver), sum)
    a$lag <- a$lag / a$w; a
  }
  q <- one(S1$sats[[nm]], 1); m <- one(sats_m[[nm]], 3)
  rbind(if (!is.null(q)) cbind(target = nm, freq = "quarter", q), if (!is.null(m)) cbind(target = nm, freq = "month", m))
}))

saveRDS(list(sign_check = sign_check, sats_free_summary = stage1_summary(sats_free),
             multipliers = mult, lp = lp, lag_profile = lag_profile), out_path("dynamics.rds"))
ngfb_log("dynamics: %d multiplier paths, %d local-projection rows", nrow(mult), nrow(lp))
