# =============================================================================
#  analysis/15_risk_budget.R -- choosing the levers together: a risk-budgeted
#                               optimiser across the macro episodes
#
#  The strategy lab (14) tests each strategy on its own. An ALCO sets the
#  levers together, under limits. Here every combination of the levers'
#  settings is scored across the 61 historical episodes, and the best is
#  chosen subject to a risk budget:
#   objective  NGFB's three-year average ROE, averaged over the episodes;
#   tail       the 5th percentile of that ROE across the episodes no lower
#              than the plan's;
#   capital    CET1, with the bonds bought for HQLA at fair value through
#              OCI, at or above the 11.5% floor in every quarter of every
#              episode;
#   liquidity  the LCR proxy, with those bonds at market value, at or above
#              120%, and the NSFR proxy at or above 110% (an assumed
#              management floor), in every quarter of every episode;
#   IRRBB      the change in the economic value of the hedge and bond books
#              for a 200 bps parallel rise, against the plan's, within +/- B
#              of CET1 in every quarter of every episode; B is traced to give
#              the frontier, 5% of CET1 (a third of the outlier test's 15% of
#              Tier 1) is the central budget.
#  The levers (R5 is left out: it does nothing in this engine):
#   hedge      ratio policy {0, 20, 35 (rate-neutral), 49.6 (plan), 65, 80% of
#              non-maturity deposits, or R1} x reinvestment policy {3, 5, 7,
#              10 years, or R2}: run together, because they interact;
#   duration   10-year government bonds, share of earning HQLA {0, 12.5, 25,
#              37.5, 50%, or R3};
#   asset swap asset-swapped 5-year government bonds, same shares, or R6;
#   buffer     the LCR proxy the release targets at the jump-off {the plan,
#              180, 170, 160%, or R7};
#   payout     {30, 50, 70%, or R4};
#  the bond books and the release within 90% of earning HQLA in every quarter
#  of every path (combinations that breach it are not admissible).
#
#  Method.
#   1. Every setting runs through the engine on its own, all else at the
#      plan, on every path (the episodes, the three scenarios and the
#      downside with cuts), keeping each quarter's ROE, NII, CET1 at fair
#      value, LCR, NSFR, the books' PV01 and CET1.
#   2. A combination is scored by a surrogate. Its settings' changes against
#      the plan in net income, RWA, the OCI reserve, PV01, LCR and NSFR are
#      added quarter by quarter to the plan's path; equity is then rolled
#      forward with the combination's own payout (a static ratio, or R4 on
#      the combination's own CET1), and ROE, CET1 and the EVE ratio are
#      computed from it. A payout acts on every rand the other levers add, so
#      adding ROE changes would miss that interaction; rolling equity forward
#      keeps it, and the funding plug's interest moves with the equity the
#      combination actually retains. The surrogate reproduces every single
#      setting's run exactly; for combinations what it leaves out is second
#      order. Minima, maxima and percentiles are taken after adding.
#   3. Every admissible combination is enumerated (no search heuristic) and
#      the best taken at each budget: the frontier, with all settings and
#      with static settings only.
#   4. The chosen combinations are run exactly through the engine on every
#      path; the surrogate's error is measured and the constraints checked
#      on the exact runs.
#   5. Out of sample: the optimum chosen on the first 25 episodes is scored
#      on the last 25 and the reverse; the 11 episodes between them, which
#      share quarters with both halves, are left out.
#  The episodes are equally weighted; their macro paths are known (perfect
#  foresight). Nothing here forecasts.
# =============================================================================
source("analysis/00_config.R")
P  <- readRDS(out_path("panels.rds"))
N  <- readRDS(out_path("ngfb.rds"))
SC <- readRDS(out_path("scenarios.rds"))
A  <- readRDS(out_path("audit.rds"))
L  <- readRDS(out_path("lab.rds"))
hist <- P$macro[P$macro$date <= CFG$date0, ]
base <- SC$base
ea <- SC$eng_args; h0 <- SC$hedge0; nmd0 <- N$nmd0; n <- CFG$horizon
hj <- pf_history(hist, attr(base, "junction"))
source("analysis/lab_tools.R", local = TRUE)
t_start <- Sys.time()

inp_all <- L$inputs; kind <- L$kind; paths <- names(inp_all)
ep <- paths[kind == "episode"]; K <- length(ep)
sc_paths <- paths[kind == "scenario"]
guard(K == nrow(L$info) && identical(ep, L$info$episode), "the lab's episodes and inputs disagree")
## the rules' signals, recomputed from the paths and checked against the lab's
rp_all <- lapply(stats::setNames(paths, paths), function(k) rule_paths(L$scen_paths[[k]], inp_all[[k]]))
for (k in paths)
  guard(isTRUE(all.equal(rp_all[[k]][c("ratio", "theta", "alpha", "release")], L$rule_paths[[k]][c("ratio", "theta", "alpha", "release")])),
        "the rules' signals differ from the lab's in %s", k)
guard(max(abs(run(inp_all$baseline)$quarterly$roe - SC$results$baseline$ngfb$run$quarterly$roe)) < 1e-12,
      "the optimiser does not reproduce the plan")
guard(abs(ea$payout - 0.5) < 1e-12, "the plan's payout is not 50%")
nsfr_floor <- 1.10
budgets <- c(0.01, 0.025, 0.05, 0.075, 0.10, 0.15, 0.20, 0.30)
b_central <- 0.05

## ---- 1. the levers and their settings -------------------------------------------------------
ratio_set <- c(stats::setNames(as.list(ratios), sprintf("%.0f%%", 100 * ratios)), list(R1 = "R1"))
tenor_set <- c(stats::setNames(as.list(names(tenors)), sprintf("%d years", tenors / 4L)), list(R2 = "R2"))
hedge_set <- expand.grid(ratio = names(ratio_set), tenor = names(tenor_set), stringsAsFactors = FALSE)
plan_r_lab <- sprintf("%.0f%%", 100 * plan_ratio)
i_plan <- which(hedge_set$ratio == plan_r_lab & hedge_set$tenor == "5 years")
guard(length(i_plan) == 1, "the plan is not in the hedge grid")
shares <- c(0, 0.125, 0.25, 0.375, 0.5)
share_lab <- function(x) sub("\\.0%", "%", sprintf("%.1f%%", 100 * x))
dur_set <- c(stats::setNames(as.list(shares), share_lab(shares)), list(R3 = "R3"))
as_set <- c(stats::setNames(as.list(shares), share_lab(shares)), list(R6 = "R6"))
lcr_tg <- c(1.80, 1.70, 1.60)
rel_tg <- vapply(lcr_tg, rel_lcr, 0)
guard(all(diff(rel_tg) > 0) && max(rel_tg) / sc_hq < hq_cap, "the buffer releases are out of order")
buf_set <- c(list(plan = NA), stats::setNames(as.list(lcr_tg), sprintf("LCR %.0f%%", 100 * lcr_tg)), list(R7 = "R7"))
pay_set <- list(`30%` = 0.3, `50%` = 0.5, `70%` = 0.7, R4 = "R4")
levers <- list(hedge = hedge_set, dur = dur_set, as = as_set, buf = buf_set, pay = pay_set)
## the plan's setting on each lever
plan_opt <- c(hedge = i_plan, dur = 1L, as = 1L, buf = 1L, pay = 2L)
is_rule <- list(hedge = hedge_set$ratio == "R1" | hedge_set$tenor == "R2", dur = names(dur_set) == "R3",
                as = names(as_set) == "R6", buf = names(buf_set) == "R7", pay = names(pay_set) == "R4")

## the strategy on one lever for one path
dur_path <- function(o, rp) if (identical(dur_set[[o]], "R3")) rp$theta else rep(dur_set[[o]], n)
as_path <- function(o, rp) if (identical(as_set[[o]], "R6")) rp$alpha else rep(as_set[[o]], n)
rel_path <- function(o, rp) { s <- buf_set[[o]]
  if (identical(s, "R7")) rp$release else if (is.na(s)) rep(0, n) else rep(rel_tg[match(s, lcr_tg)], n) }
pay_of <- function(o, rp) if (identical(pay_set[[o]], "R4")) payout_rule(rp$good) else pay_set[[o]]
hedge_of <- function(o, rp) hedge_spec(ratio_set[[hedge_set$ratio[o]]], tenor_set[[hedge_set$tenor[o]]], rp)
## a combination (one option index per lever) as engine arguments
combo_run <- function(inp, rp, cb) {
  bk <- list()
  if (cb[["dur"]] != 1L) bk <- c(bk, list(dur_book(dur_path(cb[["dur"]], rp), inp)))
  if (cb[["as"]] != 1L) bk <- c(bk, list(as_book(as_path(cb[["as"]], rp), inp)))
  rel <- if (cb[["buf"]] == 1L) NULL else rel_path(cb[["buf"]], rp)
  run(inp, hedge = hedge_of(cb[["hedge"]], rp), payout = pay_of(cb[["pay"]], rp), books = bk, release = rel)
}

## ---- 2. every setting on its own, on every path ---------------------------------------------
mets <- c("roe", "nii", "ni", "cet1fv", "lcr", "nsfr", "pv01", "cet1amt", "rwa", "oci")
qx <- function(r) {
  q <- r$quarterly; bk <- r$books
  pv <- q$hedge_pv01 + (if (length(bk)) Reduce(`+`, lapply(bk, `[[`, "pv01")) else 0)
  oci <- oci_reserve(r)
  cbind(roe = q$roe, nii = q$nii, ni = q$net_income, cet1fv = q$cet1_ratio + oci / q$rwa,
        lcr = q$lcr, nsfr = q$nsfr, pv01 = pv, cet1amt = ea$cet1_share * q$equity, rwa = q$rwa, oci = oci,
        plug0 = as.numeric(q$plug_balance <= 1e-6))
}
one_path <- function(k) {
  inp <- inp_all[[k]]; rp <- rp_all[[k]]
  one <- function(lv, o) { cb <- plan_opt; cb[[lv]] <- o; qx(combo_run(inp, rp, cb)) }
  pl <- qx(run(inp))
  out <- list(plan = pl)
  for (lv in names(levers)) {
    no <- if (lv == "hedge") nrow(hedge_set) else length(levers[[lv]])
    out[[lv]] <- lapply(seq_len(no), function(o) if (o == plan_opt[[lv]]) pl else one(lv, o))
  }
  ## HQLA out of bills, as a share of the jump-off's earning HQLA
  out$use <- list(dur = lapply(seq_along(dur_set), function(o) dur_path(o, rp)),
                  as = lapply(seq_along(as_set), function(o) as_path(o, rp)),
                  buf = lapply(seq_along(buf_set), function(o) rel_path(o, rp) / sc_hq))
  out
}
rb_cache <- Sys.getenv("NGFB_RB_CACHE")                     # development only: reuse the runs
runs <- if (nzchar(rb_cache) && file.exists(rb_cache)) readRDS(rb_cache) else lapply(stats::setNames(paths, paths), one_path)
if (nzchar(rb_cache) && !file.exists(rb_cache)) saveRDS(runs, rb_cache)
## the plan's settings reproduce the plan
for (k in paths) guard(max(abs(runs[[k]]$plan[, "roe"] - L$rule_paths[[k]]$plan_roe_q)) < 1e-12,
                       "the plan differs from the lab's in %s", k)
n_runs <- sum(vapply(names(levers), function(lv) length(runs[[1]][[lv]]) - 1L, 0L)) + 1L
t_runs <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))

## changes against the plan, flattened: one row per option, the episodes'
## quarters in columns (episode-major); the plan's own path; HQLA use
flat <- function(lv, m, pth) {
  no <- length(runs[[1]][[lv]])
  t(vapply(seq_len(no), function(o) as.vector(vapply(pth, function(k) runs[[k]][[lv]][[o]][, m] - runs[[k]]$plan[, m],
                                                    numeric(n))), numeric(n * length(pth))))
}
D <- lapply(stats::setNames(names(levers), names(levers)), function(lv)
  lapply(stats::setNames(mets, mets), function(m) flat(lv, m, ep)))
PL <- lapply(stats::setNames(mets, mets), function(m) as.vector(vapply(ep, function(k) runs[[k]]$plan[, m], numeric(n))))
U <- lapply(c(dur = "dur", as = "as", buf = "buf"), function(lv)
  t(vapply(seq_along(runs[[1]]$use[[lv]]), function(o) as.vector(vapply(paths, function(k) runs[[k]]$use[[lv]][[o]], numeric(n))),
           numeric(n * length(paths)))))
eve_plan <- 200 * PL$pv01 / PL$cet1amt
## equity and CET1 at the jump-off, as the engine starts them
eb0 <- N$engine_bs; ia0 <- eb0$side == "asset"
E0 <- sum(eb0$balance[eb0$side == "equity"])
cet1_0 <- ea$cet1_share * E0 / (sum(eb0$balance[ia0] * eb0$rw[ia0]) + ea$rwa_other * sum(eb0$balance[ia0]))
good_ep <- vapply(ep, function(k) rp_all[[k]]$good, logical(n))        # quarters x episodes, R4's growth test
ws_ep <- vapply(ep, function(k) inp_all[[k]]$line_rates[[ea$plug]], numeric(n))   # the plug's rate (%), quarters x episodes
guard(all(is.finite(ws_ep)), "no wholesale rate on some path")

## ---- 3. every combination, scored by the surrogate ----------------------------------------------
halves <- list(all = seq_len(K), first = 1:25, last = (K - 24):K)
guard(K == 61, "the out-of-sample split is written for 61 episodes")
qtr_add <- function(d, q) seq(d, by = paste(3 * q, "months"), length.out = 2)[2]
guard(qtr_add(L$info$start[25], n - 1) < L$info$start[K - 24], "the two halves share quarters")
q_type7 <- function(x, p) unname(stats::quantile(x, p, type = 7))
## per-episode statistics of a block of combinations (rows) from their
## flattened quarterly paths
by_ep <- function(X, f) { a <- array(X, c(nrow(X), n, K)); m <- a[, 1, , drop = TRUE]
  if (nrow(X) == 1L) m <- matrix(m, 1)
  for (t in 2:n) { b <- a[, t, , drop = TRUE]; if (nrow(X) == 1L) b <- matrix(b, 1); m <- f(m, b) }
  m }
ep_mean <- function(X) { a <- array(X, c(nrow(X), n, K)); s <- a[, 1, ]; if (nrow(X) == 1L) s <- matrix(s, 1)
  for (t in 2:n) { b <- a[, t, ]; if (nrow(X) == 1L) b <- matrix(b, 1); s <- s + b }; s / n }
plan_ep <- ep_mean(matrix(PL$roe, 1))[1, ]
plan_stats <- lapply(halves, function(S) c(roe = mean(plan_ep[S]), p5 = q_type7(plan_ep[S], 0.05)))
nl <- c(hedge = nrow(hedge_set), dur = length(dur_set), as = length(as_set), buf = length(buf_set), pay = length(pay_set))
gi <- expand.grid(dur = seq_len(nl[["dur"]]), as = seq_len(nl[["as"]]), buf = seq_len(nl[["buf"]]))
use_all <- U$dur[gi$dur, , drop = FALSE] + U$as[gi$as, , drop = FALSE] + U$buf[gi$buf, , drop = FALSE]
use_max <- apply(use_all, 1, max)                          # the same for every hedge and payout
rm(use_all)
## equity rolled forward with the combination's payout: ROE, CET1 and CET1
## in rand, quarter by quarter, from net income and RWA (rows: combinations).
## EQA is the equity the added net income assumed (the plan's plus each
## setting's own change); where the combination retains more or less than
## that, the funding plug is smaller or larger and its interest moves with it
equity_path <- function(NI, RWA, p, EQA) {
  R <- nrow(NI); ni <- array(NI, c(R, n, K)); rw <- array(RWA, c(R, n, K)); eqa <- array(EQA, c(R, n, K))
  sl <- function(a, t) { x <- a[, t, ]; if (R == 1L) matrix(x, 1) else x }
  E <- matrix(E0, R, K); cprev <- matrix(cet1_0, R, K)
  roe <- cet <- eq <- array(0, c(R, n, K))
  for (t in seq_len(n)) {
    nt <- sl(ni, t)
    if (t > 1L) nt <- nt + (1 - ea$tax) * matrix(ws_ep[t, ], R, K, byrow = TRUE) / 400 * (E - sl(eqa, t - 1L))
    po <- if (identical(pay_set[[p]], "R4")) ifelse(matrix(good_ep[t, ], R, K, byrow = TRUE) & cprev > CFG$cet1_target, 0.7, 0.3) else
      pay_set[[p]]
    En <- E + nt - po * pmax(nt, 0)
    roe[, t, ] <- 4 * nt / ((E + En) / 2)
    cprev <- ea$cet1_share * En / sl(rw, t)
    cet[, t, ] <- cprev; eq[, t, ] <- En
    E <- En
  }
  list(roe = matrix(roe, R), cet1 = matrix(cet, R), cet1amt = ea$cet1_share * matrix(eq, R))
}
score_block <- function(h, p) {
  add <- c("nii", "ni", "lcr", "nsfr", "pv01", "rwa", "oci", "cet1amt")
  X <- lapply(stats::setNames(add, add), function(m) {
    s <- D$dur[[m]][gi$dur, , drop = FALSE] + D$as[[m]][gi$as, , drop = FALSE] + D$buf[[m]][gi$buf, , drop = FALSE]
    sweep(s, 2, PL[[m]] + D$hedge[[m]][h, ] + D$pay[[m]][p, ], "+")
  })
  ep_ <- equity_path(X$ni, X$rwa, p, X$cet1amt / ea$cet1_share)
  eve <- abs(200 * X$pv01 / ep_$cet1amt - rep(eve_plan, each = nrow(gi)))
  roe_e <- ep_mean(ep_$roe); nii_e <- ep_mean(X$nii) * n
  cet_e <- by_ep(ep_$cet1 + X$oci / X$rwa, pmin); lcr_e <- by_ep(X$lcr, pmin); nsf_e <- by_ep(X$nsfr, pmin)
  eve_e <- by_ep(eve, pmax)
  do.call(rbind, lapply(names(halves), function(hv) { S <- halves[[hv]]
    data.frame(half = hv, hedge = h, dur = gi$dur, as = gi$as, buf = gi$buf, pay = p,
               roe = rowMeans(roe_e[, S, drop = FALSE]), roe_p5 = apply(roe_e[, S, drop = FALSE], 1, q_type7, 0.05),
               win = rowMeans(roe_e[, S, drop = FALSE] > rep(plan_ep[S], each = nrow(gi)) + 1e-12),
               nii = rowMeans(nii_e[, S, drop = FALSE]),
               cet1_min = apply(cet_e[, S, drop = FALSE], 1, min), lcr_min = apply(lcr_e[, S, drop = FALSE], 1, min),
               nsfr_min = apply(nsf_e[, S, drop = FALSE], 1, min), eve_max = apply(eve_e[, S, drop = FALSE], 1, max),
               use_max = use_max, stringsAsFactors = FALSE) }))
}
cand <- do.call(rbind, lapply(seq_len(nl[["hedge"]]), function(h) do.call(rbind, lapply(seq_len(nl[["pay"]]), function(p) score_block(h, p)))))
cand$rules <- is_rule$hedge[cand$hedge] | is_rule$dur[cand$dur] | is_rule$as[cand$as] | is_rule$buf[cand$buf] | is_rule$pay[cand$pay]
cand$p5_floor <- vapply(cand$half, function(hv) plan_stats[[hv]][["p5"]], 0)
cand$ok_base <- cand$use_max <= hq_cap + 1e-9 & cand$roe_p5 >= cand$p5_floor - 1e-12 & cand$cet1_min >= CFG$cet1_min &
  cand$lcr_min >= CFG$lcr_min & cand$nsfr_min >= nsfr_floor
t_enum <- as.numeric(difftime(Sys.time(), t_start, units = "mins")) - t_runs
ca <- cand[cand$half == "all", ]
## the plan is a candidate, satisfies every limit and scores as itself; and
## the surrogate reproduces every single setting's own run
pr <- ca[ca$hedge == i_plan & ca$dur == 1 & ca$as == 1 & ca$buf == 1 & ca$pay == 2, ]
guard(nrow(pr) == 1 && pr$ok_base && abs(pr$roe - plan_stats$all[["roe"]]) < 1e-10 && pr$eve_max < 1e-10,
      "the plan is not a feasible candidate scoring as itself")
single_err <- unlist(lapply(names(levers), function(lv) vapply(seq_len(nl[[lv]]), function(o) {
  cb <- plan_opt; cb[[lv]] <- o
  z <- ca[ca$hedge == cb[["hedge"]] & ca$dur == cb[["dur"]] & ca$as == cb[["as"]] & ca$buf == cb[["buf"]] & ca$pay == cb[["pay"]], ]
  ex <- mean(vapply(ep, function(k) mean(runs[[k]][[lv]][[o]][, "roe"]), 0))
  ex_c <- min(vapply(ep, function(k) min(runs[[k]][[lv]][[o]][, "cet1fv"]), 0))
  max(abs(z$roe - ex), abs(z$cet1_min - ex_c))
}, 0)))
guard(max(single_err) < 1e-9, "the surrogate does not reproduce a single setting's own run")
## what binds: the share of combinations each limit rules out on its own
binds <- c(hqla_cap = mean(ca$use_max > hq_cap + 1e-9), tail = mean(ca$roe_p5 < ca$p5_floor - 1e-12),
           cet1 = mean(ca$cet1_min < CFG$cet1_min), lcr = mean(ca$lcr_min < CFG$lcr_min),
           nsfr = mean(ca$nsfr_min < nsfr_floor), eve_central = mean(ca$eve_max > b_central))

## the frontier: the best admissible combination at each budget, with every
## setting and with static settings only
pick <- function(d, B, static = FALSE) {
  z <- d[d$ok_base & d$eve_max <= B + 1e-12 & (!static | !d$rules), ]
  if (!nrow(z)) return(NULL)
  z[which.max(z$roe), ]
}
frontier <- do.call(rbind, lapply(c(FALSE, TRUE), function(st) do.call(rbind, lapply(budgets, function(B) {
  z <- pick(ca, B, st); if (is.null(z)) return(NULL); cbind(budget = B, static_only = st, z) }))))
guard(nrow(frontier) == 2 * length(budgets), "some budget has no admissible combination")
guard(all(diff(frontier$roe[!frontier$static_only]) >= -1e-12) && all(diff(frontier$roe[frontier$static_only]) >= -1e-12),
      "the frontier is not monotone in the budget")

## ---- 4. the chosen combinations, run exactly -----------------------------------------------------
key <- function(d) paste(d$hedge, d$dur, d$as, d$buf, d$pay)
chosen <- unique(frontier[, c("hedge", "dur", "as", "buf", "pay")])
exact_runs <- lapply(seq_len(nrow(chosen)), function(i) {
  cb <- unlist(chosen[i, ])
  lapply(stats::setNames(paths, paths), function(k) combo_run(inp_all[[k]], rp_all[[k]], cb))
})
names(exact_runs) <- key(chosen)
exact_stats <- function(rs, S = seq_len(K)) {
  qq <- lapply(ep, function(k) qx(rs[[k]]))
  pe <- vapply(qq, function(x) mean(x[, "roe"]), 0)
  eve <- vapply(seq_len(K), function(j) max(abs(200 * qq[[j]][, "pv01"] / qq[[j]][, "cet1amt"] -
                                               eve_plan[(j - 1) * n + seq_len(n)])), 0)
  c(roe = mean(pe[S]), roe_p5 = q_type7(pe[S], 0.05), win = mean(pe[S] > plan_ep[S] + 1e-12),
    nii = mean(vapply(qq[S], function(x) sum(x[, "nii"]), 0)),
    cet1_min = min(vapply(qq[S], function(x) min(x[, "cet1fv"]), 0)),
    lcr_min = min(vapply(qq[S], function(x) min(x[, "lcr"]), 0)),
    nsfr_min = min(vapply(qq[S], function(x) min(x[, "nsfr"]), 0)), eve_max = max(eve[S]),
    plug0 = mean(vapply(qq[S], function(x) any(x[, "plug0"] > 0), TRUE)))
}
verify <- do.call(rbind, lapply(seq_len(nrow(chosen)), function(i) {
  s <- ca[key(ca) == key(chosen[i, ]), ]; e <- exact_stats(exact_runs[[i]])
  data.frame(key = key(chosen[i, ]), roe_sur = s$roe, roe_exact = e[["roe"]], p5_sur = s$roe_p5, p5_exact = e[["roe_p5"]],
             cet1_sur = s$cet1_min, cet1_exact = e[["cet1_min"]], lcr_sur = s$lcr_min, lcr_exact = e[["lcr_min"]],
             nsfr_exact = e[["nsfr_min"]], eve_sur = s$eve_max, eve_exact = e[["eve_max"]], win_exact = e[["win"]],
             nii_exact = e[["nii"]], plug0_exact = e[["plug0"]], stringsAsFactors = FALSE)
}))
verify$roe_err <- verify$roe_exact - verify$roe_sur
verify$feasible_exact <- verify$p5_exact >= plan_stats$all[["p5"]] - 1e-9 & verify$cet1_exact >= CFG$cet1_min &
  verify$lcr_exact >= CFG$lcr_min & verify$nsfr_exact >= nsfr_floor
print(verify[, c("key", "roe_sur", "roe_exact", "roe_err", "cet1_sur", "cet1_exact", "eve_sur", "eve_exact")])
guard(max(abs(verify$roe_err)) < 0.0005, "the surrogate misses an exact run by more than 5 bps of ROE")

## ---- 5. the central portfolio --------------------------------------------------------------------
fc <- frontier[!frontier$static_only & abs(frontier$budget - b_central) < 1e-12, ]
fs <- frontier[frontier$static_only & abs(frontier$budget - b_central) < 1e-12, ]
cen <- unlist(fc[, c("hedge", "dur", "as", "buf", "pay")]); cen_s <- unlist(fs[, c("hedge", "dur", "as", "buf", "pay")])
rs_c <- exact_runs[[key(fc)]]; rs_s <- exact_runs[[key(fs)]]
guard(verify$feasible_exact[verify$key == key(fc)] && verify$eve_exact[verify$key == key(fc)] <= b_central + 0.002,
      "the central portfolio breaks a limit when run exactly")
setting_labels <- function(cb) c(
  hedge_ratio = hedge_set$ratio[cb[["hedge"]]], hedge_tenor = hedge_set$tenor[cb[["hedge"]]],
  duration = names(dur_set)[cb[["dur"]]], asset_swap = names(as_set)[cb[["as"]]],
  buffer = names(buf_set)[cb[["buf"]]], payout = names(pay_set)[cb[["pay"]]])
## each lever's contribution to the change in ROE, episode by episode (the
## surrogate's terms), and what the exact run adds through interactions
contrib <- function(cb, rs) {
  cm <- vapply(names(levers), function(lv) ep_mean(D[[lv]]$roe[cb[[lv]], , drop = FALSE])[1, ], numeric(K))
  ex <- vapply(ep, function(k) mean(rs[[k]]$quarterly$roe), 0) - plan_ep
  data.frame(episode = ep, cm, interaction = ex - rowSums(cm), total = ex, check.names = FALSE)
}
ct_c <- contrib(cen, rs_c); ct_s <- contrib(cen_s, rs_s)
info <- L$info; info$d_repo_avg <- info$repo_mean - L$params$repo_jo
reg <- data.frame(episode = info$episode,
                  rates = ifelse(info$d_repo_avg > 0.25, "rising", ifelse(info$d_repo_avg < -0.25, "falling", "flat")),
                  growth = ifelse(info$gdp_mean < 1, "below 1%", "1% or more"),
                  contraction = info$gdp_min < 0, stringsAsFactors = FALSE)
by_regime <- function(ct) {
  d <- merge(ct, reg, by = "episode")
  grp <- list(`All episodes` = rep(TRUE, nrow(d)), `Rates rising` = d$rates == "rising", `Rates falling` = d$rates == "falling",
              `Rates flat` = d$rates == "flat", `Growth below 1%` = d$growth == "below 1%",
              `Growth 1% or more` = d$growth == "1% or more", `A contraction` = d$contraction,
              `No contraction` = !d$contraction)
  do.call(rbind, lapply(names(grp), function(g) { z <- d[grp[[g]], ]
    data.frame(regime = g, n = nrow(z), as.list(colMeans(z[, c(names(levers), "interaction", "total")])), check.names = FALSE) }))
}
regime_c <- by_regime(ct_c); regime_s <- by_regime(ct_s)
## the central portfolio in the scenarios, exactly, against the plan
scen_tab <- do.call(rbind, lapply(sc_paths, function(k) {
  q0 <- runs[[k]]$plan; rc <- rs_c[[k]]; qc <- qx(rc); qs <- qx(rs_s[[k]])
  data.frame(scenario = k, roe_plan = mean(q0[, "roe"]), roe_central = mean(qc[, "roe"]), roe_static = mean(qs[, "roe"]),
             nii_plan = sum(q0[, "nii"]), nii_central = sum(qc[, "nii"]),
             ni_plan = sum(q0[, "ni"]), ni_central = sum(qc[, "ni"]),
             cet1_plan = min(q0[, "cet1fv"]), cet1_central = min(qc[, "cet1fv"]),
             lcr_plan = min(q0[, "lcr"]), lcr_central = min(qc[, "lcr"]), stringsAsFactors = FALSE)
}))
## the central portfolio's settings quarter by quarter in the scenarios
## (what the rules do as each scenario unfolds)
settings_path <- do.call(rbind, lapply(sc_paths, function(k) { rp <- rp_all[[k]]; rc <- rs_c[[k]]
  data.frame(scenario = k, date = inp_all[[k]]$rates$date,
             hedge_ratio = rc$quarterly$hedge_notional / nmd0,
             duration = dur_path(cen[["dur"]], rp), asset_swap = as_path(cen[["as"]], rp),
             release = rel_path(cen[["buf"]], rp) / sc_hq, payout = rc$quarterly$dividends / pmax(rc$quarterly$net_income, 1e-9),
             spread = rp$spread_seen, slope = lag1(L$scen_paths[[k]]$gov10y - L$scen_paths[[k]]$float3m, slope_jo),
             repo = lag1(L$scen_paths[[k]]$repo, repo_jo), stress = rp$stress, stringsAsFactors = FALSE) }))

## what the central portfolio uses of each limit (exact runs)
vc <- verify[verify$key == key(fc), ]
limits_used <- data.frame(
  limit = c("Tail: 5th percentile of ROE against the plan's", "CET1 at fair value, lowest quarter", "LCR at market value, lowest quarter",
            "NSFR, lowest quarter", "EVE change for ±200 bps, largest", "Earning HQLA out of bills, largest"),
  value = c(vc$p5_exact - plan_stats$all[["p5"]], vc$cet1_exact, vc$lcr_exact, vc$nsfr_exact, vc$eve_exact, fc$use_max),
  limit_value = c(0, CFG$cet1_min, CFG$lcr_min, nsfr_floor, b_central, hq_cap), stringsAsFactors = FALSE)

## variants at the central budget: which levers carry the result, and how much
## it rests on the assumed cap on the liquidity book
variants <- list(
  `Every setting` = rep(TRUE, nrow(ca)),
  `Static settings only` = !ca$rules,
  `Hedge and payout only` = ca$dur == 1 & ca$as == 1 & ca$buf == 1,
  `Liquidity book only` = ca$hedge == i_plan & ca$pay == 2,
  `Asset swap at most 25%, LCR at least 170%` = ca$as <= 3 & names(buf_set)[ca$buf] %in% c("plan", "LCR 180%", "LCR 170%", "R7"),
  `Liquidity book capped at 70% of HQLA` = ca$use_max <= 0.7 + 1e-9,
  `Liquidity book capped at 50% of HQLA` = ca$use_max <= 0.5 + 1e-9)
var_pick <- lapply(variants, function(v) pick(ca[v, ], b_central))
var_new <- unique(do.call(rbind, lapply(var_pick, function(z) z[, c("hedge", "dur", "as", "buf", "pay")])))
var_new <- var_new[!key(var_new) %in% names(exact_runs), , drop = FALSE]
for (i in seq_len(nrow(var_new))) {
  cb <- unlist(var_new[i, ])
  exact_runs[[key(var_new[i, ])]] <- lapply(stats::setNames(paths, paths), function(k) combo_run(inp_all[[k]], rp_all[[k]], cb))
}
var_tab <- do.call(rbind, lapply(names(variants), function(v) { z <- var_pick[[v]]; e <- exact_stats(exact_runs[[key(z)]])
  data.frame(variant = v, key = key(z), t(setting_labels(unlist(z[, c("hedge", "dur", "as", "buf", "pay")]))),
             d_roe = e[["roe"]] - plan_stats$all[["roe"]], d_p5 = e[["roe_p5"]] - plan_stats$all[["p5"]], win = e[["win"]],
             cet1_min = e[["cet1_min"]], lcr_min = e[["lcr_min"]], eve_max = e[["eve_max"]], use_max = z$use_max,
             surrogate_err = e[["roe"]] - z$roe, stringsAsFactors = FALSE, check.names = FALSE) }))
guard(max(abs(var_tab$surrogate_err)) < 0.0005, "the surrogate misses a variant's exact run by more than 5 bps")

## ---- 6. out of sample ---------------------------------------------------------------------------------
oos <- do.call(rbind, lapply(list(c("first", "last"), c("last", "first")), function(pr_) {
  tr <- cand[cand$half == pr_[1], ]; te <- cand[cand$half == pr_[2], ]
  do.call(rbind, lapply(c(FALSE, TRUE), function(st) {
    ch <- pick(tr, b_central, st); best <- pick(te, b_central, st)
    on_te <- te[key(te) == key(ch), ]
    data.frame(train = pr_[1], test = pr_[2], static_only = st, choice = key(ch),
               roe_train = ch$roe - plan_stats[[pr_[1]]][["roe"]], roe_test = on_te$roe - plan_stats[[pr_[2]]][["roe"]],
               roe_test_best = best$roe - plan_stats[[pr_[2]]][["roe"]],
               p5_test = on_te$roe_p5 - plan_stats[[pr_[2]]][["p5"]], feasible_test = on_te$ok_base & on_te$eve_max <= b_central + 1e-12,
               cet1_test = on_te$cet1_min, eve_test = on_te$eve_max, stringsAsFactors = FALSE)
  }))
}))
t_all <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))

saveRDS(list(levers = list(ratio = names(ratio_set), tenor = names(tenor_set), duration = names(dur_set),
                           asset_swap = names(as_set), buffer = names(buf_set), payout = names(pay_set)),
             hedge_set = hedge_set, release_by_target = stats::setNames(rel_tg, sprintf("LCR %.0f%%", 100 * lcr_tg)),
             plan_opt = plan_opt, plan_stats = plan_stats, n_episodes = K, n_paths = length(paths),
             n_combos = nrow(ca), n_admissible = sum(ca$use_max <= hq_cap + 1e-9), n_feasible = sum(ca$ok_base),
             n_runs_per_path = n_runs, binds = binds,
             candidates = ca, frontier = frontier, verify = verify, budgets = budgets, b_central = b_central,
             central = list(all = setting_labels(cen), static = setting_labels(cen_s), opt = cen, opt_static = cen_s),
             contrib = list(all = ct_c, static = ct_s), regime = list(all = regime_c, static = regime_s), regimes = reg,
             scenarios = scen_tab, settings_path = settings_path, oos = oos, limits_used = limits_used, variants = var_tab,
             limits = c(cet1 = CFG$cet1_min, lcr = CFG$lcr_min, nsfr = nsfr_floor, hq_cap = hq_cap),
             minutes = c(runs = t_runs, enumeration = t_enum, total = t_all)),
        out_path("risk_budget.rds"))
ngfb_log("risk budget: %d combinations (%d feasible) from %d runs per path on %d paths in %.1f min; at %.0f%% of CET1 EVE: %s %+.0f bps (static only %+.0f bps)",
         nrow(ca), sum(ca$ok_base), n_runs, length(paths), t_all, 100 * b_central,
         paste(setting_labels(cen), collapse = " / "), 1e4 * (fc$roe - plan_stats$all[["roe"]]), 1e4 * (fs$roe - plan_stats$all[["roe"]]))
