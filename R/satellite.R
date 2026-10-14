# =============================================================================
#  satellite.R -- Stage 1: macro -> banking-sector "satellite" models
#
#  Each sector driver y (a product's volume growth, a margin, the credit-loss
#  ratio, ...) gets an autoregressive distributed-lag model
#
#      y_t = c + rho * y_{t-1} + sum_j beta_j * x_{j, t - l_j} + e_t
#
#  chosen by a disciplined search rather than by hand:
#    1. every subset of at most `max_k` candidate drivers (at most one per
#       economic group, so repo and its change never compete in one equation),
#       each at one lag in `lags`;
#    2. a specification survives only if every driver carries its a-priori
#       economic sign (in the short run AND in the long-run multiplier
#       beta / (1 - rho)) with |t_HAC| >= t_min, and |rho| < rho_max;
#    3. survivors are ranked by DYNAMIC pseudo-out-of-sample RMSE: from each
#       expanding-window origin the model is re-estimated and simulated h
#       quarters ahead conditional on the realised macro path -- exactly how
#       it is used in a scenario -- and errors are pooled over horizons 1..h;
#    4. the AR-only model is the benchmark. If no macro specification beats
#       it, that is reported as a finding, not hidden.
# =============================================================================

.sat_design <- function(panel, target, drivers, lags, ar = TRUE, controls = character(0)) {
  y <- panel[[target]]
  cols <- list(`(Intercept)` = rep(1, length(y)))
  if (ar) cols[["ar1"]] <- lag_vec(y, 1L)
  for (cc in controls) cols[[cc]] <- panel[[cc]]
  for (j in seq_along(drivers)) {
    nm <- sprintf("%s_L%d", drivers[j], lags[j])
    cols[[nm]] <- lag_vec(panel[[drivers[j]]], lags[j])
  }
  X <- do.call(cbind, cols)
  ok <- stats::complete.cases(cbind(y, X))
  list(y = y, X = X, ok = ok)
}

# dynamic h-step simulation from origin index o (rows are panel rows)
.sat_simulate <- function(b, X, y, o, h, ar) {
  n <- length(y); out <- rep(NA_real_, h)
  yprev <- y[o]
  for (s in seq_len(h)) {
    t <- o + s
    if (t > n) break
    x <- X[t, ]
    if (ar) x["ar1"] <- yprev
    if (any(is.na(x))) break
    out[s] <- sum(b * x)
    yprev <- out[s]
  }
  out
}

.sat_oos <- function(dz, ar, h, min_train, origin_step = 1L) {
  idx <- which(dz$ok)
  if (length(idx) < min_train + h) return(list(rmse = NA_real_, err = NULL))
  origins <- idx[idx >= idx[min_train] & idx <= max(idx) - 1L]
  if (origin_step > 1L) origins <- origins[seq(length(origins), 1L, by = -origin_step)]
  origins <- sort(origins)
  E <- matrix(NA_real_, length(origins), h)
  n <- length(dz$y)
  for (i in seq_along(origins)) {
    o <- origins[i]
    tr <- idx[idx <= o]
    b <- stats::lm.fit(dz$X[tr, , drop = FALSE], dz$y[tr])$coefficients
    b[is.na(b)] <- 0
    p <- .sat_simulate(b, dz$X, dz$y, o, h, ar)
    tt <- o + seq_len(h)
    a <- rep(NA_real_, h); a[tt <= n] <- dz$y[tt[tt <= n]]
    E[i, ] <- a - p
  }
  err <- data.frame(origin = rep(origins, h), h = rep(seq_len(h), each = length(origins)),
                    err = as.numeric(E))
  err <- err[is.finite(err$err), , drop = FALSE]
  list(rmse = if (NROW(err)) sqrt(mean(err$err^2)) else NA_real_, err = err)
}

#' Search for a sign-restricted ARDL satellite model
#'
#' @param panel Quarterly data frame with `date`, the target and the drivers.
#' @param target Name of the dependent variable.
#' @param candidates Character vector of candidate drivers.
#' @param signs Named numeric vector of a-priori signs (+1, -1, or 0 = free)
#'   for the candidates.
#' @param groups Optional named character vector mapping candidate -> group;
#'   at most one candidate per group enters a specification.
#' @param max_k Maximum number of macro drivers.
#' @param lags Admissible lags (quarters).
#' @param ar Include the lagged dependent variable.
#' @param h Horizon (quarters) of the dynamic out-of-sample evaluation.
#' @param min_train Minimum estimation window for the first origin.
#' @param t_min Minimum |HAC t| for each driver.
#' @param rho_max Maximum |rho|.
#' @param sample_from Optional first date of the estimation sample.
#' @param controls Always-included control regressors (e.g. COVID-quarter
#'   dummies); they are set to zero in projections.
#' @param tol Parsimony tolerance: specifications within `tol` (relative) of
#'   the best out-of-sample RMSE are treated as equivalent and the smallest
#'   one wins.
#' @param required Drivers that enter every specification (e.g. the policy
#'   rate in a deposit-cost equation, so the long run is anchored to it).
#' @param comb_k Size of the out-of-sample forecast combination: the `comb_k`
#'   sign-valid specifications with the lowest out-of-sample RMSE, equally
#'   weighted (stored as `comb`, an alternative to the BIC-weighted `bma`).
#' @param origin_step Use every `origin_step`-th pseudo-out-of-sample origin
#'   (counted back from the last), e.g. 3 for quarter-end origins in a
#'   monthly panel.
#' @param oos_top Evaluate out of sample only the `oos_top` sign-valid
#'   specifications with the lowest BIC (a screen for large monthly lag
#'   grids); `Inf` evaluates all of them.
#' @return An object of class `baeconstrat_satellite`.
#' @export
satellite_search <- function(panel, target, candidates, signs, groups = NULL,
                             max_k = 3L, lags = 0:2, ar = TRUE, h = 4L,
                             min_train = 28L, t_min = 1.0, rho_max = 0.98,
                             sample_from = NULL, controls = character(0), tol = 0.02,
                             required = character(0), comb_k = 5L, origin_step = 1L,
                             oos_top = Inf) {
  if (!is.null(sample_from)) panel <- panel[panel$date >= as.Date(sample_from), , drop = FALSE]
  candidates <- intersect(candidates, names(panel))
  candidates <- candidates[vapply(candidates, function(v) sum(is.finite(panel[[v]])) > min_train, TRUE)]
  if (is.null(groups)) groups <- stats::setNames(candidates, candidates)
  grp <- groups[candidates]; grp[is.na(grp)] <- candidates[is.na(grp)]

  # benchmark: AR(1) (or the mean when ar = FALSE)
  dz0 <- .sat_design(panel, target, character(0), integer(0), ar, controls)
  oos0 <- .sat_oos(dz0, ar, h, min_train, origin_step)

  specs <- list()
  req <- intersect(required, candidates)
  free <- setdiff(candidates, req)
  if (length(req)) {
    # required drivers enter every specification (at every admissible lag)
    lg0 <- as.matrix(expand.grid(rep(list(lags), length(req))))
    for (r in seq_len(nrow(lg0))) specs[[length(specs) + 1L]] <- list(d = req, l = as.integer(lg0[r, ]))
  }
  kmax <- min(max_k - length(req), length(free))
  for (k in seq_len(max(kmax, 0L))) {
    cmb <- utils::combn(free, k, simplify = FALSE)
    cmb <- Filter(function(cc) !anyDuplicated(grp[c(req, cc)]), cmb)
    cmb <- lapply(cmb, function(cc) c(req, cc))
    lg <- as.matrix(expand.grid(rep(list(lags), length(req) + k)))
    for (cc in cmb) for (r in seq_len(nrow(lg))) specs[[length(specs) + 1L]] <- list(d = cc, l = as.integer(lg[r, ]))
  }

  rows <- vector("list", length(specs))
  keep_coef <- list(); keep_vcov <- list()
  rej <- c(sample = 0L, rho = 0L, sign = 0L, t = 0L)
  for (i in seq_along(specs)) {
    s <- specs[[i]]
    dz <- .sat_design(panel, target, s$d, s$l, ar, controls)
    if (sum(dz$ok) < min_train + h) { rej[["sample"]] <- rej[["sample"]] + 1L; next }
    f <- ols_hac(dz$y[dz$ok], dz$X[dz$ok, , drop = FALSE])
    bn <- sprintf("%s_L%d", s$d, s$l)
    rho <- if (ar) f$coef[["ar1"]] else 0
    if (abs(rho) >= rho_max) { rej[["rho"]] <- rej[["rho"]] + 1L; next }
    sg <- signs[s$d]; sg[is.na(sg)] <- 0
    beta <- f$coef[bn]; tt <- f$t[bn]
    lr <- beta / (1 - rho)
    ok_sign <- all(sg == 0 | (sign(beta) == sg & sign(lr) == sg))
    ok_t <- all(abs(tt) >= t_min, na.rm = FALSE)
    if (!isTRUE(ok_sign)) { rej[["sign"]] <- rej[["sign"]] + 1L; next }
    if (!isTRUE(ok_t)) { rej[["t"]] <- rej[["t"]] + 1L; next }
    keep_coef[[as.character(i)]] <- f$coef
    keep_vcov[[as.character(i)]] <- f$vcov
    rows[[i]] <- data.frame(spec = paste(sprintf("%s[L%d]", s$d, s$l), collapse = " + "),
                            k = length(s$d), rmse_oos = NA_real_, bic = f$bic, r2 = f$r2,
                            rho = rho, min_abs_t = min(abs(tt)), n = f$n,
                            stringsAsFactors = FALSE)
    rows[[i]]$id <- i
  }
  tab <- rbind_fill(rows)
  n_sv <- if (is.null(tab)) 0L else nrow(tab)
  ## out-of-sample evaluation of the sign-valid specifications (all of them,
  ## or the oos_top with the lowest BIC)
  if (!is.null(tab)) {
    ev <- tab$id[order(tab$bic)]
    if (is.finite(oos_top)) ev <- utils::head(ev, oos_top)
    for (i in ev) {
      s <- specs[[i]]
      dz <- .sat_design(panel, target, s$d, s$l, ar, controls)
      tab$rmse_oos[tab$id == i] <- .sat_oos(dz, ar, h, min_train, origin_step)$rmse
    }
  }
  best <- NULL
  if (!is.null(tab)) {
    tab <- tab[is.finite(tab$rmse_oos), , drop = FALSE]
    tab <- tab[order(tab$rmse_oos, tab$bic), , drop = FALSE]
    if (nrow(tab)) {
      # parsimony: among specifications within `tol` of the best out-of-sample
      # RMSE, take the one with the fewest drivers, then the lowest BIC
      near <- tab[tab$rmse_oos <= tab$rmse_oos[1] * (1 + tol), , drop = FALSE]
      near <- near[order(near$k, near$bic), , drop = FALSE]
      best <- specs[[near$id[1]]]
      tab$selected <- tab$id == near$id[1]
    }
  }
  ## Bayesian model averaging over the sign-valid specifications (BIC
  ## approximation to posterior model probabilities, Raftery 1995)
  bma <- NULL
  if (!is.null(tab) && nrow(tab)) {
    wt <- exp(-0.5 * (tab$bic - min(tab$bic)))
    wt <- wt / sum(wt)
    keep <- wt > 1e-4
    keep[which.max(wt)] <- TRUE
    ids <- tab$id[keep]; wt <- wt[keep] / sum(wt[keep])
    bma <- list(specs = specs[ids], coefs = keep_coef[as.character(ids)], weights = wt,
                spec_labels = tab$spec[keep], rmse_oos = tab$rmse_oos[keep],
                vcovs = keep_vcov[as.character(ids)])
  }
  ## out-of-sample forecast combination: the comb_k best specifications by
  ## dynamic out-of-sample RMSE, equally weighted (a "trimmed" combination in
  ## the sense of Aiolfi and Timmermann, 2006)
  comb <- NULL
  if (!is.null(tab) && nrow(tab)) {
    top <- utils::head(tab$id[order(tab$rmse_oos, tab$bic)], comb_k)
    comb <- list(specs = specs[top], coefs = keep_coef[as.character(top)],
                 weights = rep(1 / length(top), length(top)),
                 spec_labels = tab$spec[match(top, tab$id)], rmse_oos = tab$rmse_oos[match(top, tab$id)])
  }
  out <- list(target = target, ar = ar, h = h, min_train = min_train, required = req,
              origin_step = origin_step, oos_top = oos_top, lags = lags, rejected = rej,
              n_sign_valid = n_sv,
              benchmark_rmse = oos0$rmse, table = tab, n_specs = length(specs),
              signs = signs, candidates = candidates, sample_from = sample_from,
              controls = controls, n_valid = if (is.null(tab)) 0L else nrow(tab), bma = bma,
              comb = comb)
  if (is.null(best)) {
    best <- list(d = character(0), l = integer(0))
    out$note <- "no macro specification survived the sign/t restrictions; AR benchmark used"
  }
  fit <- satellite_fit(panel, target, best$d, best$l, ar = ar, h = h, min_train = min_train,
                       controls = controls, origin_step = origin_step)
  out$spec <- best; out$fit <- fit$fit; out$oos <- fit$oos
  out$skill <- 1 - out$oos$rmse / oos0$rmse
  if (!is.null(out$table) && nrow(out$table)) out$table <- utils::head(out$table, 25L)
  class(out) <- "baeconstrat_satellite"
  out
}

#' Estimate one satellite specification
#' @inheritParams satellite_search
#' @param drivers Driver names.
#' @param lags Integer lags, one per driver.
#' @param controls Always-included control regressors.
#' @param origin_step Out-of-sample origin spacing (see [satellite_search()]).
#' @return List with `fit` (see [ols_hac()]), `oos`, `spec`.
#' @export
satellite_fit <- function(panel, target, drivers, lags, ar = TRUE, h = 4L, min_train = 28L,
                          controls = character(0), origin_step = 1L) {
  dz <- .sat_design(panel, target, drivers, lags, ar, controls)
  f <- ols_hac(dz$y[dz$ok], dz$X[dz$ok, , drop = FALSE])
  f$dates <- panel$date[dz$ok]
  f$y <- dz$y[dz$ok]
  oo <- .sat_oos(dz, ar, h, min_train, origin_step)
  list(fit = f, oos = oo, spec = list(d = drivers, l = lags))
}

#' Long-run multipliers of a satellite model with delta-method standard errors
#' @param sat A `baeconstrat_satellite` object.
#' @return Data frame `driver, lag, beta, se, t, long_run, long_run_se`.
#' @export
satellite_multipliers <- function(sat) {
  f <- sat$fit; d <- sat$spec$d
  if (!length(d)) return(data.frame())
  bn <- sprintf("%s_L%d", d, sat$spec$l)
  rho <- if (sat$ar) f$coef[["ar1"]] else 0
  out <- lapply(seq_along(bn), function(j) {
    b <- f$coef[[bn[j]]]
    lr <- b / (1 - rho)
    # gradient of b/(1-rho) w.r.t. (b, rho)
    g <- c(1 / (1 - rho), b / (1 - rho)^2)
    nm <- c(bn[j], if (sat$ar) "ar1")
    V <- f$vcov[nm, nm, drop = FALSE]
    g <- g[seq_along(nm)]
    data.frame(driver = d[j], lag = sat$spec$l[j], beta = b, se = f$se[[bn[j]]],
               t = f$t[[bn[j]]], long_run = lr,
               long_run_se = sqrt(as.numeric(t(g) %*% V %*% g)))
  })
  do.call(rbind, out)
}

#' Project a satellite model along a macro path
#'
#' @param sat A `baeconstrat_satellite`.
#' @param history Quarterly history (must contain the target and drivers).
#' @param path Future quarters: data frame with `date` and every driver.
#' @param add_factor Optional constant added to every projected value (used
#'   for judgemental overlays; zero by default).
#' @param method `"bma"` (default) averages the projections of every
#'   sign-valid specification with BIC weights; `"comb"` averages the
#'   out-of-sample combination (see `comb_k` in [satellite_search()]);
#'   `"best"` uses the selected specification.
#' @param ic_window If positive, an intercept correction: the mean one-step
#'   residual over the last `ic_window` observed quarters is carried through
#'   the projection (see [satellite_intercept_correction()]).
#' @param ic_mode How the correction enters. `"level"` (default) shifts the
#'   projected path by the mean residual; `"equation"` adds it to the
#'   equation's intercept inside the dynamic simulation, so it accumulates
#'   through the autoregressive term to `ic / (1 - rho)` -- the right
#'   correction for a shift in the equilibrium mean, but one that amplifies
#'   noise without bound as `rho` approaches one.
#' @return Data frame `date, value` for the path dates.
#' @export
satellite_project <- function(sat, history, path, add_factor = 0,
                              method = c("bma", "best", "comb"), ic_window = 0L,
                              ic_mode = c("level", "equation")) {
  method <- match.arg(method); ic_mode <- match.arg(ic_mode)
  avg <- if (method == "bma") sat$bma else if (method == "comb") sat$comb else NULL
  if (!is.null(avg) && length(avg$weights) > 1L) {
    pr <- lapply(seq_along(avg$weights), function(i) {
      one <- sat
      one$spec <- avg$specs[[i]]
      one$fit$coef <- avg$coefs[[i]]
      one$bma <- NULL; one$comb <- NULL
      satellite_project(one, history, path, add_factor, method = "best", ic_window = ic_window,
                        ic_mode = ic_mode)$value
    })
    v <- Reduce(`+`, Map(function(p, w) p * w, pr, avg$weights))
    return(data.frame(date = path$date, value = v))
  }
  if (!is.null(avg) && length(avg$weights) == 1L) {
    sat$spec <- avg$specs[[1]]; sat$fit$coef <- avg$coefs[[1]]
  }
  ic <- if (ic_window > 0L) satellite_intercept_correction(sat, history, ic_window) else 0
  if (ic_mode == "equation") { add_factor <- add_factor + ic; ic <- 0 }
  d <- sat$spec$d; l <- sat$spec$l; tg <- sat$target
  cols <- unique(c("date", tg, d))
  h <- history[, intersect(cols, names(history)), drop = FALSE]
  p <- path[, intersect(c("date", d), names(path)), drop = FALSE]
  p[[tg]] <- NA_real_
  for (m in setdiff(names(h), names(p))) p[[m]] <- NA_real_
  all <- rbind(h[, names(h)], p[, names(h)])
  all <- all[order(all$date), , drop = FALSE]
  b <- sat$fit$coef
  rho <- if (sat$ar) b[["ar1"]] else 0
  fut <- which(all$date %in% p$date)
  y <- all[[tg]]
  sm <- .seasonal_effect(b, all$date)
  for (t in fut) {
    v <- b[["(Intercept)"]] + rho * (if (sat$ar) y[t - 1L] else 0) + sm[t]
    for (j in seq_along(d)) v <- v + b[[sprintf("%s_L%d", d[j], l[j])]] * all[[d[j]]][t - l[j]]
    y[t] <- v + add_factor
  }
  data.frame(date = all$date[fut], value = y[fut] + ic)
}

# month-of-year effects of a monthly satellite (controls sm02..sm12; January
# is the base); zero for quarterly satellites, which have none
.seasonal_effect <- function(b, dates) {
  sm <- b[grepl("^sm[0-9]{2}$", names(b))]
  if (!length(sm)) return(rep(0, length(dates)))
  k <- sprintf("sm%02d", as.integer(format(dates, "%m")))
  v <- unname(sm[k]); v[is.na(v)] <- 0
  v
}

#' Month-of-year dummies for a monthly panel
#' @param dates Month dates.
#' @return Data frame with columns `sm02` ... `sm12` (January is the base).
#' @export
month_dummies <- function(dates) {
  m <- as.integer(format(dates, "%m"))
  out <- as.data.frame(lapply(2:12, function(k) as.numeric(m == k)))
  names(out) <- sprintf("sm%02d", 2:12)
  out
}

#' @export
print.baeconstrat_satellite <- function(x, ...) {
  cat(sprintf("satellite: %s ~ %s%s\n", x$target,
              if (x$ar) "AR(1) + " else "",
              if (length(x$spec$d)) paste(sprintf("%s[L%d]", x$spec$d, x$spec$l), collapse = " + ")
              else "(no macro driver)"))
  cat(sprintf("  n = %d, R2 = %.2f, OOS RMSE (h<=%d) = %.3f vs AR %.3f (skill %+.1f%%)\n",
              x$fit$n, x$fit$r2, x$h, x$oos$rmse, x$benchmark_rmse, 100 * x$skill))
  invisible(x)
}

#' Model-averaged long-run multipliers and posterior inclusion probabilities
#'
#' @param sat A `baeconstrat_satellite` with a `bma` element.
#' @return Data frame `driver, pip, long_run` (long-run multiplier averaged
#'   over models, zero where a driver is absent), `long_run_cond` (conditional
#'   on inclusion).
#' @export
satellite_bma_summary <- function(sat) {
  b <- sat$bma
  if (is.null(b)) return(data.frame())
  rows <- list()
  for (i in seq_along(b$weights)) {
    sp <- b$specs[[i]]; cf <- b$coefs[[i]]
    rho <- if (sat$ar) cf[["ar1"]] else 0
    for (j in seq_along(sp$d)) {
      rows[[length(rows) + 1L]] <- data.frame(driver = sp$d[j], w = b$weights[i],
                                              lr = cf[[sprintf("%s_L%d", sp$d[j], sp$l[j])]] / (1 - rho))
    }
  }
  d <- do.call(rbind, rows)
  if (is.null(d)) return(data.frame())
  out <- do.call(rbind, lapply(split(d, d$driver), function(z)
    data.frame(driver = z$driver[1], pip = sum(z$w), long_run = sum(z$w * z$lr),
               long_run_cond = sum(z$w * z$lr) / sum(z$w))))
  out[order(-out$pip), ]
}

#' Intercept correction for a satellite projection
#'
#' Ratio targets such as non-interest revenue over assets drift structurally
#' (fee pressure, digitisation), so a model estimated on 2008-2025 mean-reverts
#' towards a level the sector has left. The standard remedy (Clements and
#' Hendry, 1998) carries the model's recent one-step error into the forecast:
#' the mean residual over the last `k` observed quarters, excluding quarters
#' flagged by an impulse or COVID dummy, is added to every projected quarter.
#' @param sat A `baeconstrat_satellite` (single specification).
#' @param history Quarterly history with the target and drivers.
#' @param k Window (quarters).
#' @return Scalar add factor (target units).
#' @export
satellite_intercept_correction <- function(sat, history, k = 4L) {
  h <- history[order(history$date), , drop = FALSE]
  y <- h[[sat$target]]
  b <- sat$fit$coef
  d <- sat$spec$d; l <- sat$spec$l
  obs <- which(is.finite(y))
  if (!length(obs)) return(0)
  ctl <- intersect(sat$controls %||% character(0), names(b))
  ctl <- ctl[!grepl("^sm[0-9]{2}$", ctl)]          # month dummies are seasonality, not outliers
  sm <- .seasonal_effect(b, h$date)
  flagged <- function(t) {
    for (cc in ctl) {
      if (cc %in% names(h) && isTRUE(h[[cc]][t] != 0)) return(TRUE)
      m <- regmatches(cc, regexpr("[0-9]{6}$", cc))
      if (length(m) && identical(format(h$date[t], "%Y%m"), m)) return(TRUE)
    }
    FALSE
  }
  cand <- rev(obs)
  r <- numeric(0)
  for (t in cand) {
    if (length(r) >= k) break
    if (t < 2L || t <= max(c(l, 0L)) || flagged(t)) next
    v <- b[["(Intercept)"]] + sm[t]
    if (isTRUE(sat$ar)) v <- v + b[["ar1"]] * y[t - 1L]
    for (j in seq_along(d)) v <- v + b[[sprintf("%s_L%d", d[j], l[j])]] * h[[d[j]]][t - l[j]]
    e <- y[t] - v
    if (is.finite(e)) r <- c(r, e)
  }
  if (!length(r)) 0 else mean(r)
}
