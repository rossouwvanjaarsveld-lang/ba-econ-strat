# =============================================================================
#  optimise.R -- pre-positioning the balance sheet ahead of a scenario
#
#  Decision: a vector x of balance-sheet moves made at the jump-off, each a
#  transfer of 1% of total assets (or, for the hedge, 1% of the NMD base)
#  between two lines -- e.g. HQLA -> mortgages, wholesale -> retail savings,
#  or extending the receive-fixed structural hedge.
#
#  The full engine is the source of truth. Around the status quo the engine is
#  close to linear in x, so for every scenario s we measure, by central finite
#  differences, the horizon-average ROE gradient G[s, ] and the gradients of
#  the binding constraints (CET1 and LCR troughs).
#
#  Why not simply maximise the engine's ROE? Because the engine prices new
#  volume at the book's AVERAGE yield, loss rate and cost ratio and holds book
#  equity fixed, so almost every move out of HQLA "earns" 30-50 bps of ROE per
#  1% of assets in every scenario. That static attractiveness is a franchise
#  and capacity question (marginal pricing, cost-to-serve, adverse selection)
#  that public data cannot price. The positioning question is different: what
#  should change BECAUSE scenario X is coming? We therefore treat the status
#  quo as optimal under the baseline -- the bank's balance sheet reflects its
#  central plan -- which reveals an implied marginal cost for every move,
#  c = G[baseline, ] (the same reverse-optimisation step Black and Litterman
#  apply to market weights). Positioning then optimises the scenario-specific
#  part of the gradient, G[s, ] - c:
#
#    max_x  sum_s w_s (R0_s + (G_s - c) x) - lambda/2 Var_w(R0 + G x) - 1/2 x'Kx
#    s.t.   CET1_s(0) + Jc_s x >= cet1_min,  LCR_s(0) + Jl_s x >= lcr_min,
#           lower <= x <= upper
#
#  K is the execution cost (pricing concessions and acquisition cost to grow
#  a book quickly, attrition to shrink it). Every result is re-run through the
#  full engine and reported NET of c'x and 1/2 x'Kx.
# =============================================================================

#' Apply balance-sheet moves to a jump-off balance sheet
#'
#' @param bs Balance-sheet definition (see [run_engine()]).
#' @param moves Move table: `move`, `from`, `to`, `unit` ("assets" or "nmd"
#'   or "hedge").
#' @param x Numeric vector of move sizes (percent of the unit base).
#' @param hedge Hedge list (modified for hedge moves).
#' @return List `bs`, `hedge`.
#' @export
apply_moves <- function(bs, moves, x, hedge = NULL) {
  ta <- sum(bs$balance[bs$side == "asset"])
  nmd <- sum(bs$balance[bs$line %in% c("beh_operational", "beh_savings", "beh_notice")])
  for (k in seq_len(nrow(moves))) {
    if (!is.finite(x[k]) || x[k] == 0) next
    if (moves$unit[k] == "hedge") {
      if (!is.null(hedge)) hedge$notional <- max(0, hedge$notional + x[k] / 100 * nmd)
      next
    }
    base <- if (moves$unit[k] == "nmd") nmd else ta
    amt <- x[k] / 100 * base
    i_from <- match(moves$from[k], bs$line); i_to <- match(moves$to[k], bs$line)
    bs$balance[i_from] <- bs$balance[i_from] - amt
    bs$balance[i_to] <- bs$balance[i_to] + amt
  }
  list(bs = bs, hedge = hedge)
}

#' Evaluate a positioning under one scenario with the full engine
#' @param x Move vector.
#' @param ctx Context list: `bs`, `moves`, `hedge`, and `run(bs, hedge, s)`
#'   which returns an engine run for scenario `s`.
#' @param s Scenario index/name.
#' @param quarters Quarters over which ROE is averaged.
#' @return Named numeric: `roe`, `cet1_min`, `lcr_min`, `nim`, `nim_eq`
#'   (pre-tax profit over interest-earning assets, the ALCO-bridge unit) and
#'   `net_income` (total over the quarters, R'000).
#' @export
eval_positioning <- function(x, ctx, s, quarters = NULL) {
  m <- apply_moves(ctx$bs, ctx$moves, x, ctx$hedge)
  if (any(m$bs$balance[m$bs$side != "equity"] < -1e-6))
    return(c(roe = NA, cet1_min = NA, lcr_min = NA, nim = NA, nim_eq = NA, net_income = NA))
  r <- ctx$run(m$bs, m$hedge, s)
  q <- r$quarterly
  if (!is.null(quarters)) q <- q[quarters, , drop = FALSE]
  c(roe = mean(q$roe), cet1_min = min(q$cet1_ratio), lcr_min = min(q$lcr), nim = mean(q$nim),
    nim_eq = mean(4 * q$pbt / q$iea), net_income = sum(q$net_income))
}

#' Finite-difference gradients of ROE and constraints, per scenario
#' @param ctx Context (see [eval_positioning()]).
#' @param scenarios Scenario names.
#' @param step Finite-difference step (percent of base).
#' @param quarters Averaging quarters.
#' @return List: `R0`, `C0`, `L0`, `N0`, `NI0` (status-quo ROE, CET1 and
#'   LCR troughs, NIM-equivalent and net income, by scenario) and `G`, `Jc`,
#'   `Jl`, `Jn`, `Jni` (scenario x move matrices of their gradients; `Jn` is
#'   the NIM-equivalent, `Jni` net income in R'000 over the horizon).
#' @export
positioning_gradients <- function(ctx, scenarios, step = 0.5, quarters = NULL) {
  K <- nrow(ctx$moves)
  mk <- function() matrix(NA_real_, length(scenarios), K, dimnames = list(scenarios, ctx$moves$move))
  G <- mk(); Jc <- mk(); Jl <- mk(); Jn <- mk(); Jni <- mk()
  R0 <- C0 <- L0 <- N0 <- NI0 <- stats::setNames(numeric(length(scenarios)), scenarios)
  for (s in scenarios) {
    b <- eval_positioning(rep(0, K), ctx, s, quarters)
    R0[s] <- b[["roe"]]; C0[s] <- b[["cet1_min"]]; L0[s] <- b[["lcr_min"]]; N0[s] <- b[["nim_eq"]]
    NI0[s] <- b[["net_income"]]
    for (k in seq_len(K)) {
      e <- rep(0, K); e[k] <- step
      up <- eval_positioning(e, ctx, s, quarters)
      dn <- eval_positioning(-e, ctx, s, quarters)
      one_sided <- any(!is.finite(dn))              # at a boundary
      if (one_sided) dn <- b
      h <- if (one_sided) step else 2 * step
      G[s, k] <- (up[["roe"]] - dn[["roe"]]) / h
      Jc[s, k] <- (up[["cet1_min"]] - dn[["cet1_min"]]) / h
      Jl[s, k] <- (up[["lcr_min"]] - dn[["lcr_min"]]) / h
      Jn[s, k] <- (up[["nim_eq"]] - dn[["nim_eq"]]) / h
      Jni[s, k] <- (up[["net_income"]] - dn[["net_income"]]) / h
    }
  }
  list(R0 = R0, C0 = C0, L0 = L0, N0 = N0, NI0 = NI0, G = G, Jc = Jc, Jl = Jl, Jn = Jn, Jni = Jni,
       step = step)
}

#' Execution-cost matrix
#'
#' `cost` is the ROE (decimal) given up by a one-percentage-point move made
#' inside the year; costs are quadratic, so a 2-point move costs four times as
#' much.
#' @param moves Move table with `cost`.
#' @return Diagonal matrix K with 1/2 x'Kx the execution cost of x.
#' @export
execution_cost_matrix <- function(moves) diag(2 * moves$cost, nrow(moves))

#' Solve the pre-positioning quadratic programme
#'
#' @param grad Output of [positioning_gradients()].
#' @param moves Move table with `lower`, `upper` (percent of base) and `cost`
#'   (see [execution_cost_matrix()]).
#' @param weights Scenario probabilities (named, summing to one).
#' @param lambda Risk aversion to the cross-scenario variance of ROE.
#' @param cet1_min,lcr_min Constraint floors (decimals).
#' @param reference Scenario under which the status quo is taken to be
#'   optimal (its gradient is the implied marginal cost of each move);
#'   `NULL` maximises the engine's raw ROE.
#' @return List: `x` (optimal moves), `roe_pred` (linear prediction of ROE
#'   net of implied cost, by scenario, before execution cost), `exec_cost`,
#'   `implied_cost`, `objective`, `status`.
#' @export
solve_positioning <- function(grad, moves, weights, lambda = 0, cet1_min = 0.115,
                              lcr_min = 1.0, reference = "baseline") {
  if (!requireNamespace("quadprog", quietly = TRUE)) stop("package 'quadprog' is required")
  sc <- names(weights); w <- as.numeric(weights) / sum(weights)
  G <- grad$G[sc, , drop = FALSE]; R0 <- grad$R0[sc]
  K <- ncol(G)
  c0 <- if (is.null(reference)) stats::setNames(rep(0, K), colnames(G)) else grad$G[reference, ]
  Gn <- sweep(G, 2, c0)
  gbar <- colSums(w * Gn)
  Gc <- sweep(G, 2, colSums(w * G))
  covG <- t(Gc) %*% (w * Gc)
  rc <- R0 - sum(w * R0)
  covGR <- as.numeric(t(Gc) %*% (w * rc))
  Kmat <- execution_cost_matrix(moves)
  D <- Kmat + lambda * covG + diag(1e-10, K)
  d <- gbar - lambda * covGR
  # constraints A'x >= b
  A <- cbind(diag(K), -diag(K))
  b <- c(moves$lower, -moves$upper)
  okc <- is.finite(grad$C0[sc]); okl <- is.finite(grad$L0[sc])
  if (any(okc)) { A <- cbind(A, t(grad$Jc[sc, , drop = FALSE][okc, , drop = FALSE]))
                  b <- c(b, cet1_min - grad$C0[sc][okc]) }
  if (any(okl)) { A <- cbind(A, t(grad$Jl[sc, , drop = FALSE][okl, , drop = FALSE]))
                  b <- c(b, lcr_min - grad$L0[sc][okl]) }
  A[!is.finite(A)] <- 0
  sol <- tryCatch(quadprog::solve.QP(Dmat = D, dvec = d, Amat = A, bvec = b, meq = 0),
                  error = function(e) NULL)
  if (is.null(sol)) {
    # infeasible: relax the floors to the status-quo levels and retry
    b2 <- b; b2[-(1:(2 * K))] <- pmin(b2[-(1:(2 * K))], 0)
    sol <- tryCatch(quadprog::solve.QP(Dmat = D, dvec = d, Amat = A, bvec = b2, meq = 0),
                    error = function(e) NULL)
    status <- if (is.null(sol)) "failed" else "floors relaxed to status quo"
  } else status <- "optimal"
  x <- if (is.null(sol)) rep(0, K) else sol$solution
  x[abs(x) < 1e-6] <- 0
  names(x) <- moves$move
  binding <- if (is.null(sol)) character(0) else {
    lhs <- as.numeric(t(A) %*% x) - b
    nm <- c(paste0("lower:", moves$move), paste0("upper:", moves$move),
            if (any(okc)) paste0("cet1:", sc[okc]), if (any(okl)) paste0("lcr:", sc[okl]))
    nm[abs(lhs) < 1e-7]
  }
  list(x = x, roe_pred = R0 + as.numeric(Gn %*% x), exec_cost = 0.5 * sum(x * (Kmat %*% x)),
       implied_cost = c0, objective = if (is.null(sol)) NA else -sol$value,
       status = status, binding = binding)
}

#' Net value of a positioning under one scenario (full engine)
#'
#' Engine ROE less the implied marginal cost of the moves and their execution
#' cost -- the ROE a bank would actually book. Net income is treated the same
#' way: the engine's net income over the horizon, less the implied marginal
#' cost in net-income terms (`implied_ni`, the net-income gradient under the
#' reference scenario, so that the status quo is optimal in net income too)
#' and the execution cost converted at `m_ni`, the scenario's net income per
#' unit of ROE.
#' @param x Move vector.
#' @param ctx Context.
#' @param s Scenario.
#' @param implied_cost Implied marginal cost per unit move (ROE, decimal).
#' @param moves Move table.
#' @param m_ni Net income (R'000 over the horizon) per unit of ROE; `NA`
#'   leaves `ni_net` missing.
#' @param implied_ni Implied marginal cost per unit move in net income
#'   (R'000); default `implied_cost * m_ni`.
#' @return Named numeric: `roe_net`, `roe_engine`, `cet1_min`, `lcr_min`,
#'   `nim_eq`, `ni_net`, `ni_engine`, `cost` (ROE, implied plus execution),
#'   `exec_cost` (ROE).
#' @export
positioning_value <- function(x, ctx, s, implied_cost, moves, m_ni = NA_real_, implied_ni = NULL) {
  e <- eval_positioning(x, ctx, s)
  Kmat <- execution_cost_matrix(moves)
  exec <- 0.5 * sum(x * (Kmat %*% x))
  cost <- sum(implied_cost * x) + exec
  imp_ni <- if (is.null(implied_ni)) sum(implied_cost * x) * m_ni else sum(implied_ni * x)
  c(roe_net = e[["roe"]] - cost, roe_engine = e[["roe"]], cet1_min = e[["cet1_min"]],
    lcr_min = e[["lcr_min"]], nim_eq = e[["nim_eq"]], ni_net = e[["net_income"]] - imp_ni - exec * m_ni,
    ni_engine = e[["net_income"]], cost = cost, exec_cost = exec)
}

#' Regret matrix of scenario-specific positionings
#'
#' For each scenario X, position optimally as if X were certain; then evaluate
#' that positioning under every scenario Y with the full engine, net of the
#' implied and execution costs.
#' @param ctx Context (see [eval_positioning()]).
#' @param grad Gradients.
#' @param moves Move table.
#' @param scenarios Scenario names.
#' @param ... Passed to [solve_positioning()] (`reference`, floors).
#' @return List: `positions` (scenario x move), `roe` and `ni` (positioned
#'   for x realised, net of implied and execution costs), `regret` and
#'   `regret_ni`, `gain` and `gain_ni` (versus not positioning),
#'   `status_quo`, `status_quo_ni`, `binding`, `m_ni`.
#' @export
regret_matrix <- function(ctx, grad, moves, scenarios, ...) {
  sols <- lapply(scenarios, function(X) solve_positioning(grad, moves, stats::setNames(1, X), ...))
  pos <- t(vapply(sols, function(z) z$x, numeric(nrow(moves))))
  rownames(pos) <- scenarios
  c0 <- sols[[1]]$implied_cost
  args <- list(...)
  ref <- if ("reference" %in% names(args)) args$reference else "baseline"
  c0_ni <- if (is.null(ref)) rep(0, nrow(moves)) else grad$Jni[ref, ]
  m_ni <- grad$NI0[scenarios] / grad$R0[scenarios]
  mk <- function() matrix(NA_real_, length(scenarios), length(scenarios),
                          dimnames = list(positioned_for = scenarios, realised = scenarios))
  roe <- mk(); ni <- mk()
  for (X in scenarios) for (Y in scenarios) {
    v <- positioning_value(pos[X, ], ctx, Y, c0, moves, m_ni = m_ni[[Y]], implied_ni = c0_ni)
    roe[X, Y] <- v[["roe_net"]]; ni[X, Y] <- v[["ni_net"]]
  }
  sq <- vapply(scenarios, function(Y) grad$R0[[Y]], 0)
  sq_ni <- vapply(scenarios, function(Y) grad$NI0[[Y]], 0)
  list(positions = pos, roe = roe, ni = ni,
       regret = sweep(-roe, 2, -apply(roe, 2, max)), regret_ni = sweep(-ni, 2, -apply(ni, 2, max)),
       gain = sweep(roe, 2, sq), gain_ni = sweep(ni, 2, sq_ni), status_quo = sq, status_quo_ni = sq_ni,
       binding = lapply(sols, `[[`, "binding"), m_ni = m_ni, implied_ni = c0_ni)
}

#' How aggressive should the positioning be?
#'
#' Two ways of being more aggressive, both evaluated with the full engine in
#' every scenario, net of the implied marginal cost and of the execution cost:
#'
#' * **bigger moves at the same cost** (`alpha`): the optimal positioning for
#'   `X` scaled by `alpha` (clipped to the move bounds). With quadratic
#'   execution cost the net gain if `X` arrives is `(2 alpha - alpha^2)` times
#'   the optimum's, so it peaks at `alpha = 1` and is gone at `alpha = 2`,
#'   while the loss if another scenario arrives keeps growing;
#' * **a bank that can execute more cheaply** (`cost_scale`, optionally with
#'   wider bounds): the optimisation is re-solved with the execution costs
#'   scaled down. Moves, the gain if right and the loss if wrong all scale
#'   roughly with `1 / cost_scale` until a bound or a CET1/LCR floor binds.
#' @param ctx Context (see [eval_positioning()]).
#' @param grad Gradients.
#' @param moves Move table.
#' @param X Scenario positioned for.
#' @param scenarios Scenarios to evaluate in.
#' @param weights Scenario probabilities (for the expected gain).
#' @param alpha Size multipliers for the first dial.
#' @param cost_scale Execution-cost multipliers for the second dial.
#' @param bound_scale Bound multipliers combined with each `cost_scale`.
#' @param ... Floors and `reference`, passed to [solve_positioning()].
#' @return List: `size` and `capacity`, data frames with one row per setting
#'   and scenario (`gain`, `gain_ni`, `cet1_min`, `lcr_min`) plus the move
#'   vector, the binding constraints and the linearisation gap.
#' @export
positioning_aggressiveness <- function(ctx, grad, moves, X, scenarios, weights,
                                       alpha = c(0.5, 1, 1.5, 2, 3),
                                       cost_scale = c(1, 0.5, 0.25, 0.1), bound_scale = c(1, 2), ...) {
  args <- list(...)
  ref <- if ("reference" %in% names(args)) args$reference else "baseline"
  c0 <- if (is.null(ref)) rep(0, nrow(moves)) else grad$G[ref, ]
  c0_ni <- if (is.null(ref)) rep(0, nrow(moves)) else grad$Jni[ref, ]
  m_ni <- grad$NI0[scenarios] / grad$R0[scenarios]
  eval_x <- function(x, mv) do.call(rbind, lapply(scenarios, function(Y) {
    v <- positioning_value(x, ctx, Y, c0, mv, m_ni = m_ni[[Y]], implied_ni = c0_ni)
    data.frame(realised = Y, gain = v[["roe_net"]] - grad$R0[[Y]], gain_ni = v[["ni_net"]] - grad$NI0[[Y]],
               cet1_min = v[["cet1_min"]], lcr_min = v[["lcr_min"]], stringsAsFactors = FALSE)
  }))
  x1 <- solve_positioning(grad, moves, stats::setNames(1, X), ...)$x
  size <- do.call(rbind, lapply(alpha, function(a) {
    x <- pmin(pmax(a * x1, moves$lower), moves$upper)
    e <- eval_x(x, moves)
    cbind(alpha = a, clipped = any(abs(x - a * x1) > 1e-9), e,
          exp_gain = sum(weights[e$realised] * e$gain), exp_gain_ni = sum(weights[e$realised] * e$gain_ni),
          t(x))
  }))
  capacity <- do.call(rbind, lapply(bound_scale, function(bsc) do.call(rbind, lapply(cost_scale, function(k) {
    mv <- moves; mv$cost <- mv$cost * k
    mv$lower <- moves$lower * bsc; mv$upper <- moves$upper * bsc
    ## lending can only fall as far as the book: keep the original floor on
    ## the size of each book (lower bounds were set at -min(bound, share))
    mv$lower <- pmax(mv$lower, -abs(attr(moves, "book_share") %||% rep(Inf, nrow(moves))))
    z <- solve_positioning(grad, mv, stats::setNames(1, X), ...)
    e <- eval_x(z$x, mv)
    lin_gap <- (e$gain[e$realised == X] + z$exec_cost) - (z$roe_pred[[X]] - grad$R0[[X]])
    cbind(cost_scale = k, bound_scale = bsc, e,
          exp_gain = sum(weights[e$realised] * e$gain), exp_gain_ni = sum(weights[e$realised] * e$gain_ni),
          binding = paste(z$binding, collapse = "; "), lin_gap = lin_gap, status = z$status, t(z$x))
  }))))
  list(size = size, capacity = capacity, x_opt = x1)
}
