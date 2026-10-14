# =============================================================================
#  attribution.R -- where a scenario's ROE effect comes from, and the ALCO
#                   bridge from the planned ROE to the positioned ROE
#
#  Channel attribution swaps one block of engine inputs at a time from the
#  alternative scenario into the base run -- volume (growth paths), pricing
#  (reference rates, line yields and deposit costs, and hence the hedge),
#  credit (the credit-loss ratio) and fees and costs (NIR and opex ratios) --
#  and leaves the interaction as the residual, so the bars always add up to
#  the full-engine total.
#
#  The ALCO bridge is stated in basis points of NIM-equivalent (pre-tax profit
#  over interest-earning assets) and converted to ROE with one factor,
#  m = ROE / NIM-equivalent of the reference run (approximately
#  (1 - tax) x IEA / equity), the Post 3 convention.
# =============================================================================

#' Summary metrics of an engine run used in attribution
#' @param run Engine run.
#' @return Named numeric `roe`, `nim_eq`, `nim`, `clr` (averages over the
#'   horizon) and `net_income` (total over the horizon, R'000).
#' @export
run_metrics <- function(run) {
  q <- run$quarterly
  c(roe = mean(q$roe), nim_eq = mean(4 * q$pbt / q$iea), nim = mean(q$nim), clr = mean(q$clr),
    net_income = sum(q$net_income))
}

#' Channel attribution of a scenario's effect on an engine run
#'
#' @param base_inputs,alt_inputs Outputs of [engine_inputs()] for the base and
#'   the alternative scenario.
#' @param runner `function(inputs)` returning an engine run on a fixed
#'   balance sheet and hedge.
#' @return Data frame, one row per metric (`roe`, `nim_eq`, `nim`, `clr`,
#'   `net_income`): `base`, `volume`, `pricing`, `credit`, `fees_costs`,
#'   `interaction`, `total`, `alt`.
#' @export
channel_attribution <- function(base_inputs, alt_inputs, runner) {
  b <- run_metrics(runner(base_inputs))
  a <- run_metrics(runner(alt_inputs))
  swap <- function(which) {
    inp <- base_inputs
    if (which == "volume") inp$growth <- alt_inputs$growth
    if (which == "pricing") { inp$rates <- alt_inputs$rates; inp$line_rates <- alt_inputs$line_rates }
    if (which == "credit") inp$credit <- alt_inputs$credit
    if (which == "fees_costs") inp$ratios <- alt_inputs$ratios
    run_metrics(runner(inp)) - b
  }
  ch <- vapply(c("volume", "pricing", "credit", "fees_costs"), swap, numeric(length(b)))
  tot <- a - b
  data.frame(metric = names(b), base = b, ch, interaction = tot - rowSums(ch), total = tot,
             alt = a, row.names = NULL, stringsAsFactors = FALSE)
}

#' Sequential bridge of a positioning: each move's net contribution
#'
#' Moves are added one at a time (largest linear contribution first) and each
#' step is re-run through the full engine, so the bars add up exactly to the
#' positioned result; each bar is net of that move's implied and execution
#' cost. Net income is converted from the ROE costs with the scenario's
#' net-income-per-unit-of-ROE factor and the implied cost in net income
#' (see [positioning_value()]).
#' @param x Positioning (move vector).
#' @param ctx Context (see [eval_positioning()]).
#' @param s Scenario.
#' @param grad Gradients ([positioning_gradients()]), for ordering and the
#'   net-income conversion.
#' @param implied_cost Implied marginal cost per unit move.
#' @param moves Move table.
#' @param implied_ni Implied marginal cost per unit move in net income
#'   (default: the net-income gradient under the baseline).
#' @return Data frame `move, x, roe_net` (decimal ROE) and `ni_net`
#'   (net income over the horizon, R'000).
#' @export
positioning_bridge <- function(x, ctx, s, grad, implied_cost, moves,
                               implied_ni = grad$Jni["baseline", ]) {
  lin <- (grad$G[s, ] - implied_cost) * x
  ord <- order(-abs(lin)); ord <- ord[x[ord] != 0]
  Kmat <- execution_cost_matrix(moves)
  m_ni <- grad$NI0[[s]] / grad$R0[[s]]
  cur <- rep(0, length(x)); prev <- eval_positioning(cur, ctx, s)
  out <- list()
  for (k in ord) {
    cur[k] <- x[k]
    r <- eval_positioning(cur, ctx, s)
    exec <- 0.5 * Kmat[k, k] * x[k]^2
    out[[length(out) + 1L]] <- data.frame(move = moves$move[k], x = x[k],
      roe_net = (r[["roe"]] - prev[["roe"]]) - implied_cost[k] * x[k] - exec,
      ni_net = (r[["net_income"]] - prev[["net_income"]]) - implied_ni[k] * x[k] - exec * m_ni,
      stringsAsFactors = FALSE)
    prev <- r
  }
  if (!length(out)) return(data.frame(move = character(0), x = numeric(0), roe_net = numeric(0),
                                      ni_net = numeric(0)))
  do.call(rbind, out)
}
