# baeconstrat — macro scenarios to bank balance sheet, earnings and ROE

`baeconstrat` is an R package that links South African macroeconomic scenarios
to a bank's balance sheet, net interest income, credit losses, capital and
return on equity. Everything is built from public data: the SARB BA-return
family (BA900 per bank; BA100, BA120, BA200 and BA700 for the sector), SARB KBP
time series and FRED.

The bank in question is **No Gut Feelings Bank (NGFB)**. It is a fictional bank,
drawn from the posterior predictive of a hierarchical model of the six large
South African banks and embedded in the sector, so it competes for market
share with real peer groups.

## Design: two stages, top-down

```
 macro scenario ──► Stage 1: sector satellites ──► sector volumes, margins, credit losses, costs
                         (sign-restricted ARDL,          │
                          dynamic OOS selection)         ▼
                                              Stage 2: bank vs sector
                                              (market-share dynamics by regime,
                                               random-effects pooling across banks)
                                                         │
                                                         ▼
                                     NGFB balance sheet ──► P&L / capital / ROE engine
                                                         │
                                                         ▼
                              sensitivities · scenario ROE · pre-positioning optimiser
```

The macro elasticities are estimated once, on the long, well-identified sector
aggregates. The bank layer only has to explain how a bank's share of each
product moves with the sector cycle and across regimes. This keeps the economic
relationships intact when they are carried down to an individual bank.

## Layout

| path | contents |
|---|---|
| `R/` | package functions: data access (`sarb_api.R`, `sarb_forms.R`, `sarb_macro.R`, `fred.R`, `pull.R`), mapping (`map_ba900.R`, `map_sector.R`), panels and peer groups (`panel_macro.R`, `panel_bank.R`), liquidity proxies (`liquidity.R`), Stage 1 (`ols_hac.R`, `satellite.R`, `industry_models.R`, `pipeline.R`), model checks (`model_checks.R`), monthly layer and Kalman-filter disaggregation (`monthly.R`, `disaggregate.R`), the out-of-sample harness, polynomial lags and Bayesian shrinkage (`evaluate.R`), dynamic multipliers and local projections (`multipliers.R`), Stage 2 (`bank_relative.R`, `hierarchy.R`), NGFB (`ngfb.R`, `calibration.R`), scenarios and projection (`scenarios.R`, `projection.R`), engine (`pnl_engine.R`), attribution and ALCO bridge (`attribution.R`), the ZAR swap curve and the hedge book's fixed rate (`swaps.R`), optimiser and aggressiveness analysis (`optimise.R`), share strategy and marginal product economics (`strategy.R`), macro-adjusted performance (`performance.R`), charts (`plots.R`) |
| `data-raw/` | `00_pull_data.bat` / `01_pull_data.R` download everything (run on your own machine); `02_pull_supplement.R` tops up later additions; `03_pull_monthly.bat` / `03_pull_monthly.R` add the months between quarter ends to BA900 (and the monthly sector balance sheet, BA100); `04_pull_swaps.bat` / `04_pull_swaps.R` download daily ZAR swap rates, JIBAR, ZARONIA and generic government yields from rbond.co.za (CC BY 4.0) into `data-raw/rbond/` |
| `analysis/` | thin, ordered scripts `00_config.R` … `15_risk_budget.R` and `run_all.R`, each writing one `output/*.rds`; `lab_tools.R` holds the strategies the lab and the optimiser share |
| `output/` | `panels.rds`, `stage1.rds`, `stage2.rds`, `ngfb.rds`, `scenarios.rds`, `optimise.rds`, `backtest.rds`, `model_checks.rds`, `monthly.rds`, `dynamics.rds`, `strategy.rds`, `performance.rds`, `audit.rds`, `levers.rds`, `lab.rds`, `risk_budget.rds` |
| `report/` | the Quarto write-up (`ngfb_macro_strategy.qmd`, rendered to a self-contained `ngfb_macro_strategy.html`); every number in it is computed inline from `output/*.rds` |
| `inst/artefacts/` | committed NGFB artefacts (canonical deposit composition and the full NGFB identity, `ngfb_canonical.rds`) |
| `tests/testthat/` | unit tests: parsing, satellite search and both forms of the intercept correction, model checks (Clark-West, bounds test, asymmetric decomposition, peer-group roster), pooling, engine, liquidity, attribution in ROE and net income, scenarios and optimiser (including the aggressiveness analysis), Kalman-filter disaggregation (against the closed-form GLS solution), monthly drivers, dynamic multipliers (against the ARDL formula), polynomial lags, shrinkage, local projections, share positioning, scorecards and product economics |

## Running it

```r
# 1. data (internet access to custom.resbank.co.za, fred.stlouisfed.org and rbond.co.za)
#    double-click data-raw/00_pull_data.bat, or in R at the package root:
source("data-raw/01_pull_data.R")
source("data-raw/02_pull_supplement.R")
source("data-raw/03_pull_monthly.R")      # optional: monthly BA900 and BA100 (60-100 min)
source("data-raw/04_pull_swaps.R")        # ZAR swap curve (rbond.co.za), a few seconds

# 2. install the package and run the analysis
install.packages(".", repos = NULL, type = "source")      # or devtools::install()
source("analysis/run_all.R")                               # writes output/*.rds

# 3. the write-up
quarto::quarto_render("report/ngfb_macro_strategy.qmd")    # or: quarto render report/...
```

Dependencies: base R (`stats`, `utils`), with `jsonlite`, `xml2` and `curl` for
downloads, `quadprog` for the optimiser and `ggplot2`, `ggrepel` and `scales`
for the report.

## Conventions carried over from the NGFB series

* Seed `20260622`. Deposit composition and betas are the committed Post 1 draw;
  leverage and deposit ratios are the Post 3 BA100 bridge constants.
* k-anonymity ≥ 4. Real banks are only ever published pooled, as peer groups
  of at least four members (the report lists who is in each group, never a
  result for an individual bank). Bank effects enter NGFB solely through
  population hyperparameters.
* No number in the report is typed by hand, and `guard()` stops the render if
  any figure drifts.
* The report includes a *What didn't work* section.

## No macro forecasting: perfect foresight

The models exist to find good relationships between macro variables and
balance-sheet items, not to forecast the macro variables. Every scenario fixes
the macro path, and every model is judged the way a scenario uses it:
re-estimated on data up to an origin and simulated along the *realised* macro
path over the scenario horizon of twelve quarters (thirty-six months for the
monthly models). Values not yet published at the jump-off (GDP, unemployment
and house prices lag the policy rate by a quarter) belong to the scenario, never
to a carried-forward last observation, and the monthly GDP path is chosen by
in-sample fit, not by a nowcast test. A refinement, including an asymmetric
response, enters the core only if it is significant in sample and improves both
the twelve-quarter scenario back-test of the model average and a full re-search
back-test from 2019Q4 and 2021Q4 (`analysis/08_model_checks.R`).

## What the analysis answers

* **How exposed is the bank?** One-variable macro shocks around the baseline,
  decomposed into volume, pricing (including the structural hedge), credit and
  fee/cost channels (`analysis/05_scenarios.R`).
* **What does scenario X do to returns and earnings?** Three scenarios
  (baseline; upside: faster disinflation and a growth dividend; downside: a
  stagflationary shock with SARB hikes and a two-year contraction) and a
  downside variant in which the SARB cuts instead, run through Stage 1,
  Stage 2 and the engine for NGFB and for the sector, in ROE and net income
  (`analysis/05_scenarios.R`), with model-risk variants for convex credit
  losses and the credit-loss intercept correction.
* **How should the balance sheet be positioned before X arrives?** A
  quadratic programme over balance-sheet moves (lending books, deposit
  gathering, the structural hedge) that treats the status quo as optimal under
  the baseline and trades on how each scenario changes the value of each move,
  with execution costs, CET1 and LCR floors, a move-by-move walk-through, a
  regret matrix, a robust positioning, conviction ladders, ALCO bridges in ROE
  and net income, and an analysis of how aggressive the positioning should be
  (`analysis/06_optimise.R`).
* **Are the industry models good enough, and where are they asymmetric?**
  Clark-West tests against the AR(1) twelve quarters ahead, Pesaran-Shin-Smith
  bounds tests, asymmetric pricing (partial sums of rises and cuts), asymmetric
  volume responses to rate rises and falls, contraction kinks in credit losses
  and volumes, the form of the intercept correction and forecast combination,
  each with a fixed three-part adoption rule that the script enforces
  (`analysis/08_model_checks.R`). Adopted: asymmetric pricing for current
  accounts, savings deposits, commercial mortgages and cards, and a
  contraction kink for card lending (term deposits until v0.4; the corrected
  equity series moved the verdict).
* **Would monthly data do better?** Quarterly GDP, unemployment and house
  prices disaggregated to months with the Kalman filter (indicators chosen by
  in-sample fit); monthly satellites with the same registry
  and signs; one horse race scored on the same quarterly outcomes over twelve
  quarters along the realised macro path (quarterly and monthly ARDL
  averages, polynomial distributed lags, Bayesian shrinkage, the AR(1)), and
  monthly models fed the smooth GDP path a quarterly scenario implies
  (`analysis/09_monthly.R`). What the sign restrictions do, lag
  profiles, dynamic multipliers with parameter bands and a local-projection
  check (`analysis/09b_dynamics.R`). Without the monthly BA900 pull the
  volume comparison uses the SARB's monthly credit statistics instead, and the
  monthly sector balance sheet is interpolated between quarter ends.
* **Where should the bank grow?** Market growth and the bank's share effect in
  each macro regime against the large-bank population, a playbook (push,
  defend, guard, let go, ride) and the value of closing each gap from marginal
  product economics (funds-transfer pricing, liquidity buffer, expected loss,
  capital charge) (`analysis/10_share_strategy.R`).
* **Could it have done better?** Scenario scorecards that split growth into
  the market (macro) and the share effect (management), with par and frontier
  benchmarks, and the industry's growth against macro par
  (`analysis/11_performance.R`).
* **Are the numbers right?** Actual ZAR swap rates against the government-curve
  proxy that priced the structural hedge up to v0.3, the term spreads the
  scenarios now use, the hedge book's fixed rate, NII sensitivity against a
  published disclosure, the rate-neutral hedge ratio, a jump-off
  reconciliation, funding spreads, and JIBAR's cessation: the engine's
  floating benchmark is 3-month JIBAR to its last fixing on 31 December 2026
  and compounded ZARONIA plus the 16.19 bp fallback spread from 2027Q1
  (`R/benchmark.R`); the audit measures what the switch does, the basis it
  freezes and the spread at risk on re-papered loans (`analysis/12_audit.R`).
* **What else drives ROE and NII?** Hedge ratio and reinvestment tenor, a hedge
  of equity, HQLA duration (carry through NII, mark-to-market through CET1),
  asset-swapped government bonds (the fiscal premium without the duration;
  the bond–swap spread modelled from the long bond's non-policy move), the
  size of the liquidity buffer (surplus bills released to repay wholesale
  funding), payout and capital distribution, and deposit pricing, each run
  through the engine in every scenario (`R/treasury.R`, `analysis/13_levers.R`).
  Bonds held for liquidity are at fair value through OCI: realised gains and
  losses go to profit, the OCI reserve to CET1, and the mark-to-market to the
  HQLA stock at market value.
* **How should the balance sheet be positioned across the macro outcomes
  history has produced?** Every complete 12-quarter macro episode since 2008
  replayed from today's starting point through the full chain; static
  positioning (hedge ratio x tenor, HQLA duration, asset swaps, buffer size,
  payout) against rules that react to the macro picture as it unfolds (hedge
  ratio and tenor with the policy rate, HQLA duration with the curve's slope,
  payout with growth and capital, unsecured growth with the labour market,
  asset swaps with the bond–swap spread, the buffer with stress), with the
  hindsight oracle as a bound and a swap curve that prices half of the coming
  path as a robustness check (`analysis/14_strategy_lab.R`).
* **How should the levers be set together?** A risk-budgeted optimiser scores
  every combination of the hedge ratio and tenor, the liquidity book and the
  payout (25,200) across the episodes, under limits on the ROE tail, CET1 at
  fair value, the LCR at market value, the NSFR and the change in EVE; a
  surrogate built from single-setting engine runs (equity rolled forward with
  each combination's payout) is checked against exact runs, and the choice is
  tested out of sample on two halves of the episodes
  (`analysis/15_risk_budget.R`).
