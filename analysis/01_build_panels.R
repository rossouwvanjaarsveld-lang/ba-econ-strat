# =============================================================================
#  analysis/01_build_panels.R -- raw caches -> quarterly panels
#    BA900 lines (bank level) -> bank panel -> peer-group panel (k >= 4)
#    BA100 + BA120 -> sector P&L panel;  KBP + FRED -> macro panel
# =============================================================================
source("analysis/00_config.R")

b9   <- readRDS(file.path(CFG$raw, "sarb/BA900_long.rds"))
lines <- ba900_lines(b9)
guard(nrow(lines$guard) == 0, "BA900 description guard failed for %d item-periods", nrow(lines$guard))
rc <- ba900_reconcile(lines$lines)
guard(max(abs(rc$asset_gap), na.rm = TRUE) < 1e-4, "BA900 assets do not reconcile (max gap %.2e)",
      max(abs(rc$asset_gap), na.rm = TRUE))
guard(max(abs(rc$le_gap), na.rm = TRUE) < 1e-4, "BA900 funding does not reconcile")

bp  <- add_composites(bank_panel_quarterly(lines$lines))
gp  <- add_composites(group_panel(bp))
mem <- group_membership(bp)
guard(min(mem$members) >= ngfb_canonical()$k_anon, "a peer group has fewer than %d members",
      ngfb_canonical()$k_anon)
## who is in which group, and when (membership only; never per-bank results)
roster <- peer_group_roster(b9, file.path(CFG$raw, "sarb/institutions"))
guard(all(stats::na.omit(roster$key) %in% peer_group_profiles()$key), "a named bank has no profile")

## depositor x bucket matrices for the liquidity proxies: the sector, and the
## six-bank reference pool (pooled, never per bank) from which NGFB is drawn
depmat <- rbind(cbind(entity = "Sector", ba900_deposit_matrix(b9, "TOTAL")),
                cbind(entity = "Population", ba900_deposit_matrix(b9, ngfb_population_keys())))
chk <- merge(stats::aggregate(list(m = depmat$value[depmat$entity == "Sector"]),
                              list(date = depmat$date[depmat$entity == "Sector"]), sum),
             data.frame(date = gp$date[gp$entity == "Sector"],
                        b = rowSums(gp[gp$entity == "Sector", c("beh_operational", "beh_savings",
                                                                "beh_notice", "beh_term")])))
guard(max(abs(chk$m / chk$b - 1)) < 1e-6, "deposit matrix does not add up to the behavioural lines")

b100 <- readRDS(file.path(CFG$raw, "sarb/BA100_long.rds"))
b120 <- readRDS(file.path(CFG$raw, "sarb/BA120_long.rds"))
spnl <- sector_pnl_panel(b100, b120)

b700 <- readRDS(file.path(CFG$raw, "sarb/BA700_long.rds"))
b200 <- readRDS(file.path(CFG$raw, "sarb/BA200_long.rds"))
pick <- function(d, rx) { x <- d[grepl(rx, tolower(as.character(d$item_desc))), c("period", "value")]
  x <- x[!duplicated(x$period), ]; x$date <- quarter_end(month_start(x$period)); x[, c("date", "value")] }
cap <- Reduce(function(a, b) merge(a, b, by = "date", all = TRUE), list(
  stats::setNames(pick(b700, "^common equity tier 1"), c("date", "cet1_ratio")),
  stats::setNames(pick(b700, "^tier 1 capital adequacy"), c("date", "tier1_ratio")),
  stats::setNames(pick(b700, "^total capital adequacy"), c("date", "car")),
  stats::setNames(pick(b700, "^total risk weighted exposure"), c("date", "rwa")),
  stats::setNames(pick(b700, "^leverage ratio"), c("date", "leverage_ratio")),
  stats::setNames(pick(b200, "^total$"), c("date", "credit_exposure"))))
b2last <- b200[b200$period == max(b200$period), c("item_no", "item_desc", "value")]

kbp  <- read_kbp_cache(file.path(CFG$raw, "macro"))
fred <- read_fred_cache(file.path(CFG$raw, "fred"))
macro <- build_macro_panel(kbp, fred)

s1pan <- stage1_panel(gp, macro, spnl, end = CFG$date0)

saveRDS(list(lines = lines$lines, reconcile = rc, bp = bp, gp = gp, membership = mem, roster = roster,
             depmat = depmat, spnl = spnl, capital = cap, ba200_last = b2last, macro = macro,
             macro_sources = attr(macro, "sources"), s1pan = s1pan,
             manifest = utils::read.csv(file.path(CFG$raw, "manifest.csv"))),
        out_path("panels.rds"))
ngfb_log("panels: %d bank-quarters, %d sector quarters, macro %s..%s",
         nrow(bp), nrow(spnl), min(macro$date), max(macro$date))
