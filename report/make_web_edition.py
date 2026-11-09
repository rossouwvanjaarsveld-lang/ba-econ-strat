"""Build ngfb_macro_strategy_web.qmd, the public edition, from ngfb_macro_strategy_full.qmd.

Run from anywhere: python3 report/make_web_edition.py. The public edition is the complete edition
without the industry models' internals or the peer-group membership, with the code hidden.
Publish its rendered HTML only: the .qmd still reads every output.

Removed chapters keep their R chunks (switched to include: false) so that every object later
chapters use is still computed; their prose, figures, tables and diagrams go. Every other edit is
an exact-text operation that must match exactly once.
"""
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
src = open(os.path.join(HERE, "ngfb_macro_strategy_full.qmd"), encoding="utf-8").read()
lines = src.split("\n")


def chunk_spans(ls):
    """(start, end) line indices of fenced blocks, end inclusive."""
    spans, i = [], 0
    while i < len(ls):
        if ls[i].startswith("```"):
            j = i + 1
            while j < len(ls) and not ls[j].startswith("```"):
                j += 1
            spans.append((i, j)); i = j + 1
        else:
            i += 1
    return spans


def in_chunk(idx, spans):
    return any(a <= idx <= b for a, b in spans)


def heading_level(l):
    m = re.match(r"^(#{1,6}) ", l)
    return len(m.group(1)) if m else None


def section_range(ls, heading_prefix):
    spans = chunk_spans(ls)
    hits = [i for i, l in enumerate(ls) if l.startswith(heading_prefix) and not in_chunk(i, spans)]
    if len(hits) != 1:
        sys.exit(f"heading found {len(hits)} times: {heading_prefix!r}")
    i = hits[0]; lev = heading_level(ls[i])
    j = i + 1
    while j < len(ls):
        if not in_chunk(j, spans) and heading_level(ls[j]) is not None and heading_level(ls[j]) <= lev:
            break
        j += 1
    return i, j, spans


def hidden_chunks(ls, a, b, spans):
    """The R chunks inside [a, b), switched to include: false; other fenced blocks are dropped."""
    out = []
    for s, e in spans:
        if a <= s < b and ls[s].startswith("```{r"):
            body = ls[s:e + 1]
            k = 1; has = False
            while k < len(body) and body[k].startswith("#|"):
                if body[k].startswith("#| include:"):
                    body[k] = "#| include: false"; has = True
                k += 1
            if not has:
                body.insert(1, "#| include: false")
            out += body + [""]
    return out


def strip_section(ls, heading_prefix, keep_heading=False, new_body=None):
    a, b, spans = section_range(ls, heading_prefix)
    head = [ls[a], ""] if keep_heading else []
    body = (new_body.split("\n") + [""]) if new_body else []
    return ls[:a] + head + body + hidden_chunks(ls, a, b, spans) + ls[b:]


# ---------------------------------------------------------------- the chapters and sections that go
lines = strip_section(lines, "# Stage 1: macro to the industry", keep_heading=True, new_body="""Stage 1 maps a macro path to the industry: the growth of each loan and
deposit book, lending spreads over prime and the three-month benchmark,
deposit costs, the credit-loss ratio, fees and operating costs. That is
`r nrow(S1$summary)` series in all, built on the SARB's BA900 and BA100/BA120 returns. The
workhorse is a sign-restricted ARDL model, and that is all this edition says
about it: its specification, its estimates, its tests and how it was chosen
stay in the drawer, like any koeksister recipe worth having. Here it is a
black box with a simple contract: hand it a macro path, and it hands back the
industry's volumes, margins and losses along that path. The
[scenarios](#scenarios) show what it hands back.""")
lines = strip_section(lines, "# Is ARDL the right tool")
lines = strip_section(lines, "# Monthly frequency")
lines = strip_section(lines, "## Back-test: would the industry models have tracked the industry?")
lines = strip_section(lines, "## Who is in each peer group", keep_heading=True, new_body="""Real banks only ever appear pooled, in peer groups with at least four members
in every quarter. The **Big Four** are the universal banks that have dominated
South African banking for decades. The **Challengers** are the retail and
digital banks that have taken share since 2008. The **Specialists** are
business, private-client, Islamic and community banks, and everyone else is
**Other**. This edition does not list the members of each group.""")
lines = strip_section(lines, "## Model risk in the scenarios", keep_heading=True, new_body="""The level of credit losses is where the industry models carry the most model
risk. Two alternative credit-model choices were run through the engine: they
move the level of NGFB's returns, by at most `r ubps(max(abs(c(mr$convex - mr$core, mr$no_ic_credit - mr$core))))` of ROE in any scenario, but not the
differences between scenarios, and not the positioning.""")
out = "\n".join(lines)


def replace_once(old, new):
    global out
    n = out.count(old)
    if n != 1:
        sys.exit(f"anchor found {n} times: {old[:90]!r}")
    out = out.replace(old, new)


def regex_once(pattern, new, flags=re.S):
    global out
    m = list(re.finditer(pattern, out, flags))
    if len(m) != 1:
        sys.exit(f"pattern found {len(m)} times: {pattern[:90]!r}")
    out = out[:m[0].start()] + new + out[m[0].end():]


def drop_bullet(title):
    """Remove a top-level bullet that starts '* **title' and its indented continuation lines."""
    global out
    ls = out.split("\n")
    hits = [i for i, l in enumerate(ls) if l.startswith("* **" + title)]
    if len(hits) != 1:
        sys.exit(f"bullet found {len(hits)} times: {title!r}")
    i = hits[0]; j = i + 1
    while j < len(ls) and ls[j].startswith("  ") and ls[j].strip():
        j += 1
    out = "\n".join(ls[:i] + ls[j:])


# ---------------------------------------------------------------- front matter: the public edition, no code
replace_once('subtitle: "The complete edition: what each macro outcome does to the balance sheet, how to reposition against the market, and how to measure the bank against its peers, with public SARB data"',
             'subtitle: "The public edition: what each macro outcome does to the balance sheet, how to reposition against the market, and how to measure the bank against its peers, with public SARB data"')
replace_once("execute:\n  echo: true\n", "execute:\n  echo: false\n")
replace_once("    code-fold: true\n", "")
replace_once('    code-summary: "Code"\n', "")

# ---------------------------------------------------------------- the version notes become one note about this edition
regex_once(r'::: \{\.callout-note appearance="simple"\}\n## What this complete edition adds\n.*?every new lever is an option\nagainst it\.\n:::\n',
           '''::: {.callout-note appearance="simple"}
## About this edition

This is the public edition of the complete technical write-up on No Gut
Feelings Bank. It carries every chapter except the industry models'
internals: the Stage 1 models are described in one paragraph, and their
specification, estimates and tests, the monthly-frequency comparison and the
back-test of the industry models are left out, as is the list of banks in each
peer group. No Gut Feelings Bank does not exist. Real banks appear only pooled
into peer groups of at least four, and nothing here ranks, names or
reverse-engineers an individual institution. Nothing here is financial advice.
:::
''')

# ---------------------------------------------------------------- the summary: the two findings about the industry models
regex_once(r"12\. \*\*Is ARDL the right tool, and should it be asymmetric\?\*\*.*?credit booms\.\n", "")

# ---------------------------------------------------------------- the design: no model-averaging detail, no package map
replace_once('sign-restricted ARDL models, BMA"]', 'sign-restricted ARDL models"]')
regex_once(r"Each piece is a module of the `baeconstrat` package.*?\{\.striped\}\n", "")

# ---------------------------------------------------------------- calibration: no word on the industry models' dummies
replace_once("is a one-off outlier that the Stage 1 impulse dummies\nflag; it is replaced",
             "is a one-off outlier; it is replaced")

# ---------------------------------------------------------------- the audit
regex_once(r"Re-estimated on clean data, `r length\(eq_sats\)`.*?\(\[Contraction kinks\]\(#is-ardl-the-right-tool\)\)\.",
           "Every industry model was re-estimated on the clean series.")
replace_once("The Stage 1 NCD model already carries that lag.", "The industry models already reproduce that repricing lag.")
replace_once("price the deposits it would lose, and Stage 1 already finds the sector\n  passing cuts through faster than rises on current accounts.",
             "price the deposits it would lose.")
replace_once("Published: Nedbank Group 2025 results,", "Published: a large South African bank's 2025 results,")

# ---------------------------------------------------------------- validation: the unit tests in general terms
regex_once(r"\* \*\*Tests\.\*\* The package's unit tests cover parsing, the satellite search.*?for oversized positions\.",
           "* **Tests.** The package's unit tests cover parsing, the industry-model\n  pipeline, random-effects pooling, the engine (hand-computed NII, balance,\n  hedge direction), liquidity factors and calibration, attribution in ROE and\n  net income, the scenario library and the optimiser, including the\n  $(2\\alpha - \\alpha^2)$ law for oversized positions.")

# ---------------------------------------------------------------- what didn't work: the industry-model items go
for t in ["The intercept correction inside the equation.", "Selecting the single best specification.",
          "Asymmetric responses everywhere.", "Nowcasting the jump-off.", "COVID dummies on the credit-loss model.",
          "Macro models for mortgage growth.", "A mean-reverting fee model.", "Monthly satellites in production.",
          "Polynomial distributed lags and Bayesian shrinkage for pricing."]:
    drop_bullet(t)
replace_once("No specification containing it\n  survives the sign and $t$-tests once GDP is available, so the engine has no\n  unemployment channel.",
             "No credit-loss relationship with\n  unemployment survives testing once GDP is in it, so the engine has no\n  unemployment channel.")

# ---------------------------------------------------------------- limitations
replace_once("""predate the swap data); the intercept correction on credit losses is worth
  `r ubps(mr["baseline", "core"] - mr["baseline", "no_ic_credit"])`. The scenario differences and the positioning are much less
  sensitive to either.""", """predate the swap data), and the level of credit losses is where the industry
  models carry the most model risk. The scenario differences and the
  positioning are much less sensitive to either.""")
replace_once("""The volume models beat a naive benchmark
  only modestly, and the share models' regime effects are imprecise.""", """Volumes are harder to tie to the macro
  cycle than pricing and credit, and the share models' regime effects are
  imprecise.""")
drop_bullet("Monthly data.")
drop_bullet("The local projections are not identified.")

# ---------------------------------------------------------------- reproducing becomes sources
replace_once("# Reproducing the analysis {#reproducing-the-analysis}", "# Sources {#reproducing-the-analysis}")
regex_once(r"The code is an R package, `baeconstrat`, with the analysis in separate\nscripts\..*?drifts from what the\ntext says\.\n",
           "Every number in this document is computed inline from the analysis outputs,\nand checks in the code stop the render if an input drifts from what the text\nsays.\n")
regex_once(r"Nedbank Group, \*2025\nannual results\* \(NII sensitivity and endowment hedging\)",
           "a large South African bank's *2025\nannual results* (NII sensitivity and endowment hedging)")
regex_once(r"Board of Governors of the Federal Reserve System, \*Supervisory Stress Test\n.*?\*Sociological\nMethodology\* \(1995\); ", "")
regex_once(r" Peer-group histories: company and press reports on Absa.*?Bank South Africa \(Grobank, Bank of Athens\)\.", "")

open(os.path.join(HERE, "ngfb_macro_strategy_web.qmd"), "w", encoding="utf-8").write(out)
print("written", len(out), "chars;", out.count("\n"), "lines")
