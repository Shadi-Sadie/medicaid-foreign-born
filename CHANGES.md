# Changes from the original code

This file lists every place where the code in `code/analysis/` differs from the original scripts
it was copied from. The originals were kept, unchanged, in `archive/` until 2 November 2026, when
that folder was removed; they are in git commit `21c7fe0` (restore with
`git checkout 21c7fe0 -- archive`).

Each change is one of two kinds:

- **Bug fix**: the original code was broken. It used an object that was never defined, a misspelled
  value, or the wrong model.
- **Reconstructed to match**: the original code was missing a definition, or had no code at all.
  The version here was **chosen because it reproduces the published numbers**. It was not
  recovered from the original code, so it may not be what was run for the paper.
  These are marked `# RECONSTRUCTED:` in the code.

Bug fixes are marked `# FIX:` in the code.

## Changes that apply to every script

- Each script runs on its own from a fresh R session. It sources `code/00_setup.R`, loads
  `data/clean/acs_clean.rds`, keeps the sample it is run on with `make_sample()`, and saves its
  table or figure to `output/`. The originals assumed an interactive RStudio session with the data
  and other objects already in memory. (The scripts were first checked against the paper on
  `data/CLNACS2.RData`; the notes below that mention `CLNACS2` refer to that check.)
- Commented-out options and unused helper code were dropped when copying (for example, unused
  colour palettes). Estimation and plotting code is otherwise unchanged.
- The lines `Data$expansion <- as.numeric(Data$expansion)` / `ifelse(Data$expansion == 2, 0, ...)`
  in `05` and `07` were dropped. They converted an old factor coding. In `CLNACS2.RData`,
  `expansion` is already 0/1, so the lines did nothing.
- **New sample rule (not in the paper):** the main sample excludes adults with Medicare
  (`HINS3 == 1`), who are not eligible for the ACA expansion group. The sensitivity sample
  `with_medicare` keeps them. See `main_rules` in `code/00_setup.R`.
- **New sample rule (not in the paper):** the main sample excludes the six states that expanded
  coverage partly before 2014 under the ACA early option or a waiver (CA, CO, CT, MN, NJ, WA), as
  well as the five that covered low-income adults before 2014 (DE, DC, MA, NY, VT). Part of the
  early expanders' low-income adults were already covered in 2011-2013, so those years are not a
  clean period before expansion; these six states hold about two thirds of the foreign-born adults
  in expansion states. The sensitivity sample `keep_early_option` keeps them as 2014 expanders,
  and `keep_colorado` keeps Colorado only (its 2012 waiver was capped at 10,000 people).
- **New outcome (not in the paper):** every table and figure uses any health insurance coverage
  (`HICOV`) where the paper used uninsured (`UNINS`). `HICOV` is `1 - UNINS`, so the estimates
  are the paper's with the sign reversed; standard errors and p-values do not change. The
  outcome list of the figure scripts is now defined once, in `code/00_setup.R`.
- **Bug fix (changes the paper's results): standard errors.** Every model now clusters standard
  errors by state (`vcov = ~ST`), the level at which Medicaid expansion is assigned. The originals
  used heteroskedasticity-robust errors, which treat each person as independent; Tables 7 and 8
  reported two-way (state and year) clustered errors, which are unreliable with 9 years. The
  confidence intervals in the event-study figures (1, 2, 3, 8, 10) use the clustered
  covariance matrix. Clustered errors are roughly 5-15 times larger.
- **Bug fix (changes the paper's results): fixed effects in the pooled models.** Models that
  compare the expansion effect across groups in one regression (Tables 2, 5, 6; Figures 8, 10)
  are triple differences, but had only state and year fixed effects. Any national change
  in the gap between groups was then counted as an expansion effect. The fixed effects are now
  state x group and year x group: `ST^ForeginBorn + YEAR^ForeginBorn` (Tables 2, 5; Figure 8),
  `ST^CITSTAT + YEAR^CITSTAT` (Table 6), and state and year crossed with the two group dummies
  (Figure 10). The group main effects are absorbed by these fixed effects. Figures 1-3 are new
  (see below) and use `ST^grp + YEAR^grp`. Models
  estimated separately by nativity (Tables 7, 8; Figures 5, 7, 9) already compare within one
  group and keep state and year fixed effects.

## Item by item

| Paper item | Script | Copied from (in `archive/`, commit `21c7fe0`) | Changes |
|---|---|---|---|
| Table 1 | `table1.R` | `codes/R/03-survey-design-descriptive.R`, `tableA1` (lines 430-542) | **Redesigned for the HSR submission; no longer reproduces the paper.** The paper's table was unweighted (although its title said "(Weighted)"), split only by nativity, and tested every row for differences. The new table is descriptive: weighted (`PWGTP`) means and percentages, pre-ACA (2011-2013), for US-born adults, naturalized citizens and non-citizens in non-expansion and expansion states, with each column's unweighted N and each state group's number of states in the headers. No tests or standardized differences, since the design does not rest on baseline balance. New rows: employment, the immigrant characteristics used as controls (years in the US, >25% of life in the US, English proficiency, region of birth; foreign-born only), and the state controls (unemployment rate, IPC, ADA score shown as 0-100). The likely-undocumented flag is left out because it is imputed, not measured; it belongs in an appendix exhibit. |
| Tables 2, 5 | `table2.R` (was `table2_5.R`) | `codes/R/06-did-regressions.R` lines 1-145 | **Redesigned for the HSR submission; no longer reproduces the paper.** The paper's Tables 2 (with controls) and 5 (without) pooled the foreign-born into one group (`treat*ForeginBorn`). Table 2 now gives the expansion effect for US-born adults, naturalized citizens and non-citizens and the three differences between them, for any insurance, Medicaid, employer-sponsored insurance and direct purchase, in percentage points with 95% CIs (no stars), with each group's pre-2014 mean in expansion states. The model, `fit_static_models()` in `00_setup.R`, is the static version of the Figure 1-3 event studies: each group has its own expansion effect, state and year fixed effects and covariate effects (same covariates and reference levels), weighted, clustered by state. The table uses all covariates (specification E); `table2_by_spec.csv` has specifications A-E, and A replaces the paper's Table 5. (Earlier fix, now moot: Table 2's title said "Without Controls" for the controlled model.) |
| Figure C2; row of Table C1 | `sunab_event.R`; a row of `robustness.R` | none | **New (appendix).** The Sun and Abraham (2021) interaction-weighted estimator, robust to effects that differ by expansion cohort and over time, in specification E only (the preferred model; Figures 1-2 already show the covariate specifications). `fixest::sunab()` cannot give each group its own cohort effects, so `add_sunab_cells()` and `fit_sunab_models()` in `00_setup.R` build the cohort x year-since-expansion x group dummies (states that did not expand by 2019 as controls, the year before expansion as reference) in one model per outcome, with the covariates, fixed effects, weights and clustering of Table 2 and Figures 1-3. `sunab_estimate()` averages a group's cells weighted by their weighted number of people, as `aggregate()` does for `sunab()`. `sunab_event.R` is the event study (each event time, groups and differences between groups, four outcomes); its average from the year of expansion on is the "Sun and Abraham estimator" row of the robustness table (`robustness.R`). Checked against `sunab()` run separately for each group: the same point estimates (overall and at each event time), standard errors within 0.0015. Estimates in `output/tables/sunab_event_estimates.csv`. |
| Figs 1-3, C3-C4 | `fig1.R`, `fig2.R`, `fig3.R`, `specs_employer.R`, `specs_direct.R` | none | **New for the HSR submission; replaces the paper's Figures 1-3** (pooled foreign-born, undocumented vs legal, and citizenship event studies; the old scripts and outputs are in `archive/2026-11-02/`). One event-study model per outcome and specification in which US-born adults, naturalized citizens and non-citizens each have their own expansion effects, state and year fixed effects and covariate effects (the same effects as separate regressions by group). Outcomes in percentage points. Event times -4 to +5 are plotted; -5 and earlier (2-4 states) are combined into one coefficient that is not plotted. Figure 1 (any coverage) and Figure 2 (Medicaid) show five nested specifications: A none; B age, sex, race and ethnicity, marital status, education; C + disability, poverty group, employment; D + English proficiency, >25% of life in the US, region of birth; E + state unemployment rate, ADA score, IPC. Appendix Figures C3 (employer-sponsored) and C4 (directly purchased) show the same five specifications. Figure 3 shows the differences between the groups' effects (naturalized - US-born, non-citizen - US-born, non-citizen - naturalized) on all four outcomes in specification E, with clustered SEs from the model's joint covariance. The model, specifications and plot are defined once in `code/00_setup.R` ("Event studies by citizenship"), so all five figures use the same models. Each factor covariate has its own reference level in each group (its most common level there); with one reference for all groups, the region-of-birth dummies of the foreign-born summed to their group fixed effects, which fixest did not always detect. Estimates in `output/tables/fig1_estimates.csv`, `fig2_estimates.csv`, `fig3_estimates.csv`, `specs_employer_estimates.csv`, `specs_direct_estimates.csv`. |
| Fig B1 | `trends_raw.R` (was `figA3.R`) | none | **New (appendix).** Weighted (`PWGTP`) percentage with each type of coverage by calendar year for US-born adults, naturalized citizens and non-citizens, in states that expanded in 2014 and states that never expanded (later expanders left out, so 2014 is every expansion state's first year). Raw means, no model; shows whether the event-study effects come from the expansion states or the comparison states. Means and unweighted cell sizes in `output/tables/trends_raw_means.csv`. |
| Fig B2 | `control_outcomes.R` (was `figA4.R`) | none | **New (appendix).** Event studies with control variables as the outcome, to check that expansion did not change them (a control that expansion changes should not be adjusted for): disability, income at or below 100% of poverty, employed and not in the labor force by group (same model as Figures 1-3, no covariates); income at or below 138% among adults up to 400% of poverty (new sample `pov_400`), which tests whether expansion changed who is in the sample; and the state unemployment rate, ADA score and IPC on the state-year panel (state and year fixed effects, clustered by state). |
| Tables B1, C1, D1; Fig C1 | `comparison_samples.R` (was `placebo.R`), `robustness.R`, `income_bands.R` (were one script, `tableA1_figA5.R`) | none | **New (appendix).** The Table 2 model (static, all covariates) on other samples, split by the question each check answers: Table B1 (a higher-income comparison sample, income at 139-400% of poverty, above the expansion limit; renamed from "placebo" because the rest of the ACA, including Marketplace subsidies, affects this group; adults with a high school education or less at any income up to 400% (`edu_hs`), a sample that expansion cannot select, as Figure B2 shows it shifts who is at or below 138%); Table C1 and its forest plot, Figure C1 (the Sun and Abraham estimator, see above; including the early-option states, `keep_early_option`; non-citizens in the US at least 10 years instead of 5, `residency_10y`); Table D1 (income at or below 100% and 101-138% of poverty, where adults in non-expansion states could not and could get subsidized Marketplace coverage). Each table starts with the main sample. The model, table and plot are `check_estimates()`, `check_table()` and `check_plot()` in `00_setup.R`. Robustness checks use the preferred model only, not specifications A-E. |
| Table B2 | `pretrend_tests.R` | none | **New (appendix).** Joint Wald tests that the event-study coefficients for 4, 3 and 2 years before expansion are all zero, in the Figure 1-3 model (specification E), for US-born adults, naturalized citizens, non-citizens and the three differences between groups, for the four outcomes. F statistics on 3 and G - 1 degrees of freedom (G = 40 states) from the state-clustered covariance; `pretrend_test()` in `00_setup.R`, checked against `fixest::wald()`. The combined coefficient for 5 or more years before expansion (only the 2016 and 2019 expanders) is not tested. Results in `output/tables/pretrend_tests.csv`. |
| Figure C5 | `leave_one_out.R` | none | **New (appendix), at a reviewer's request.** The Table 2 model (static, specification E) re-estimated once for each of the 40 states of the main sample with that state left out, for the four outcomes; every effect and difference of Table 2 in `output/tables/leave_one_out.csv`, and in the figure the three groups and naturalized minus US-born, each state's estimate and 95% CI against the main estimate and its CI, the three states that move each estimate most labelled. Each state's sample is prepared as `make_sample()` would prepare it (`droplevels()`, then `prep_event_data()`), so the estimates without Florida and Texas equal those of the `drop_florida` and `drop_texas` samples (checked to 1e-8). Replaces the "Without Florida" and "Without Texas" rows of Table C1. |
| Figure B3 | `honest_did.R` | none | **New (appendix).** HonestDiD (Rambachan and Roth 2023) confidence intervals for the equal-weight average of the event-study effects in years 0-5 (specification E), allowing post-expansion departures from parallel trends tied to those before expansion (leads -4 to -2), in four panels: A, relative magnitudes (Mbar 0-2); B, as A with the departure after expansion restricted to the sign of the pre-expansion trend (HonestDiD `biasDirection`, set from each target's change from year -4 to -1; positive for all three); C, as A with leads -3 and -2 only (the -4 lead comes only from 2015+ expanders); D, smoothness (M from 0 to the largest change in slope of the target's pre-expansion path). Three targets: naturalized any insurance; naturalized minus US-born any insurance; naturalized direct purchase (jointly significant leads in Table B2). The relative-magnitudes grid is widened to +-50 SE (the default +-20 SE cut off the widest intervals), with a check that no interval reaches its edge. Inputs from `honest_inputs()` in `00_setup.R`, which shares `event_weights()` with `pretrend_test()`. |

## Exhibits of the earlier paper (archived)

The earlier paper's Tables 3-8 and Figures 4-10 are not part of the HSR submission. Their scripts were
moved to `archive/2026-11-02/code/` on 2 November 2026 and are not run by `run_all.R`; they still
need the packages and control lists that were then removed from `00_setup.R` (ggpubr, ggiplot,
gtsummary, gt, survey, bacondecomp, cowplot; `controls`, `foriegn`, `controlvar`,
`save_estimates()`), so restore those from git to run them. The changes made to them before they
were archived:

| Paper item | Script | Copied from (in `archive/`, commit `21c7fe0`) | Changes |
|---|---|---|---|
| Table 3, Fig 4 | `table3_fig4.R` | `codes/R/07-robustness-check.R` lines 168-236 | **Bug fix:** the original table typed the coefficients and weights in by hand. They are now computed from the decomposition. The values are the same. |
| Table 4 | `table4.R` | `texts/Draft-Result.Rmd` lines 425-446 | **Does not reproduce the paper.** On `CLNACS2` it gives different totals and rows (for example, 4 race groups where the paper has 5). The paper's table is internally inconsistent: the expansion-state disability count (116,770,669) exceeds that column's total (59,137,774). It probably came from an older dataset. **Bug fix:** the original built a design with the 80 replicate weights (`srvyr::as_survey()`, JK1), but 342 rows from 2011 have missing replicate weights, which the survey package rejects. The design now uses the person weight `PWGTP` only (`survey::svydesign()`); weighted counts and percentages are unchanged, and only the p-values can differ. |
| Table 6 | `table6.R` | `work.R` lines 12-34 | **Bug fix:** the citizen group was coded with `"US-citizen Born abroad "`, but the data say `"US-citizen Born Abroad"`. 16,187 foreign-born citizens were counted as US-born. The models run on `Data` as loaded; the original's `data` copy had no effect, because no model variable has missing values. **Remaining difference:** the expansion main effect differs from the paper in the 4th decimal (uninsured −0.08296 vs −0.0831; Medicaid 0.14088 vs 0.1410). The citizenship interactions, the other main effects and R² match. No alternative coding tested matched better. |
| Tables 7, 8 | `table7_8.R` | `codes/R/05-event-study.R` | **Reconstructed to match:** `controls` and `foriegn` were never defined in any saved script. The definitions in `00_setup.R` reproduce both tables exactly: `controls` = SEX, DIS, AGEP, POVPIPG, SCHLG, MARG, RACE1, ESRG, UnempR, ADA and `foriegn` = ENG, LTINU, CORIGIN, NonCit, IPC. The paper's appendix list leaves out employment status (ESRG) and country of origin (CORIGIN). The draft figures in `05` are not in the paper and were left out. |
| Fig 5 | `fig5.R` | `codes/R/09-sun&abraham-vs-event-fig3.R` lines 73-210 | **Bug fix:** `year_treated` was only created in `07`, on a copy of the data, so the script failed on its own. **Bug fix:** panel (b), labelled "Foreign-Born", plotted the US-born models (`fen`/`san`, `mfen`/`msan`); it now plots `fef`/`saf` and `mfef`/`msaf`. Lines 1-67 (an appendix figure not in the paper, with a `plot11` typo) were left out. |
| Fig 6 | none (retired) | `codes/R/04-causal-discovery-dag.R` lines 310-349 | **Dropped.** The DAG figure is no longer part of the analysis; its script is `archive/fig6.R` in commit `21c7fe0`. |
| Fig 7 | `fig7.R` | `codes/R/08-function-event-plot-adj.R` lines 190-245 | Uncommented; otherwise unchanged. |
| Fig 8 | `fig8.R` | `codes/R/08-function-event-plot-unad.R` | None. The colours differ from the paper's Figure 8 because the palette in the original script was changed after the paper's figure was made. |
| Fig 9 | `fig9.R` | none | **Reconstructed to match:** no code for Figure 9 existed. It is the Figure 7 code with the control variables removed. |
| Fig 10 | `fig10.R` | `codes/R/10-event-plot-mexican-adj.R` | None. |

## Data preparation (`code/data_prep/`)

- `01_get_data.R` was rewritten. It downloads the national PUMS files from the Census Bureau's
  website instead of using the Census API, so it needs no API key. The old script had its API key
  written in the code; that key is still in the git history.
- `02_bind_subset.R` was rewritten as `02_merge.R`. It reads the yearly files from
  `01_get_data.R`, builds one checked state-by-year table (`data/state_year.csv`) and adds it to
  every person, matching on the FIPS code and year. It no longer applies sample restrictions
  (these move to `make_sample()` in `code/00_setup.R`). Errors found and fixed on 27 Oct 2026:
  - `data/UnempR.xlsx` had North Carolina/North Dakota and South Carolina/South Dakota swapped
    from 2010 on, and wrong values for New Mexico (up to 3.1 points) and New York (up to 1.8).
    The rates now come straight from BLS. **The paper's estimates used the wrong rates.**
  - ADA for California 2012 was 0.49 in `Pol.xlsx` Sheet4 but 0.58 in the 2012 sheet it is
    built from; 0.58 is used.
  - DC was dropped by accident: the IPC files spell it "DC" and the old merges matched on state
    names. DC is now kept. Its ADA score is missing (DC has no voting member of Congress).
  - The old script labelled expansion states "Non-expansion" and the reverse (factor levels
    relabelled in the wrong order). `expansion` is now 0/1.
  - IPC and `treat` are identical to the values in `data/CLNACS2.RData`.
- `03_clean.R` was rewritten. It recodes every ACS variable to mean the same in every year, builds
  the analysis variables and drops nobody; the sample rules are in `make_sample()`.
- These scripts are not run by `run_all.R`; run them first (see README.md, "Instructions to
  replicators").

## Archive

`archive/2026-11-02/` holds the tables and figures as they were on 2 November 2026, before the
analysis was revised for the HSR submission, and the scripts of the replaced Figures 1-3.

The earlier contents of `archive/` were removed on the same day. They are in git commit `21c7fe0`:

- `codes/R/`: scripts `03`-`10`, `table1.R`, `graphs-draft.R`, `scratch-codes.R`,
  `code Notebook.Rmd`/`.nb.html`, `Specification Check.Rmd`/`.nb.html`.
- `work.R` and `func-event-adj.R` from the project root. `func-event-adj.R` is an older version of
  the Figure 1 code with a different specification (different control interactions, runs on
  `Data` instead of `data`); the version in `08-function-event-plot-adj.R` is the one that matches
  the paper.
- `output_old/`: the 2023 draft images that were in `output/`.
- `fig6.R` from `code/analysis/` (the DAG, Figure 6 of the earlier paper). Dropped from the
  analysis; dagitty and ggdag, which only it used, are no longer required.
- `check_paper.R` from `code/`. It compared key numbers with `Paper_1.pdf`. It was retired
  because the analysis no longer aims to reproduce that paper: the sample, outcome and standard
  errors have changed (see above).
- `gitmodules`: the old `.gitmodules`, which pointed to an Overleaf submodule that is not in the
  repository.
