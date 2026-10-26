# Changes from the original code

This file lists every place where the code in `code/analysis/` differs from the original scripts
it was copied from. The originals are kept, unchanged, in `archive/`.

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
  `data/CLNACS2.RData` itself, and saves its table or figure to `output/`. The originals assumed an
  interactive RStudio session with the data and other objects already in memory.
- Commented-out options and unused helper code were dropped when copying (for example, unused
  colour palettes). Estimation and plotting code is otherwise unchanged.
- The lines `Data$expansion <- as.numeric(Data$expansion)` / `ifelse(Data$expansion == 2, 0, ...)`
  in `05` and `07` were dropped. They converted an old factor coding. In `CLNACS2.RData`,
  `expansion` is already 0/1, so the lines did nothing.

## Item by item

| Paper item | Script | Copied from (in `archive/`) | Changes |
|---|---|---|---|
| Table 1 | `table1.R` | `codes/R/03-survey-design-descriptive.R`, `tableA1` (lines 430-542) | **Reconstructed to match.** The original block no longer ran (it used a `data` object from another script and the `dummy` package). Rebuilt on `CLNACS2`, pre-ACA. The paper's title says "(Weighted)", but only the **unweighted** means match its numbers, so the table is unweighted and the caption says so. The "Exp. Var." columns are the non-expansion minus expansion difference within each nativity group, pre-ACA. This matches the paper's numbers, though the paper's header gives all-years counts (389,955 and 1,585,639) for those columns. |
| Tables 2, 5 | `table2_5.R` | `codes/R/06-did-regressions.R` lines 1-145 | **Bug fix:** Table 2's title said "Without Controls" for the controlled model. The appendix block after line 145 was left out: it used `reg4`-`reg6` before defining them, and it is not in the paper. |
| Table 3, Fig 4 | `table3_fig4.R` | `codes/R/07-robustness-check.R` lines 168-236 | **Bug fix:** the original table typed the coefficients and weights in by hand. They are now computed from the decomposition. The values are the same. |
| Table 4 | `table4.R` | `texts/Draft-Result.Rmd` lines 425-446 | **Does not reproduce the paper.** On `CLNACS2` it gives different totals and rows (for example, 4 race groups where the paper has 5). The paper's table is internally inconsistent: the expansion-state disability count (116,770,669) exceeds that column's total (59,137,774). It probably came from an older dataset. **Bug fix:** the original built a design with the 80 replicate weights (`srvyr::as_survey()`, JK1), but 342 rows from 2011 have missing replicate weights, which the survey package rejects. The design now uses the person weight `PWGTP` only (`survey::svydesign()`); weighted counts and percentages are unchanged, and only the p-values can differ. |
| Table 6 | `table6.R` | `work.R` lines 12-34 | **Bug fix:** the citizen group was coded with `"US-citizen Born abroad "`, but the data say `"US-citizen Born Abroad"`. 16,187 foreign-born citizens were counted as US-born. The models run on `Data` as loaded; the original's `data` copy had no effect, because no model variable has missing values. **Remaining difference:** the expansion main effect differs from the paper in the 4th decimal (uninsured −0.08296 vs −0.0831; Medicaid 0.14088 vs 0.1410). The citizenship interactions, the other main effects and R² match. No alternative coding tested matched better. |
| Tables 7, 8 | `table7_8.R` | `codes/R/05-event-study.R` | **Reconstructed to match:** `controls` and `foriegn` were never defined in any saved script. The definitions in `00_setup.R` reproduce both tables exactly: `controls` = SEX, DIS, AGEP, POVPIPG, SCHLG, MARG, RACE1, ESRG, UnempR, ADA and `foriegn` = ENG, LTINU, CORIGIN, NonCit, IPC. The paper's appendix list leaves out employment status (ESRG) and country of origin (CORIGIN). The draft figures in `05` are not in the paper and were left out. |
| Fig 1 | `fig1.R` | `codes/R/08-function-event-plot-adj.R` lines 1-176, data prep from `06` lines 19-28 | None. Keeps `06`'s data preparation, which it ran on when it was checked against the paper. |
| Fig 2 | `fig2.R` | `codes/R/10-event-plot-undoc-adj.R` | **Reconstructed to match:** `FBLEG` was never defined. It is now "foreign-born and not undocumented" (`ForeginBorn == 1 & UNDOC == 0`). |
| Fig 3 | `fig3.R` | `codes/R/10-event-plot-cit-adj.R` | **Bug fix:** `FBCIT` used the same misspelled citizenship value as Table 6; it is now `ForeginBorn == 1 & NonCit == 0`. **Bug fix:** the script ran on the `data` copy from `06`, which turns ordered factors into plain factors. The coefficient names then no longer matched `variable_list` and it crashed. It now runs on `Data`. |
| Fig 5 | `fig5.R` | `codes/R/09-sun&abraham-vs-event-fig3.R` lines 73-210 | **Bug fix:** `year_treated` was only created in `07`, on a copy of the data, so the script failed on its own. **Bug fix:** panel (b), labelled "Foreign-Born", plotted the US-born models (`fen`/`san`, `mfen`/`msan`); it now plots `fef`/`saf` and `mfef`/`msaf`. Lines 1-67 (an appendix figure not in the paper, with a `plot11` typo) were left out. |
| Fig 6 | `fig6.R` | `codes/R/04-causal-discovery-dag.R` lines 310-349 | None. The causal-discovery part of `04` (pcalg, kpcalg, Rgraphviz, ...) is not needed for the figure. |
| Fig 7 | `fig7.R` | `codes/R/08-function-event-plot-adj.R` lines 190-245 | Uncommented; otherwise unchanged. |
| Fig 8 | `fig8.R` | `codes/R/08-function-event-plot-unad.R` | None. The colours differ from the paper's Figure 8 because the palette in the original script was changed after the paper's figure was made. |
| Fig 9 | `fig9.R` | none | **Reconstructed to match:** no code for Figure 9 existed. It is the Figure 7 code with the control variables removed. |
| Fig 10 | `fig10.R` | `codes/R/10-event-plot-mexican-adj.R` | None. |

## Data preparation (`code/data_prep/`)

- `01_get_data.R`: the Census API key was written in the code. It is now read from the
  `CENSUS_API_KEY` environment variable. The old key is still in the git history.
- Otherwise unchanged. These scripts are not run by `run_all.R` (see README.md, "Known gaps").

## Files moved to `archive/`

- `codes/R/`: scripts `03`-`10`, `table1.R`, `graphs-draft.R`, `scratch-codes.R`,
  `code Notebook.Rmd`/`.nb.html`, `Specification Check.Rmd`/`.nb.html`.
- `work.R` and `func-event-adj.R` from the project root. `func-event-adj.R` is an older version of
  the Figure 1 code with a different specification (different control interactions, runs on
  `Data` instead of `data`); the version in `08-function-event-plot-adj.R` is the one that matches
  the paper.
- `output_old/`: the 2023 draft images that were in `output/`.
- `gitmodules`: the old `.gitmodules`, which pointed to an Overleaf submodule that is not in the
  repository.
