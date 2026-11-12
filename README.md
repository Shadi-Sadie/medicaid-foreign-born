# Replication package: Medicaid expansion and foreign-born adults

This package builds the analysis of *Medicaid Expansion and Foreign-Born Adults* from the raw
data. It studies the effect of the ACA Medicaid expansions on health insurance coverage of
low-income foreign-born and US-born adults aged 26-64 not enrolled in Medicare, using ACS 1-year
PUMS data for 2011-2019 and difference-in-differences and event-study models.

The tables and figures are those of the Health Services Research submission. The earlier version
of the paper (`Paper_1.pdf`) had different exhibits; its scripts are archived (see
`archive/`), and the data are rebuilt from the Census files, with a different sample, outcome,
fixed effects and standard errors. `CHANGES.md` lists every difference.

The README follows the structure of the [Social Science Data Editors' template](https://social-science-data-editors.github.io/template_README/).
Items still to be completed are marked **TODO**.

## Overview

- The three scripts in `code/data_prep/` download the ACS from the Census Bureau, add the
  state-by-year data and build the cleaned file `data/clean/acs_clean.rds`. They drop nobody.
  Run them first; `run_all.R` does not run them.
- `run_all.R` re-creates every table and figure from `data/clean/acs_clean.rds`.
- The sample rules are written once, in `make_sample()` in `code/00_setup.R`, each with the
  reason for it. Sensitivity samples are named there too.
- Each table or figure has its own script in `code/analysis/` (see the list below).
- Running time: `run_all.R` takes about 42 minutes on a 24-core machine with 62 GB RAM (11 Nov
  2026); the data preparation is not included (see "Memory and runtime").

## Data availability and provenance statements

### Statement about rights

- [ ] I certify that the author(s) of the manuscript have legitimate access to and permission to use the data used in this manuscript.
- [ ] I certify that the author(s) of the manuscript have documented permission to redistribute/publish the data contained within this replication package. **TODO: confirm for the IPC index and the ADA scores.**

### Summary of availability

- [ ] All data **are** publicly available.
- [x] Some data **cannot be made** publicly available. **TODO: confirm once redistribution rights are checked.**

### Details on each data source

| Data | Files | Source | Provided |
|---|---|---|---|
| American Community Survey (ACS) 1-year PUMS, person and housing records, 2011-2019 | `data/raw/census_files/` (the original Census zip files); `data/raw/acs_YYYY.rds`, `repwt_YYYY.rds` (read and filtered); `data/clean/acs_merged.rds`, `acs_clean.rds` (merged and cleaned, used by the analysis) | U.S. Census Bureau, national PUMS files from [www2.census.gov/programs-surveys/acs/data/pums/](https://www2.census.gov/programs-surveys/acs/data/pums/), downloaded by `code/data_prep/01_get_data.R`. File checksums and download dates are saved in `data/raw/download_log.csv`. | No: public, and re-downloaded by `01_get_data.R` (about 10 GB, not in the git repository). **TODO:** decide whether to also deposit the extract with a DOI (for example openICPSR or Zenodo). |
| State unemployment rates | `data/raw/bls/la.data.2.AllStatesU` (downloaded by `code/data_prep/02_merge.R`; the annual averages are saved in `data/state_year.csv`) | Bureau of Labor Statistics, Local Area Unemployment Statistics, [bls.gov/lau](https://www.bls.gov/lau/), series LAUST<FIPS>0000000000003, annual average. The old `data/UnempR.xlsx` had errors and was removed (see CHANGES.md; it is in the git history). | Yes |
| Medicaid expansion status and dates | `data/expansion.csv` | KFF, [Status of State Medicaid Expansion Decisions](https://www.kff.org/medicaid/issue-brief/status-of-state-medicaid-expansion-decisions/) | Yes |
| Immigration Policy Climate (IPC) index | `data/IPC.csv`, `data/IPC_Final.csv` | [IPC Index dataset](https://www.goleensamari.com/data) | **TODO: check redistribution terms** |
| State political ideology (ADA scores) | `data/Pol.xlsx` | **TODO: source and citation** | **TODO: check redistribution terms** |

Variable descriptions for the ACS extract and the cleaning steps are in
[docs/data_description.md](docs/data_description.md) (the project's earlier README).

## Computational requirements

### Software

- R 4.5.3, from the conda environment `r45` (`~/anaconda3/envs/r45`).
- R packages (already installed in `~/R/x86_64-pc-linux-gnu-library/4.5`): fixest, ggplot2,
  dplyr, scales, kableExtra, flextable, officer, HonestDiD (0.2.8, for `honest_did.R`).
  `code/00_setup.R` stops with a message if any are missing. HonestDiD's solvers need the GLPK
  system library (Ubuntu: `sudo apt install libglpk40`, or `libglpk-dev` to build from source;
  building its dependencies from source also needs `libgmp-dev` and Rust).
- The exact package versions of the last run are written to `output/session_info.txt`.
- The data-preparation scripts also need data.table and readxl, and an internet connection.
  `02_merge.R` downloads the BLS unemployment file, and BLS refuses downloads that do not name a
  contact: set the environment variable `BLS_CONTACT` to your email (for example in
  `~/.Renviron`). No Census API key is needed.

### Memory and runtime

- Memory: the cleaned file has 12,413,778 rows (everyone aged 18 and older with income up to
  400% of poverty) and 106 columns; the main sample has 1,340,837 people. At least 40 GB of RAM
  is recommended: in the full run of 11 Nov 2026 the most demanding script peaked at about 36 GB
  (`table2.R` alone peaks at about 21 GB).
- Disk: about 10 GB for the Census zip files and the files built from them.
- Running time: `01_get_data.R` takes about 15 minutes (peak memory about 5 GB). `run_all.R` takes
  about 42 minutes, longest `leave_one_out.R` (10 minutes) and `honest_did.R` (7 minutes).
  `02_merge.R` and `03_clean.R`: **TODO**.

## Description of programs

| Folder / file | Purpose |
|---|---|
| `run_all.R` | Master script. Runs every script below in a fresh R process, writes logs to `output/logs/`. |
| `code/00_setup.R` | Checks packages; sets paths; defines the main and sensitivity samples (`make_sample()`), the outcomes, and the models, estimates and table formatting that several scripts share. Sourced by every analysis script. |
| `code/analysis/` | One script per table or figure (list below). Each runs on its own. |
| `code/data_prep/` | `01_get_data.R` (downloads and reads the Census files), `02_merge.R` (stacks the ACS years and adds the state-by-year data, also saved as `data/state_year.csv`), `03_clean.R` (recoding and analysis variables). Run in this order, before `run_all.R`. |
| `archive/` | Tables, figures and scripts of the earlier paper as of 2 November 2026 (`archive/2026-11-02/`), including its Tables 3-8 and Figures 4-10, which the submission does not use. Not run. The original scripts are in git commit `21c7fe0`. See `CHANGES.md`. |
| `CHANGES.md` | Every difference between the scripts here and the original code. |

## Instructions to replicators

All commands run from the project root.

1. Build the data (once). Each script skips work whose output already exists:

   ```bash
   export R_LIBS_USER=~/R/x86_64-pc-linux-gnu-library/4.5
   export BLS_CONTACT=you@example.org
   ~/anaconda3/envs/r45/bin/Rscript code/data_prep/01_get_data.R
   ~/anaconda3/envs/r45/bin/Rscript code/data_prep/02_merge.R
   ~/anaconda3/envs/r45/bin/Rscript code/data_prep/03_clean.R
   ```

2. Re-create the tables and figures:

   ```bash
   ~/anaconda3/envs/r45/bin/Rscript run_all.R
   ```

3. Tables are written to `output/tables/` (`.tex`, plus `.csv` files with the estimates), figures
   to `output/figures/` (`.pdf`), and a log for each script to `output/logs/`.

A single item can be re-created by running its script on its own, for example
`Rscript code/analysis/fig3.R`. To run it on a sensitivity sample, add the sample's name, for
example `Rscript code/analysis/table2.R pov_100`. The results then go to
`output/tables_pov_100/` and `output/figures_pov_100/`, so they never overwrite the
main results. The sample names and the reason for each are in `sample_variants` in
`code/00_setup.R`.

## List of tables and figures

The main exhibits keep their numbers. The appendix is grouped by the question each exhibit answers,
and numbered within each section (A data and sample; B identification; C robustness;
D heterogeneity). Appendix scripts and outputs are named by content, and their numbers are set in
one place, `exhibits` in `code/00_setup.R`: the Word and LaTeX tables take their numbers from it,
so renumbering changes one line there (and the table below).

### Main exhibits

| Paper item | Program | Output | Note |
|---|---|---|---|
| Table 1 | `code/analysis/table1.R` | `output/tables/table1.tex`, `table1.docx`, `table1.csv` | Weighted, pre-ACA, by citizenship and expansion status |
| Table 2 | `code/analysis/table2.R` | `output/tables/table2.tex`, `table2.docx`, `table2.csv`, `table2_by_spec.csv` | Effects by citizenship and differences between groups |
| Figure 1 | `code/analysis/fig1.R` | `output/figures/fig1.pdf`, `output/tables/fig1_estimates.csv` | Any coverage by citizenship, five covariate specifications |
| Figure 2 | `code/analysis/fig2.R` | `output/figures/fig2.pdf`, `output/tables/fig2_estimates.csv` | Medicaid by citizenship, five covariate specifications |
| Figure 3 | `code/analysis/fig3.R` | `output/figures/fig3.pdf`, `output/tables/fig3_estimates.csv` | Differences between the groups' effects, four coverage outcomes |

### Appendix

| Section | Item | Program | Output | Note |
|---|---|---|---|---|
| A. Data and sample | (none yet) | | | |
| B. Identification | Figure B1 | `trends_raw.R` | `output/figures/trends_raw.pdf`, `output/tables/trends_raw_means.csv` | Weighted coverage rates by year, citizenship and expansion status (2014 expanders vs never expanded) |
| B. Identification | Figure B2 | `control_outcomes.R` | `output/figures/control_outcomes.pdf`, `output/tables/control_outcomes_estimates.csv` | Event studies with the control variables as outcomes (bad-control and sample-selection check) |
| B. Identification | Table B1 | `comparison_samples.R` | `output/tables/comparison_samples.tex`, `comparison_samples.docx`, `comparison_samples.csv` | Table 2 model on a higher-income comparison sample (139-400% of poverty, above the expansion limit; not a placebo, since the rest of the ACA affects it) and adults with a high school education or less up to 400% |
| B. Identification | Table B2 | `pretrend_tests.R` | `output/tables/pretrend_tests.tex`, `pretrend_tests.docx`, `pretrend_tests.csv` | Joint F tests that the event-study coefficients 2-4 years before expansion are zero, by citizenship group and for differences between groups, four outcomes, specification E |
| B. Identification | Figure B3 | `honest_did.R` | `output/figures/honest_did.pdf`, `output/tables/honest_did.csv` | HonestDiD sensitivity to departures from parallel trends (relative magnitudes; with the sign of the pre-trend; with leads -3 to -2; smoothness): naturalized any insurance, naturalized minus US-born any insurance, naturalized direct purchase |
| C. Robustness | Table C1, Figure C1 | `robustness.R` | `output/tables/robustness.tex`, `robustness.docx`, `robustness.csv`; `output/figures/robustness_plot.pdf` | Sun and Abraham estimator; including the early-option states; non-citizens in the US 10+ years |
| C. Robustness | Table C2 | `wild_bootstrap.R` | `output/tables/wild_bootstrap.tex`, `wild_bootstrap.docx`, `wild_bootstrap.csv` | Wild cluster restricted bootstrap p-values (Rademacher weights by state, 9,999 replications) for the Table 2 effects and differences between groups, four outcomes |
| C. Robustness | Figure C2 | `sunab_event.R` | `output/figures/sunab_event.pdf`, `output/tables/sunab_event_estimates.csv` | Sun and Abraham event studies by citizenship and differences between groups, four outcomes, specification E |
| C. Robustness | Figure C3 | `specs_employer.R` | `output/figures/specs_employer.pdf`, `output/tables/specs_employer_estimates.csv` | Employer-sponsored insurance by citizenship, five covariate specifications |
| C. Robustness | Figure C4 | `specs_direct.R` | `output/figures/specs_direct.pdf`, `output/tables/specs_direct_estimates.csv` | Directly purchased insurance by citizenship, five covariate specifications |
| C. Robustness | Figure C5 | `leave_one_out.R` | `output/figures/leave_one_out.pdf`, `output/tables/leave_one_out.csv` | Table 2 model re-estimated leaving out one state at a time (40 states), four outcomes, groups and naturalized minus US-born |
| D. Heterogeneity | Table D1 | `income_bands.R` | `output/tables/income_bands.tex`, `income_bands.docx`, `income_bands.csv` | Table 2 model at or below 100% and at 101-138% of poverty |
| D. Heterogeneity | Figure D1 | `marketplace_composition.R` | `output/figures/marketplace_reweighting.pdf`, `output/tables/marketplace_reweighting.csv`, `marketplace_reweighting_yearly.csv`, `marketplace_composition_changes.csv`, `marketplace_balance.csv` | Non-expansion states: coverage of naturalized citizens, US-born adults, and US-born adults reweighted (DiNardo-Fortin-Lemieux) to the naturalized citizens' characteristics and state; whether composition explains the naturalized citizens' larger gain after 2014 |

All appendix programs are in `code/analysis/`. `comparison_samples.R`, `robustness.R` and `income_bands.R`
run their own list of samples, so they take no sample name.

## Known gaps

- **`FBLEG` was reconstructed**, because the original code never defined it; it was chosen
  because it reproduced the earlier paper's numbers. See `CHANGES.md`.
- **Sample:** Oregon (state FIPS 41) is in the main sample, although the earlier paper says it
  was excluded.
- The old files `data/RAWACS*` and `data/CLNACS*` (from the earlier Census API download) are no
  longer used by any script.
- The old `.gitmodules` pointed to an Overleaf submodule (`manuscripts/...`) that is not in the
  repository; it is `archive/gitmodules` in git commit `21c7fe0`.
- **TODO:** convert this README to PDF for a journal deposit.
