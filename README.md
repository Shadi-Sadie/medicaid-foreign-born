# Replication package: Medicaid expansion and foreign-born adults

This package builds the analysis of *Medicaid Expansion and Foreign-Born Adults* from the raw
data. It studies the effect of the ACA Medicaid expansions on health insurance coverage of
low-income foreign-born and US-born adults aged 26-64 not enrolled in Medicare, using ACS 1-year
PUMS data for 2011-2019 and difference-in-differences and event-study models.

The tables and figures keep the numbering of the earlier version of the paper (`Paper_1.pdf`),
but the results differ from it: the data are rebuilt from the Census files, and the sample,
outcome, fixed effects and standard errors have changed. `CHANGES.md` lists every difference.

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
- Running time: **TODO: fill in after the first full run** on a 24-core machine with 62 GB RAM.

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
| State unemployment rates | `data/raw/bls/la.data.2.AllStatesU` (downloaded by `code/data_prep/02_merge.R`; the annual averages are saved in `data/state_year.csv`) | Bureau of Labor Statistics, Local Area Unemployment Statistics, [bls.gov/lau](https://www.bls.gov/lau/), series LAUST<FIPS>0000000000003, annual average. The old `data/UnempR.xlsx` had errors and is no longer used (see CHANGES.md). | Yes |
| Medicaid expansion status and dates | `data/expansion.csv` | KFF, [Status of State Medicaid Expansion Decisions](https://www.kff.org/medicaid/issue-brief/status-of-state-medicaid-expansion-decisions/) | Yes |
| Immigration Policy Climate (IPC) index | `data/IPC.csv`, `data/IPC_Final.csv` | [IPC Index dataset](https://www.goleensamari.com/data) | **TODO: check redistribution terms** |
| State political ideology (ADA scores) | `data/Pol.xlsx` | **TODO: source and citation** | **TODO: check redistribution terms** |

Variable descriptions for the ACS extract and the cleaning steps are in
[docs/data_description.md](docs/data_description.md) (the project's earlier README).

## Computational requirements

### Software

- R 4.5.3, from the conda environment `r45` (`~/anaconda3/envs/r45`).
- R packages (already installed in `~/R/x86_64-pc-linux-gnu-library/4.5`): fixest, ggplot2,
  ggpubr, ggiplot, dplyr, scales, gtsummary, gt, survey, bacondecomp, cowplot, kableExtra.
  `code/00_setup.R` stops with a message if any are missing.
- The exact package versions of the last run are written to `output/session_info.txt`.
- The data-preparation scripts also need data.table and readxl, and an internet connection.
  `02_merge.R` downloads the BLS unemployment file, and BLS refuses downloads that do not name a
  contact: set the environment variable `BLS_CONTACT` to your email (for example in
  `~/.Renviron`). No Census API key is needed.

### Memory and runtime

- Memory: the cleaned file has 12,413,778 rows (everyone aged 18 and older with income up to
  400% of poverty) and 106 columns; the main sample has 1,716,909 people. At least 16 GB of RAM
  is recommended.
- Disk: about 10 GB for the Census zip files and the files built from them.
- Running time: `01_get_data.R` takes about 15 minutes (peak memory about 5 GB). `02_merge.R`,
  `03_clean.R` and `run_all.R`: **TODO** (see Overview).

## Description of programs

| Folder / file | Purpose |
|---|---|
| `run_all.R` | Master script. Runs every script below in a fresh R process, writes logs to `output/logs/`. |
| `code/00_setup.R` | Checks packages; sets paths; defines the main and sensitivity samples (`make_sample()`), the outcomes, the shared control-variable lists and a helper that saves estimates. Sourced by every analysis script. |
| `code/analysis/` | One script per table or figure (list below). Each runs on its own. |
| `code/data_prep/` | `01_get_data.R` (downloads and reads the Census files), `02_merge.R` (stacks the ACS years and adds the state-by-year data, also saved as `data/state_year.csv`), `03_clean.R` (recoding and analysis variables). Run in this order, before `run_all.R`. |
| `archive/` | Original scripts, drafts and old outputs. Not used. See `CHANGES.md`. |
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
example `Rscript code/analysis/table2_5.R with_medicare`. The results then go to
`output/tables_with_medicare/` and `output/figures_with_medicare/`, so they never overwrite the
main results. The sample names and the reason for each are in `sample_variants` in
`code/00_setup.R`.

## List of tables and figures

| Paper item | Program | Output | Note |
|---|---|---|---|
| Table 1 | `code/analysis/table1.R` | `output/tables/table1.tex`, `table1.csv` | Unweighted (see Known gaps) |
| Table 2 | `code/analysis/table2_5.R` | `output/tables/table2.tex` | |
| Table 3 | `code/analysis/table3_fig4.R` | `output/tables/table3.tex`, `table3.csv` | |
| Table 4 | `code/analysis/table4.R` | `output/tables/table4.tex`, `table4.csv` | See Known gaps |
| Table 5 | `code/analysis/table2_5.R` | `output/tables/table5.tex` | |
| Table 6 | `code/analysis/table6.R` | `output/tables/table6.tex` | |
| Table 7 | `code/analysis/table7_8.R` | `output/tables/table7.tex` | |
| Table 8 | `code/analysis/table7_8.R` | `output/tables/table8.tex` | |
| Figure 1 | `code/analysis/fig1.R` | `output/figures/fig1.pdf` | |
| Figure 2 | `code/analysis/fig2.R` | `output/figures/fig2.pdf` | |
| Figure 3 | `code/analysis/fig3.R` | `output/figures/fig3.pdf` | |
| Figure 4 | `code/analysis/table3_fig4.R` | `output/figures/fig4.pdf` | |
| Figure 5 | `code/analysis/fig5.R` | `output/figures/fig5.pdf` | |
| Figure 7 | `code/analysis/fig7.R` | `output/figures/fig7.pdf` | |
| Figure 8 | `code/analysis/fig8.R` | `output/figures/fig8.pdf` | Colours differ from the paper |
| Figure 9 | `code/analysis/fig9.R` | `output/figures/fig9.pdf` | Reconstructed (see Known gaps) |
| Figure 10 | `code/analysis/fig10.R` | `output/figures/fig10.pdf` | |

## Known gaps

- **Some definitions were reconstructed**, because the original code never defined them: the
  control lists for Tables 7-8, `FBLEG` (Figure 2) and all of Figure 9. They were chosen because
  they reproduced the earlier paper's numbers. Table 1 is unweighted, although its title in the
  earlier paper says "(Weighted)". See `CHANGES.md`.
- **Table 4** in the earlier paper is internally inconsistent, and its code never reproduced it.
  Check its output before using it.
- **Sample:** Oregon (state FIPS 41) is in the main sample, although the earlier paper says it
  was excluded.
- The old files `data/RAWACS*` and `data/CLNACS*` (from the earlier Census API download) are no
  longer used by any script.
- The old `.gitmodules` pointed to an Overleaf submodule (`manuscripts/...`) that is not in the
  repository; it is kept in `archive/gitmodules`.
- **TODO:** convert this README to PDF for a journal deposit.
