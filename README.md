# Replication package: Medicaid expansion and foreign-born adults

This package reproduces the tables and figures of the paper *Medicaid Expansion and Foreign-Born
Adults* (`Paper_1.pdf`). The paper studies the effect of the ACA Medicaid expansions on health
insurance coverage of low-income foreign-born and US-born adults aged 26-64, using ACS 1-year PUMS
data for 2011-2019 and difference-in-differences and event-study models.

The README follows the structure of the [Social Science Data Editors' template](https://social-science-data-editors.github.io/template_README/).
Items still to be completed are marked **TODO**.

## Overview

- `run_all.R` re-creates every table and figure from the cleaned analysis file
  `data/CLNACS2.RData`, then checks key numbers against the paper.
- Each table or figure has its own script in `code/analysis/` (see the list below).
- The scripts that build the cleaned file from the raw data are in `code/data_prep/`. They are
  **not** run by `run_all.R` (see "Known gaps").
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
| American Community Survey (ACS) 1-year PUMS, person records, 2011-2019 | `data/RAWACS*.RData`, `data/RAWACS*.csv` (raw); `data/CLNACS2.RData` (cleaned, used by the analysis) | U.S. Census Bureau, downloaded through the Census API with the R package `tidycensus` (`code/data_prep/01_get_data.R`). [ACS microdata](https://www.census.gov/programs-surveys/acs/microdata.html) | **TODO**: the files are 0.3-1.3 GB and are not in the git repository. Deposit the extract with a DOI (for example openICPSR or Zenodo) and cite it here. |
| State unemployment rates | `data/UnempR.xlsx` | Bureau of Labor Statistics, Local Area Unemployment Statistics, [bls.gov/lau](https://www.bls.gov/lau/) | Yes |
| Medicaid expansion status and dates | `data/expansion.xlsx` | KFF, [Status of State Medicaid Expansion Decisions](https://www.kff.org/medicaid/issue-brief/status-of-state-medicaid-expansion-decisions/) | Yes |
| Immigration Policy Climate (IPC) index | `data/IPC.csv`, `data/IPC_Final.csv` | [IPC Index dataset](https://www.goleensamari.com/data) | **TODO: check redistribution terms** |
| State political ideology (ADA scores) | `data/Pol.xlsx` | **TODO: source and citation** | **TODO: check redistribution terms** |

Variable descriptions for the ACS extract and the cleaning steps are in
[docs/data_description.md](docs/data_description.md) (the project's earlier README).

## Computational requirements

### Software

- R 4.5.3, from the conda environment `r45` (`~/anaconda3/envs/r45`).
- R packages (already installed in `~/R/x86_64-pc-linux-gnu-library/4.5`): fixest, ggplot2,
  ggpubr, ggiplot, dplyr, scales, gtsummary, gt, survey, bacondecomp, cowplot, kableExtra,
  dagitty, ggdag. `code/00_setup.R` stops with a message if any are missing.
- The exact package versions of the last run are written to `output/session_info.txt`.
- The data-preparation scripts also need tidycensus, sqldf and readxl, and a Census API key in the
  environment variable `CENSUS_API_KEY` (set it in `~/.Renviron`).

### Memory and runtime

- Memory: the cleaned file has 1,975,594 rows and 152 columns. At least 16 GB of RAM is
  recommended; `table4.R` builds a survey design with 80 replicate weights.
- Running time of `run_all.R`: **TODO** (see Overview).

## Description of programs

| Folder / file | Purpose |
|---|---|
| `run_all.R` | Master script. Runs every script below in a fresh R process, writes logs to `output/logs/`, then runs `code/check_paper.R`. |
| `code/00_setup.R` | Checks packages; sets paths; defines the shared control-variable lists and a helper that saves estimates. Sourced by every analysis script. |
| `code/analysis/` | One script per table or figure (list below). Each runs on its own. |
| `code/check_paper.R` | Compares key numbers with the paper (Tables 1, 2, 3, 5, 6, 7, 8); writes `output/check_paper.csv` and fails if any differ. |
| `code/data_prep/` | `01_get_data.R` (download), `02_bind_subset.R` (sample restrictions, merge state data), `03_clean.R` (recoding). Not run by `run_all.R`. |
| `archive/` | Original scripts, drafts and old outputs. Not used. See `CHANGES.md`. |
| `CHANGES.md` | Every difference between the scripts here and the original code. |

## Instructions to replicators

1. Put the cleaned analysis file at `data/CLNACS2.RData`.
2. From the project root, run:

   ```bash
   export R_LIBS_USER=~/R/x86_64-pc-linux-gnu-library/4.5
   ~/anaconda3/envs/r45/bin/Rscript run_all.R
   ```

3. Tables are written to `output/tables/` (`.tex`, plus `.csv` files with the estimates), figures
   to `output/figures/` (`.pdf`). The final step prints how many checks against the paper pass.

A single item can be re-created by running its script on its own, for example
`Rscript code/analysis/fig3.R`.

## List of tables and figures

| Paper item | Program | Output | Note |
|---|---|---|---|
| Table 1 | `code/analysis/table1.R` | `output/tables/table1.tex`, `table1.csv` | Unweighted (see Known gaps) |
| Table 2 | `code/analysis/table2_5.R` | `output/tables/table2.tex` | |
| Table 3 | `code/analysis/table3_fig4.R` | `output/tables/table3.tex`, `table3.csv` | |
| Table 4 | `code/analysis/table4.R` | `output/tables/table4.tex`, `table4.csv` | **Does not reproduce the paper** |
| Table 5 | `code/analysis/table2_5.R` | `output/tables/table5.tex` | |
| Table 6 | `code/analysis/table6.R` | `output/tables/table6.tex` | |
| Table 7 | `code/analysis/table7_8.R` | `output/tables/table7.tex` | |
| Table 8 | `code/analysis/table7_8.R` | `output/tables/table8.tex` | |
| Figure 1 | `code/analysis/fig1.R` | `output/figures/fig1.pdf` | |
| Figure 2 | `code/analysis/fig2.R` | `output/figures/fig2.pdf` | |
| Figure 3 | `code/analysis/fig3.R` | `output/figures/fig3.pdf` | |
| Figure 4 | `code/analysis/table3_fig4.R` | `output/figures/fig4.pdf` | |
| Figure 5 | `code/analysis/fig5.R` | `output/figures/fig5.pdf` | |
| Figure 6 | `code/analysis/fig6.R` | `output/figures/fig6.pdf` | No data needed |
| Figure 7 | `code/analysis/fig7.R` | `output/figures/fig7.pdf` | |
| Figure 8 | `code/analysis/fig8.R` | `output/figures/fig8.pdf` | Colours differ from the paper |
| Figure 9 | `code/analysis/fig9.R` | `output/figures/fig9.pdf` | Reconstructed (see Known gaps) |
| Figure 10 | `code/analysis/fig10.R` | `output/figures/fig10.pdf` | |

## Known gaps

- **The cleaned file cannot yet be rebuilt from the raw data.** No script in the project creates
  `CLNACS2.RData` or its variable `CORIGIN`. The current data-preparation scripts write
  `CLNACS2025.RData`, which has 2,147,755 rows and lacks `NonCit` and `CORIGIN`, so it cannot
  produce the paper's results. They also use absolute paths (`/home/shadi/...`).
- **Some definitions were reconstructed to match the paper**, because the original code never
  defined them: the control lists for Tables 7-8, `FBLEG` (Figure 2) and all of Figure 9. Table 1
  is unweighted, although its title in the paper says "(Weighted)". See `CHANGES.md`.
- **Table 4 does not reproduce.** Its code gives different numbers on `CLNACS2`, and the paper's
  table is internally inconsistent.
- **Sample:** Oregon (state FIPS 41) is in the data, although the paper says it was excluded.
- The old `.gitmodules` pointed to an Overleaf submodule (`manuscripts/...`) that is not in the
  repository; it is kept in `archive/gitmodules`.
- **TODO:** convert this README to PDF for a journal deposit.
