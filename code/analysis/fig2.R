# Title: Figure 2. Event study of Medicaid coverage by citizenship, five covariate specifications
# Author: Shadi Seyedi
# Description: Expansion effect on Medicaid coverage for US-born adults, naturalized citizens and
#              non-citizens, by years since expansion. Panels A-E add the covariate blocks of
#              event_specs one at a time. Model and helpers in code/00_setup.R ("Event studies by
#              citizenship"). Replaces the paper's Figure 2 (undocumented vs legal foreign-born),
#              archived in archive/2026-11-02/.
# Output: output/figures/fig2.pdf, output/tables/fig2_estimates.csv

source("code/00_setup.R")
Data <- prep_event_data(make_sample(readRDS(data_file), sample_name))

library(ggplot2)
library(fixest)

spec_figure(Data, "HINS4", "Change in probability of Medicaid coverage (percentage points)", "fig2")
