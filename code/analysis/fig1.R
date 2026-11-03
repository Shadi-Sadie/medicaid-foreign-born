# Title: Figure 1. Event study of any coverage by citizenship, five covariate specifications
# Author: Shadi Seyedi
# Description: Expansion effect on any health insurance coverage for US-born adults, naturalized
#              citizens and non-citizens, by years since expansion. Panels A-E add the covariate
#              blocks of event_specs one at a time. Model and helpers in code/00_setup.R
#              ("Event studies by citizenship"). Replaces the paper's Figure 1 (pooled
#              foreign-born), archived in archive/2026-11-02/.
# Output: output/figures/fig1.pdf, output/tables/fig1_estimates.csv

source("code/00_setup.R")
Data <- prep_event_data(make_sample(readRDS(data_file), sample_name))

library(ggplot2)
library(fixest)

spec_figure(Data, "HICOV", "Change in probability of any insurance (percentage points)", "fig1")
