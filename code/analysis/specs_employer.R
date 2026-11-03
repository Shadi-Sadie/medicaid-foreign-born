# Title: Event study of employer-sponsored coverage by citizenship, five covariate
#        specifications (appendix)
# Author: Shadi Seyedi
# Description: As Figure 1, for employer-sponsored insurance. Model and helpers in
#              code/00_setup.R ("Event studies by citizenship").
# Output: output/figures/specs_employer.pdf, output/tables/specs_employer_estimates.csv

source("code/00_setup.R")
Data <- prep_event_data(make_sample(readRDS(data_file), sample_name))

library(ggplot2)
library(fixest)

spec_figure(Data, "HINS1", "Change in probability of employer-sponsored insurance (percentage points)", "specs_employer")
