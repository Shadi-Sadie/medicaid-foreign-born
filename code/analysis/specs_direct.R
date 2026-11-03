# Title: Event study of directly purchased coverage by citizenship, five covariate
#        specifications (appendix)
# Author: Shadi Seyedi
# Description: As Figure 1, for directly purchased insurance. The ACS counts Marketplace plans as
#              directly purchased, so this outcome includes adults who would have bought a
#              subsidized Marketplace plan without expansion. Model and helpers in code/00_setup.R
#              ("Event studies by citizenship").
# Output: output/figures/specs_direct.pdf, output/tables/specs_direct_estimates.csv

source("code/00_setup.R")
Data <- prep_event_data(make_sample(readRDS(data_file), sample_name))

library(ggplot2)
library(fixest)

spec_figure(Data, "HINS2", "Change in probability of directly purchased insurance (percentage points)", "specs_direct")
