# Title: Effects by income band (appendix section D, heterogeneity; number in `exhibits` in
#        00_setup.R)
# Author: Shadi Seyedi
# Description: The Table 2 model (static difference-in-differences, specification E) at or below
#              100% of poverty, where adults in states that did not expand could not get subsidized
#              Marketplace coverage, and at 101-138%, where they could. The difference between the
#              bands shows how much of the effect on direct purchase comes from Marketplace coverage
#              in the comparison states. The main sample is the first row. The model and table are
#              check_estimates() and check_table() in 00_setup.R. Runs its own list of samples, so
#              it takes no sample name.
# Output: output/tables/income_bands.tex (LaTeX), income_bands.docx (Word), income_bands.csv

source("code/00_setup.R")
if (sample_name != "main") stop("income_bands.R runs its own list of samples; run it without a sample name.")

library(fixest)   # attached: i() in the covariates is looked up by name
library(kableExtra)

checks <- c("Main sample"                        = "main",
            "Income at or below 100%"            = "pov_100",
            "Income 101-138%"                    = "pov_101_138")

estimates <- check_estimates(checks)
check_table(estimates, checks, "income_bands",
            title = "Effects of Medicaid expansion on coverage by citizenship and income",
            rows = paste(
                    "Each row is the model of Table 2 (all covariates) on the main sample of Table 1 or one of its income",
                    "bands: at or below 100% of the federal poverty level, where adults in states that did not expand",
                    "could not get subsidized Marketplace coverage, and 101% to 138%, where they could."))
