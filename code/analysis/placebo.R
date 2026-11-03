# Title: Placebo checks (appendix section B, identification; number in `exhibits` in 00_setup.R)
# Author: Shadi Seyedi
# Description: The Table 2 model (static difference-in-differences, specification E) on samples
#              that expansion should not affect: adults with income at 139-400% of poverty, above
#              the expansion limit, and adults with a high school education or less at any income
#              up to 400%, a sample that expansion cannot select by moving incomes across the limit
#              (see control_outcomes.R). The main sample is the first row. Each sample and the
#              reason for it are in sample_variants in 00_setup.R; the model, table and note
#              layout are check_estimates() and check_table() there. Runs its own list of
#              samples, so it takes no sample name.
# Output: output/tables/placebo.tex (LaTeX), placebo.docx (Word), placebo.csv

source("code/00_setup.R")
if (sample_name != "main") stop("placebo.R runs its own list of samples; run it without a sample name.")

library(fixest)   # attached: i() in the covariates is looked up by name
library(kableExtra)

checks <- c("Main sample"              = "main",
            "Placebo: income 139-400%" = "pov_139_400",
            "High school or less"      = "edu_hs")

estimates <- check_estimates(checks)
check_table(estimates, checks, "placebo",
            title = "Placebo checks of the effects of Medicaid expansion on coverage by citizenship",
            rows = paste(
                    "Each row is the model of Table 2 (all covariates) on another sample: the main sample of Table 1;",
                    "adults with income at 139% to 400% of the federal poverty level, above the expansion limit, who",
                    "were not eligible for expansion Medicaid; and adults with a high school education or less at any",
                    "income up to 400% of poverty, a sample that expansion cannot change by moving incomes across the",
                    sprintf("limit (%s).", exhibits[["control_outcomes"]])))
