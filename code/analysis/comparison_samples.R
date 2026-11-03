# Title: Comparison samples (appendix section B, identification; number in `exhibits` in
#        00_setup.R)
# Author: Shadi Seyedi
# Description: The Table 2 model (static difference-in-differences, specification E) on two other
#              samples: a higher-income comparison sample, adults with income at 139-400% of
#              poverty, above the expansion limit and so not eligible for expansion Medicaid but
#              affected by the rest of the ACA (Marketplace subsidies in every state), so not a
#              clean placebo; and adults with a high school education or less at any income up to
#              400%, a sample that expansion cannot select by moving incomes across the limit (see
#              control_outcomes.R). The main sample is the first row. Each sample and the reason
#              for it are in sample_variants in 00_setup.R; the model, table and note layout are
#              check_estimates() and check_table() there. Runs its own list of samples, so it
#              takes no sample name.
# Output: output/tables/comparison_samples.tex (LaTeX), comparison_samples.docx (Word),
#         comparison_samples.csv

source("code/00_setup.R")
if (sample_name != "main") stop("comparison_samples.R runs its own list of samples; run it without a sample name.")

library(fixest)   # attached: i() in the covariates is looked up by name
library(kableExtra)

checks <- c("Main sample"                           = "main",
            "Higher-income comparison: 139-400%"    = "pov_139_400",
            "High school or less"                   = "edu_hs")

estimates <- check_estimates(checks)
check_table(estimates, checks, "comparison_samples",
            title = "Effects of Medicaid expansion on coverage by citizenship in a higher-income comparison sample and a sample defined by education",
            rows = paste(
                    "Each row is the model of Table 2 (all covariates) on another sample: the main sample of Table 1;",
                    "a higher-income comparison sample of adults with income at 139% to 400% of the federal poverty",
                    "level, above the expansion limit, who were not eligible for expansion Medicaid but could get",
                    "subsidized Marketplace coverage in every state, so that the rest of the ACA can affect them; and",
                    "adults with a high school education or less at any income up to 400% of poverty, a sample that",
                    sprintf("expansion cannot change by moving incomes across the limit (%s).", exhibits[["control_outcomes"]])))
