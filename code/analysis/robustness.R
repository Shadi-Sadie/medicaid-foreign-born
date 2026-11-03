# Title: Robustness to the estimator and the sample (appendix section C; numbers in `exhibits` in
#        00_setup.R)
# Author: Shadi Seyedi
# Description: The Table 2 effects with the Sun and Abraham (2021) estimator, robust to effects that
#              differ by expansion cohort (fit_sunab_models() and sunab_estimate() in 00_setup.R:
#              the average from the year of expansion on of the event study in sunab_event.R), and
#              the Table 2 model (static, specification E) on other samples: without Florida,
#              without Texas, including the early-option states, and non-citizens in the US at
#              least 10 years (instead of 5), who are more likely to be lawfully present and past
#              the 5-year bar on federally funded Medicaid. The main sample is the first
#              row. A table and a forest plot of the same estimates. Each sample and the reason for
#              it are in sample_variants in 00_setup.R; the model, table and plot are
#              check_estimates(), check_table() and check_plot() there. Runs its own list of
#              samples, so it takes no sample name.
# Output: output/tables/robustness.tex (LaTeX), robustness.docx (Word), robustness.csv;
#         output/figures/robustness_plot.pdf

source("code/00_setup.R")
if (sample_name != "main") stop("robustness.R runs its own list of samples; run it without a sample name.")

library(fixest)   # attached: i() in the covariates is looked up by name
library(kableExtra)
library(ggplot2)

checks <- c("Main sample"                       = "main",
            "Sun and Abraham"                   = "main",
            "Without Florida"                   = "drop_florida",
            "Without Texas"                     = "drop_texas",
            "With the early-option states"      = "keep_early_option",
            "Non-citizens in the US 10+ years"  = "residency_10y")

estimates <- check_estimates(checks)
check_table(estimates, checks, "robustness",
            title = "Robustness of the effects of Medicaid expansion on coverage by citizenship to the estimator and the sample",
            rows = paste(
                    "Each row is the model of Table 2 (all covariates) on the main sample of Table 1, except as stated.",
                    "Sun and Abraham: the interaction-weighted estimator of Sun and Abraham, which allows effects",
                    "to differ by the year a state expanded (2014, 2015, 2016 or 2019) and over time; each group's estimate",
                    "averages its effects from the year of expansion on, weighted by the weighted number of people in each",
                    sprintf("expansion year and year since expansion (%s shows the effects by year).", exhibits[["sunab_event"]]),
                    "Without Florida, without Texas: the main sample without one of the two largest states that did not",
                    "expand. With the early-option states: the main sample with California, Colorado, Connecticut,",
                    "Minnesota, New Jersey and Washington, which expanded partly before 2014, counted as 2014 expanders.",
                    "Non-citizens in the US 10+ years: non-citizens in the US fewer than 10 years (instead of 5) left out."))
check_plot(estimates, checks, "robustness_plot",
           caption = paste("Static difference-in-differences estimates with all covariates (Table 2), and the Sun and Abraham",
                           "estimator on the main sample, with 95% confidence intervals clustered by state."))
