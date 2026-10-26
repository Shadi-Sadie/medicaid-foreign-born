# Title: Table 1. Baseline characteristics, pre-ACA, by nativity and expansion status
# Author: Shadi Seyedi
# Description: Means for foreign-born (FB) and US-born (USB) adults before the ACA, in
#              non-expansion and expansion states, with the FB - USB difference in each state
#              group and the non-expansion - expansion difference within each nativity group
#              ("Exp. Var."). Rebuilt from `tableA1` in codes/R/03-survey-design-descriptive.R
#              lines 430-542 (now in archive/), which no longer ran.
# Output: output/tables/table1.tex and table1.csv
#
# RECONSTRUCTED: the paper's title says "(Weighted)", but its numbers are the UNWEIGHTED means
# computed here. This script reproduces the table as published; whether it should be weighted is
# an open decision (see CHANGES.md).

source("code/00_setup.R")
load(data_file)

library(dplyr)
library(gtsummary)


####################
## Data Preparation
####################

# As in the original tableA1, every category becomes its own 0/1 variable, so that each level
# gets its own difference and test.
pre <- Data[Data$ACA == "Pre-ACA", ]

tab_data <- data.frame(UNINS = pre$UNINS, HINS4 = pre$HINS4, HINS2 = pre$HINS2, HINS1 = pre$HINS1)

pre$SEX <- factor(pre$SEX, levels = c(2, 1), labels = c("Female", "Male"))
for (v in c("RACE1", "SEX", "ESRG", "SCHLG", "POVPIPG")) {
        for (l in levels(pre[[v]])) tab_data[[paste0(v, ": ", l)]] <- as.integer(pre[[v]] == l)
}

tab_data$DIS  <- pre$DIS
tab_data$MARG <- pre$MARG
tab_data$AGEP <- pre$AGEP
tab_data$NATIVITY  <- factor(pre$NATIVITY, levels = c("Foregin-born", "US-born"), labels = c("FB", "USB"))
tab_data$expansion <- factor(pre$expansion, levels = c(0, 1), labels = c("Non-expansion", "Expansion"))

labels <- list(UNINS ~ "Uninsured", HINS4 ~ "Medicaid", HINS2 ~ "Individually Purchased",
               HINS1 ~ "Employer Sponsored", DIS ~ "Disability", MARG ~ "Married", AGEP ~ "Age")


#######################
## Table 1
######################

# FB vs USB within one group of states
by_nativity <- function(d) {
        d %>%
                select(-expansion) %>%
                tbl_summary(
                        by = NATIVITY,
                        missing = "no",
                        label = labels,
                        statistic = list(all_continuous() ~ "{mean} ({sd})",
                                         all_dichotomous() ~ "{p}")
                ) %>%
                add_difference() %>%
                add_significance_stars(pattern = "{estimate}{stars}", thresholds = c(0.01, 0.05, 0.1),
                                       hide_ci = TRUE, hide_p = TRUE)
}

# Non-expansion vs expansion within one nativity group; only the difference is shown
by_expansion <- function(d) {
        d %>%
                select(-NATIVITY) %>%
                tbl_summary(
                        by = expansion,
                        missing = "no",
                        label = labels,
                        statistic = list(all_continuous() ~ "{mean} ({sd})",
                                         all_dichotomous() ~ "{p}")
                ) %>%
                add_difference() %>%
                add_significance_stars(pattern = "{estimate}{stars}", thresholds = c(0.01, 0.05, 0.1),
                                       hide_ci = TRUE, hide_p = TRUE) %>%
                modify_column_hide(columns = all_stat_cols())
}

table1 <- tbl_merge(
        tbls = list(by_nativity(filter(tab_data, expansion == "Non-expansion")),
                    by_nativity(filter(tab_data, expansion == "Expansion")),
                    by_expansion(filter(tab_data, NATIVITY == "FB")),
                    by_expansion(filter(tab_data, NATIVITY == "USB"))),
        tab_spanner = c("**Non-expansion**", "**Expansion**", "**Exp. Var. FB**", "**Exp. Var. USB**")
) %>%
        modify_caption("**Table 1. Baseline Characteristics (Unweighted)**")

write.csv(as_tibble(table1, col_labels = FALSE), file.path(table_dir, "table1.csv"), row.names = FALSE)
gt::gtsave(as_gt(table1), file.path(table_dir, "table1.tex"))
