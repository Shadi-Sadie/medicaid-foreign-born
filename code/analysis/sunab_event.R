# Title: Event studies by citizenship with the Sun and Abraham estimator (appendix)
# Author: Shadi Seyedi
# Description: Sun and Abraham (2021) interaction-weighted event studies, robust to effects that
#              differ by expansion cohort, for any coverage, Medicaid, employer-sponsored and
#              directly purchased insurance. Left column: each group's effects (as Figures 1, 2,
#              specs_employer.R, specs_direct.R); right column: the differences between groups (as Figure 3). Specification E
#              only, the preferred model of Table 2 and Figure 3; Figures 1-2 show that the
#              covariates barely move the estimates. Each event time averages the expansion cohorts
#              observed at it, weighted by their weighted number of people. Same model as the Sun and Abraham row of the robustness table
#              (fit_sunab_models() and sunab_event_estimates() in code/00_setup.R).
# Output: output/figures/sunab_event.pdf, output/tables/sunab_event_estimates.csv

source("code/00_setup.R")
Data <- add_sunab_cells(prep_event_data(make_sample(readRDS(data_file), sample_name)))

library(ggplot2)
library(fixest)

# Same series names as Figures 1-3
series <- c(setNames(lapply(cit_groups, function(g) list(series = g, w = 1)), cit_groups),
            list("Naturalized citizen - US-born"     = list(series = c("Naturalized citizen", "US-born"), w = c(1, -1)),
                 "Non-citizen - US-born"             = list(series = c("Non-citizen", "US-born"), w = c(1, -1)),
                 "Non-citizen - Naturalized citizen" = list(series = c("Non-citizen", "Naturalized citizen"), w = c(1, -1))))

# One model per outcome, freed before the next is fit
est <- do.call(rbind, lapply(outcome_vars, function(y) {
        m <- fit_sunab_models(Data, y)
        cells <- sunab_cells(m, Data)
        out <- do.call(rbind, lapply(names(series), function(s) {
                cbind(outcome = outcome_labels[[y]], series = s,
                      panel = if (s %in% cit_groups) "Effect of expansion" else "Difference between groups",
                      sunab_event_estimates(m, cells, series[[s]]$series, series[[s]]$w))
        }))
        rm(m); gc()
        out
}))
write.csv(est, file.path(table_dir, "sunab_event_estimates.csv"), row.names = FALSE)

n_states <- function(k) length(unique(Data$ST[Data$expansion == 1 & Data$ttot == k]))
caption <- paste(strwrap(sprintf(paste(
        "Sun and Abraham interaction-weighted estimates with 95%% confidence intervals clustered by state; specification E;",
        "event time -1 is the reference. Each event time averages the expansion cohorts observed at it, weighted by their",
        "weighted number of people: -4 is estimated from %d expansion states and +5 from %d (%d in total). Earlier event",
        "times are estimated but not shown. The two-way fixed effects estimates are in Figures 1-3."),
        n_states(-4), n_states(5), length(unique(Data$ST[Data$expansion == 1]))), 150), collapse = "\n")

est$outcome <- factor(est$outcome, levels = unique(est$outcome))
est$panel <- factor(est$panel, levels = c("Effect of expansion", "Difference between groups"))
fig <- event_plot(est, "Percentage points", c(group_palette, diff_palette), caption) +
        facet_grid(outcome ~ panel, scales = "free_y") +
        scale_shape_manual(values = rep(c(16, 17, 15), 2), name = NULL) +   # as in Figures 1-3
        guides(colour = guide_legend(nrow = 2, byrow = TRUE), shape = guide_legend(nrow = 2, byrow = TRUE))
ggsave(file.path(figure_dir, "sunab_event.pdf"), fig, width = 11, height = 11)
