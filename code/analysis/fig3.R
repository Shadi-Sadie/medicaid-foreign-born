# Title: Figure 3. Differences in expansion effects between citizenship groups
# Author: Shadi Seyedi
# Description: Differences between the groups' expansion effects on any coverage, Medicaid,
#              employer-sponsored and directly purchased insurance, by years since expansion, from
#              the fully adjusted model (specification E) of Figures 1-2, specs_employer.R and specs_direct.R. The standard
#              errors use the model's joint covariance, clustered by state. A person can report
#              more than one source, so the source panels do not add up to any coverage. Model and helpers in code/00_setup.R ("Event studies by citizenship").
#              Replaces the paper's Figure 3 (citizenship, four outcomes), archived in
#              archive/2026-11-02/.
# Output: output/figures/fig3.pdf, output/tables/fig3_estimates.csv

source("code/00_setup.R")
Data <- prep_event_data(make_sample(readRDS(data_file), sample_name))

library(ggplot2)
library(fixest)

contrasts <- list("Naturalized citizen - US-born"     = c("Naturalized citizen", "US-born"),
                  "Non-citizen - US-born"             = c("Non-citizen", "US-born"),
                  "Non-citizen - Naturalized citizen" = c("Non-citizen", "Naturalized citizen"))

est <- do.call(rbind, lapply(outcome_vars, function(y) {
        m <- fit_event_models(Data, y, nested = FALSE)
        do.call(rbind, lapply(names(contrasts), function(d) {
                cbind(outcome = outcome_labels[[y]], series = d, event_estimates(m, contrasts[[d]], c(1, -1)))
        }))
}))
write.csv(est, file.path(table_dir, "fig3_estimates.csv"), row.names = FALSE)

est$outcome <- factor(est$outcome, levels = unique(est$outcome))
fig3 <- event_plot(est, "Difference in effect (percentage points)", diff_palette, event_caption(Data)) +
        facet_wrap(~outcome, ncol = 2)
ggsave(file.path(figure_dir, "fig3.pdf"), fig3, width = 11, height = 8)
