# Title: Coverage by year, citizenship and state expansion status (appendix)
# Author: Shadi Seyedi
# Description: Weighted (PWGTP) percentage with each type of coverage by calendar year, in states
#              that expanded Medicaid in 2014 and states that never expanded, for US-born adults,
#              naturalized citizens and non-citizens. Raw means, no model: shows whether the
#              event-study effects come from changes in expansion states or in the comparison
#              states. States that expanded in 2015-2019 are left out, so that 2014 is the first
#              expansion year for every expansion state in the figure.
# Output: output/figures/trends_raw.pdf, output/tables/trends_raw_means.csv

source("code/00_setup.R")
Data <- prep_event_data(make_sample(readRDS(data_file), sample_name))

library(ggplot2)

Data <- Data[Data$expansion == 0 | Data$ExpansionY %in% 2014, ]
Data$state_group <- factor(Data$expansion, levels = c(1, 0),
                           labels = c("Expanded in 2014", "Did not expand"))

means <- do.call(rbind, lapply(outcome_vars, function(y) {
        m <- aggregate(cbind(wy = Data[[y]] * Data$PWGTP, w = Data$PWGTP, n = 1) ~ YEAR + grp + state_group,
                       data = Data, FUN = sum)
        data.frame(outcome = outcome_labels[[y]], m[c("YEAR", "grp", "state_group")], percent = m$wy / m$w, n = m$n)
}))
write.csv(means, file.path(table_dir, "trends_raw_means.csv"), row.names = FALSE)

n_states <- tapply(Data$ST, Data$state_group, function(s) length(unique(s)))
caption <- sprintf(paste("Weighted percentages (ACS person weights). %d states expanded in 2014 and %d never expanded;",
                         "states that expanded in 2015-2019 are not shown. The dashed line marks 2014."),
                   n_states[["Expanded in 2014"]], n_states[["Did not expand"]])

means$outcome <- factor(means$outcome, levels = outcome_labels[outcome_vars])
trends_raw <- ggplot(means, aes(x = YEAR, y = percent, colour = state_group, shape = state_group)) +
        geom_vline(xintercept = 2013.5, lty = "dashed") +
        geom_line() +
        geom_point(size = 1.8) +
        facet_grid(outcome ~ grp, scales = "free_y") +
        scale_x_continuous(breaks = 2011:2019) +
        scale_colour_manual(values = c("#232424", "#CC6677"), name = NULL) +
        scale_shape_discrete(name = NULL) +
        labs(x = "Year", y = "Percentage with coverage", caption = caption) +
        theme(panel.background = element_rect(fill = "white", colour = "grey50"),
              legend.position = "bottom", strip.background = element_blank(),
              strip.text = element_text(face = "bold"), plot.caption = element_text(hjust = 0),
              axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(figure_dir, "trends_raw.pdf"), trends_raw, width = 11, height = 10)
