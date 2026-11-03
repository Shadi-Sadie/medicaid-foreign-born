# Title: Did expansion change the control variables? (appendix)
# Author: Shadi Seyedi
# Description: Event studies with control variables as the outcome. A control that expansion
#              changes is a "bad control": adjusting for it removes part of the effect or adds bias,
#              so it should not be in the preferred model. Tested:
#              - person level, by group, same model as Figures 1-3 without covariates: disability,
#                income at or below 100% of poverty, employed, not in the labor force;
#              - sample selection: income at or below 138% of poverty among adults with income up
#                to 400% (the pov_400 sample), i.e. whether expansion changed who is in the sample;
#              - state level, on the state-year panel (unweighted, state and year fixed effects):
#                unemployment rate, ADA score (0-100) and Immigrant Policy Climate.
#              Model and helpers in code/00_setup.R ("Event studies by citizenship").
# Output: output/figures/control_outcomes.pdf, output/tables/control_outcomes_estimates.csv

source("code/00_setup.R")
raw <- readRDS(data_file)
Data <- prep_event_data(make_sample(raw, sample_name))
Wide <- prep_event_data(make_sample(raw, "pov_400"))
rm(raw)

library(ggplot2)
library(fixest)

# Person-level controls, in percentage points
Data$pov_100     <- 100 * (Data$POVPIP <= 100)
Data$employed    <- 100 * (Data$ESRG == "Employed")
Data$not_in_lf   <- 100 * (Data$ESRG == "Not in labor force")
Data$disability  <- 100 * Data$DIS
Wide$pov_138     <- 100 * (Wide$POVPIP <= 138)

person_outcomes <- c(disability = "Disability", pov_100 = "Income at or below 100% of poverty",
                     employed = "Employed", not_in_lf = "Not in labor force")

fit_group <- function(d, y) {
        feols(as.formula(paste(y, "~ i(et, grp, ref = -1) | ST^grp + YEAR^grp")),
              data = d, weights = ~PWGTP, vcov = ~ST)
}
group_rows <- function(m, label) {
        do.call(rbind, lapply(cit_groups, function(g) cbind(outcome = label, series = g, event_estimates(m, g))))
}

est <- do.call(rbind, c(
        lapply(names(person_outcomes), function(y) group_rows(fit_group(Data, y), person_outcomes[[y]])),
        list(group_rows(fit_group(Wide, "pov_138"), "Income at or below 138% (sample selection)"))
))

# State-level controls on the state-year panel of the sample's states
states <- unique(Data[, c("ST", "YEAR", "expansion", "ttot", "et", "UnempR", "ADA", "IPC")])
states$ADA <- 100 * states$ADA
state_outcomes <- c(UnempR = "State unemployment rate (%)", ADA = "ADA score (0-100)",
                    IPC = "Immigrant Policy Climate (index)")
state_est <- do.call(rbind, lapply(names(state_outcomes), function(y) {
        m <- feols(as.formula(paste(y, "~ i(et, ref = -1) | ST + YEAR")), data = states, vcov = ~ST)
        do.call(rbind, lapply(event_years, function(k) {
                if (k == -1) return(data.frame(event_time = k, estimate = 0, se = NA))
                term <- sprintf("et::%d", k)
                data.frame(event_time = k, estimate = coef(m)[[term]], se = se(m)[[term]])
        })) |> transform(ci_low = estimate - qnorm(0.975) * se, ci_high = estimate + qnorm(0.975) * se) |>
                cbind(outcome = state_outcomes[[y]], series = "State level")
}))

est <- rbind(est, state_est[names(est)])
write.csv(est, file.path(table_dir, "control_outcomes_estimates.csv"), row.names = FALSE)

est$outcome <- factor(est$outcome, levels = unique(est$outcome))
control_outcomes <- event_plot(est, "Change in the control variable (percentage points unless stated)",
                    c(group_palette, "#0072B2"), event_caption(Data)) +
        facet_wrap(~outcome, ncol = 3, scales = "free_y")
ggsave(file.path(figure_dir, "control_outcomes.pdf"), control_outcomes, width = 11, height = 9)
