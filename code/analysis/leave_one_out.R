# Title: Leave one state out (appendix section C, robustness; number in `exhibits` in 00_setup.R)
# Author: Shadi Seyedi
# Description: The Table 2 model (static, specification E; fit_static_models() in 00_setup.R)
#              re-estimated once for each state in the main sample, with that state left out, for
#              the four outcomes and every effect and difference of Table 2 (effect_contrasts in
#              00_setup.R). The figure shows the estimate without each state, with its 95% CI
#              clustered by state, against the estimate and CI on the whole main sample; states
#              are ordered by estimate within each panel, and the three that move the estimate
#              most are labelled. Replaces the earlier "without Florida" and "without Texas"
#              checks in robustness.R. Runs on the main sample only, so it takes no sample name.
# Output: output/figures/leave_one_out.pdf, output/tables/leave_one_out.csv

source("code/00_setup.R")
if (sample_name != "main") stop("leave_one_out.R runs on the main sample only; run it without a sample name.")

library(fixest)   # attached: i() in the covariates is looked up by name
library(ggplot2)

# The main sample before prep_event_data(): each state's sample is cut from it and prepared as
# make_sample() and check_estimates() would prepare a sample without that state (droplevels(),
# then prep_event_data()), so each state's estimates equal those of a sample that drops that
# state by a sample rule
Main <- make_sample(readRDS(data_file), "main")
states <- sort(unique(Main$ST))

# Postal code and expansion status of each state (data/expansion.csv numbers ST without padding)
expan <- read.csv("data/expansion.csv")
state_abb <- setNames(state.abb[match(expan$StateN, state.name)], sprintf("%02d", expan$ST))
expanded <- tapply(Main$expansion, Main$ST, max)
stopifnot(!anyNA(state_abb[states]), !anyNA(expanded[states]))

# The four outcomes in one fixest_multi; returns every contrast of effect_contrasts for each outcome
y_all <- paste0("c(", paste(outcome_vars, collapse = ", "), ")")
loo_fit <- function(Data) {
        models <- fit_static_models(Data, y_all, nested = FALSE)
        info <- fixest::models(models)
        out <- do.call(rbind, lapply(seq_along(models), function(k) do.call(rbind, lapply(names(effect_contrasts), function(cn) {
                cbind(outcome = info$lhs[k], contrast = cn,
                      static_estimate(models[[k]], effect_contrasts[[cn]]$series, effect_contrasts[[cn]]$w),
                      n = nobs(models[[k]]))
        }))))
        rm(models)
        invisible(gc())
        out
}

t0 <- Sys.time()
main_est <- loo_fit(prep_event_data(Main))
invisible(gc())
loo <- do.call(rbind, lapply(states, function(s) {
        cat(sprintf("Leaving out %s (%s) ... ", s, state_abb[[s]]))
        Data <- prep_event_data(droplevels(Main[Main$ST != s, , drop = FALSE]))
        out <- cbind(dropped_state = s, state_abb = state_abb[[s]],
                     expansion = if (expanded[[s]] == 1) "Expanded by 2019" else "Not expanded by 2019", loo_fit(Data))
        rm(Data)
        invisible(gc())
        cat(sprintf("%.1f min elapsed\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
        out
}))
rownames(loo) <- NULL
write.csv(loo, file.path(table_dir, "leave_one_out.csv"), row.names = FALSE)


####################
## Figure
####################

show_contrasts <- c("US-born", "Naturalized citizen", "Non-citizen", "Naturalized citizen minus US-born")
d <- loo[loo$contrast %in% show_contrasts, ]
ref <- main_est[main_est$contrast %in% show_contrasts, ]
d$main <- ref$estimate[match(paste(d$outcome, d$contrast), paste(ref$outcome, ref$contrast))]
d$shift <- d$estimate - d$main
# Rank of each state by its estimate within each panel, and the three states that move it most
d <- do.call(rbind, lapply(split(d, list(d$outcome, d$contrast), drop = TRUE), function(p) {
        p$rank <- rank(p$estimate, ties.method = "first")
        p$label <- ifelse(rank(-abs(p$shift), ties.method = "first") <= 3, p$state_abb, NA)
        # Labels at least 3.5 ranks apart, so that labelled states next to each other do not overlap:
        # push up from the bottom, then down from the top of the panel, each label as near its point
        # as the gap allows
        lab <- which(!is.na(p$label))
        lab <- lab[order(p$rank[lab])]
        y <- p$rank[lab]
        for (j in seq_along(y)[-1]) y[j] <- max(y[j], y[j - 1] + 3.5)
        y[length(y)] <- min(y[length(y)], nrow(p))
        for (j in rev(seq_along(y))[-1]) y[j] <- min(y[j], y[j + 1] - 3.5)
        p$label_y <- p$rank
        p$label_y[lab] <- y
        p
}))
panel_factors <- function(x) {
        x$outcome <- factor(outcome_labels[x$outcome], levels = outcome_labels[outcome_vars])
        x$contrast <- factor(x$contrast, levels = show_contrasts,
                             labels = c("US-born", "Naturalized citizen", "Non-citizen", "Naturalized citizen\nminus US-born"))
        x
}
d <- panel_factors(d)
ref <- panel_factors(ref)

# The largest change for naturalized citizens' any insurance, for the caption
nat <- loo[loo$outcome == "HICOV" & loo$contrast == "Naturalized citizen", ]
nat_main <- main_est$estimate[main_est$outcome == "HICOV" & main_est$contrast == "Naturalized citizen"]
big <- nat[which.max(abs(nat$estimate - nat_main)), ]
caption <- paste(strwrap(sprintf(paste(
        "Each point is the effect of expansion in the model of Table 2 (static difference-in-differences, all covariates)",
        "with one of the %d states of the main sample left out, with its 95%% confidence interval clustered by state;",
        "states are ordered by estimate within each panel, and the three that change the estimate most are labelled.",
        "The vertical line and shaded band are the estimate and 95%% confidence interval on the whole main sample.",
        "The largest change in naturalized citizens' any coverage comes from leaving out %s: %.1f instead of %.1f",
        "percentage points."),
        length(states), state.name[match(big$state_abb, state.abb)], big$estimate, nat_main), 150), collapse = "\n")

fig <- ggplot(d, aes(y = rank)) +
        geom_rect(data = ref, aes(xmin = ci_low, xmax = ci_high, ymin = -Inf, ymax = Inf), inherit.aes = FALSE,
                  fill = "grey88") +
        geom_vline(xintercept = 0, lty = "dashed") +
        geom_linerange(aes(xmin = ci_low, xmax = ci_high, colour = expansion), linewidth = 0.25, alpha = 0.35) +
        geom_vline(data = ref, aes(xintercept = estimate), colour = "grey20", linewidth = 0.4) +
        geom_point(aes(x = estimate, colour = expansion), size = 1) +
        geom_label(aes(x = ci_high, y = label_y, label = label), hjust = -0.15, size = 2.3, na.rm = TRUE,
                   linewidth = 0, label.padding = unit(0.05, "lines"), fill = "white") +
        # Rows are outcomes and columns groups, with an x axis for each panel (facet_grid frees it
        # only by column, which squeezes the outcomes with small effects)
        facet_wrap(~ outcome + contrast, ncol = length(show_contrasts), scales = "free_x") +
        scale_x_continuous(expand = expansion(mult = c(0.05, 0.12))) +
        scale_colour_manual(values = c("Expanded by 2019" = group_palette[3], "Not expanded by 2019" = group_palette[2]),
                            name = "Left-out state") +
        labs(x = "Effect of expansion (percentage points)", y = NULL, caption = caption) +
        theme(panel.background = element_rect(fill = "white", colour = "grey50"),
              axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.grid = element_blank(),
              legend.position = "bottom", strip.background = element_blank(),
              strip.text = element_text(hjust = 0, face = "bold"), plot.caption = element_text(hjust = 0))
ggsave(file.path(figure_dir, "leave_one_out.pdf"), fig, width = 11, height = 11)
cat(sprintf("Finished in %.1f min.\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
