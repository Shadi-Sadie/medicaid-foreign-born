# Title: Sensitivity to departures from parallel trends (appendix section B, identification;
#        number in `exhibits` in 00_setup.R)
# Author: Shadi Seyedi
# Description: HonestDiD (Rambachan and Roth 2023) confidence intervals for the average effect in
#              the years 0 to 5 after expansion, in the event-study model of Figures 1-3
#              (specification E), when the trends of expansion and comparison states may differ
#              after expansion by an amount tied to their difference before it. Three targets:
#              naturalized citizens' any insurance, whose leads are large though jointly not
#              significant (Table B2); the difference between naturalized citizens and the
#              US-born in any insurance; and naturalized citizens' direct purchase, whose leads
#              are jointly significant. Four restrictions: A, relative magnitudes (a post-expansion
#              change in the trend difference of at most Mbar times the largest change before
#              expansion); B, as A with the departure after expansion of the same sign as the
#              trend before it (a rising pre-trend that continued would overstate the effects);
#              C, as A with the leads -3 and -2 only (the -4 lead comes only from the states that
#              expanded in 2015 or later); D, smoothness (the trend difference bends by at most M
#              a year). The model inputs are honest_inputs() in 00_setup.R.
# Output: output/figures/honest_did.pdf, output/tables/honest_did.csv

source("code/00_setup.R")
Data <- prep_event_data(make_sample(readRDS(data_file), sample_name))

library(fixest)   # attached: i() in the covariates is looked up by name
library(ggplot2)

targets <- list(
        "Naturalized citizen: any insurance"               = list(y = "HICOV", series = "Naturalized citizen", w = 1),
        "Naturalized citizen minus US-born: any insurance" = list(y = "HICOV", series = c("Naturalized citizen", "US-born"), w = c(1, -1)),
        "Naturalized citizen: direct purchase"             = list(y = "HINS2", series = "Naturalized citizen", w = 1)
)
pre <- -4:-2
post <- 0:5
l_vec <- rep(1 / length(post), length(post))   # equal-weight average of the post-expansion effects
Mbarvec <- seq(0, 2, by = 0.25)

# One fixest_multi: any insurance and direct purchase, specification E
models <- fit_event_models(Data, "c(HICOV, HINS2)", nested = FALSE)
lhs <- fixest::models(models)$lhs
rm(Data)
gc()

restrictions <- c("Relative magnitudes", "Relative magnitudes, sign of pre-trend", "Relative magnitudes, leads -3 to -2",
                  "Smoothness")

res <- do.call(rbind, lapply(names(targets), function(tn) {
        tg <- targets[[tn]]
        m <- models[[match(tg$y, lhs)]]
        inputs <- function(pre) {
                h <- honest_inputs(m, tg$series, tg$w, pre, post)
                list(betahat = unname(h$betahat), sigma = unname(h$sigma), numPrePeriods = length(pre),
                     numPostPeriods = length(post), l_vec = l_vec)
        }
        args <- inputs(pre)
        b <- args$betahat
        S <- args$sigma
        orig <- do.call(HonestDiD::constructOriginalCS, args)
        # The original CI is the average post effect +- 1.96 SE
        post_i <- length(pre) + seq_along(post)
        est <- sum(l_vec * b[post_i])
        se <- sqrt(drop(t(l_vec) %*% S[post_i, post_i] %*% l_vec))
        stopifnot(isTRUE(all.equal(c(orig$lb, orig$ub), est + c(-1, 1) * qnorm(0.975) * se, tolerance = 1e-6)))
        # Sign of the pre-expansion trend, from year -4 to year -1 (= 0). A rising trend difference
        # that continued after expansion would add to the estimates (positive bias, delta >= 0)
        bias_direction <- if (0 - b[1] > 0) "positive" else "negative"
        # Smoothness grid: up to the largest bend of the path before expansion (year -1 is 0)
        M_max <- max(abs(diff(diff(c(b[seq_along(pre)], 0)))))
        # The relative-magnitudes CI is found on a grid, by default +-20 SE, which the widest CIs
        # reach; a wider grid, and a check that no CI is cut off at its edge
        grid <- 50 * se
        rm <- function(args, ...) do.call(HonestDiD::createSensitivityResults_relativeMagnitudes,
                                          c(args, list(Mbarvec = Mbarvec, grid.lb = -grid, grid.ub = grid, gridPoints = 2000, ...)))
        out <- list(rm(args), rm(args, biasDirection = bias_direction), rm(inputs(-3:-2)),
                    do.call(HonestDiD::createSensitivityResults, c(args, list(Mvec = seq(0, M_max, length.out = 7)))))
        do.call(rbind, lapply(seq_along(out), function(k) {
                r <- out[[k]]
                # An empty CI comes back as Inf/-Inf
                stopifnot(is.finite(c(r$lb, r$ub)), r$lb > -grid * 0.999, r$ub < grid * 0.999)
                data.frame(target = tn, restriction = restrictions[k], M = if (k < 4) r$Mbar else r$M, lb = r$lb, ub = r$ub)
        })) |>
                transform(estimate = est, original_lb = orig$lb, original_ub = orig$ub, bias_direction = bias_direction)
}))
rm(models)

# Breakdown value: the smallest M on the grid whose robust CI includes 0, printed as on the x-axis
# (0.00 if the CI includes 0 even at M = 0)
breakdown <- function(d) {
        hit <- d$M[d$lb <= 0 & d$ub >= 0]
        if (length(hit) == 0) sprintf("> %.2f", max(d$M)) else sprintf("%.2f", min(hit))
}
key <- paste(res$target, res$restriction)
res$breakdown <- vapply(split(res, key), breakdown, "")[key]
stopifnot(nrow(res) == length(targets) * (3 * length(Mbarvec) + 7))
write.csv(res, file.path(table_dir, "honest_did.csv"), row.names = FALSE)

## Figure: the original CI, then the robust CIs as M grows; one row per target
orig <- unique(res[c("target", "restriction", "estimate", "original_lb", "original_ub")])
plot_d <- rbind(data.frame(target = orig$target, restriction = orig$restriction, x = "Original",
                           lb = orig$original_lb, ub = orig$original_ub, type = "Original"),
                data.frame(target = res$target, restriction = res$restriction, x = sprintf("%.2f", res$M),
                           lb = res$lb, ub = res$ub, type = "Robust"))
# Each panel has its own grid (the smoothness grid depends on the target), so the x positions are
# ordered within each panel
plot_d$x <- factor(plot_d$x, levels = c("Original", unique(sprintf("%.2f", sort(res$M)))))
restriction_labels <- setNames(c("A. Relative magnitudes (Mbar)", "B. Relative magnitudes, sign of\npre-trend (Mbar)",
                                 "C. Relative magnitudes, leads\n-3 to -2 (Mbar)", "D. Smoothness (M, percentage\npoints)"),
                               restrictions)
plot_d$panel <- factor(paste0(sub(": ", ":\n", plot_d$target), "\n", restriction_labels[plot_d$restriction]),
                       levels = paste0(rep(sub(": ", ":\n", names(targets)), each = length(restrictions)), "\n", restriction_labels))

bd <- unique(res[c("target", "restriction", "breakdown")])
bd_text <- paste(vapply(restrictions, function(r) {
        sprintf("%s: %s", substr(restriction_labels[[r]], 1, 1),
                paste(sprintf("%s %s", sub(":", "", bd$target[bd$restriction == r]), bd$breakdown[bd$restriction == r]),
                      collapse = ", "))
}, ""), collapse = "; ")
directions <- unique(res$bias_direction)
caption <- paste(strwrap(paste(
        "95% confidence intervals for the average effect in the years 0 to 5 after expansion (percentage points), from the",
        "event-study model of Figures 1-3 (all covariates), with standard errors clustered by state. This average differs",
        "from the static estimate of Table 2, which uses all years before expansion as the baseline. Original: assumes",
        "parallel trends; the point is its estimate. A, relative magnitudes: each year-to-year change in the trend difference",
        "after year -1 is at most Mbar times the largest such change among years -4 to -1. B: as A, and the departure",
        "from parallel trends after expansion has the sign of the trend before it",
        if (length(directions) == 1) sprintf("(%s for all three: the coefficients rise from year -4 to year -1).",
                                             directions) else "(by target).",
        "C: as A, with the changes among years -3 to -1 only. The coefficient for 4 years before expansion comes only",
        "from states that expanded in 2015 or later. D, smoothness: the trend difference changes slope by at most M a year;",
        "the grid runs to the largest change in slope of that target before expansion. Smallest grid value at which the",
        paste0("interval includes 0 (0.00: it includes 0 even at 0): ", bd_text, ".")), 160), collapse = "\n")

fig <- ggplot(plot_d, aes(x = x, ymin = lb, ymax = ub, colour = type)) +
        geom_hline(yintercept = 0) +
        geom_errorbar(width = 0.3) +
        geom_point(data = plot_d[plot_d$type == "Original", ], aes(y = (lb + ub) / 2), size = 1.5) +
        facet_wrap(~panel, ncol = length(restrictions), scales = "free") +
        # "Original" and every other grid value from the second one on, so that four panels fit across the page
        scale_x_discrete(breaks = function(l) l[seq_along(l) == 1 | (seq_along(l) >= 4 & seq_along(l) %% 2 == 0)]) +
        scale_colour_manual(values = c(Original = "grey50", Robust = group_palette[2]), guide = "none") +
        labs(x = NULL, y = "Average effect, years 0-5 (percentage points)", caption = caption) +
        theme(panel.background = element_rect(fill = "white", colour = "grey50"),
              strip.background = element_blank(), strip.text = element_text(hjust = 0, face = "bold"),
              plot.caption = element_text(hjust = 0))
ggsave(file.path(figure_dir, "honest_did.pdf"), fig, width = 13, height = 9.5)
