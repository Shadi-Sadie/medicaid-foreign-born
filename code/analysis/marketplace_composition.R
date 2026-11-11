# Title: Why did direct purchase rise more among naturalized citizens in non-expansion states?
# Author: Shadi Seyedi
# Description: In the states that never expanded Medicaid, directly purchased coverage (which
#              includes the Marketplace) rose far more from 2011-2013 to 2014-2019 among
#              naturalized citizens than among US-born adults (trends_raw.R). That rise is what
#              makes the naturalized citizens' estimated effect on any coverage small. This script
#              asks whether the difference comes from who the naturalized citizens are and where
#              they live, or from something specific to naturalized citizens.
#              1. Changes from 2011-2013 to 2014-2019 in each coverage type, by group, in
#                 non-expansion states overall and by income band, by state (Florida, Texas,
#                 others), and for naturalized citizens by region of birth; and in 2014 expansion
#                 states for reference. Weighted means, no model.
#              2. DiNardo-Fortin-Lemieux reweighting, within non-expansion states and separately
#                 for each year: US-born adults are reweighted to have the characteristics of
#                 naturalized citizens, using the odds from a logit of being a naturalized citizen
#                 (A) on personal characteristics (age, sex, race and ethnicity, marital status,
#                 education, disability, income band, employment status) and (B) on these and the
#                 state. If the reweighted US-born adults' change matches the naturalized
#                 citizens', the difference is composition (A) or composition and place (B).
#                 Confidence intervals from 199 bootstrap samples of the 17 states.
#              The figure is Figure D1 (`exhibits` in code/00_setup.R). Report the gaps after
#              reweighting with their CIs; the "share of gap explained" rows divide by an
#              uncertain gap, so their bootstrap CIs are too wide to use.
# Output: output/tables/marketplace_composition_changes.csv (part 1),
#         output/tables/marketplace_reweighting.csv (part 2, changes, gaps and share explained),
#         output/tables/marketplace_reweighting_yearly.csv, output/tables/marketplace_balance.csv,
#         output/figures/marketplace_reweighting.pdf

source("code/00_setup.R")
Data <- prep_event_data(make_sample(readRDS(data_file), sample_name))

library(ggplot2)

pre_years  <- 2011:2013
post_years <- 2014:2019
covars <- c("AGEP", "SEX", "RACE1", "MARG", "SCHLG", "DIS", "POVPIPG", "ESRG")
keep <- c("YEAR", "ST", "expansion", "ExpansionY", "grp", "CORIGIN", "PWGTP", outcome_vars, covars)
Data <- Data[Data$expansion == 0 | Data$ExpansionY %in% 2014, keep]
gc()

wmean <- function(y, w) sum(y * w) / sum(w)


####################
## 1. Changes in coverage by subgroup (weighted means)
####################

Data$period <- ifelse(Data$YEAR %in% pre_years, "pre", "post")
Data$state_cat <- ifelse(Data$expansion == 1, "Expanded in 2014",
                         ifelse(Data$ST == "12", "Did not expand: Florida",
                                ifelse(Data$ST == "48", "Did not expand: Texas", "Did not expand: other states")))
Data$income <- ifelse(Data$POVPIPG == levels(Data$POVPIPG)[1], "Income 0-100%", "Income 101-138%")

# Mean of each outcome in each period for the rows in `rows`; each period's mean is the average of
# its yearly weighted means, so that years count equally whatever their sample size
changes <- function(rows, label) {
        d <- Data[rows, ]
        do.call(rbind, lapply(split(d, d$grp, drop = TRUE), function(g) {
                do.call(rbind, lapply(outcome_vars, function(y) {
                        yearly <- sapply(split(g, g$YEAR), function(x) wmean(x[[y]], x$PWGTP))
                        pre <- mean(yearly[as.character(pre_years)])
                        post <- mean(yearly[as.character(post_years)])
                        data.frame(subgroup = label, grp = g$grp[1], outcome = outcome_labels[[y]],
                                   pre = pre, post = post, change = post - pre, n = nrow(g))
                }))
        }))
}

non_exp <- Data$expansion == 0
caribbean <- Data$CORIGIN == "Caribbean"
part1 <- rbind(
        changes(non_exp, "Did not expand: all"),
        changes(non_exp & Data$income == "Income 0-100%", "Did not expand: income 0-100%"),
        changes(non_exp & Data$income == "Income 101-138%", "Did not expand: income 101-138%"),
        changes(Data$state_cat == "Did not expand: Florida", "Did not expand: Florida"),
        changes(Data$state_cat == "Did not expand: Texas", "Did not expand: Texas"),
        changes(Data$state_cat == "Did not expand: other states", "Did not expand: other states"),
        changes(non_exp & Data$grp == "Naturalized citizen" & caribbean, "Did not expand: born in the Caribbean"),
        changes(non_exp & Data$grp == "Naturalized citizen" & !caribbean, "Did not expand: born elsewhere abroad"),
        changes(Data$expansion == 1, "Expanded in 2014: all")
)
write.csv(part1, file.path(table_dir, "marketplace_composition_changes.csv"), row.names = FALSE)

# Where the naturalized citizens of non-expansion states live
d <- Data[non_exp & Data$grp != "Non-citizen", ]
cat("\nShare of each group's weighted population in non-expansion states, 2011-2013:\n")
print(round(100 * prop.table(xtabs(PWGTP ~ state_cat + grp, d[d$period == "pre", ], drop.unused.levels = TRUE), 2), 1))


####################
## 2. Reweighting US-born adults to look like naturalized citizens (non-expansion states)
####################

d <- d[complete.cases(d[covars]), ]
d$nat <- as.integer(d$grp == "Naturalized citizen")
d$ST <- factor(d$ST)
cat("\nReweighting sample:", format(nrow(d), big.mark = ","), "people,",
    format(sum(d$nat), big.mark = ","), "naturalized citizens\n")

# Design matrices, built once: (A) personal characteristics, (B) these plus state dummies
X_A <- model.matrix(~ AGEP + I(AGEP^2) + SEX + RACE1 + MARG + SCHLG + DIS + POVPIPG + ESRG, d)
X_B <- cbind(X_A, model.matrix(~ ST, d)[, -1, drop = FALSE])
stopifnot(nrow(X_A) == nrow(d))
Y <- as.matrix(d[outcome_vars])
cells <- split(seq_len(nrow(d)), list(as.character(d$ST), d$YEAR), drop = TRUE)   # "12.2011", ...
years <- sort(unique(d$YEAR))

# DFL weights for the US-born rows in `r` (one year): person weight times the odds of being a
# naturalized citizen. The logit is weighted by the person weights; probabilities are capped at
# 0.99 so that no person gets more than 99 times the odds weight.
dfl_weights <- function(X, r) {
        w <- d$PWGTP[r]
        fit <- suppressWarnings(glm.fit(X[r, , drop = FALSE], d$nat[r], weights = w / mean(w), family = quasibinomial()))
        p <- pmin(fit$fitted.values, 0.99)
        w * p / (1 - p)
}

# Yearly means of every outcome for naturalized citizens, US-born adults, and US-born adults
# reweighted with A and with B, for a draw of states (a vector of state codes, repeats allowed)
yearly_means <- function(states, diagnostics = FALSE) {
        out <- lapply(years, function(yr) {
                r <- unlist(cells[paste(states, yr, sep = ".")], use.names = FALSE)
                us <- r[d$nat[r] == 0]
                nat <- r[d$nat[r] == 1]
                w_A <- dfl_weights(X_A, r)[d$nat[r] == 0]
                w_B <- dfl_weights(X_B, r)[d$nat[r] == 0]
                m <- rbind("Naturalized citizen" = colSums(Y[nat, ] * d$PWGTP[nat]) / sum(d$PWGTP[nat]),
                           "US-born" = colSums(Y[us, ] * d$PWGTP[us]) / sum(d$PWGTP[us]),
                           "US-born, reweighted on characteristics (A)" = colSums(Y[us, ] * w_A) / sum(w_A),
                           "US-born, reweighted on characteristics and state (B)" = colSums(Y[us, ] * w_B) / sum(w_B))
                res <- data.frame(YEAR = yr, series = rownames(m), m, check.names = FALSE, row.names = NULL)
                if (diagnostics) {
                        attr(res, "ess") <- c(A = sum(w_A)^2 / sum(w_A^2), B = sum(w_B)^2 / sum(w_B^2), n_us = length(us))
                        attr(res, "w_B") <- w_B
                        attr(res, "us") <- us
                }
                res
        })
        if (diagnostics) return(out)
        do.call(rbind, out)
}

# Changes from 2011-2013 to 2014-2019 (average of the yearly means in each period), the gap
# between naturalized citizens and each US-born series, and the share of the raw gap that the
# reweighting explains
summarise_changes <- function(ym) {
        do.call(rbind, lapply(outcome_vars, function(y) {
                ch <- sapply(split(ym, ym$series), function(s) mean(s[[y]][s$YEAR %in% post_years]) - mean(s[[y]][s$YEAR %in% pre_years]))
                nat <- ch[["Naturalized citizen"]]
                raw_gap <- nat - ch[["US-born"]]
                gap_A <- nat - ch[["US-born, reweighted on characteristics (A)"]]
                gap_B <- nat - ch[["US-born, reweighted on characteristics and state (B)"]]
                data.frame(outcome = y,
                           stat = c(paste("Change:", names(ch)), "Gap: naturalized minus US-born",
                                    "Gap after reweighting (A)", "Gap after reweighting (B)",
                                    "Share of gap explained (A)", "Share of gap explained (B)"),
                           value = c(unname(ch), raw_gap, gap_A, gap_B, 1 - gap_A / raw_gap, 1 - gap_B / raw_gap))
        }))
}

all_states <- levels(d$ST)
diag <- yearly_means(all_states, diagnostics = TRUE)
ym <- do.call(rbind, diag)
point <- summarise_changes(ym)

cat("\nEffective number of reweighted US-born adults per year (A, B) out of n:\n")
print(round(t(sapply(diag, attr, "ess"))))

# Balance: weighted means of selected characteristics (2011-2019 pooled) for naturalized
# citizens, US-born adults, and US-born adults reweighted with B
bal_vars <- list("Hispanic" = function(x) x$RACE1 == "Hispanic", "Lives in Florida" = function(x) x$ST == "12",
                 "Lives in Texas" = function(x) x$ST == "48", "Employed" = function(x) x$ESRG == "Employed",
                 "Income 101-138%" = function(x) x$POVPIPG != levels(x$POVPIPG)[1], "Disability" = function(x) x$DIS == 1,
                 "Married" = function(x) x$MARG == 1)
us_rows <- unlist(lapply(diag, attr, "us"))
w_B_all <- unlist(lapply(diag, attr, "w_B"))
nat_rows <- which(d$nat == 1)
balance <- do.call(rbind, lapply(names(bal_vars), function(v) {
        f <- bal_vars[[v]]
        data.frame(characteristic = v,
                   naturalized = 100 * wmean(f(d[nat_rows, ]), d$PWGTP[nat_rows]),
                   us_born = 100 * wmean(f(d[us_rows, ]), d$PWGTP[us_rows]),
                   us_born_reweighted_B = 100 * wmean(f(d[us_rows, ]), w_B_all))
}))
write.csv(balance, file.path(table_dir, "marketplace_balance.csv"), row.names = FALSE)
cat("\nBalance (%):\n")
print(balance, digits = 3)

## State-cluster bootstrap: resample the 17 states with replacement
# L'Ecuyer-CMRG gives each parallel worker its own reproducible random stream (mclapply's default
# mc.set.seed = TRUE); with the default generator the draws would change from run to run
RNGkind("L'Ecuyer-CMRG")
set.seed(20141)
B <- 199
boot <- parallel::mclapply(seq_len(B), function(b) {
        summarise_changes(yearly_means(sample(all_states, replace = TRUE)))$value
}, mc.cores = 8)
ok <- vapply(boot, is.numeric, logical(1))
if (!all(ok)) warning(sum(!ok), " bootstrap replications failed and are dropped")
boot <- do.call(cbind, boot[ok])
point$ci_low <- apply(boot, 1, quantile, 0.025, na.rm = TRUE)
point$ci_high <- apply(boot, 1, quantile, 0.975, na.rm = TRUE)
point$outcome <- outcome_labels[point$outcome]
point$bootstrap_reps <- ncol(boot)
write.csv(point, file.path(table_dir, "marketplace_reweighting.csv"), row.names = FALSE)

ym_long <- do.call(rbind, lapply(outcome_vars, function(y) data.frame(outcome = outcome_labels[[y]], ym[c("YEAR", "series")], percent = ym[[y]])))
write.csv(ym_long, file.path(table_dir, "marketplace_reweighting_yearly.csv"), row.names = FALSE)

cat("\nResults:\n")
print(transform(point, value = round(value, 2), ci_low = round(ci_low, 2), ci_high = round(ci_high, 2)), row.names = FALSE)

## Figure: yearly coverage in non-expansion states
ym_long$outcome <- factor(ym_long$outcome, levels = outcome_labels[outcome_vars])
ym_long$series <- factor(ym_long$series, levels = unique(ym$series))
# The gaps for the caption, from `point` (so the note always matches this run)
gap <- function(o, s) {
        r <- point[point$outcome == outcome_labels[[o]] & point$stat == s, ]
        sprintf("%.1f (%.1f, %.1f)", r$value, r$ci_low, r$ci_high)
}
gap_line <- function(o) {
        sprintf("%s %s before reweighting, %s after (A), %s after (B)", tolower(outcome_labels[[o]]),
                gap(o, "Gap: naturalized minus US-born"), gap(o, "Gap after reweighting (A)"), gap(o, "Gap after reweighting (B)"))
}
gap_note <- paste0("Naturalized citizens' change from 2011-2013 to 2014-2019 minus US-born adults' change, percentage points ",
                   "(95% CI from 199 bootstrap samples of the states):\n", gap_line("HICOV"), ";\n", gap_line("HINS2"), ".")
fig <-ggplot(ym_long, aes(x = YEAR, y = percent, colour = series, shape = series, linetype = series)) +
        geom_vline(xintercept = 2013.5, lty = "dashed", colour = "grey50") +
        geom_line() +
        geom_point(size = 1.8) +
        facet_wrap(~outcome, scales = "free_y", ncol = 2) +
        scale_x_continuous(breaks = years) +
        scale_colour_manual(values = c("#CC6677", "#232424", "#56B4E9", "#009E73"), name = NULL) +
        scale_shape_discrete(name = NULL) +
        scale_linetype_manual(values = c("solid", "solid", "dashed", "dashed"), name = NULL) +
        guides(colour = guide_legend(ncol = 2)) +
        labs(x = "Year", y = "Percentage with coverage",
             caption = paste("States that never expanded Medicaid. Weighted percentages (ACS person weights). US-born adults are",
                             "reweighted each year\n(DiNardo, Fortin and Lemieux 1996) to have the characteristics of naturalized",
                             "citizens: (A) age, sex, race and ethnicity, marital status,\neducation, disability, income band and",
                             paste0("employment status; (B) these and the state.\n", gap_note))) +
        theme(panel.background = element_rect(fill = "white", colour = "grey50"),
              legend.position = "bottom", strip.background = element_blank(),
              strip.text = element_text(hjust = 0, face = "bold"), plot.caption = element_text(hjust = 0),
              axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(figure_dir, "marketplace_reweighting.pdf"), fig, width = 10, height = 8.2)
