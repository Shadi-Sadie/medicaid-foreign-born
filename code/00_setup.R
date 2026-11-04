# Title: Shared setup for the analysis scripts
# Author: Shadi Seyedi
# Description: Every script in code/analysis/ sources this file first. It checks that the
#              required packages are installed, sets the input and output paths, defines the
#              analysis samples (make_sample()), and holds the models, estimates and table
#              formatting that several scripts share.
#              Run all scripts from the project root folder (see README.md).


####################
## Required Packages
####################

required_packages <- c("fixest", "ggplot2", "dplyr", "scales", "kableExtra", "flextable", "officer", "HonestDiD")

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
        stop("Missing R packages: ", paste(missing_packages, collapse = ", "),
             "\nSee README.md, section 'Computational requirements'.")
}


####################
## Paths
####################

if (!file.exists("run_all.R")) stop("Run the scripts from the project root folder.")

data_file <- "data/clean/acs_clean.rds"   # from code/data_prep/03_clean.R (all people, no sample rules)
if (!file.exists(data_file)) stop(data_file, " is missing. Run the scripts in code/data_prep/ first.")

# The sample a script runs on: "main" by default, or a sensitivity sample named on the command
# line, e.g.  Rscript code/analysis/table2.R pov_100  (see sample_variants below).
# Results for a sensitivity sample go to their own folders, so they never overwrite the main ones.
sample_name <- if (length(commandArgs(trailingOnly = TRUE)) > 0) commandArgs(trailingOnly = TRUE)[1] else "main"
out_suffix  <- if (sample_name == "main") "" else paste0("_", sample_name)
table_dir   <- paste0("output/tables", out_suffix)
figure_dir  <- paste0("output/figures", out_suffix)

dir.create(table_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)


####################
## Sample definitions
####################

# Every sample rule is written here, once, with the reason for it. code/data_prep/ drops nobody;
# make_sample() applies these rules to the cleaned file. The main sample keeps a person only if
# every rule in main_rules is TRUE. A sensitivity sample removes some main rules (drop) and/or
# adds one (rule). Each "why" is written so it can go into the paper's appendix.

# States with Medicaid or similar coverage for low-income adults, including childless adults, up
# to at least 100% of poverty with open enrollment before 2014 (Delaware, Massachusetts, New York,
# Vermont), and DC, which expanded under the ACA early option in 2010. Miller, Johnson and Wherry
# (2021, QJE), Wherry and Miller (2016, Annals of Internal Medicine), Kaestner et al. (2017, JPAM).
prior_full_states <- c("10", "11", "25", "36", "50")                 # DE, DC, MA, NY, VT
# States that expanded partly before 2014 under the ACA early option or a waiver (capped,
# county-based or below 138% of poverty; "Early Expansion" in data/expansion.csv). The main sample
# excludes them, since part of their low-income adults were already covered in 2011-2013, the
# years before expansion. Sommers et al. (2013, Medicare & Medicaid Research Review); Colorado's
# 2012 waiver was capped at 10,000 people (Kaestner et al. 2017).
early_option_states <- c("06", "08", "09", "27", "34", "53")         # CA, CO, CT, MN, NJ, WA
# Simon, Soni and Cawley (2017, JPAM): every state with any coverage of childless adults before 2014
simon_states <- c("04", "06", "09", "10", "11", "15", "19", "23", "25", "27", "33", "36", "50",
                  "53", "55")         # AZ, CA, CT, DE, DC, HI, IA, ME, MA, MN, NH, NY, VT, WA, WI

main_rules <- list(
        age = list(rule = quote(AGEP >= 26 & AGEP <= 64),
                   why = "Adults aged 26-64: young adults under 26 could stay on a parent's plan under the ACA, and adults 65 and older have Medicare."),
        income = list(rule = quote(POVPIP <= 138),
                      why = "Family income at or below 138% of the federal poverty level, the ACA Medicaid expansion eligibility limit."),
        medicare = list(rule = quote(HINS3 == 0),
                        why = "Adults with Medicare are excluded: people entitled to Medicare are not eligible for the ACA expansion group. Under 65, they qualify through disability or end-stage renal disease."),
        group_quarters = list(rule = quote(in_gq == 0),
                              why = "People living in households; residents of group quarters (e.g. college dormitories) are excluded."),
        cit_birth_abroad = list(rule = quote(cit_birth_abroad == 0),
                                why = "US citizens born in Puerto Rico, the US island areas or abroad to American parents are excluded: they are citizens at birth but not born in the US."),
        residency = list(rule = quote(!(NonCit == 1 & recent_arrival == 1)),
                         why = "Non-citizens in the US fewer than 5 years are excluded: most lawfully present immigrants are barred from federally funded Medicaid for their first 5 years."),
        prior_full = list(rule = quote(!ST %in% prior_full_states),
                          why = "Delaware, the District of Columbia, Massachusetts, New York and Vermont are excluded: they covered low-income adults before 2014 (Miller, Johnson and Wherry 2021)."),
        early_option = list(rule = quote(!ST %in% early_option_states),
                            why = "California, Colorado, Connecticut, Minnesota, New Jersey and Washington are excluded: they expanded coverage partly before 2014 under the ACA early option or a waiver, so 2011-2013 is not a clean period before expansion.")
)

sample_variants <- list(
        all_states = list(drop = c("prior_full", "early_option"),
                          why = "Keeps every state. DC has no ADA score, so models that control for ADA drop it."),
        keep_early_option = list(drop = "early_option",
                                 why = "Keeps the states that expanded partly before 2014 (CA, CO, CT, MN, NJ, WA), counted as 2014 expanders."),
        keep_colorado = list(drop = "early_option", rule = quote(!ST %in% setdiff(early_option_states, "08")),
                             why = "Keeps Colorado, whose 2012 waiver was capped at 10,000 people, counted as a 2014 expander."),
        simon_states = list(drop = "prior_full", rule = quote(!ST %in% simon_states),
                            why = "Excludes every state with any coverage of childless adults before 2014 (Simon, Soni and Cawley 2017), as well as the early expanders."),
        drop_wisconsin = list(rule = quote(ST != "55"),
                              why = "Excludes Wisconsin, which covered childless adults up to 100% of poverty through a waiver without adopting the ACA expansion."),
        drop_midyear = list(rule = quote(!ST %in% c("02", "18", "22", "26", "33")),
                            why = "Excludes states whose expansion started after January (AK, IN, LA, MI, NH; data/expansion.csv), so the expansion year is only partly treated. Maine's 10 January 2019 start counts as January."),
        drop_late = list(rule = quote(Expan_STS != "Late Expansion"),
                         why = "Excludes states that expanded in 2015-2019, leaving 2014 expanders and states that never expanded."),
        drop_2019 = list(rule = quote(!ExpansionY %in% 2019),
                         why = "Excludes Maine and Virginia, which expanded in 2019 and so have a single treated year."),
        pov_100 = list(drop = "income", rule = quote(POVPIP <= 100),
                       why = "Income at or below 100% of poverty. In non-expansion states these adults could not get subsidized Marketplace coverage, which starts at 100%."),
        pov_101_138 = list(drop = "income", rule = quote(POVPIP > 100 & POVPIP <= 138),
                           why = "Income from 101% to 138% of poverty. In non-expansion states these adults could get subsidized Marketplace coverage instead of Medicaid."),
        drop_florida = list(rule = quote(ST != "12"),
                            why = "Excludes Florida, which has 39% of the naturalized citizens in non-expansion states and very high Marketplace enrollment."),
        drop_texas = list(rule = quote(ST != "48"),
                          why = "Excludes Texas, which has 32% of the naturalized citizens and 44% of the non-citizens in non-expansion states."),
        edu_hs = list(drop = "income", rule = quote(SCHLG %in% c("Less than high school", "High school")),
                      why = "Adults with a high school education or less, at any income in the data (up to 400% of poverty), instead of the income limit. Expansion can change income but not the education of adults aged 26-64, so this sample cannot be selected by expansion."),
        pov_400 = list(drop = "income",
                       why = "All incomes in the data (up to 400% of poverty). Used to test whether expansion changed who has income at or below 138%."),
        pov_139_400 = list(drop = "income", rule = quote(POVPIP > 138 & POVPIP <= 400),
                           why = "Higher-income comparison sample: income from 139% to 400% of poverty, above the expansion limit and within the Marketplace subsidy range in every state. Not a placebo, since the rest of the ACA affects it."),
        pov_200 = list(drop = "income", rule = quote(POVPIP <= 200),
                       why = "Income at or below 200% of poverty."),
        age_19_64 = list(drop = "age", rule = quote(AGEP >= 19 & AGEP <= 64),
                         why = "All non-elderly adults aged 19-64, including those who could stay on a parent's plan."),
        cit_abroad_as_native = list(drop = "cit_birth_abroad",
                                    why = "Keeps US citizens born in the territories or abroad to American parents, counted as US-born."),
        no_residency_rule = list(drop = "residency",
                                 why = "Keeps non-citizens who have been in the US fewer than 5 years."),
        residency_10y = list(rule = quote(!(NonCit == 1 & YLIU < 10)),
                             why = "Excludes non-citizens in the US fewer than 10 years."),
        no_undoc = list(rule = quote(UNDOC == 0),
                        why = "Excludes non-citizens flagged as likely undocumented, who are not eligible for federally funded Medicaid."),
        undoc_benefits = list(edit = function(d) {
                                      d$UNDOC <- d$UNDOC_BEN
                                      d$FBLEG <- as.integer(d$ForeginBorn == 1 & d$UNDOC == 0)
                                      d
                              },
                              why = "Uses the likely-undocumented flag that also counts receipt of Social Security, SSI, Medicare, TRICARE or VA coverage as a sign of legal status (Borjas and Cassidy 2019)."),
        with_medicare = list(drop = "medicare",
                             why = "Keeps adults under 65 with Medicare."),
        drop_imputed_ins = list(rule = quote(hins_imputed == 0),
                                why = "Excludes people whose Medicaid answer was imputed by the Census Bureau."),
        drop_medicaid_edit = list(rule = quote(medicaid_edit == 0),
                                  why = "Excludes people given Medicaid by the Census Bureau's coverage edit rather than by their own answer."),
        drop_ipc_imputed = list(rule = quote(ipc_imputed == 0),
                                why = "Excludes North Carolina 2016, whose Immigrant Policy Climate score is imputed.")
)

# Returns the rows of Data in the named sample, with unused factor levels dropped (e.g. the
# income groups above 138% in the main sample). Prints the sample name and size.
make_sample <- function(Data, name = "main") {
        valid <- c("main", names(sample_variants))
        if (!name %in% valid) stop("Unknown sample '", name, "'. Valid names: ", paste(valid, collapse = ", "))
        rules <- main_rules
        variant <- if (name == "main") list() else sample_variants[[name]]
        rules <- rules[setdiff(names(rules), variant$drop)]
        if (!is.null(variant$rule)) rules$variant <- list(rule = variant$rule)
        keep <- rep(TRUE, nrow(Data))
        for (r in rules) keep <- keep & eval(r$rule, Data, environment())
        out <- Data[!is.na(keep) & keep, , drop = FALSE]
        if (!is.null(variant$edit)) out <- variant$edit(out)
        out <- droplevels(out)
        cat("Sample:", name, "-", format(nrow(out), big.mark = ","), "people\n")
        out
}

## Check make_sample() on made-up people: the first row passes every main rule, each other row
## breaks exactly one (the script stops here if a rule is wrong)
local({
        d <- data.frame(AGEP = c(40, 25, 40, 40, 40, 40, 40, 40, 40), POVPIP = c(100, 100, 150, 100, 100, 100, 100, 100, 100),
                        HINS3 = c(0, 0, 0, 0, 0, 0, 0, 1, 0),
                        in_gq = c(0, 0, 0, 1, 0, 0, 0, 0, 0), cit_birth_abroad = c(0, 0, 0, 0, 1, 0, 0, 0, 0),
                        NonCit = c(1, 0, 0, 0, 0, 1, 0, 0, 0), recent_arrival = c(0, 0, 0, 0, 0, 1, 0, 0, 0),
                        ST = c("48", "48", "48", "48", "48", "48", "36", "48", "06"),
                        rule = c("passes", "age", "income", "group_quarters", "cit_birth_abroad", "residency", "prior_full",
                                 "medicare", "early_option"))
        run <- function(name) { capture.output(s <- make_sample(d, name)); s$rule }
        stopifnot(identical(run("main"), "passes"),
                  identical(run("with_medicare"), c("passes", "medicare")),
                  identical(run("pov_200"), c("passes", "income")),
                  identical(run("all_states"), c("passes", "prior_full", "early_option")),
                  identical(run("keep_early_option"), c("passes", "early_option")),
                  identical(run("no_residency_rule"), c("passes", "residency")),
                  identical(run("cit_abroad_as_native"), c("passes", "cit_birth_abroad")),
                  identical(run("pov_100"), "passes"),
                  inherits(try(make_sample(d, "typo"), silent = TRUE), "try-error"))
})


####################
## Outcomes
####################

# The four coverage outcomes of the figure scripts, in panel order. HICOV is any health insurance
# coverage (1 - UNINS), so its estimates are those for uninsured with the sign reversed. A person
# can report more than one source, so the source effects do not add up to the HICOV effect.
outcome_vars <- c("HICOV", "HINS4", "HINS1", "HINS2")
outcome_labels <- c("HICOV" = "Any Coverage", "HINS4" = "Medicaid", "HINS1" = "Employer-Sponsored", "HINS2" = "Directly Purchased")


####################
## Event studies by citizenship (Figures 1-3; specs_employer.R, specs_direct.R)
####################

# One event-study model per outcome in which US-born adults, naturalized citizens and non-citizens
# each have their own expansion effects, state and year fixed effects and covariate effects. The
# group effects are the same as from separate regressions by group; estimating them jointly gives
# the clustered covariance between groups that Figure 3's differences need.

cit_groups <- c("US-born", "Naturalized citizen", "Non-citizen")
event_years <- -4:5   # event times plotted

# Nested covariate specifications: each adds a block to the one before (A has none)
event_specs <- list(
        "B. Demographics"                      = c("AGEP", "SEX", "RACE1", "MARG", "SCHLG"),
        "C. + Disability, poverty, employment" = c("DIS", "POVPIPG", "ESRG"),
        "D. + Immigrant characteristics"       = c("ENG", "LTINU", "CORIGIN"),
        "E. + State controls"                  = c("UnempR", "ADA", "IPC")
)
event_spec_names <- c("A. No covariates", names(event_specs))

# Adds the variables the models need: the group, event time, and the outcomes in percentage points.
# Event time puts the 2-4 states observed 5 or more years before expansion (those that expanded in
# 2016 or 2019) into one coefficient (-5), which is not plotted. Never-expanding states are set to
# the omitted year (-1), so they only serve as controls.
prep_event_data <- function(Data) {
        Data$grp <- factor(ifelse(Data$ForeginBorn == 0, 1, ifelse(Data$NonCit == 0, 2, 3)), levels = 1:3,
                           labels = cit_groups)
        Data$et <- ifelse(Data$expansion == 1, pmax(Data$ttot, -5), -1)
        for (v in outcome_vars) Data[[v]] <- 100 * Data[[v]]
        # Group x level factor for each factor covariate (e.g. "Non-citizen_Mexico"), so that each
        # group gets its own reference level in fit_event_models()
        for (v in event_factors) Data[[paste0(v, "_g")]] <- interaction(Data$grp, Data[[v]], drop = TRUE, sep = "_")
        Data
}
event_factors <- c("SEX", "RACE1", "SCHLG", "POVPIPG", "ESRG", "ENG", "CORIGIN")

# Fits outcome y with every covariate interacted with group. nested = TRUE fits specifications A-E
# (a fixest_multi, one model per specification); FALSE fits E only.
# Each factor's reference level is its most common level within each group. With one reference for
# all groups, a group that never has it (no foreign-born person has "United States" as region of
# birth) keeps a full set of dummies, which sum to that group's fixed effects. fixest does not
# always detect this, and the near-collinear columns add noise to the standard errors.
fit_event_models <- function(Data, y, nested = TRUE) {
        fixest::feols(as.formula(paste(y, "~ i(et, grp, ref = -1) +", event_covariates(Data, nested),
                                       "| ST^grp + YEAR^grp")),
                      data = Data, weights = ~PWGTP, vcov = ~ST)
}

# The covariate part of the formula, every covariate interacted with group: specifications A-E
# nested (csw0) or E only. Shared by the event-study and the static models.
event_covariates <- function(Data, nested = TRUE) {
        term <- function(v) {
                if (!v %in% event_factors) return(paste0(v, ":grp"))
                g <- Data[[paste0(v, "_g")]]
                refs <- tapply(as.character(g), Data$grp, function(x) names(which.max(table(x))))
                sprintf("i(%s_g, ref = c(%s))", v, paste0('"', refs, '"', collapse = ", "))
        }
        blocks <- vapply(event_specs, function(b) paste(vapply(b, term, ""), collapse = " + "), "")
        if (nested) paste0("csw0(", paste(blocks, collapse = ", "), ")") else paste(blocks, collapse = " + ")
}

# The static difference-in-differences version of fit_event_models() (Table 2): one expansion
# effect per group (treat is 1 from the calendar year of expansion on), with the same covariates,
# fixed effects, weights and clustering. y can name several outcomes, e.g. "c(HICOV, HINS4)".
fit_static_models <- function(Data, y, nested = TRUE) {
        fixest::feols(as.formula(paste(y, "~ i(grp, treat) +", event_covariates(Data, nested),
                                       "| ST^grp + YEAR^grp")),
                      data = Data, weights = ~PWGTP, vcov = ~ST)
}

# Expansion effect for a weighted sum of the groups in `series` (w = 1 for one group, c(1, -1) for
# a difference) from a fit_static_models() model, with a 95% CI clustered by state
static_estimate <- function(m, series, w = 1) {
        terms <- sprintf("grp::%s:treat", series)
        V <- vcov(m)[terms, terms, drop = FALSE]
        est <- sum(w * coef(m)[terms])
        se <- sqrt(drop(t(w) %*% V %*% w))
        data.frame(estimate = est, se = se, ci_low = est - qnorm(0.975) * se, ci_high = est + qnorm(0.975) * se)
}

# The Sun and Abraham (2021) interaction-weighted version of these models (robustness.R,
# sunab_event.R), robust to effects that differ by expansion cohort. fixest::sunab() cannot give each group its own
# cohort effects, so add_sunab_cells() builds them: one dummy per group x expansion year x year
# since expansion (sa_cell, e.g. "Non-citizen_2015_2"). States that did not expand by 2019 are the
# controls: they and the year before expansion share the reference value. Covariates, fixed
# effects, weights and clustering are as in fit_event_models(), specification E.
add_sunab_cells <- function(Data) {
        cohort <- ifelse(Data$expansion == 1, Data$year_treated, NA)
        Data$rel <- Data$YEAR - cohort
        Data$sa_cell <- ifelse(is.na(cohort) | Data$rel == -1, "ref", paste(Data$grp, cohort, Data$rel, sep = "_"))
        Data
}
fit_sunab_models <- function(Data, y) {
        fixest::feols(as.formula(paste(y, '~ i(sa_cell, ref = "ref") +', event_covariates(Data, nested = FALSE),
                                       "| ST^grp + YEAR^grp")),
                      data = Data, weights = ~PWGTP, vcov = ~ST)
}

# The cells of a fit_sunab_models() model, with their group, year since expansion and weighted
# number of people in the model's sample
sunab_cells <- function(m, Data) {
        d <- Data[fixest::obs(m), c("sa_cell", "PWGTP")]
        n <- tapply(d$PWGTP[d$sa_cell != "ref"], d$sa_cell[d$sa_cell != "ref"], sum)
        parts <- strsplit(names(n), "_", fixed = TRUE)
        data.frame(term = paste0("sa_cell::", names(n)), grp = vapply(parts, `[`, "", 1),
                   rel = as.integer(vapply(parts, `[`, "", 3)), n = as.vector(n))
}

# Effect for a weighted sum of the groups in `series` (as in static_estimate()) in the years since
# expansion `rel`: each group's cells in those years averaged with weights proportional to their
# weighted number of people, as fixest::aggregate() does for sunab(). One year gives an
# event-study estimate; the default, every year from expansion on, the overall effect.
sunab_estimate <- function(m, cells, series, w = 1, rel = 0:100) {
        a <- unlist(lapply(seq_along(series), function(j) {
                s <- cells[cells$grp == series[j] & cells$rel %in% rel, ]
                setNames(w[j] * s$n / sum(s$n), s$term)
        }))
        stopifnot(length(a) > 0, names(a) %in% names(coef(m)))   # no cell dropped for collinearity
        est <- sum(a * coef(m)[names(a)])
        se <- sqrt(drop(t(a) %*% vcov(m)[names(a), names(a)] %*% a))
        data.frame(estimate = est, se = se, ci_low = est - qnorm(0.975) * se, ci_high = est + qnorm(0.975) * se)
}

# sunab_estimate() at each plotted event time, laid out as event_estimates()
sunab_event_estimates <- function(m, cells, series, w = 1) {
        do.call(rbind, lapply(event_years, function(k) {
                if (k == -1) return(data.frame(event_time = k, estimate = 0, se = NA, ci_low = NA, ci_high = NA))
                cbind(event_time = k, sunab_estimate(m, cells, series, w, rel = k))
        }))
}

# Estimates of a weighted sum of the coefficients of the groups in `series` (w = 1 for one group's
# effect, c(1, -1) for a difference) at each plotted event time, with 95% CIs clustered by state.
# Event time -1 is the omitted year (0 by design).
event_estimates <- function(m, series, w = 1) {
        do.call(rbind, lapply(event_years, function(k) {
                if (k == -1) return(data.frame(event_time = k, estimate = 0, se = NA))
                terms <- sprintf("et::%d:grp::%s", k, series)
                V <- vcov(m)[terms, terms, drop = FALSE]
                data.frame(event_time = k, estimate = sum(w * coef(m)[terms]), se = sqrt(drop(t(w) %*% V %*% w)))
        })) |> transform(ci_low = estimate - qnorm(0.975) * se, ci_high = estimate + qnorm(0.975) * se)
}

# Joint Wald test (pretrend_tests.R) that the weighted sum of the coefficients of the groups in
# `series` (as in event_estimates()) is zero at every event time in `years`: by default the plotted
# years before expansion, -4 to -2. The combined coefficient for -5 and earlier, estimated only
# from the states that expanded in 2016 or 2019, is not tested. F = W / q with q = length(years),
# on q and G - 1 degrees of freedom (G states), from the state-clustered covariance.
pretrend_test <- function(m, series, w = 1, years = -4:-2) {
        b <- coef(m)
        R <- event_weights(m, series, w, years)
        Rb <- R %*% b
        q <- length(years)
        F <- drop(t(Rb) %*% solve(R %*% vcov(m) %*% t(R)) %*% Rb) / q
        df2 <- fixest::degrees_freedom(m, "t")
        data.frame(F = F, df1 = q, df2 = df2, p = pf(F, q, df2, lower.tail = FALSE))
}

# The matrix that maps the coefficients of model m to the weighted sum of the groups in `series`
# at each event time in `years` (one row per year, named by the year). Stops if a term was
# dropped for collinearity.
event_weights <- function(m, series, w = 1, years) {
        b <- coef(m)
        R <- matrix(0, length(years), length(b), dimnames = list(as.character(years), names(b)))
        for (i in seq_along(years)) {
                terms <- sprintf("et::%d:grp::%s", years[i], series)
                stopifnot(terms %in% names(b))
                R[i, terms] <- w
        }
        R
}

# Inputs for the HonestDiD sensitivity analysis (honest_did.R; Rambachan and Roth 2023): the
# event-study estimates of the weighted sum of the groups in `series` before (`pre`) and after
# (`post`) expansion, pre-periods first and without the omitted year -1, and their covariance,
# clustered by state. The covariance of a difference between groups comes from the model's joint
# covariance.
honest_inputs <- function(m, series, w = 1, pre = -4:-2, post = 0:5) {
        R <- event_weights(m, series, w, c(pre, post))
        list(betahat = drop(R %*% coef(m)), sigma = R %*% vcov(m) %*% t(R))
}

# Each group's estimates in each specification of a nested fit
group_estimates <- function(models) {
        do.call(rbind, lapply(seq_along(event_spec_names), function(s) do.call(rbind, lapply(cit_groups, function(g) {
                cbind(spec = event_spec_names[s], series = g, event_estimates(models[[s]], g))
        }))))
}

# Figure caption, with the number of expansion states behind the end points
event_caption <- function(Data) {
        n_states <- function(k) length(unique(Data$ST[Data$expansion == 1 & Data$ttot == k]))
        text <- sprintf(paste("Coefficients with 95%% confidence intervals clustered by state; event time -1 is omitted.",
                              "Event time -4 is estimated from %d expansion states and +5 from %d (%d in total);",
                              "-5 and earlier are combined into one coefficient, not shown."),
                        n_states(-4), n_states(5), length(unique(Data$ST[Data$expansion == 1])))
        paste(strwrap(text, 140), collapse = "\n")
}

# Groups and differences get different colours, so a colour means the same thing in every figure
group_palette <- c("#232424", "#CC6677", "#56B4E9")
diff_palette  <- c("#009E73", "#D55E00", "#0072B2")

# Event-study plot of the estimates in d (one line per series); the caller adds the facets.
# Needs ggplot2 loaded.
event_plot <- function(d, ylab, palette, caption) {
        d$series <- factor(d$series, levels = unique(d$series))
        dodge <- position_dodge(width = 0.45)
        ggplot(d, aes(x = event_time, y = estimate, ymin = ci_low, ymax = ci_high, colour = series, shape = series)) +
                geom_hline(yintercept = 0) +
                geom_vline(xintercept = 0, lty = "dashed") +
                geom_line(position = dodge) +
                geom_errorbar(width = 0.2, position = dodge, na.rm = TRUE) +
                geom_point(size = 2, position = dodge) +
                scale_x_continuous(breaks = event_years) +
                scale_y_continuous(breaks = scales::breaks_pretty()) +
                scale_colour_manual(values = palette, name = NULL) +
                scale_shape_discrete(name = NULL) +
                labs(x = "Years relative to Medicaid expansion", y = ylab, caption = caption) +
                theme(panel.background = element_rect(fill = "white", colour = "grey50"),
                      legend.position = "bottom", strip.background = element_blank(),
                      strip.text = element_text(hjust = 0, face = "bold"), plot.caption = element_text(hjust = 0))
}

# The five-specification figure for outcome y (Figures 1-2, specs_*.R): fits specifications A-E, saves
# the estimates to <name>_estimates.csv and the figure to <name>.pdf. Needs ggplot2 loaded.
spec_figure <- function(Data, y, ylab, name) {
        est <- group_estimates(fit_event_models(Data, y))
        write.csv(est, file.path(table_dir, paste0(name, "_estimates.csv")), row.names = FALSE)
        est$spec <- factor(est$spec, levels = event_spec_names)
        fig <- event_plot(est, ylab, group_palette, event_caption(Data)) + facet_wrap(~spec, ncol = 3)
        ggsave(file.path(figure_dir, paste0(name, ".pdf")), fig, width = 11, height = 7.5)
}


####################
## Exhibit numbers
####################

# The main exhibits are Tables 1-2 and Figures 1-3. The appendix is grouped by the question each
# exhibit answers, and numbered within each section:
#   A. Data and sample
#   B. Identification: are the states that did not expand a valid comparison?
#   C. Robustness: do the results depend on the estimator, the covariates or the sample?
#   D. Heterogeneity
# Appendix scripts and their output files are named by content (the names below). This list is the
# only place that numbers them: the Word and LaTeX tables take their number from it, so
# renumbering changes one line here.
exhibits <- c(
        trends_raw       = "Figure B1",   # trends_raw.R
        control_outcomes = "Figure B2",   # control_outcomes.R
        comparison_samples = "Table B1",  # comparison_samples.R
        pretrend_tests   = "Table B2",    # pretrend_tests.R
        honest_did       = "Figure B3",   # honest_did.R
        robustness       = "Table C1",    # robustness.R (table and figure)
        robustness_plot  = "Figure C1",
        sunab_event      = "Figure C2",   # sunab_event.R
        specs_employer   = "Figure C3",   # specs_employer.R
        specs_direct     = "Figure C4",   # specs_direct.R
        leave_one_out    = "Figure C5",   # leave_one_out.R
        income_bands     = "Table D1"     # income_bands.R
)
exhibit_number <- function(name) sub("^(Table|Figure) ", "", exhibits[[name]])   # e.g. "C1"


####################
## Checks of the Table 2 model on other samples (comparison_samples.R, robustness.R, income_bands.R)
####################

# The effects and differences of Table 2
effect_contrasts <- list(
        "US-born"                               = list(series = "US-born", w = 1),
        "Naturalized citizen"                   = list(series = "Naturalized citizen", w = 1),
        "Non-citizen"                           = list(series = "Non-citizen", w = 1),
        "Naturalized citizen minus US-born"     = list(series = c("Naturalized citizen", "US-born"), w = c(1, -1)),
        "Non-citizen minus US-born"             = list(series = c("Non-citizen", "US-born"), w = c(1, -1)),
        "Non-citizen minus naturalized citizen" = list(series = c("Non-citizen", "Naturalized citizen"), w = c(1, -1))
)

# Estimates of the Table 2 model (static, specification E) for each check: a named vector of
# sample names (sample_variants above), e.g. c("Without Texas" = "drop_texas"). The check named
# "Sun and Abraham" changes the estimator instead (fit_sunab_models()). The data are read
# again for each check, and each check's models dropped before the next, so that the full file and
# several sets of models are never in memory together.
check_estimates <- function(checks) {
        do.call(rbind, lapply(names(checks), function(ch) {
                Data <- prep_event_data(make_sample(readRDS(data_file), checks[[ch]]))
                if (ch == "Sun and Abraham") {
                        # One outcome at a time: each Sun and Abraham model is larger than the Table 2 model
                        Data <- add_sunab_cells(Data)
                        out <- do.call(rbind, lapply(outcome_vars, function(y) {
                                m <- fit_sunab_models(Data, y)
                                cells <- sunab_cells(m, Data)
                                res <- do.call(rbind, lapply(names(effect_contrasts), function(cn) {
                                        k <- effect_contrasts[[cn]]
                                        cbind(check = ch, outcome = y, contrast = cn, sunab_estimate(m, cells, k$series, k$w), n = nobs(m))
                                }))
                                rm(m)
                                gc()
                                res
                        }))
                } else {
                        models <- fit_static_models(Data, paste0("c(", paste(outcome_vars, collapse = ", "), ")"), nested = FALSE)
                        info <- fixest::models(models)
                        out <- do.call(rbind, lapply(seq_along(models), function(k) do.call(rbind, lapply(names(effect_contrasts), function(cn) {
                                cbind(check = ch, outcome = info$lhs[k], contrast = cn,
                                      static_estimate(models[[k]], effect_contrasts[[cn]]$series, effect_contrasts[[cn]]$w),
                                      n = nobs(models[[k]]))
                        }))))
                        rm(models)
                }
                rm(Data)
                gc()
                out
        }))
}

# Writes the table of check_estimates() as <name>.tex, <name>.docx and <name>.csv: one block of rows
# per outcome, one row per check. `rows` describes the checks for the note, which adds the data
# source at the start and the confidence intervals at the end. Needs kableExtra loaded.
check_table <- function(estimates, checks, name, title, rows) {
        write.csv(estimates, file.path(table_dir, paste0(name, ".csv")), row.names = FALSE)
        fmt <- function(x) sub("^-", "−", sprintf("%.1f", x))   # true minus sign
        row_labels <- c(HICOV = "Any insurance", HINS4 = "Medicaid", HINS1 = "Employer-sponsored insurance",
                        HINS2 = "Direct purchase")
        # The estimate with its 95% CI below it, so that six columns fit a portrait page
        estimates$cell <- sprintf("%s\n(%s, %s)", fmt(estimates$estimate), fmt(estimates$ci_low), fmt(estimates$ci_high))
        tab <- do.call(rbind, lapply(outcome_vars, function(y) do.call(rbind, lapply(names(checks), function(ch) {
                e <- estimates[estimates$outcome == y & estimates$check == ch, ]
                data.frame(Outcome = row_labels[[y]], Check = ch,
                           t(setNames(e$cell[match(names(effect_contrasts), e$contrast)], names(effect_contrasts))),
                           check.names = FALSE)
        }))))
        note <- paste("Note: American Community Survey (ACS), 2011 to 2019.", rows,
                      "Cells are percentage-point estimates with 95% confidence intervals clustered by state.")
        abbreviations <- "Abbreviations: ACS, American Community Survey; US, United States."
        col_heads <- c("Sample", "US-born", "Naturalized\ncitizen", "Non-citizen",
                       "Naturalized\ncitizen minus\nUS-born", "Non-citizen\nminus\nUS-born", "Non-citizen\nminus\nnaturalized\ncitizen")
        spanners <- c("Effect of expansion" = 3, "Difference between groups" = 3)

        ## LaTeX: one block of rows per outcome; the table is numbered as in `exhibits`
        tex_body <- tab[, -1]
        tex_body[] <- lapply(tex_body, function(x) {
                x <- gsub("−", "$-$", gsub("%", "\\\\%", x))
                ifelse(grepl("\n", x), sprintf("\\makecell[tr]{%s}", gsub("\n", "\\\\\\\\", x)), x)
        })
        tex <- kableExtra::kbl(tex_body, col.names = sprintf("\\makecell[br]{%s}", gsub("\n", "\\\\\\\\", col_heads)),
                               format = "latex", booktabs = TRUE, escape = FALSE, align = c("l", rep("r", 6)),
                               caption = title, linesep = "") |>
                kableExtra::add_header_above(c(" " = 1, spanners))
        for (k in seq_along(outcome_vars)) {
                tex <- kableExtra::pack_rows(tex, row_labels[[outcome_vars[k]]], (k - 1) * length(checks) + 1, k * length(checks))
        }
        tex <- kableExtra::footnote(tex, general = c(note, abbreviations), general_title = "", threeparttable = TRUE)
        # The number goes before the caption (kbl writes the caption first); the size inside the float,
        # since some classes (apa7) reset the font size in tables
        tex <- sub("\\begin{table}", paste0("\\begin{table}\\renewcommand{\\thetable}{", exhibit_number(name), "}"),
                   tex, fixed = TRUE)
        # 7 point (one size below Table 2), so that five checks of two lines each fit a portrait page
        tex <- sub("\\centering", "\\centering\\renewcommand{\\baselinestretch}{1}\\scriptsize\\setlength{\\tabcolsep}{2.5pt}",
                   tex, fixed = TRUE)
        writeLines(tex, file.path(table_dir, paste0(name, ".tex")))

        ## Word: one bold row per outcome with its checks indented below, on a landscape page with
        ## each estimate and its CI on one line (a portrait page leaves no room for the note, which
        ## Word does not split across pages)
        tab[names(effect_contrasts)] <- lapply(tab[names(effect_contrasts)], function(x) sub("\n", " ", x, fixed = TRUE))
        blank <- tab[1, -1]
        blank[] <- ""
        docx_body <- do.call(rbind, lapply(outcome_vars, function(y) {
                heading <- blank
                heading$Check <- row_labels[[y]]
                rbind(heading, tab[tab$Outcome == row_labels[[y]], -1])
        }))
        names(docx_body) <- c("label", paste0("c", 1:6))
        heading_rows <- which(docx_body$label %in% row_labels)
        indented_rows <- setdiff(seq_len(nrow(docx_body)), heading_rows)
        ft <- flextable::flextable(docx_body) |>
                flextable::set_header_labels(values = setNames(as.list(gsub("\n", " ", col_heads)), names(docx_body))) |>
                flextable::add_header_row(values = c("", names(spanners)), colwidths = c(1, 3, 3)) |>
                flextable::add_footer_lines(c(note, abbreviations)) |>
                docx_style(spanner_cols = list(2:4, 5:7)) |>
                flextable::merge_h_range(i = heading_rows, j1 = 1, j2 = 7) |>
                flextable::bold(i = heading_rows, j = 1) |>
                flextable::padding(i = indented_rows, padding.top = 0, padding.bottom = 0) |>
                flextable::padding(i = indented_rows, j = 1, padding.left = 10) |>
                flextable::padding(i = heading_rows[-1], padding.top = 3) |>
                flextable::fontsize(size = 9, part = "body") |>
                flextable::fontsize(size = 8, part = "footer") |>
                flextable::align(j = 2:7, align = "right", part = "all") |>
                flextable::align(i = 1, j = 2:7, align = "center", part = "header") |>
                flextable::width(j = 1, width = 2.1) |>
                flextable::width(j = 2:7, width = 1.15) |>
                flextable::height_all(height = 0.1, part = "body") |>   # rows as tall as their text (default is 0.25 in)
                flextable::height_all(height = 0.1, part = "footer")
        save_docx_table(ft, exhibit_number(name), title, file.path(table_dir, paste0(name, ".docx")), landscape = TRUE)
}

# Each group's effect in each check as a forest plot, the first check at the top, saved as
# <name>.pdf. Needs ggplot2 loaded.
check_plot <- function(estimates, checks, name, caption) {
        d <- estimates[estimates$contrast %in% cit_groups, ]
        d$check <- factor(d$check, levels = rev(names(checks)))
        d$contrast <- factor(d$contrast, levels = rev(cit_groups))
        d$outcome <- factor(outcome_labels[d$outcome], levels = outcome_labels[outcome_vars])
        fig <- ggplot(d, aes(x = check, y = estimate, ymin = ci_low, ymax = ci_high, colour = contrast, shape = contrast)) +
                geom_hline(yintercept = 0) +
                geom_vline(xintercept = length(checks) - 0.5, colour = "grey60") +
                geom_pointrange(position = position_dodge(width = 0.6), size = 0.3) +
                coord_flip() +
                facet_wrap(~outcome, ncol = 2, scales = "free_x") +
                scale_colour_manual(values = setNames(group_palette, cit_groups), breaks = cit_groups, name = NULL) +
                scale_shape_discrete(breaks = cit_groups, name = NULL) +
                labs(x = NULL, y = "Effect of expansion (percentage points)", caption = caption) +
                theme(panel.background = element_rect(fill = "white", colour = "grey50"),
                      legend.position = "bottom", strip.background = element_blank(),
                      strip.text = element_text(hjust = 0, face = "bold"), plot.caption = element_text(hjust = 0))
        ggsave(file.path(figure_dir, paste0(name, ".pdf")), fig, width = 11, height = 2 + 0.9 * length(checks))
}


####################
## Helpers
####################

# Word tables for HSR, in two steps so that a table can add its own indents in between:
# docx_style() sets Times New Roman 10 point (notes 9 point), compact rows and horizontal rules
# only: above and below the header, below the body, and under each column spanner (spanner_cols,
# e.g. list(2:4, 5:7), for spanners in the first header row). save_docx_table() writes it on a
# letter page (portrait, or landscape for wide tables) with 1-inch margins, below "Table <number>."
# and the title.
docx_style <- function(ft, spanner_cols = list()) {
        rule <- officer::fp_border(color = "black", width = 0.75)
        ft <- ft |>
                flextable::font(fontname = "Times New Roman", part = "all") |>
                flextable::fontsize(size = 10, part = "all") |>
                flextable::fontsize(size = 9, part = "footer") |>
                flextable::padding(padding.top = 1, padding.bottom = 1, padding.left = 2, padding.right = 2, part = "all") |>
                flextable::line_spacing(space = 1, part = "all") |>
                flextable::valign(valign = "bottom", part = "header") |>
                flextable::border_remove() |>
                flextable::hline_top(border = rule, part = "header") |>
                flextable::hline_bottom(border = rule, part = "header") |>
                flextable::hline_bottom(border = rule, part = "body") |>
                flextable::hrule(rule = "auto", part = "all") |>
                flextable::set_table_properties(layout = "fixed")
        for (j in spanner_cols) ft <- flextable::hline(ft, i = 1, j = j, border = rule, part = "header")
        ft
}

save_docx_table <- function(ft, number, title, file, landscape = FALSE) {
        title_text <- officer::fp_text(font.family = "Times New Roman", font.size = 10)
        officer::read_docx() |>
                officer::body_add_fpar(officer::fpar(officer::ftext(paste0("Table ", number, ". "), update(title_text, bold = TRUE)),
                                                     officer::ftext(title, title_text))) |>
                flextable::body_add_flextable(ft, align = "left") |>
                officer::body_set_default_section(officer::prop_section(
                        page_size = officer::page_size(orient = if (landscape) "landscape" else "portrait", width = 8.5, height = 11),
                        page_margins = officer::page_mar(top = 1, bottom = 1, left = 1, right = 1,
                                                         header = 0.5, footer = 0.5, gutter = 0))) |>
                print(target = file)
}
