# Title: Shared setup for the analysis scripts
# Author: Shadi Seyedi
# Description: Every script in code/analysis/ sources this file first. It checks that the
#              required packages are installed, sets the input and output paths, and defines
#              the control-variable lists that several scripts share, and defines the analysis
#              samples (make_sample()).
#              Run all scripts from the project root folder (see README.md).


####################
## Required Packages
####################

required_packages <- c("fixest", "ggplot2", "ggpubr", "ggiplot", "dplyr", "scales",
                       "gtsummary", "gt", "survey", "bacondecomp", "cowplot", "kableExtra")

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
# line, e.g.  Rscript code/analysis/table2_5.R pov_100  (see sample_variants below).
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
# county-based or below 138% of poverty), and are treated as 2014 expanders in the main sample.
# Sommers et al. (2013, Medicare & Medicaid Research Review); Colorado's 2012 waiver was capped at
# 10,000 people (Kaestner et al. 2017).
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
                          why = "Delaware, the District of Columbia, Massachusetts, New York and Vermont are excluded: they covered low-income adults before 2014 (Miller, Johnson and Wherry 2021).")
)

sample_variants <- list(
        all_states = list(drop = "prior_full",
                          why = "Keeps every state. DC has no ADA score, so models that control for ADA drop it."),
        drop_early_option = list(rule = quote(!ST %in% early_option_states),
                                 why = "Also excludes the states that expanded partly before 2014 (CA, CO, CT, MN, NJ, WA)."),
        simon_states = list(drop = "prior_full", rule = quote(!ST %in% simon_states),
                            why = "Excludes every state with any coverage of childless adults before 2014 (Simon, Soni and Cawley 2017)."),
        drop_colorado = list(rule = quote(ST != "08"),
                             why = "Excludes Colorado, whose small capped waiver began in 2012."),
        drop_wisconsin = list(rule = quote(ST != "55"),
                              why = "Excludes Wisconsin, which covered childless adults up to 100% of poverty through a waiver without adopting the ACA expansion."),
        drop_midyear = list(rule = quote(!ST %in% c("02", "18", "22", "26", "33")),
                            why = "Excludes states whose expansion started after January (AK, IN, LA, MI, NH; data/expansion.csv), so the expansion year is only partly treated. Maine's 10 January 2019 start counts as January."),
        drop_late = list(rule = quote(Expan_STS != "Late Expansion"),
                         why = "Excludes states that expanded in 2015-2019, leaving 2014 expanders and states that never expanded."),
        drop_2019 = list(rule = quote(!ExpansionY %in% 2019),
                         why = "Excludes Maine and Virginia, which expanded in 2019 and so have a single treated year."),
        pov_100 = list(drop = "income", rule = quote(POVPIP <= 100),
                       why = "Income at or below 100% of poverty."),
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
        d <- data.frame(AGEP = c(40, 25, 40, 40, 40, 40, 40, 40), POVPIP = c(100, 100, 150, 100, 100, 100, 100, 100),
                        HINS3 = c(0, 0, 0, 0, 0, 0, 0, 1),
                        in_gq = c(0, 0, 0, 1, 0, 0, 0, 0), cit_birth_abroad = c(0, 0, 0, 0, 1, 0, 0, 0),
                        NonCit = c(1, 0, 0, 0, 0, 1, 0, 0), recent_arrival = c(0, 0, 0, 0, 0, 1, 0, 0),
                        ST = c("48", "48", "48", "48", "48", "48", "36", "48"),
                        rule = c("passes", "age", "income", "group_quarters", "cit_birth_abroad", "residency", "prior_full", "medicare"))
        run <- function(name) { capture.output(s <- make_sample(d, name)); s$rule }
        stopifnot(identical(run("main"), "passes"),
                  identical(run("with_medicare"), c("passes", "medicare")),
                  identical(run("pov_200"), c("passes", "income")),
                  identical(run("all_states"), c("passes", "prior_full")),
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
## Control variables
####################

# RECONSTRUCTED: `controls` and `foriegn` were used by the original event-study and robustness
# scripts but never defined in any saved script. These definitions reproduce Tables 7 and 8 exactly.
controls <- c("SEX", "DIS", "AGEP", "POVPIPG", "SCHLG", "MARG", "RACE1", "ESRG", "UnempR", "ADA")
foriegn  <- c("ENG", "LTINU", "CORIGIN", "NonCit", "IPC")

# Pooled control list from the original Figure 1 script (also used for Figure 5).
controlvar <- c("UnempR", "SEX", "DIS", "AGEP", "SCHLG", "MARG", "RACE1", "ESRG", "ENG", "LTINU",
                "NonCit", "IPC", "POVPIPG", "ADA")


####################
## Helpers
####################

# Saves coefficients, standard errors, N and R2 of fixest models to a CSV file.
save_estimates <- function(models, file) {
        if (inherits(models, "fixest_multi")) {
                models <- as.list(models)
                names(models) <- vapply(models, function(m) deparse(m$fml[[2]]), "")   # outcome name
        }
        if (inherits(models, "fixest")) models <- list(models)
        if (is.null(names(models))) names(models) <- paste0("model", seq_along(models))
        rows <- lapply(names(models), function(m) {
                ct <- fixest::coeftable(models[[m]])
                data.frame(model = m, term = rownames(ct), estimate = ct[, 1], se = ct[, 2],
                           n = nobs(models[[m]]), r2 = fixest::r2(models[[m]], "r2"),
                           wr2 = fixest::r2(models[[m]], "wr2"), row.names = NULL)
        })
        write.csv(do.call(rbind, rows), file, row.names = FALSE)
}
