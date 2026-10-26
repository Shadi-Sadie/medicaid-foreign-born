# Title: Compare the results with the numbers printed in the paper
# Description: Reads the files written by code/analysis/ and compares key numbers with Paper_1.pdf.
#              A number passes when it is within one unit of the last digit the paper reports.
#              Differences that are already known and documented in CHANGES.md are reported as
#              "known difference" and do not stop the run.
#              Table 4 is not checked, because its code does not reproduce the paper (see table4.R).
#              Figures are checked by eye against the paper.
# Output: output/check_paper.csv; stops with an error if any check fails.

source("code/00_setup.R")

results <- list()
# Known differences: Table 6's expansion main effect differs from the paper in the 4th decimal
# (see CHANGES.md).
known <- c("Table 6: UNINS treat", "Table 6: HINS4 treat")

check <- function(item, what, got, paper, digits) {
        ok <- !is.na(got) && abs(got - paper) <= 1.0001 * 10^-digits
        status <- if (ok) "pass" else if (paste0(item, ": ", what) %in% known) "known difference" else "FAIL"
        results[[length(results) + 1]] <<- data.frame(item = item, what = what, paper = paper,
                                                      got = round(got, digits + 2), status = status)
}

read_est <- function(file) read.csv(file.path(table_dir, file))
get_est <- function(est, model, term, col = "estimate") {
        v <- est[est$model == model & est$term == term, col]
        if (length(v) == 1) v else NA
}
get_stat <- function(est, model, col) {
        v <- unique(est[est$model == model, col])
        if (length(v) == 1) v else NA
}

outcomes <- c("UNINS", "HINS4", "HINS1", "HINS2")


####################
## Table 1 (unweighted, pre-ACA)
####################

t1 <- read.csv(file.path(table_dir, "table1.csv"), check.names = FALSE)
num <- function(x) as.numeric(gsub("[^0-9.-]", "", gsub("−", "-", x)))
cell <- function(label, col) num(t1[t1$label == label, col][1])

check("Table 1", "Uninsured, non-expansion FB (%)",  cell("Uninsured", "stat_1_1"),   67,   0)
check("Table 1", "Uninsured, non-expansion USB (%)", cell("Uninsured", "stat_2_1"),   40,   0)
check("Table 1", "Uninsured, non-expansion diff",    cell("Uninsured", "estimate_1"), 28,   0)
check("Table 1", "Uninsured, expansion FB (%)",      cell("Uninsured", "stat_1_2"),   53,   0)
check("Table 1", "Uninsured, Exp. Var. FB",          cell("Uninsured", "estimate_3"), 14,   0)
check("Table 1", "Uninsured, Exp. Var. USB",         cell("Uninsured", "estimate_4"), 6.7,  1)
check("Table 1", "Medicaid, Exp. Var. USB",          cell("Medicaid",  "estimate_4"), -7.7, 1)
check("Table 1", "Individually purchased, non-expansion USB (%)", cell("Individually Purchased", "stat_2_1"), 8.3, 1)
check("Table 1", "Disability, expansion USB (%)",    cell("Disability", "stat_2_2"),  28,   0)
check("Table 1", "Married, expansion FB (%)",        cell("Married",    "stat_1_2"),  58,   0)


####################
## Tables 2 and 5 (DiD)
####################

paper_t2 <- list(treat = c(-0.083, 0.141, -0.023, -0.029), ForeginBorn = c(-0.083, -0.038, 0.062, 0.031),
                 `treat:ForeginBorn` = c(0.033, -0.036, 0.014, -0.017),
                 r2 = c(0.17791, 0.19429, 0.10807, 0.05183))
paper_t5 <- list(treat = c(-0.075, 0.136, -0.024, -0.030), ForeginBorn = c(0.244, -0.158, -0.043, -0.019),
                 `treat:ForeginBorn` = c(-0.031, 0.007, 0.018, -0.003),
                 r2 = c(0.10174, 0.08522, 0.00411, 0.00827))

for (tab in c("Table 2", "Table 5")) {
        est   <- read_est(if (tab == "Table 2") "table2_estimates.csv" else "table5_estimates.csv")
        paper <- if (tab == "Table 2") paper_t2 else paper_t5
        for (k in seq_along(outcomes)) {
                for (term in c("treat", "ForeginBorn", "treat:ForeginBorn")) {
                        check(tab, paste(outcomes[k], term), get_est(est, outcomes[k], term), paper[[term]][k], 3)
                }
                check(tab, paste(outcomes[k], "R2"), get_stat(est, outcomes[k], "r2"), paper$r2[k], 5)
                check(tab, paste(outcomes[k], "N"),  get_stat(est, outcomes[k], "n"), 1975594, 0)
        }
}


####################
## Table 3 (Bacon decomposition)
####################

t3 <- read.csv(file.path(table_dir, "table3.csv"))
paper_t3 <- data.frame(Outcome = rep(c("Uninsured", "Medicaid"), each = 3),
                       Comparison = rep(c("Earlier vs Later Treated", "Later vs Earlier Treated", "Treated vs Untreated"), 2),
                       Coefficient = c(-0.09309, -0.05521, -0.08462, 0.14610, 0.08782, 0.14452),
                       Weight = c(0.11871, 0.08771, 0.79358, 0.11871, 0.08771, 0.79358))
for (i in seq_len(nrow(paper_t3))) {
        row <- t3[t3$Outcome == paper_t3$Outcome[i] & t3$Comparison == paper_t3$Comparison[i], ]
        check("Table 3", paste(paper_t3$Outcome[i], paper_t3$Comparison[i], "coefficient"), row$Coefficient, paper_t3$Coefficient[i], 5)
        check("Table 3", paste(paper_t3$Outcome[i], paper_t3$Comparison[i], "weight"), row$Weight, paper_t3$Weight[i], 5)
}
check("Table 3", "Uninsured overall DiD", unique(t3$Overall[t3$Outcome == "Uninsured"]), -0.08304, 5)
check("Table 3", "Medicaid overall DiD",  unique(t3$Overall[t3$Outcome == "Medicaid"]),   0.13973, 5)


####################
## Table 6 (citizenship)
####################

est <- read_est("table6_estimates.csv")
paper_t6 <- list(treat = c(-0.0831, 0.1410, -0.0230, -0.0294),
                 `treat:CITSTATFB-Citizen` = c(0.0215, 0.0056, 0.0007, -0.0261),
                 `treat:CITSTATFB-Non-citizen` = c(0.0402, -0.0620, 0.0214, -0.0106),
                 r2 = c(0.178, 0.195, 0.108, 0.052))
for (k in seq_along(outcomes)) {
        for (term in c("treat", "treat:CITSTATFB-Citizen", "treat:CITSTATFB-Non-citizen")) {
                check("Table 6", paste(outcomes[k], term), get_est(est, outcomes[k], term), paper_t6[[term]][k], 4)
        }
        check("Table 6", paste(outcomes[k], "R2"), get_stat(est, outcomes[k], "r2"), paper_t6$r2[k], 3)
}


####################
## Tables 7 and 8 (event studies)
####################

event_checks <- list(
        "Table 7" = list(file = "table7_estimates.csv", models = paste0("Event", 5:8),
                         `0` = c(0.082, 0.080, 0.067, 0.064), `5` = c(0.156, 0.157, 0.181, 0.159),
                         `-5` = c(-0.024, -0.027, -0.030, -0.031), se0 = c(0.004, 0.006, 0.002, 0.005),
                         r2 = c(0.06514, 0.18775, 0.10655, 0.18073)),
        "Table 8" = list(file = "table8_estimates.csv", models = paste0("Event", 1:4),
                         `0` = c(-0.052, -0.051, -0.035, -0.023), `5` = c(-0.100, -0.100, -0.099, -0.065),
                         `-5` = c(0.021, 0.028, 0.007, 0.024), se0 = c(0.005, 0.006, 0.008, 0.011),
                         r2 = c(0.05930, 0.11980, 0.08577, 0.21568))
)
for (tab in names(event_checks)) {
        p <- event_checks[[tab]]
        est <- read_est(p$file)
        for (k in 1:4) {
                m <- p$models[k]
                for (yr in c("-5", "0", "5")) {
                        check(tab, paste(m, "year", yr), get_est(est, m, paste0("ttot::", yr, ":expansion")), p[[yr]][k], 3)
                }
                check(tab, paste(m, "year 0 SE"), get_est(est, m, "ttot::0:expansion", "se"), p$se0[k], 3)
                check(tab, paste(m, "R2"), get_stat(est, m, "r2"), p$r2[k], 5)
                check(tab, paste(m, "N"), get_stat(est, m, "n"), c(1585639, 1585639, 389955, 389955)[k], 0)
        }
}


####################
## Report
####################

results <- do.call(rbind, results)
write.csv(results, "output/check_paper.csv", row.names = FALSE)

cat(sprintf("\n%d of %d checks pass.\n", sum(results$status == "pass"), nrow(results)))
if (any(results$status != "pass")) print(results[results$status != "pass", ], row.names = FALSE)
if (any(results$status == "FAIL")) stop("Some results do not match the paper. See output/check_paper.csv.")
