# Title: Shared setup for the analysis scripts
# Author: Shadi Seyedi
# Description: Every script in code/analysis/ sources this file first. It checks that the
#              required packages are installed, sets the input and output paths, and defines
#              the control-variable lists that several scripts share.
#              Run all scripts from the project root folder (see README.md).


####################
## Required Packages
####################

required_packages <- c("fixest", "ggplot2", "ggpubr", "ggiplot", "dplyr", "scales",
                       "gtsummary", "gt", "survey", "bacondecomp", "cowplot", "kableExtra",
                       "dagitty", "ggdag")

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
        stop("Missing R packages: ", paste(missing_packages, collapse = ", "),
             "\nSee README.md, section 'Computational requirements'.")
}


####################
## Paths
####################

if (!file.exists("run_all.R")) stop("Run the scripts from the project root folder.")

data_file  <- "data/CLNACS2.RData"   # cleaned analysis file, N = 1,975,594 (loads the object `Data`)
table_dir  <- "output/tables"
figure_dir <- "output/figures"

dir.create(table_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)


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

# Saves coefficients, standard errors, N and R2 of fixest models to a CSV file, so that
# code/check_paper.R can compare them with the paper.
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
