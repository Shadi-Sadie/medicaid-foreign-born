# Title: Master script
# Description: Re-creates every table and figure in the paper from data/clean/acs_clean.rds. Each
#              analysis script runs in its own fresh R process, so none depends on objects left
#              over from another.
#              The data-preparation scripts in code/data_prep/ build that file and are NOT run
#              here: run them first (see README.md, "Instructions to replicators").
# Usage: from the project root,  Rscript run_all.R
# Output: output/tables/, output/figures/, output/logs/, output/session_info.txt

source("code/00_setup.R")

scripts <- c(
        "code/analysis/table1.R",
        "code/analysis/table2_5.R",
        "code/analysis/table3_fig4.R",
        "code/analysis/table4.R",
        "code/analysis/table6.R",
        "code/analysis/table7_8.R",
        "code/analysis/fig1.R",
        "code/analysis/fig2.R",
        "code/analysis/fig3.R",
        "code/analysis/fig5.R",
        "code/analysis/fig7.R",
        "code/analysis/fig8.R",
        "code/analysis/fig9.R",
        "code/analysis/fig10.R"
)

log_dir <- "output/logs"
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
rscript <- file.path(R.home("bin"), "Rscript")

start <- Sys.time()
for (s in scripts) {
        log_file <- file.path(log_dir, sub("\\.R$", ".log", basename(s)))
        cat("Running", s, "... ")
        t0 <- Sys.time()
        status <- system2(rscript, shQuote(s), stdout = log_file, stderr = log_file)
        cat(sprintf("%.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
        if (status != 0) stop(s, " failed. See ", log_file)
}
cat(sprintf("All scripts finished in %.1f min.\n", as.numeric(difftime(Sys.time(), start, units = "mins"))))

# Record the R and package versions used
suppressPackageStartupMessages(invisible(lapply(required_packages, library, character.only = TRUE)))
writeLines(capture.output(sessionInfo()), "output/session_info.txt")
