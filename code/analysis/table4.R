# Title: Table 4. Descriptive statistics of the main sample by state expansion status
# Author: Shadi Seyedi
# Description: Weighted counts and percentages by expansion status, with p-values.
#              Copied from texts/Draft-Result.Rmd lines 425-446.
# Output: output/tables/table4.tex and table4.csv
#
# WARNING: THIS SCRIPT DOES NOT REPRODUCE TABLE 4 IN THE PAPER. On CLNACS2 it gives different
# totals and rows (for example, CLNACS2 has 4 race groups, the paper 5). The paper's own numbers
# are inconsistent (the expansion-state disability count is larger than the column total), so the
# table probably came from an older dataset. It is kept so the code behind the table is on record.

source("code/00_setup.R")
load(data_file)

library(survey)
library(gtsummary)

# FIX: the original used srvyr::as_survey() with the 80 replicate weights (JK1, scale 4/80). In
# CLNACS2, 342 rows from 2011 have missing replicate weights, which the survey package does not
# allow. The design now uses the person weight PWGTP only. The weighted counts and percentages
# are the same; only the p-values can differ.
svy <- svydesign(
        ids = ~1,
        weights = ~PWGTP,
        data = Data
)

table4 <- svy %>%  tbl_svysummary(
        by = expansion,
        include = c(DIS, SEX,  MARG ,SCHLG ,ETHN, ESRG, POVPIPG, RACE,AGEP)
) %>%
        add_p()

write.csv(as_tibble(table4, col_labels = FALSE), file.path(table_dir, "table4.csv"), row.names = FALSE)
gt::gtsave(as_gt(table4), file.path(table_dir, "table4.tex"))
