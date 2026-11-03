# Title: Table 2. Effects of Medicaid expansion on coverage, by citizenship
# Author: Shadi Seyedi
# Description: Static difference-in-differences effects of expansion on any insurance, Medicaid,
#              employer-sponsored insurance and direct purchase for US-born adults, naturalized
#              citizens and non-citizens, and the differences between groups. One model per
#              outcome, fit by fit_static_models() in 00_setup.R: the static version of the
#              Figure 1-3 event studies, with each group's own expansion effect, state and year
#              fixed effects and covariate effects. The table uses the full covariate set
#              (specification E); table2_by_spec.csv has specifications A-E.
#              Replaces Tables 2 and 5 of the earlier paper (codes/R/06-did-regressions.R), which
#              pooled the foreign-born into one group.
# Output: output/tables/table2.tex (LaTeX), table2.docx (Word), table2.csv, table2_by_spec.csv

source("code/00_setup.R")
Data <- prep_event_data(make_sample(readRDS(data_file), sample_name))

library(fixest)   # attached: i() in the nested (csw0) covariates is looked up by name
library(dplyr)
library(kableExtra)


####################
## Models
####################

# One fixest_multi: 4 outcomes x specifications A-E
models <- fit_static_models(Data, paste0("c(", paste(outcome_vars, collapse = ", "), ")"))

# The effects in each model: each group's, and the three differences between groups
contrasts <- list(
        "US-born"                             = list(series = "US-born", w = 1),
        "Naturalized citizen"                 = list(series = "Naturalized citizen", w = 1),
        "Non-citizen"                         = list(series = "Non-citizen", w = 1),
        "Naturalized citizen minus US-born"   = list(series = c("Naturalized citizen", "US-born"), w = c(1, -1)),
        "Non-citizen minus US-born"           = list(series = c("Non-citizen", "US-born"), w = c(1, -1)),
        "Non-citizen minus naturalized citizen" = list(series = c("Non-citizen", "Naturalized citizen"), w = c(1, -1))
)

# fixest::models() gives each model's outcome and right-hand side; the right-hand sides appear in
# the order of the nested specifications A-E
info <- fixest::models(models)
spec_of <- setNames(event_spec_names, unique(info$rhs))
estimates <- do.call(rbind, lapply(seq_along(models), function(k) {
        m <- models[[k]]
        do.call(rbind, lapply(names(contrasts), function(cn) {
                cbind(outcome = info$lhs[k], spec = spec_of[[info$rhs[k]]], contrast = cn,
                      static_estimate(m, contrasts[[cn]]$series, contrasts[[cn]]$w), n = nobs(m))
        }))
}))
stopifnot(length(unique(info$rhs)) == length(event_spec_names), setequal(info$lhs, outcome_vars))
write.csv(estimates, file.path(table_dir, "table2_by_spec.csv"), row.names = FALSE)

main <- estimates[estimates$spec == tail(event_spec_names, 1), ]


####################
## Baseline means, sample sizes
####################

# Mean of each outcome in expansion states before 2014 (as in Table 1), by group
pre_exp <- Data[Data$expansion == 1 & Data$YEAR < 2014, ]
baseline <- sapply(cit_groups, function(g) {
        d <- pre_exp[pre_exp$grp == g, ]
        sapply(outcome_vars, function(y) weighted.mean(d[[y]], d$PWGTP))
})

n_people <- table(Data$grp)[cit_groups]
n_states <- length(unique(Data$ST))
n_exp_states <- length(unique(Data$ST[Data$expansion == 1]))


#######################
## Table 2
######################

fmt <- function(x) sub("^-", "−", sprintf("%.1f", x))   # true minus sign
# Same wording as Table 1
row_labels <- c(HICOV = "Any insurance", HINS4 = "Medicaid", HINS1 = "Employer-sponsored insurance",
                HINS2 = "Direct purchase")
# Under each outcome: the estimates, their 95% CIs, and the baseline mean (group columns only);
# then one row of sample sizes
rows <- do.call(rbind, lapply(outcome_vars, function(y) {
        e <- main[main$outcome == y, ]
        e <- e[match(names(contrasts), e$contrast), ]
        rbind(c(row_labels[[y]], "Estimate", fmt(e$estimate)),
              c(row_labels[[y]], "95% CI", sprintf("(%s, %s)", fmt(e$ci_low), fmt(e$ci_high))),
              c(row_labels[[y]], "Baseline mean", fmt(baseline[y, ]), rep("", 3)))
}))
rows <- rbind(rows, c("", "Respondents, n", format(as.integer(n_people), big.mark = ","), rep("", 3)))
table2 <- as.data.frame(rows, stringsAsFactors = FALSE)
names(table2) <- c("Outcome", "Row", names(contrasts))

write.csv(table2, file.path(table_dir, "table2.csv"), row.names = FALSE)

# HSR: the title has no abbreviations or punctuation, and the table defines every abbreviation it
# uses, so that it can be read without the text
title <- "Effects of Medicaid expansion on health insurance coverage by citizenship"
note <- paste(
        sprintf("Note: American Community Survey (ACS), 2011 to 2019; the sample is as in Table 1 (%d states, %d of which expanded Medicaid).", n_states, n_exp_states),
        "Estimates are percentage-point changes from weighted difference-in-differences regressions, one per outcome,",
        "in which each group has its own expansion effect, state and year fixed effects, and coefficients on age, sex,",
        "race and ethnicity, marital status, education, disability, income relative to poverty, employment, English",
        "proficiency, share of life in the US, region of birth, the state unemployment rate, the ADA score and the",
        "Immigrant Policy Climate index. Expansion is 1 from the calendar year a state expanded (2014 to 2019).",
        "Baseline mean is the group's weighted percentage with the coverage in expansion states, 2011 to 2013.",
        "Differences are between the groups' effects in the same model. Confidence intervals use standard errors",
        "clustered by state. A person can report more than one source of coverage, so the effects on the sources",
        "do not add up to the effect on any insurance.",
        if (sample_name != "main") paste("Sensitivity sample:", sample_variants[[sample_name]]$why))
abbreviations <- paste("Abbreviations: ACS, American Community Survey; ADA, Americans for Democratic Action;",
                       "CI, confidence interval; US, United States.")

# Column headings with their line breaks (so no line breaks at the hyphen in "US-born")
col_heads <- c("US-born", "Naturalized\ncitizen", "Non-citizen",
               "Naturalized\ncitizen minus\nUS-born", "Non-citizen\nminus\nUS-born", "Non-citizen\nminus\nnaturalized\ncitizen")
spanners <- c("Effect of expansion" = 3, "Difference between groups" = 3)
outcome_rows <- rle(table2$Outcome[table2$Outcome != ""])   # 3 rows per outcome, in order

## LaTeX: a regular table (it fits one page), single-spaced and 8 point, each outcome a bold
## heading across the table with its rows indented below. Needs booktabs, makecell, threeparttable.
tex_heads <- c("", sprintf("\\makecell[br]{%s}", gsub("\n", "\\\\\\\\", col_heads)))
tex_body <- table2[, -1]
tex_body[] <- lapply(tex_body, function(x) gsub("−", "$-$", gsub("%", "\\\\%", x)))
tex <- kbl(tex_body, col.names = tex_heads, format = "latex", booktabs = TRUE, escape = FALSE,
           align = c("l", rep("r", 6)), caption = title, linesep = "") %>%
        add_header_above(c(" " = 1, spanners))
start <- 1
for (k in seq_along(outcome_rows$values)) {
        tex <- pack_rows(tex, outcome_rows$values[k], start, start + outcome_rows$lengths[k] - 1)
        start <- start + outcome_rows$lengths[k]
}
tex <- tex %>%
        row_spec(nrow(tex_body) - 1, extra_latex_after = "\\addlinespace[0.3em]") %>%
        footnote(general = c(note, abbreviations), general_title = "", threeparttable = TRUE)
# The size goes inside the float: some classes (apa7) reset the font size in tables
tex <- sub("\\centering", "\\centering\\renewcommand{\\baselinestretch}{1}\\footnotesize\\setlength{\\tabcolsep}{3pt}",
           tex, fixed = TRUE)
writeLines(tex, file.path(table_dir, "table2.tex"))

## Word: the same layout, one bold row per outcome with its rows indented below
blank <- table2[1, -1]
blank[] <- ""
docx_body <- do.call(rbind, c(lapply(outcome_rows$values, function(o) {
        heading <- blank
        heading$Row <- o
        rbind(heading, table2[table2$Outcome == o, -1])
}), list(table2[table2$Outcome == "", -1])))
names(docx_body) <- c("label", paste0("c", 1:6))
heading_rows <- which(docx_body$label %in% outcome_rows$values)
indented_rows <- setdiff(seq_len(nrow(docx_body)), c(heading_rows, nrow(docx_body)))
ft <- flextable::flextable(docx_body) %>%
        flextable::set_header_labels(values = setNames(as.list(c("", col_heads)), names(docx_body))) %>%
        flextable::add_header_row(values = c("", names(spanners)), colwidths = c(1, 3, 3)) %>%
        flextable::add_footer_lines(c(note, abbreviations)) %>%
        docx_style(spanner_cols = list(2:4, 5:7)) %>%
        flextable::merge_h_range(i = heading_rows, j1 = 1, j2 = 7) %>%
        flextable::bold(i = heading_rows, j = 1) %>%
        flextable::padding(i = indented_rows, j = 1, padding.top = 1, padding.bottom = 1, padding.left = 10) %>%
        flextable::padding(i = c(heading_rows[-1], nrow(docx_body)), padding.top = 6, padding.bottom = 1) %>%
        flextable::align(j = 2:7, align = "right", part = "all") %>%
        flextable::align(i = 1, j = 2:7, align = "center", part = "header") %>%
        flextable::width(j = 1, width = 1.3) %>%
        flextable::width(j = 2:7, width = 0.85)
save_docx_table(ft, 2, title, file.path(table_dir, "table2.docx"))
