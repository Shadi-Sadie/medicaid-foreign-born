# Title: Joint pre-trend tests (appendix section B, identification; number in `exhibits` in
#        00_setup.R)
# Author: Shadi Seyedi
# Description: Joint Wald tests that the event-study coefficients for 4, 3 and 2 years before
#              expansion are all zero, in the fully adjusted model (specification E) of Figures
#              1-3, for each citizenship group and for each difference between two groups, for the
#              four outcomes. Some single pre-expansion coefficients are significant (naturalized
#              citizens); the joint test asks whether the leads as a whole depart from zero. The
#              model and test are fit_event_models() and pretrend_test() in 00_setup.R.
# Output: output/tables/pretrend_tests.tex (LaTeX), pretrend_tests.docx (Word), pretrend_tests.csv

source("code/00_setup.R")
Data <- prep_event_data(make_sample(readRDS(data_file), sample_name))

library(fixest)   # attached: i() in the covariates is looked up by name
library(kableExtra)

# One fixest_multi: the four outcomes, specification E
models <- fit_event_models(Data, paste0("c(", paste(outcome_vars, collapse = ", "), ")"), nested = FALSE)
info <- fixest::models(models)
stopifnot(setequal(info$lhs, outcome_vars))
res <- do.call(rbind, lapply(seq_along(models), function(k) do.call(rbind, lapply(names(effect_contrasts), function(cn) {
        cbind(outcome = info$lhs[k], contrast = cn,
              pretrend_test(models[[k]], effect_contrasts[[cn]]$series, effect_contrasts[[cn]]$w))
}))))
rm(models, Data)
gc()
write.csv(res, file.path(table_dir, "pretrend_tests.csv"), row.names = FALSE)
stopifnot(nrow(res) == length(outcome_vars) * length(effect_contrasts), res$df1 == 3, length(unique(res$df2)) == 1)

## Table: one row per outcome, the F statistic with its p-value below it in each cell
name <- "pretrend_tests"
title <- "Joint tests of pre-expansion event-study coefficients by citizenship"
row_labels <- c(HICOV = "Any insurance", HINS4 = "Medicaid", HINS1 = "Employer-sponsored insurance",
                HINS2 = "Direct purchase")
res$cell <- sprintf("%.2f\n%s", res$F, ifelse(res$p < 0.001, "p < 0.001", sprintf("p = %.3f", res$p)))
tab <- do.call(rbind, lapply(outcome_vars, function(y) {
        e <- res[res$outcome == y, ]
        data.frame(Outcome = row_labels[[y]], t(setNames(e$cell[match(names(effect_contrasts), e$contrast)],
                                                         names(effect_contrasts))), check.names = FALSE)
}))
note <- paste("Note: American Community Survey (ACS), 2011 to 2019. Each cell tests that the coefficients for 4, 3 and 2",
              "years before expansion in the event-study model of Figures 1-3 (all covariates) are jointly zero, for one",
              "group or for the difference between two groups. Year -1 is the reference; the combined coefficient for 5 or",
              "more years before expansion, estimated only from the states that expanded in 2016 or 2019, is not tested.",
              sprintf("F statistics with 3 and %d degrees of freedom (the number of states minus 1),", res$df2[1]),
              "from standard errors clustered by state; p-values below. Because the coefficients are correlated, a joint",
              "test can reject when no single coefficient is significant, and the reverse.")
abbreviations <- "Abbreviations: ACS, American Community Survey; US, United States."
col_heads <- c("Outcome", "US-born", "Naturalized\ncitizen", "Non-citizen",
               "Naturalized\ncitizen minus\nUS-born", "Non-citizen\nminus\nUS-born", "Non-citizen\nminus\nnaturalized\ncitizen")
spanners <- c("Citizenship group" = 3, "Difference between groups" = 3)

## LaTeX: numbered as in `exhibits`
tex_body <- tab
tex_body[-1] <- lapply(tex_body[-1], function(x) sprintf("\\makecell[tr]{%s}", gsub("\n", "\\\\\\\\", gsub("<", "$<$", x))))
tex <- kableExtra::kbl(tex_body, col.names = sprintf("\\makecell[b%s]{%s}", c("l", rep("r", 6)), gsub("\n", "\\\\\\\\", col_heads)),
                       format = "latex", booktabs = TRUE, escape = FALSE, align = c("l", rep("r", 6)),
                       caption = title, linesep = "") |>
        kableExtra::add_header_above(c(" " = 1, spanners)) |>
        kableExtra::footnote(general = c(gsub("-1 is", "$-$1 is", note), abbreviations), general_title = "",
                             threeparttable = TRUE, escape = FALSE)
tex <- sub("\\begin{table}", paste0("\\begin{table}\\renewcommand{\\thetable}{", exhibit_number(name), "}"),
           tex, fixed = TRUE)
tex <- sub("\\centering", "\\centering\\renewcommand{\\baselinestretch}{1}\\footnotesize\\setlength{\\tabcolsep}{3pt}",
           tex, fixed = TRUE)
writeLines(tex, file.path(table_dir, paste0(name, ".tex")))

## Word: portrait, the F statistic and p-value on two lines
docx_body <- setNames(tab, c("label", paste0("c", 1:6)))
docx_body$label <- gsub("\n", " ", docx_body$label)
ft <- flextable::flextable(docx_body) |>
        flextable::set_header_labels(values = setNames(as.list(gsub("\n", " ", col_heads)), names(docx_body))) |>
        flextable::add_header_row(values = c("", names(spanners)), colwidths = c(1, 3, 3)) |>
        flextable::add_footer_lines(c(gsub("-1 is", "−1 is", note), abbreviations)) |>
        docx_style(spanner_cols = list(2:4, 5:7)) |>
        flextable::align(j = 2:7, align = "right", part = "all") |>
        flextable::align(i = 1, j = 2:7, align = "center", part = "header") |>
        flextable::width(j = 1, width = 1.4) |>
        flextable::width(j = 2:7, width = 0.85)
save_docx_table(ft, exhibit_number(name), title, file.path(table_dir, paste0(name, ".docx")))
