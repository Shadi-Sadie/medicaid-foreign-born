# Title: Table 1. Sample characteristics before the ACA, by citizenship and expansion status
# Author: Shadi Seyedi
# Description: Weighted (PWGTP) means and percentages for US-born adults, naturalized citizens
#              and non-citizens, in non-expansion and expansion states, 2011-2013. Each column
#              also gives its unweighted sample size and number of states. The table is
#              descriptive: it shows no tests or standardized differences, since the design does
#              not rest on the groups being balanced at baseline.
# Output: output/tables/table1.tex (LaTeX), table1.docx (Word) and table1.csv

source("code/00_setup.R")
Data <- make_sample(readRDS(data_file), sample_name)

library(dplyr)
library(kableExtra)


####################
## Data Preparation
####################

pre <- Data[Data$ACA == "Pre-ACA", ]
pre$group <- factor(pre$CITSTAT, levels = c("US-Born", "FB-Citizen", "FB-Non-citizen"),
                    labels = c("US-born", "Naturalized citizen", "Non-citizen"))
pre$state_group <- factor(pre$expansion, levels = c(0, 1), labels = c("Non-expansion", "Expansion"))

# Immigrant characteristics apply to the foreign-born only; NA leaves the cell blank for the
# US-born. The likely-undocumented flag (UNDOC) is an imputed status, not a measured
# characteristic, so it is described in the appendix rather than here.
fb <- pre$group != "US-born"
pre$YLIU_fb  <- ifelse(fb, pre$YLIU, NA)
pre$LTINU_fb <- ifelse(fb, pre$LTINU, NA)
pre$ENG_fb   <- factor(ifelse(fb, as.character(pre$ENG), NA), levels = levels(pre$ENG),
                       labels = sub("Only english", "Only English", levels(pre$ENG)))
pre$CORIGIN_fb <- factor(ifelse(fb, as.character(pre$CORIGIN), NA),
                         levels = setdiff(levels(pre$CORIGIN), "United States"))
# ADA scores are stored as 0-1; shown on the usual 0-100 scale
pre$ADA <- 100 * pre$ADA

# Rows of the table: variable, label, type ("pct" = 0/1 shown as %, "mean" = mean (SD),
# "cat" = one % row per level), and the panel it belongs to
rows <- tribble(
        ~var,         ~label,                            ~type,  ~panel,
        "HICOV",      "Any insurance",                   "pct",  "Health insurance coverage",
        "HINS4",      "Medicaid",                        "pct",  "Health insurance coverage",
        "HINS1",      "Employer-sponsored insurance",    "pct",  "Health insurance coverage",
        "HINS2",      "Direct purchase",                 "pct",  "Health insurance coverage",
        "AGEP",       "Age",                             "mean", "Demographics",
        "FEMALE",     "Female",                          "pct",  "Demographics",
        "RACE1",      "Race and ethnicity",              "cat",  "Demographics",
        "MARG",       "Married",                         "pct",  "Demographics",
        "DIS",        "Disability",                      "pct",  "Demographics",
        "SCHLG",      "Education",                       "cat",  "Socioeconomic status",
        "ESRG",       "Employment",                      "cat",  "Socioeconomic status",
        "POVPIPG",    "Income, % of poverty level",      "cat",  "Socioeconomic status",
        "YLIU_fb",    "Years in the US",                 "mean", "Immigrant characteristics",
        "LTINU_fb",   "Over 25% of life in the US",      "pct",  "Immigrant characteristics",
        "ENG_fb",     "English proficiency",             "cat",  "Immigrant characteristics",
        "CORIGIN_fb", "Region of birth",                 "cat",  "Immigrant characteristics",
        "UnempR",     "Unemployment rate, %",            "mean", "State context",
        "IPC",        "Immigrant Policy Climate",        "mean", "State context",
        "ADA",        "ADA score",                       "mean", "State context"
)


####################
## Weighted estimates
####################

wmean <- function(x, w) { ok <- !is.na(x); sum(w[ok] * x[ok]) / sum(w[ok]) }
wsd   <- function(x, w) { ok <- !is.na(x); m <- wmean(x, w); sqrt(sum(w[ok] * (x[ok] - m)^2) / sum(w[ok])) }
fmt_pct  <- function(p) if (is.na(p)) "" else sprintf("%.1f", 100 * p)
fmt_mean <- function(x, w) if (all(is.na(x))) "" else sprintf("%.1f (%.1f)", wmean(x, w), wsd(x, w))

# One column of the table: the cells for one citizenship group in one state group
column <- function(d) {
        w <- d$PWGTP
        unlist(lapply(seq_len(nrow(rows)), function(i) {
                x <- d[[rows$var[i]]]
                switch(rows$type[i],
                       pct  = fmt_pct(if (all(is.na(x))) NA else wmean(x, w)),
                       mean = fmt_mean(x, w),
                       cat  = c("", vapply(levels(x), function(l)
                               if (all(is.na(x))) "" else fmt_pct(wmean(as.numeric(x == l), w)), "")))
        }))
}

# Row labels in the same order as column(): each variable, with categorical variables followed
# by one row per level
row_labels <- unlist(lapply(seq_len(nrow(rows)), function(i) {
        if (rows$type[i] == "cat") c(rows$label[i], paste0("    ", levels(pre[[rows$var[i]]])))
        else rows$label[i]
}))
row_panels <- unlist(lapply(seq_len(nrow(rows)), function(i) {
        rep(rows$panel[i], if (rows$type[i] == "cat") 1 + nlevels(pre[[rows$var[i]]]) else 1)
}))


#######################
## Table 1
######################

# The unweighted sample size of each column and the number of states in each state group go in
# the column headers rather than in rows of their own
cols <- expand.grid(group = levels(pre$group), state_group = levels(pre$state_group))
cols$n <- sapply(seq_len(nrow(cols)), function(j)
        sum(pre$group == cols$group[j] & pre$state_group == cols$state_group[j]))
n_states <- tapply(pre$ST, pre$state_group, function(s) length(unique(s)))
n_states <- n_states[as.character(cols$state_group)]

table1 <- data.frame(Characteristic = row_labels,
                     sapply(seq_len(nrow(cols)), function(j)
                             column(pre[pre$group == cols$group[j] & pre$state_group == cols$state_group[j], ])),
                     check.names = FALSE, row.names = NULL)
n_label <- format(cols$n, big.mark = ",", trim = TRUE)
names(table1)[-1] <- sprintf("%s states (%d): %s (N = %s)", cols$state_group, n_states, cols$group, n_label)

write.csv(cbind(Panel = row_panels, table1), file.path(table_dir, "table1.csv"), row.names = FALSE)

# HSR: the title has no abbreviations or punctuation, and the table defines every abbreviation it
# uses, so that it can be read without the text
title <- "Characteristics of low-income adults before Medicaid expansion by citizenship and state expansion status"
note <- paste(
        "Note: American Community Survey (ACS), 2011 to 2013. Adults aged 26 to 64 in households with family income",
        "at or below 138% of the federal poverty level, excluding adults with Medicare, US citizens born in the US",
        "territories or abroad to American parents, non-citizens in the US fewer than 5 years, the five states that",
        "covered low-income adults before 2014 (Delaware, District of Columbia, Massachusetts, New York, Vermont),",
        "and the six states that expanded coverage partly before 2014 (California, Colorado, Connecticut, Minnesota,",
        "New Jersey, Washington). Values are percentages weighted with ACS person weights, except age, years in the",
        "US and the state context rows, which are weighted means (SD); n is the unweighted number of respondents and",
        "the number of states is in parentheses. Any insurance excludes Indian Health Service coverage alone",
        "(Census Bureau definition). Immigrant characteristics are for the foreign-born",
        "only. State context values are person-weighted means of the state's annual unemployment rate (BLS),",
        "Immigrant Policy Climate index without its health care items (Samari) and mean ADA score (0 to 100) of its",
        "members of Congress.",
        if (sample_name != "main") paste("Sensitivity sample:", sample_variants[[sample_name]]$why))
abbreviations <- paste("Abbreviations: ACS, American Community Survey; ADA, Americans for Democratic Action;",
                       "BLS, Bureau of Labor Statistics; SD, standard deviation; US, United States.")

spanners  <- unique(sprintf("%s states (%d)", cols$state_group, n_states))
level_rows <- grep("^    ", table1$Characteristic)
panel_rows <- rle(row_panels)

## LaTeX: 8-point and single-spaced even in a double-spaced manuscript, and a longtable that
## continues on the next page with the headers repeated, so that it never runs off a portrait
## letter page with 1-inch margins. The label column is 1.65 inches wide; a label that wraps
## continues with a hanging indent. Needs booktabs, longtable, makecell and threeparttablex (all
## in texts/preamble.tex).
# Group names on up to two lines and n below, bottom-aligned so the n's line up
tex_headers <- c("", sprintf("\\makecell[br]{%s\\\\n = %s}", sub(" ", "\\\\\\\\", cols$group), n_label))
# The headers hold LaTeX line breaks, so the body is escaped by hand; category rows are indented
# with add_indent() because LaTeX ignores leading spaces
tex_body <- table1
tex_body$Characteristic <- gsub(">", "$>$", gsub("%", "\\\\%", trimws(tex_body$Characteristic)))
tex <- kbl(tex_body, col.names = tex_headers, format = "latex", booktabs = TRUE, escape = FALSE,
           longtable = TRUE, align = c("l", rep("r", 6)), caption = title) %>%
        kable_styling(latex_options = "repeat_header", repeat_header_text = "\\textit{(continued)}") %>%
        add_header_above(setNames(c(1, 3, 3), c(" ", spanners))) %>%
        add_indent(level_rows) %>%
        column_spec(1, latex_column_spec = ">{\\\\raggedright\\\\arraybackslash\\\\hangindent=3em\\\\hangafter=1}p{1.65in}")
start <- 1
for (k in seq_along(panel_rows$values)) {
        tex <- pack_rows(tex, panel_rows$values[k], start, start + panel_rows$lengths[k] - 1)
        start <- start + panel_rows$lengths[k]
}
tex <- footnote(tex, general = c(note, abbreviations), general_title = "", threeparttable = TRUE)
writeLines(c(paste0("\\begingroup\\renewcommand{\\baselinestretch}{1}\\footnotesize",
                    "\\setlength{\\tabcolsep}{3pt}\\setlength{\\LTcapwidth}{\\textwidth}"),
             tex, "\\endgroup"),
           file.path(table_dir, "table1.tex"))

## Word: the same table on a portrait letter page with 1-inch margins, Times New Roman 10 point,
## horizontal rules only, and the header rows repeated when the table continues on the next page

# One bold row per panel, then its rows; "kind" sets the indent of the label
is_level <- seq_len(nrow(table1)) %in% level_rows
docx_body <- table1
docx_body$Characteristic <- trimws(docx_body$Characteristic)
names(docx_body) <- c("label", paste0("c", seq_len(nrow(cols))))
blank <- docx_body[1, ]
blank[] <- ""
docx_body <- do.call(rbind, lapply(unique(row_panels), function(p) {
        panel_row <- blank
        panel_row$label <- p
        rbind(panel_row, docx_body[row_panels == p, ])
}))
kind <- unlist(lapply(unique(row_panels), function(p)
        c("panel", ifelse(is_level[row_panels == p], "level", "item"))))

ft <- flextable::flextable(docx_body) %>%
        flextable::set_header_labels(values = setNames(
                as.list(c("", sprintf("%s\nn = %s", sub(" ", "\n", cols$group), n_label))), names(docx_body))) %>%
        flextable::add_header_row(values = c("", spanners), colwidths = c(1, 3, 3)) %>%
        flextable::add_footer_lines(c(note, abbreviations)) %>%
        docx_style(spanner_cols = list(2:4, 5:7)) %>%
        flextable::bold(i = which(kind == "panel"), j = 1) %>%
        flextable::padding(i = which(kind == "item"), j = 1, padding.top = 1, padding.bottom = 1, padding.left = 10) %>%
        flextable::padding(i = which(kind == "level"), j = 1, padding.top = 1, padding.bottom = 1, padding.left = 20) %>%
        flextable::align(j = 2:7, align = "right", part = "all") %>%
        flextable::align(i = 1, j = 2:7, align = "center", part = "header") %>%
        flextable::width(j = 1, width = 1.8) %>%
        flextable::width(j = 2:7, width = 0.78)
save_docx_table(ft, 1, title, file.path(table_dir, "table1.docx"))
