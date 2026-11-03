# Title: Tables 7 and 8. Event-study results, split by nativity
# Author: Shadi Seyedi
# Date: may 15, 2023
# Description: Event-study models estimated separately for US-born and foreign-born adults,
#              without and with controls. Table 7 is Medicaid coverage, Table 8 is any coverage.
#              Copied from codes/R/05-event-study.R (archive/ in git commit 21c7fe0). The draft ggiplot figures in
#              that script are not in the paper and are left out.
# Output: output/tables/table7.tex, table8.tex and the matching *_estimates.csv files.
# Note: uses `controls` and `foriegn` from code/00_setup.R (RECONSTRUCTED, see CHANGES.md).

source("code/00_setup.R")
Data <- make_sample(readRDS(data_file), sample_name)

library(fixest)

####################
## Data Preparation
####################

# need to get a new sample for foregin and native
Forgn<- Data[Data$NATIVITY == "Foregin-born", ]
NATV<- Data[Data$NATIVITY == "US-born", ]

# FIX: standard errors clustered by state, the level at which expansion is assigned. The original
# estimated with heteroskedasticity-robust errors and reported two-way (state and year) clustered
# errors in the tables; with 9 years, clustering by year is unreliable.

############################################################
## Table 8: Event study for any coverage
############################################################

Event1 = feols(HICOV ~ i(ttot, expansion, ref = -1)|
                       ST + YEAR,                             ## FEs
               vcov = ~ST ,
               weights = ~PWGTP,
               data = NATV)

Event2 = feols(HICOV ~ i(ttot, expansion, ref = -1)+.[controls]|
                       ST + YEAR,                             ## FEs
               vcov = ~ST ,
               weights = ~PWGTP,
               data = NATV)

Event3 = feols(HICOV ~ i(ttot, expansion, ref = -1)|
                       ST + YEAR,                             ## FEs
               vcov = ~ST ,
               weights = ~PWGTP,
               data = Forgn)

Event4 = feols(HICOV ~ i(ttot, expansion, ref = -1)+.[controls] + .[foriegn]|
                       ST + YEAR,                             ## FEs
               vcov = ~ST ,
               weights = ~PWGTP,
               data = Forgn)

table8<-etable(list(Event1, Event2, Event3,Event4),
               digits = 3,
               title = "The Impact of Medicaid Expansion on Any Health Insurance Coverage ",
               headers = list("^:_:"= .("US-born" = 2, "Foreign-Born"=2)),
               tex = T,
               arraystretch = 0.5,
               group = list("_^Controls"=c(controls,foriegn)),
               interaction.combine = NULL,
               style.tex = style.tex(
                       main = "base",
                       fixef.suffix = " FE",
                       fixef.title = "",
                       stats.title = "",
                       interaction.combine = " ",
                       model.title = "Variables",
                       depvar.title= " ",
                       var.title = "\\midrule",
                       yesNo = "yes"),
               depvar=FALSE,
               fontsize = "small",
               dict=c(ST = "State" , YEAR = "Year", ttot="year", HICOV="Any Coverage"),
               file = file.path(table_dir, "table8.tex"), replace = TRUE
)

save_estimates(list(Event1 = Event1, Event2 = Event2, Event3 = Event3, Event4 = Event4),
               file.path(table_dir, "table8_estimates.csv"))


############################################################
###  Table 7: Event study for Medicaid
############################################################

Event5 = feols(HINS4 ~ i(ttot, expansion, ref = -1)|
                       ST + YEAR,                             ## FEs
               vcov = ~ST ,
               weights = ~PWGTP,
               data = NATV)

Event6 = feols(HINS4 ~ i(ttot, expansion, ref = -1)+.[controls]|
                       ST + YEAR,                             ## FEs
               vcov = ~ST ,
               weights = ~PWGTP,
               data = NATV)

Event7 = feols(HINS4 ~ i(ttot, expansion, ref = -1)|
                       ST + YEAR,                             ## FEs
               vcov = ~ST ,
               weights = ~PWGTP,
               data = Forgn)

Event8 = feols(HINS4 ~ i(ttot, expansion, ref = -1)+.[controls] + .[foriegn]|
                       ST + YEAR,                             ## FEs
               vcov = ~ST ,
               weights = ~PWGTP,
               data = Forgn)

table7<-etable(list(Event5, Event6, Event7,Event8),
               title = "The Impact of Medicaid Expansion on Medicaid Coverage ",
               headers = list("^:_:"= .("US-born" = 2, "Foreign-Born"=2)),
               adjustbox= 0.6,
               arraystretch = 0.5,
               digits = 3,
               group = list("_^Controls"=c(controls,foriegn)),
               interaction.combine = " ",
               tex=TRUE,
               depvar=FALSE,
               style.tex = style.tex(
                       main = "base",
                       fixef.suffix = " FE",
                       fixef.title = "",
                       stats.title = "",
                       interaction.combine = " ",
                       model.title = "Variables",
                       depvar.title= " ",
                       var.title = "\\midrule",
                       yesNo = "yes"),
               fontsize = "small",
               dict=c(ST = "State" , YEAR = "Year", ttot="year", HICOV="Any Coverage"),
               file = file.path(table_dir, "table7.tex"), replace = TRUE
)

save_estimates(list(Event5 = Event5, Event6 = Event6, Event7 = Event7, Event8 = Event8),
               file.path(table_dir, "table7_estimates.csv"))
