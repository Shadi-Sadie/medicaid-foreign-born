# Title: Tables 7 and 8. Event-study results, split by nativity
# Author: Shadi Seyedi
# Date: may 15, 2023
# Description: Event-study models estimated separately for US-born and foreign-born adults,
#              without and with controls. Table 7 is Medicaid coverage, Table 8 is uninsured.
#              Copied from codes/R/05-event-study.R (now in archive/). The draft ggiplot figures in
#              that script are not in the paper and are left out.
# Output: output/tables/table7.tex, table8.tex and the matching *_estimates.csv files.
# Note: uses `controls` and `foriegn` from code/00_setup.R (RECONSTRUCTED, see CHANGES.md).

source("code/00_setup.R")
load(data_file)

library(fixest)

####################
## Data Preparation
####################

# need to get a new sample for foregin and native
Forgn<- Data[Data$NATIVITY == "Foregin-born", ]
NATV<- Data[Data$NATIVITY == "US-born", ]


############################################################
## Table 8: Event study for Uninsured
############################################################

Event1 = feols(UNINS ~ i(ttot, expansion, ref = -1)|
                       ST + YEAR,                             ## FEs
               vcov = "hetero" ,
               weights = ~PWGTP,
               data = NATV)

Event2 = feols(UNINS ~ i(ttot, expansion, ref = -1)+.[controls]|
                       ST + YEAR,                             ## FEs
               vcov = "hetero" ,
               weights = ~PWGTP,
               data = NATV)

Event3 = feols(UNINS ~ i(ttot, expansion, ref = -1)|
                       ST + YEAR,                             ## FEs
               vcov = "hetero" ,
               weights = ~PWGTP,
               data = Forgn)

Event4 = feols(UNINS ~ i(ttot, expansion, ref = -1)+.[controls] + .[foriegn]|
                       ST + YEAR,                             ## FEs
               vcov = "hetero" ,
               weights = ~PWGTP,
               data = Forgn)

table8<-etable(list(Event1, Event2, Event3,Event4),
               digits = 3,
               title = "The Impact of Medicaid Expansion on Uninsured Rate ",
               headers = list("^:_:"= .("US-born" = 2, "Foreign-Born"=2)),
               tex = T,
               arraystretch = 0.5,
               vcov = "twoway",
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
               dict=c(ST = "State" , YEAR = "Year", ttot="year", UNINS="Uninsured Rate"),
               file = file.path(table_dir, "table8.tex"), replace = TRUE
)

save_estimates(lapply(list(Event1 = Event1, Event2 = Event2, Event3 = Event3, Event4 = Event4),
                      summary, vcov = "twoway"),
               file.path(table_dir, "table8_estimates.csv"))


############################################################
###  Table 7: Event study for Medicaid
############################################################

Event5 = feols(HINS4 ~ i(ttot, expansion, ref = -1)|
                       ST + YEAR,                             ## FEs
               vcov = "hetero" ,
               weights = ~PWGTP,
               data = NATV)

Event6 = feols(HINS4 ~ i(ttot, expansion, ref = -1)+.[controls]|
                       ST + YEAR,                             ## FEs
               vcov = "hetero" ,
               weights = ~PWGTP,
               data = NATV)

Event7 = feols(HINS4 ~ i(ttot, expansion, ref = -1)|
                       ST + YEAR,                             ## FEs
               vcov = "hetero" ,
               weights = ~PWGTP,
               data = Forgn)

Event8 = feols(HINS4 ~ i(ttot, expansion, ref = -1)+.[controls] + .[foriegn]|
                       ST + YEAR,                             ## FEs
               vcov = "hetero" ,
               weights = ~PWGTP,
               data = Forgn)

table7<-etable(list(Event5, Event6, Event7,Event8),
               title = "The Impact of Medicaid Expansion on Medicaid Coverage ",
               headers = list("^:_:"= .("US-born" = 2, "Foreign-Born"=2)),
               adjustbox= 0.6,
               arraystretch = 0.5,
               digits = 3,
               vcov = "twoway",
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
               dict=c(ST = "State" , YEAR = "Year", ttot="year", UNINS="Uninsured Rate"),
               file = file.path(table_dir, "table7.tex"), replace = TRUE
)

save_estimates(lapply(list(Event5 = Event5, Event6 = Event6, Event7 = Event7, Event8 = Event8),
                      summary, vcov = "twoway"),
               file.path(table_dir, "table7_estimates.csv"))
