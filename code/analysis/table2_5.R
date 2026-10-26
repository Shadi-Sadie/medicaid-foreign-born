# Title: Tables 2 and 5. Difference-in-differences results
# Author: Shadi Seyedi
# Date: July 5, 2023
# Description: Table 5 is the DiD model without controls, Table 2 the model with controls.
#              Copied from codes/R/06-did-regressions.R lines 1-145 (now in archive/).
# Output: output/tables/table2.tex, table5.tex and the matching *_estimates.csv files.

source("code/00_setup.R")
load(data_file)

library(fixest)


################################################################################
#### Table 5
#### Unadjusted table
#### (OLS Weighted) for all low-income adult aged 26-65
################################################################################
reg = feols(c(UNINS,HINS4,HINS1,HINS2) ~ treat*ForeginBorn  | ST + YEAR  , vcov = "hetero", weights = ~PWGTP, data = Data) # Control for demographic + immigrants + state

table5<-
        etable(reg,
               title = "Difference-in-Differences Result Without Controls",
               digits = 3,
               tex= TRUE,
               se.row = TRUE,
               depvar= FALSE,
               family = FALSE,
               fixef.group = T,
               style.tex = style.tex(
                       main = "base",
                       fixef.suffix = " FE",
                       fixef.title = "\\midrule",
                       stats.title = "\\midrule",
                       model.title = "",
                       tpt=T,
                       depvar.title= " ",
                       var.title = "\\midrule",
                       slopes.title = "",
                       slopes.format = "State-Specific Linear Time Trends",
                       yesNo = "yes"
               ) ,
               dict=c(ST = "State" , YEAR = "Year", UNINS="Uninsured Rate", treat='Medicaid Expansion',
                      ForeginBorn= 'Foreign-Born',UnempR= "State Unemployment Rate", ADA="State Political Ideology", REGION="Region"
               ),
               file = file.path(table_dir, "table5.tex"), replace = TRUE
        )

save_estimates(reg, file.path(table_dir, "table5_estimates.csv"))


################################################################################
#### Table 2
#### Adjusted  table
#### (OLS Weighted) for all low-income adult aged 26-65
################################################################################

regadj = feols(c(UNINS,HINS4,HINS1,HINS2) ~ treat*ForeginBorn + SEX + DIS+AGEP+POVPIPG+ENG+LTINU+NonCit+ForeginBorn*(SCHLG+ MARG+ RACE1 + ESRG + ADA+UnempR+IPC) | ST + YEAR  , vcov = "hetero", weights = ~PWGTP, data = Data) # Control for demographic + immigrants + state

table2<-
        etable(regadj,
               # FIX: the original title said "Without Controls" for this controlled model.
               title = "Difference-in-Differences Result With Controls",
               group = list("-^Controls"=c("UnempR","SEX","DIS", "AGEP","SCHLG", "MARG", "RACE1", "ESRG","ENG", "LTINU","NonCit", "IPC","POVPIPG","ADA","Foreign-Born x State Political Ideology", "Foreign-Born x State Unemployment Rate","State Political Ideology","State Unemployment Rate")),
               digits = 3,
               headers = c("Uninsured","Medicaid", "Employer-Sponsored", "Directly Purchased"),
               tex= TRUE,
               depvar= FALSE,
               family = FALSE,
               arraystretch = 1,
               style.tex = style.tex(
                       main = "base",
                       fixef.suffix = " FE",
                       fixef.title = "\\midrule",
                       stats.title = "\\midrule",
                       model.title = "",
                       model.format = "[i]",
                       fontsize= 'normalsize',
                       tpt=T,
                       depvar.title= " ",
                       var.title = "\\midrule",
                       slopes.title = "",
                       slopes.format = "State-Specific Linear Time Trends",
                       yesNo = "yes"
               ) ,
               dict=c(ST = "State" , YEAR = "Year", UNINS="Uninsured Rate", treat='Medicaid Expansion',
                      ForeginBorn= 'Foreign-Born',UnempR= "State Unemployment Rate", ADA="State Political Ideology", REGION="Region"
               ),
               file = file.path(table_dir, "table2.tex"), replace = TRUE
        )

save_estimates(regadj, file.path(table_dir, "table2_estimates.csv"))
