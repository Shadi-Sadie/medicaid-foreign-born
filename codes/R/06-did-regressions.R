
# Title: ## Difference-in-Differences Model Results
# Author: Shadi Seyedi
# Date: July 5, 2023
# Description:  This code section Includes codes for the DID analysis it include both ols and logit results with two tables



####################
## Required Packages
####################

library(fixest)


####################
## Data Preparation 
####################

Data$ForeginBorn <- ifelse(Data$NATIVITY == "Foregin-born", 1, 0)
# Make sure data are factor
data<-Data
factor_columns <- sapply(data, is.factor)
data[factor_columns] <- lapply(data[factor_columns], as.character)

data <- replace(data, is.na(data), 99999999)

data[factor_columns] <- lapply(data[factor_columns], as.factor)


################################################################################
#### Table 1
#### The effect of the Medicaid expansion on uninsured rate.
#### (OLS Weighted) for all low-income adult aged 26-65
################################################################################

# this is where I found good info on how to rotate the table
#  # # #"https://stackoverflow.com/questions/25849814/rstudio-rmarkdown-both-portrait-and-landscape-layout-in-a-single-pdf



outcome_labels <- c("UNINS" = "Uninsured", "HINS4" = "Medicaid", "HINS1" = "Employer-Sponsored", "HINS2" = "Directly Purchased")



################################################################################
#### Table 1
#### Unadjusted table
#### (OLS Weighted) for all low-income adult aged 26-65
################################################################################
reg = feols(c(UNINS,HINS4,HINS1,HINS2) ~ treat*ForeginBorn  | ST + YEAR  , vcov = "hetero", weights = ~PWGTP, data = Data) # Control for demographic + immigrants + state

table1<-
        etable(reg, 
               title = "Difference-in-Differences Result Without Controls",
               #headers = list("^:_:"= .("FE OLS" = 7, "FE LOGIT"=7)),
               # extralines=list("^^Uninsured"=.(" ")) ,
               #  group = list("-^Controls"=c(NICont, Intcontrol )),
               # extralines = list("-^Controls x Foreign-born" = c("No", "No", "No", "Yes", "Yes")), 
               digits = 3,
               #  adjustbox= 1.5,
               tex= TRUE,
               se.row = TRUE, 
               depvar= FALSE,
               family = FALSE,
               # arraystretch = 1,
               #convergence = T,
               fixef.group = T,
               # fitstat = ~ n +r2 +ar2 + pr2+  aic + bic ,
               # notes = c("note 1", "source 1"),
               style.tex = style.tex(
                       main = "base",
                       fixef.suffix = " FE",
                       fixef.title = "\\midrule",
                       stats.title = "\\midrule",
                       # tabular = "*",
                       model.title = "",
                       #  model.format = "[i]",
                       # fontsize= 'normalsize', 
                       tpt=T,
                       depvar.title= " ",
                       var.title = "\\midrule",
                       slopes.title = "",
                       slopes.format = "State-Specific Linear Time Trends",
                       yesNo = "yes"
               ) ,
               dict=c(ST = "State" , YEAR = "Year", UNINS="Uninsured Rate", treat='Medicaid Expansion',
                      ForeginBorn= 'Foreign-Born',UnempR= "State Unemployment Rate", ADA="State Political Ideology", REGION="Region"
               )
        )

#"UnempR","SEX","DIS", "AGEP","SCHLG", "MARG", "RACE1", "ESRG","ENG", "LTINU","NonCit", "IPC","POVPIPG","ADA"

## 


################################################################################
#### Table 2
#### Adjusted  table
#### (OLS Weighted) for all low-income adult aged 26-65
################################################################################

regadj = feols(c(UNINS,HINS4,HINS1,HINS2) ~ treat*ForeginBorn + SEX + DIS+AGEP+POVPIPG+ENG+LTINU+NonCit+ForeginBorn*(SCHLG+ MARG+ RACE1 + ESRG + ADA+UnempR+IPC) | ST + YEAR  , vcov = "hetero", weights = ~PWGTP, data = Data) # Control for demographic + immigrants + state

table2<-
        etable(regadj, 
               title = "Difference-in-Differences Result Without Controls",
               #headers = list("^:_:"= .("FE OLS" = 7, "FE LOGIT"=7)),
               # extralines=list("^^Uninsured"=.(" ")) ,
               group = list("-^Controls"=c("UnempR","SEX","DIS", "AGEP","SCHLG", "MARG", "RACE1", "ESRG","ENG", "LTINU","NonCit", "IPC","POVPIPG","ADA","Foreign-Born x State Political Ideology", "Foreign-Born x State Unemployment Rate","State Political Ideology","State Unemployment Rate")),
               # extralines=list("__otherControls"=c("Foreign-Born x State Political Ideology","Foreign-Born x State Unemployment Rate","State Political Ideology","State Unemployment Rate")),
               #extralines = list("-^Controls x Foreign-born" = c("No", "No", "No", "Yes", "Yes")), 
               digits = 3,
               #  adjustbox= 1.5,
               headers = c("Uninsured","Medicaid", "Employer-Sponsored", "Directly Purchased"),
               tex= TRUE,
               # se.row = TRUE, 
               depvar= FALSE,
               family = FALSE,
               arraystretch = 1,
               #convergence = T,
               #fixef.group = T,
               # fitstat = ~ n +r2 +ar2 + pr2+  aic + bic ,
               # notes = c("note 1", "source 1"),
               style.tex = style.tex(
                       main = "base",
                       fixef.suffix = " FE",
                       fixef.title = "\\midrule",
                       stats.title = "\\midrule",
                       # tabular = "*",
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
               )
        )
table2




################################################################################
#### Table 2
#### Specification  table appendix
#### (OLS Weighted) for all low-income adult aged 26-65
################################################################################


reg1 = feols(UNINS ~treat*ForeginBorn | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # state and year fixed effect


reg2 = feols(UNINS ~ treat*ForeginBorn + SEX + DIS+AGEP+POVPIPG+ENG+LTINU+NonCit+SCHLG+ MARG+ RACE1 + ESRG + ADA+UnempR+IPC| ST + YEAR  , vcov = "hetero", weights = ~PWGTP, data = data) # Control for demographic + immigrants + state


reg3 = feols(UNINS ~ treat*ForeginBorn  + SEX + DIS+AGEP+POVPIPG+ENG+LTINU+NonCit+ForeginBorn*(SCHLG+ MARG+ RACE1 + ESRG + ADA+UnempR+IPC) | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # Interaction with state + immigrants



table7<-  etable(list(reg1,reg2,reg3,reg4.reg5,reg6), 
                 title = "Medicaid Expansion on Uninsured Rate (Difference-in-Differences Estimation Specifications)",
                 #headers = list("^:_:"= .("FE OLS" = 7, "FE LOGIT"=7)),
                 # extralines=list("^^Uninsured"=.(" ")) ,
                 group = list("-^Controls"=c("UnempR","SEX","DIS", "AGEP","SCHLG", "MARG", "RACE1", "ESRG","ENG", "LTINU","NonCit", "IPC","POVPIPG","ADA","Foreign-Born x State Political Ideology","Foreign-Born x State Unemployment Rate","State Political Ideology","State Unemployment Rate")),
                 extralines = list("-^Controls x Foreign-born" = c("No", "No", "Yes")), 
                 digits = 3,
                 #  adjustbox= 1.5,
                 tex= TRUE,
                 #se.row = TRUE, 
                 depvar= FALSE,
                 family = FALSE,
                 # arraystretch = 1,
                 #convergence = T,
                 fixef.group = T,
                 # fitstat = ~ n +r2 +ar2 + pr2+  aic + bic ,
                 # notes = c("note 1", "source 1"),
                 style.tex = style.tex(
                         main = "base",
                         fixef.suffix = " FE",
                         fixef.title = "\\midrule",
                         stats.title = "\\midrule",
                         # tabular = "*",
                         model.title = "",
                         #  model.format = "[i]",
                         # fontsize= 'normalsize', 
                         tpt=T,
                         depvar.title= " ",
                         var.title = "\\midrule",
                         slopes.title = "",
                         slopes.format = "State-Specific Linear Time Trends",
                         yesNo = "yes"
                 ) ,
                 dict=c(ST = "State" , YEAR = "Year", UNINS="Uninsured Rate", treat='Medicaid Expansion',
                        ForeginBorn= 'Foreign-Born',UnempR= "State Unemployment Rate", ADA="State Political Ideology", REGION="Region"
                 )
)


table7



reg4 = feols(HINS4 ~treat*ForeginBorn | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = Data) # state and year fixed effect


reg5 = feols(HINS4 ~ treat*ForeginBorn + SEX + DIS+AGEP+POVPIPG+ENG+LTINU+NonCit+SCHLG+ MARG+ RACE1 + ESRG + ADA+UnempR+IPC| ST + YEAR  , vcov = "hetero", weights = ~PWGTP, data = Data) # Control for demographic + immigrants + state


reg6 = feols(HINS4 ~ treat*ForeginBorn  + SEX + DIS+AGEP+POVPIPG+ENG+LTINU+NonCit+ForeginBorn*(SCHLG+ MARG+ RACE1 + ESRG + ADA+UnempR+IPC) | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = Data) # Interaction with state + immigrants


table7<-  etable(list(reg4,reg5,reg6), 
                 title = "Medicaid Expansion on Uninsured Rate (Difference-in-Differences Estimation Specifications)",
                 #headers = list("^:_:"= .("FE OLS" = 7, "FE LOGIT"=7)),
                 # extralines=list("^^Uninsured"=.(" ")) ,
                 group = list("-^Controls"=c("UnempR","SEX","DIS", "AGEP","SCHLG", "MARG", "RACE1", "ESRG","ENG", "LTINU","NonCit", "IPC","POVPIPG","ADA","Foreign-Born x State Political Ideology","Foreign-Born x State Unemployment Rate","State Political Ideology","State Unemployment Rate")),
                 extralines = list("-^Controls x Foreign-born" = c("No", "No", "Yes")), 
                 digits = 3,
                 #  adjustbox= 1.5,
                 tex= TRUE,
                 #se.row = TRUE, 
                 depvar= FALSE,
                 family = FALSE,
                 # arraystretch = 1,
                 #convergence = T,
                 fixef.group = T,
                 # fitstat = ~ n +r2 +ar2 + pr2+  aic + bic ,
                 # notes = c("note 1", "source 1"),
                 style.tex = style.tex(
                         main = "base",
                         fixef.suffix = " FE",
                         fixef.title = "\\midrule",
                         stats.title = "\\midrule",
                         # tabular = "*",
                         model.title = "",
                         #  model.format = "[i]",
                         # fontsize= 'normalsize', 
                         tpt=T,
                         depvar.title= " ",
                         var.title = "\\midrule",
                         slopes.title = "",
                         slopes.format = "State-Specific Linear Time Trends",
                         yesNo = "yes"
                 ) ,
                 dict=c(ST = "State" , YEAR = "Year", UNINS="Uninsured Rate", treat='Medicaid Expansion',
                        ForeginBorn= 'Foreign-Born',UnempR= "State Unemployment Rate", ADA="State Political Ideology", REGION="Region"
                 )
)

## Panel C

reg4 = feols(HINS2 ~treat*ForeginBorn | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = Data) # state and year fixed effect


reg5 = feols(HINS2 ~ treat*ForeginBorn + SEX + DIS+AGEP+POVPIPG+ENG+LTINU+NonCit+SCHLG+ MARG+ RACE1 + ESRG + ADA+UnempR+IPC| ST + YEAR  , vcov = "hetero", weights = ~PWGTP, data = Data) # Control for demographic + immigrants + state


reg6 = feols(HINS2 ~ treat*ForeginBorn  + SEX + DIS+AGEP+POVPIPG+ENG+LTINU+NonCit+ForeginBorn*(SCHLG+ MARG+ RACE1 + ESRG + ADA+UnempR+IPC) | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = Data) # Interaction with state + immigrants


table7<-  etable(list(reg4,reg5,reg6), 
                 title = "Medicaid Expansion on Uninsured Rate (Difference-in-Differences Estimation Specifications)",
                 #headers = list("^:_:"= .("FE OLS" = 7, "FE LOGIT"=7)),
                 # extralines=list("^^Uninsured"=.(" ")) ,
                 group = list("-^Controls"=c("UnempR","SEX","DIS", "AGEP","SCHLG", "MARG", "RACE1", "ESRG","ENG", "LTINU","NonCit", "IPC","POVPIPG","ADA","Foreign-Born x State Political Ideology","Foreign-Born x State Unemployment Rate","State Political Ideology","State Unemployment Rate")),
                 extralines = list("-^Controls x Foreign-born" = c("No", "No", "Yes")), 
                 digits = 3,
                 #  adjustbox= 1.5,
                 tex= TRUE,
                 #se.row = TRUE, 
                 depvar= FALSE,
                 family = FALSE,
                 # arraystretch = 1,
                 #convergence = T,
                 fixef.group = T,
                 # fitstat = ~ n +r2 +ar2 + pr2+  aic + bic ,
                 # notes = c("note 1", "source 1"),
                 style.tex = style.tex(
                         main = "base",
                         fixef.suffix = " FE",
                         fixef.title = "\\midrule",
                         stats.title = "\\midrule",
                         # tabular = "*",
                         model.title = "",
                         #  model.format = "[i]",
                         # fontsize= 'normalsize', 
                         tpt=T,
                         depvar.title= " ",
                         var.title = "\\midrule",
                         slopes.title = "",
                         slopes.format = "State-Specific Linear Time Trends",
                         yesNo = "yes"
                 ) ,
                 dict=c(ST = "State" , YEAR = "Year", UNINS="Uninsured Rate", treat='Medicaid Expansion',
                        ForeginBorn= 'Foreign-Born',UnempR= "State Unemployment Rate", ADA="State Political Ideology", REGION="Region"
                 )
)



## Panel D


reg4 = feols(HINS1 ~treat*ForeginBorn | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = Data) # state and year fixed effect


reg5 = feols(HINS1 ~ treat*ForeginBorn + SEX + DIS+AGEP+POVPIPG+ENG+LTINU+NonCit+SCHLG+ MARG+ RACE1 + ESRG + ADA+UnempR+IPC| ST + YEAR  , vcov = "hetero", weights = ~PWGTP, data = Data) # Control for demographic + immigrants + state


reg6 = feols(HINS1 ~ treat*ForeginBorn  + SEX + DIS+AGEP+POVPIPG+ENG+LTINU+NonCit+ForeginBorn*(SCHLG+ MARG+ RACE1 + ESRG + ADA+UnempR+IPC) | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = Data) # Interaction with state + immigrants


etable(list(reg4,reg5,reg6), 
       title = "Medicaid Expansion on Uninsured Rate (Difference-in-Differences Estimation Specifications)",
       #headers = list("^:_:"= .("FE OLS" = 7, "FE LOGIT"=7)),
       # extralines=list("^^Uninsured"=.(" ")) ,
       group = list("-^Controls"=c("UnempR","SEX","DIS", "AGEP","SCHLG", "MARG", "RACE1", "ESRG","ENG", "LTINU","NonCit", "IPC","POVPIPG","ADA","Foreign-Born x State Political Ideology","Foreign-Born x State Unemployment Rate","State Political Ideology","State Unemployment Rate")),
       extralines = list("-^Controls x Foreign-born" = c("No", "No", "Yes")), 
       digits = 3,
       #  adjustbox= 1.5,
       tex= TRUE,
       #se.row = TRUE, 
       depvar= FALSE,
       family = FALSE,
       # arraystretch = 1,
       #convergence = T,
       fixef.group = T,
       # fitstat = ~ n +r2 +ar2 + pr2+  aic + bic ,
       # notes = c("note 1", "source 1"),
       style.tex = style.tex(
               main = "base",
               fixef.suffix = " FE",
               fixef.title = "\\midrule",
               stats.title = "\\midrule",
               # tabular = "*",
               model.title = "",
               #  model.format = "[i]",
               # fontsize= 'normalsize', 
               tpt=T,
               depvar.title= " ",
               var.title = "\\midrule",
               slopes.title = "",
               slopes.format = "State-Specific Linear Time Trends",
               yesNo = "yes"
       ) ,
       dict=c(ST = "State" , YEAR = "Year", UNINS="Uninsured Rate", treat='Medicaid Expansion',
              ForeginBorn= 'Foreign-Born',UnempR= "State Unemployment Rate", ADA="State Political Ideology", REGION="Region"
       )
)
