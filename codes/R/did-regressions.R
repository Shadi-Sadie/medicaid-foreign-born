
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


## OLS Regressions Result

reg1 = feols(UNINS ~treat*ForeginBorn | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # state and year fixed effect
reg2 = feols(UNINS ~treat*ForeginBorn+.[adjs]  | ST + YEAR+ sw0(REGION^YEAR) , vcov = "hetero", weights = ~PWGTP, data = data) # state and year fixed effect
reg3 = feols(UNINS ~treat*ForeginBorn+.[adjs]  | ST + YEAR + ST[YEAR], vcov = "hetero", weights = ~PWGTP, data = data) # state and year fixed effect

reg4 = feols(UNINS ~ treat*ForeginBorn + ForeginBorn*.[controls]+ ForeginBorn*.[foriegn]| ST + YEAR +sw0(REGION^YEAR), vcov = "hetero", weights = ~PWGTP, data = data) # state and year fixed effect

reg5 = feols(UNINS ~ treat*ForeginBorn + ForeginBorn*.[controls]+ ForeginBorn*.[foriegn] | ST + YEAR + ST[YEAR], vcov = "hetero", weights = ~PWGTP, data = data) # state and year fixed effect

etable(reg1, reg2, reg3,reg4,reg5, 
       vcov = "twoway", markdown=TRUE)


## Logit Regressions Result
reg6 = feglm(UNINS ~treat*ForeginBorn | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data, family = 'logit') # state and year fixed effect
reg7 = feglm(UNINS ~treat*ForeginBorn+.[adjs]  | ST + YEAR+ sw0(REGION^YEAR) , vcov = "hetero", weights = ~PWGTP, data = data, family = 'logit') # state and year fixed effect
reg8 = feglm(UNINS ~treat*ForeginBorn+.[adjs]  | ST + YEAR + ST[YEAR], vcov = "hetero", weights = ~PWGTP, data = data, family = 'logit') # state and year fixed effect

reg9 = feglm(UNINS ~ treat*ForeginBorn + ForeginBorn*.[controls]+ ForeginBorn*.[foriegn]| ST + YEAR +sw0(REGION^YEAR), vcov = "hetero", weights = ~PWGTP, data = data, family = 'logit') # state and year fixed effect

reg0 = feglm(UNINS ~ treat*ForeginBorn + ForeginBorn*.[controls]+ ForeginBorn*.[foriegn] | ST + YEAR + ST[YEAR], vcov = "hetero", weights = ~PWGTP, data = data, family = 'logit') # state and year fixed effect


etable(reg6, reg7, reg3,reg8,reg9,reg0, 
       vcov = "twoway", markdown=TRUE)


## Full table result

tab<-
        etable(list(reg1,reg2,reg3, reg4,reg5,reg6,reg7,reg8, reg9,reg0), 
               title = "The Effect of Medicaid Expansion on Uninsured Rate (Difference-in-Differences Estimation)",
               headers = list("^:_:"= .("FE OLS" = 7, "FE LOGIT"=7)),
               # extralines=list("^^Uninsured"=.(" ")) ,
               group = list("-^Controls"=c(controls,foriegn)),
               digits = 3,
               adjustbox= 1.5,
               tex= TRUE,
               se.row = TRUE, 
               depvar= FALSE,
               family = FALSE,
               arraystretch = 1,
               convergence = T,
               fitstat = ~ n +r2 +ar2 + pr2+  aic + bic ,
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

print(tab)


################################################################################
#### Table 2
#### The effect of the Medicaid expansion on medicaid take-up.
#### (OLS Weighted) for all low-income adult aged 26-65
################################################################################

## OLS Regression 

reg1 = feols(HINS4 ~treat*ForeginBorn | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # state and year fixed effect
reg2 = feols(HINS4 ~treat*ForeginBorn+.[adjs]  | ST + YEAR+ sw0(REGION^YEAR) , vcov = "hetero", weights = ~PWGTP, data = data) # state and year fixed effect
reg3 = feols(HINS4 ~treat*ForeginBorn+.[adjs]  | ST + YEAR + ST[YEAR], vcov = "hetero", weights = ~PWGTP, data = data) # state and year fixed effect

reg4 = feols(HINS4 ~ treat*ForeginBorn + ForeginBorn*.[controls]+ ForeginBorn*.[foriegn]| ST + YEAR +sw0(REGION^YEAR), vcov = "hetero", weights = ~PWGTP, data = data) # state and year fixed effect

reg5 = feols(HINS4 ~ treat*ForeginBorn + ForeginBorn*.[controls]+ ForeginBorn*.[foriegn] | ST + YEAR + ST[YEAR], vcov = "hetero", weights = ~PWGTP, data = data) # state and year fixed effect
etable(reg1, reg2, reg3,reg4, reg5,
       vcov = "twoway", headers = c("(1)", "(2)", "(3)", "(4)"))


## Logit regressions


reg6 = feglm(HINS4 ~treat*ForeginBorn | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data, family = 'logit') # state and year fixed effect
reg7 = feglm(HINS4 ~treat*ForeginBorn+.[adjs]  | ST + YEAR+ sw0(REGION^YEAR) , vcov = "hetero", weights = ~PWGTP, data = data, family = 'logit') # state and year fixed effect
reg8 = feglm(HINS4 ~treat*ForeginBorn+.[adjs]  | ST + YEAR + ST[YEAR],vcov = "hetero", weights = ~PWGTP, data = data, family = 'logit') # state and year fixed effect

reg9 = feglm(HINS4 ~ treat*ForeginBorn + ForeginBorn*.[controls]+ ForeginBorn*.[foriegn]| ST + YEAR +sw0(REGION^YEAR), cluster = ~ST, weights = ~PWGTP, data = data, family = 'logit') # state and year fixed effect

reg0 = feglm(HINS4 ~ treat*ForeginBorn + ForeginBorn*.[controls]+ ForeginBorn*.[foriegn] | ST + YEAR + ST[YEAR], vcov = "hetero", weights = ~PWGTP, data = data, family = 'logit') # state and year fixed effect

etable(reg6, reg7, reg3,reg8,reg9,reg0, 
       vcov = "twoway", markdown=TRUE)


#### Full table Result

table2<-
        etable(list(reg1,reg2,reg3, reg4,reg5,reg6,reg7,reg8, reg9,reg0), 
               title = "The Effect of Medicaid Expansion on Medicaid Covergare Rate (Difference-in-Differences Estimation)",
               headers = list("^:_:"= .("FE OLS" = 7, "FE LOGIT"=7)),
               
               # extralines=list("^^Uninsured"=.(" ")) ,
               group = list("-^Controls"=c(controls,foriegn)),
               digits = 3,
               vcov = "hetero",
               adjustbox= 1.5,
               tex= TRUE,
               depvar= FALSE,
               family = FALSE,
               arraystretch = 1,
               convergence = T,
               se.row = TRUE, 
               
               fitstat = ~ n +r2 +ar2 + pr2+  aic + bic ,
               # notes = c("note 1", "source 1"),
               style.tex = style.tex(
                       main = "base",
                       fixef.suffix = " FE",
                       fixef.title = "\\midrule",
                       stats.title = "\\midrule",
                       # tabular = "*",
                       model.title = "",
                       #  model.format = "[i]",
                       fontsize= 'large', 
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

print(table2)