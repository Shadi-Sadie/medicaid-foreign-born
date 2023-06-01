#The purpuse of this scrip is to assign the suvery weights and create the 
# baseline equivalence tables which will be table 1, table2 and table 3
# Data: Apr 17, 2023
Data<-read.csv("CLNACS.csv", header = TRUE, sep ="," , fill = TRUE)


## Loading required libraries
library(tidycensus)
library(tidyverse)

#install.packages("survey")
library(gtsummary)
library(survey)
#install.packages("srvyr")
library("srvyr")


# check the Unweighted summary statistics
UNWTable<-Data %>% tbl_summary(by = expansion,
include = c(DIS, SEX,  MARG ,SCHLG ,ETHN, ESRG, POVPIPG, RACE,AGEP))%>%
  add_p()
#Setting the survey weight
#weights for the whole data 
  svy <- as_survey(Data, weight = PWGTP , repweights = matches("PWGTP[0-9]+"),
                  type = "JK1", scale = 4/ 80 , rscales = rep(1, 80 ), mse = TRUE)
#weights for the native data 
  svyn <- as_survey(NATV, weight = PWGTP , repweights = matches("PWGTP[0-9]+"),
                   type = "JK1", scale = 4/ 80 , rscales = rep(1, 80 ), mse = TRUE)
#weights for the foreign data 
  svyf <- as_survey(Forgn, weight = PWGTP , repweights = matches("PWGTP[0-9]+"),
                    type = "JK1", scale = 4/ 80 , rscales = rep(1, 80 ), mse = TRUE)
  
# weighted summary statistics for whole data
wesumtable <- svy %>%  tbl_svysummary(
    by = expansion,
    include = c(DIS, SEX,  MARG ,SCHLG ,ETHN, ESRG, POVPIPG, RACE,AGEP)
  ) %>%
  add_p()

print(wesumtable)

### Creating table-1: # weighted summary statistics for native data
table1 <- svyn %>%  tbl_svysummary(
  by = expansion,
  include = c(DIS, SEX,  MARG ,SCHLG ,ETHN, ESRG, POVPIPG, RACE,AGEP)
) %>%
  add_p()
print(table2)

#as_gt(table1) %>%
  #gt::as_latex()

### Creating table-2: weighted summary statistics for foregin born data

table2 <- svyf %>%  tbl_svysummary(
  by = expansion,
  include = c(DIS, SEX,  MARG ,SCHLG ,ETHN, ESRG, POVPIPG, RACE,AGEP, ENG, CIT, PLIU, GEOR, YLIU)
) %>%
  add_p()
print(table2)
# export the table to latex 
#as_gt(table2) %>%
 # gt::as_latex()

### Creating table-3 - Insurance 


table3<-svy %>%
  tbl_strata(
    strata = expansion,
    .tbl_fun =
      ~ .x %>% tbl_svysummary(
        by = NATIVITY,
        include = c(HICOV, HINS1, HINS2, HINS3,HINS4,HINS5,HINS6,HINS7),
      )%>%
      add_p()
  )

print(table3)

as_gt(table3) %>%
  gt::as_latex()
  









