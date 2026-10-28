# Title: Table 6. Effect of Medicaid expansion on coverage by citizenship status
# Author: Shadi Seyedi
# Description: DiD models with the expansion effect split by US-born, foreign-born citizen and
#              foreign-born non-citizen. Copied from work.R lines 12-34 (now in archive/).
# Output: output/tables/table6.tex and table6_estimates.csv

source("code/00_setup.R")
Data <- make_sample(readRDS(data_file), sample_name)

library(fixest)

data <- Data

# FIX: the fixed effects are state x citizenship status and year x citizenship status, so that
# treat:CITSTAT compares each group with the US-born within the same state and year (the original
# had state and year only). Standard errors are clustered by state (the original used
# heteroskedasticity-robust errors).
reg1 = feols(HICOV ~ treat*CITSTAT  + SEX + DIS+AGEP+POVPIPG+ENG+LTINU+ForeginBorn*(SCHLG+ MARG+ RACE1 + ESRG + ADA+UnempR+IPC) | ST^CITSTAT + YEAR^CITSTAT , vcov = ~ST, weights = ~PWGTP, data = data) # Interaction with state + immigrants

reg2 = feols(HINS4 ~ treat*CITSTAT  + SEX + DIS+AGEP+POVPIPG+ENG+LTINU+ForeginBorn*(SCHLG+ MARG+ RACE1 + ESRG + ADA+UnempR+IPC) | ST^CITSTAT + YEAR^CITSTAT , vcov = ~ST, weights = ~PWGTP, data = data) # Interaction with state + immigrants

reg3 = feols(HINS2 ~ treat*CITSTAT  + SEX + DIS+AGEP+POVPIPG+ENG+LTINU+ForeginBorn*(SCHLG+ MARG+ RACE1 + ESRG + ADA+UnempR+IPC) | ST^CITSTAT + YEAR^CITSTAT , vcov = ~ST, weights = ~PWGTP, data = data) # Interaction with state + immigrants

reg4 = feols(HINS1 ~ treat*CITSTAT  + SEX + DIS+AGEP+POVPIPG+ENG+LTINU+ForeginBorn*(SCHLG+ MARG+ RACE1 + ESRG + ADA+UnempR+IPC) | ST^CITSTAT + YEAR^CITSTAT , vcov = ~ST, weights = ~PWGTP, data = data) # Interaction with state + immigrants

etable(reg1, reg2, reg4,reg3,
       digits = 4,
       tex = TRUE,
       file = file.path(table_dir, "table6.tex"), replace = TRUE)

save_estimates(list(HICOV = reg1, HINS4 = reg2, HINS1 = reg4, HINS2 = reg3),
               file.path(table_dir, "table6_estimates.csv"))
