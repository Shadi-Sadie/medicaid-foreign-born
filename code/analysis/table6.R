# Title: Table 6. Effect of Medicaid expansion on coverage by citizenship status
# Author: Shadi Seyedi
# Description: DiD models with the expansion effect split by US-born, foreign-born citizen and
#              foreign-born non-citizen. Copied from work.R lines 12-34 (now in archive/).
# Output: output/tables/table6.tex and table6_estimates.csv

source("code/00_setup.R")
load(data_file)

library(fixest)

data <- Data

# FIX: the original code looked for "US-citizen Born abroad " (lower-case "abroad", trailing space),
# but the data spell it "US-citizen Born Abroad". With the old string, 16,187 foreign-born citizens
# were counted as US-born.
data$CITSTAT<-"US-Born"
data$CITSTAT <- ifelse(data$CIT %in% c("Naturalized-citizen", "US-citizen Born Abroad"),
                       "FB-Citizen", data$CITSTAT)
data$CITSTAT<-ifelse(data$CIT == "Non-citizen", "FB-Non-citizen", data$CITSTAT)
data$CITSTAT <- factor(data$CITSTAT, levels = c("US-Born", "FB-Citizen", "FB-Non-citizen"))

reg1 = feols(UNINS ~ treat*CITSTAT  + SEX + DIS+AGEP+POVPIPG+ENG+LTINU+ForeginBorn*(SCHLG+ MARG+ RACE1 + ESRG + ADA+UnempR+IPC) | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # Interaction with state + immigrants

reg2 = feols(HINS4 ~ treat*CITSTAT  + SEX + DIS+AGEP+POVPIPG+ENG+LTINU+ForeginBorn*(SCHLG+ MARG+ RACE1 + ESRG + ADA+UnempR+IPC) | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # Interaction with state + immigrants

reg3 = feols(HINS2 ~ treat*CITSTAT  + SEX + DIS+AGEP+POVPIPG+ENG+LTINU+ForeginBorn*(SCHLG+ MARG+ RACE1 + ESRG + ADA+UnempR+IPC) | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # Interaction with state + immigrants

reg4 = feols(HINS1 ~ treat*CITSTAT  + SEX + DIS+AGEP+POVPIPG+ENG+LTINU+ForeginBorn*(SCHLG+ MARG+ RACE1 + ESRG + ADA+UnempR+IPC) | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # Interaction with state + immigrants

etable(reg1, reg2, reg4,reg3,
       digits = 4,
       tex = TRUE,
       file = file.path(table_dir, "table6.tex"), replace = TRUE)

save_estimates(list(UNINS = reg1, HINS4 = reg2, HINS1 = reg4, HINS2 = reg3),
               file.path(table_dir, "table6_estimates.csv"))
