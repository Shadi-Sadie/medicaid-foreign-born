## Difference-in-Differences Model Results

#include region-by-year fixed effects contains state-level time varying
#demographic and other policy controls. state-specific trend

### Weighted OLS

#### Table 1: The effect of the Medicaid expansion on uninsured rate.(OLS Weighted) for all low-income adult aged 26-65


reg1 = feols(UNINS ~ treat | ST + YEAR , cluster = ~ST, weights = ~PWGTP, data = Data) # state and year fixed effect
reg2 = feols(UNINS ~ treat | ST + YEAR + REGION^YEAR,cluster = ~ST, weights = ~PWGTP, data = Data) # State & Year & Region by year
formulareg3 <- as.formula(paste("UNINS ~  treat +",                                  # all+ time-varying controls
                                paste(controls, collapse = " + ") ,
                                "+UnempR+ ADA",
                                
                                "| ST + YEAR + REGION^YEAR"))
reg3 <- feols(formulareg3, cluster = ~ST, weights = ~PWGTP, data = Data)

formulareg4 <- as.formula(paste("UNINS ~  treat +",                                  # all+ time-varying controls
                                paste(controls, collapse = " + ") ,
                                "+UnempR+ ADA",
                                "| ST + YEAR + REGION^YEAR + ST[YEAR]"))

reg4 <- feols(formulareg4, cluster = ~ST, weights = ~PWGTP, data = Data)


etable(reg1, reg2, reg3,reg4, headers = c("(1)", "(2)", "(3)", "(4)"),
       vcov = "twoway", markdown=TRUE)


#### Table 2: The effect of the Medicaid expansion on medicaid take-up.(OLS Weighted) for all low-income adult aged 26-65


reg1 = feols(HINS4 ~ treat | ST + YEAR , cluster = ~ST, weights = ~PWGTP, data = Data) # state and year fixed effect
reg2 = feols(HINS4 ~ treat | ST + YEAR + REGION^YEAR,cluster = ~ST, weights = ~PWGTP, data = Data) # State & Year & Region by year
formulareg3 <- as.formula(paste("HINS4 ~  treat +",                                  # all+ time-varying controls
                                paste(controls, collapse = " + ") ,
                                "+UnempR+ ADA",
                                
                                "| ST + YEAR + REGION^YEAR"))
reg3 <- feols(formulareg3, cluster = ~ST, weights = ~PWGTP, data = Data)

formulareg4 <- as.formula(paste("HINS4 ~  treat +",                                  # all+ time-varying controls
                                paste(controls, collapse = " + ") ,
                                "+UnempR+ ADA",
                                "| ST + YEAR + REGION^YEAR + ST[YEAR]"))

reg4 <- feols(formulareg4, cluster = ~ST, weights = ~PWGTP, data = Data)

etable(reg1, reg2, reg3,reg4,
       vcov = "twoway", headers = c("(1)", "(2)", "(3)", "(4)"))



#### Table 3: The effect of the Medicaid expansion on uninsured.(OLS Weighted) for all US-Born adult with low-income adult and aged 26-65


reg1 = feols(UNINS ~ treat | ST + YEAR , cluster = ~ST, weights = ~PWGTP, data = NATV) # state and year fixed effect
reg2 = feols(UNINS ~ treat | ST + YEAR + REGION^YEAR,cluster = ~ST, weights = ~PWGTP, data = NATV) # State & Year & Region by year
formulareg3 <- as.formula(paste("UNINS ~  treat +",                                  # all+ time-varying controls
                                paste(controls, collapse = " + ") ,
                                "+UnempR+ ADA",
                                
                                "| ST + YEAR + REGION^YEAR"))
reg3 <- feols(formulareg3, cluster = ~ST, weights = ~PWGTP, data = NATV)

formulareg4 <- as.formula(paste("UNINS ~  treat +",                                  # all+ time-varying controls
                                paste(controls, collapse = " + ") ,
                                "+UnempR+ ADA",
                                "| ST + YEAR + REGION^YEAR + ST[YEAR]"))

reg4 <- feols(formulareg4, cluster = ~ST, weights = ~PWGTP, data = NATV)

etable(reg1, reg2, reg3,reg4,
       vcov = "twoway", headers = c("(1)", "(2)", "(3)", "(4)"))


#### Table 4: The effect of the Medicaid expansion on medicaid take-up.(OLS Weighted) for US-Born adult low-income adult aged 26-65


reg1 = feols(HINS4 ~ treat | ST + YEAR , cluster = ~ST, weights = ~PWGTP, data = NATV) # state and year fixed effect
reg2 = feols(HINS4 ~ treat | ST + YEAR + REGION^YEAR,cluster = ~ST, weights = ~PWGTP, data = NATV) # State & Year & Region by year
formulareg3 <- as.formula(paste("HINS4 ~  treat +",                                  # all+ time-varying controls
                                paste(controls, collapse = " + ") ,
                                "+UnempR+ ADA",
                                "| ST + YEAR + REGION^YEAR"))
reg3 <- feols(formulareg3, cluster = ~ST, weights = ~PWGTP, data = NATV)

formulareg4 <- as.formula(paste("HINS4 ~  treat +",                                  # all+ time-varying controls
                                paste(controls, collapse = " + ") ,
                                "+UnempR+ ADA",
                                "| ST + YEAR + REGION^YEAR + ST[YEAR]"))

reg4 <- feols(formulareg4, cluster = ~ST, weights = ~PWGTP, data = NATV)

etable(reg1, reg2, reg3,reg4,
       vcov = "twoway", headers = c("(1)", "(2)", "(3)", "(4)"))


#### Table 5: The effect of the Medicaid expansion on Uninsured rate.(OLS Weighted) for Foreign-Born low-income adult aged 26-65


reg1 = feols(UNINS ~ treat | ST + YEAR , cluster = ~ST, weights = ~PWGTP, data = Forgn) # state and year fixed effect
reg2 = feols(UNINS ~ treat | ST + YEAR + REGION^YEAR,cluster = ~ST, weights = ~PWGTP, data = Forgn) # State & Year & Region by year
formulareg3 <- as.formula(paste("UNINS ~  treat +",                                  # all+ time-varying controls
                                paste(controls, collapse = " + ") ,
                                "+UnempR+ ADA+ IPCINDX",
                                "+ENG +YLIU+ GEOR",
                                "| ST + YEAR + REGION^YEAR"))
reg3 <- feols(formulareg3, cluster = ~ST, weights = ~PWGTP, data = Forgn)

formulareg4 <- as.formula(paste("UNINS ~  treat +",                                  # all+ time-varying controls
                                paste(controls, collapse = " + ") ,
                                "+UnempR+ ADA+ IPCINDX",
                                "+ENG +YLIU + GEOR",
                                "| ST + YEAR + REGION^YEAR + ST[YEAR]"))

reg4 <- feols(formulareg4, cluster = ~ST, weights = ~PWGTP, data = Forgn)

etable(reg1, reg2, reg3,reg4,
       vcov = "twoway", headers = c("(1)", "(2)", "(3)", "(4)"))


#### Table 6: The effect of the Medicaid expansion on Medicaid take-up.(OLS Weighted) for Foreign-Born low-income adult aged 26-65


reg1 = feols(HINS4 ~ treat | ST + YEAR , cluster = ~ST, weights = ~PWGTP, data = Forgn) # state and year fixed effect
reg2 = feols(HINS4 ~ treat | ST + YEAR + REGION^YEAR,cluster = ~ST, weights = ~PWGTP, data = Forgn) # State & Year & Region by year
formulareg3 <- as.formula(paste("HINS4 ~  treat +",                                  # all+ time-varying controls
                                paste(controls, collapse = " + ") ,
                                "+UnempR+ ADA+ IPCINDX",
                                "+ENG +YLIU+ GEOR",
                                "| ST + YEAR + REGION^YEAR"))
reg3 <- feols(formulareg3, cluster = ~ST, weights = ~PWGTP, data = Forgn)

formulareg4 <- as.formula(paste("HINS4 ~  treat +",                                  # all+ time-varying controls
                                paste(controls, collapse = " + ") ,
                                "+UnempR+ ADA+ IPCINDX",
                                "+ENG +YLIU + GEOR",
                                "| ST + YEAR + REGION^YEAR + ST[YEAR]"))

reg4 <- feols(formulareg4, cluster = ~ST, weights = ~PWGTP, data = Forgn)

etable(reg1, reg2, reg3,reg4,
       vcov = "twoway", headers = c("(1)", "(2)", "(3)", "(4)"))



### Generalized linear model (Logit)

#### Table 7: The effect of the Medicaid expansion on uninsured rate.(OLS Weighted) for all low-income adult aged 26-65


reg1 = feglm(UNINS ~ treat | ST + YEAR + sw0(REGION^YEAR), cluster = ~ST, weights = ~PWGTP, data = Data, family = 'logit') #
reg2 = feglm(UNINS ~ treat +.[controls]+ .[state] | ST + YEAR + REGION^YEAR+ sw0(ST[YEAR]), cluster = ~ST, weights = ~PWGTP, data = Data, family = 'logit') #


etable(reg1,reg2)


#### Table 8: The effect of the Medicaid expansion on medicaid take-up.(LOGIT) for all low-income adult aged 26-65


reg1 = feglm(HINS4 ~ treat | ST + YEAR + sw0(REGION^YEAR), cluster = ~ST, weights = ~PWGTP, data = Data, family = 'logit') #
reg2 = feglm(HINS4 ~ treat +.[controls]+ .[state]| ST + YEAR + REGION^YEAR+ sw0(ST[YEAR]), cluster = ~ST, weights = ~PWGTP, data = Data, family = 'logit') #

etable(reg1,reg2)



#### Table 9: The effect of the Medicaid expansion on uninsured.(LOGIT) for all US-Born adult with low-income adult and aged 26-65


reg1 = feglm(UNINS ~ treat | ST + YEAR + sw0(REGION^YEAR), cluster = ~ST, weights = ~PWGTP, data = NATV, family = 'logit') #
reg2 = feglm(UNINS ~ treat +.[controls]+ .[state]| ST + YEAR + REGION^YEAR+ sw0(ST[YEAR]), cluster = ~ST, weights = ~PWGTP, data = NATV, family = 'logit') #
etable(reg1,reg2)


#### Table 10: The effect of the Medicaid expansion on medicaid take-up.(OLS Weighted) for US-Born adult low-income adult aged 26-65


reg1 = feglm(HINS4 ~ treat | ST + YEAR + sw0(REGION^YEAR), cluster = ~ST, weights = ~PWGTP, data = NATV, family = 'logit') #
reg2 = feglm(HINS4 ~ treat +.[controls]+ .[state]| ST + YEAR + REGION^YEAR+ sw0(ST[YEAR]), cluster = ~ST, weights = ~PWGTP, data = NATV, family = 'logit') #
etable(reg1,reg2)


#### Table 11: The effect of the Medicaid expansion on Uninsured rate for Foreign-Born low-income adult aged 26-65

reg1 = feglm(UNINS ~ treat | ST + YEAR + sw0(REGION^YEAR), cluster = ~ST, weights = ~PWGTP, data = Forgn, family = 'logit') #
reg2 = feglm(UNINS ~ treat +.[controls]+ .[foriegn]+.[statefor]| ST + YEAR + REGION^YEAR+ sw0(ST[YEAR]), cluster = ~ST, weights = ~PWGTP, data = Forgn, family = 'logit') #
etable(reg1,reg2)


#### Table 12: The effect of the Medicaid expansion on Medicaid take-up for Foreign-Born low-income adult aged 26-65

reg1 = feglm(HINS4 ~ treat | ST + YEAR + sw0(REGION^YEAR), cluster = ~ST, weights = ~PWGTP, data = Forgn, family = 'logit') #
reg2 = feglm(HINS4 ~ treat +.[controls]+ .[foriegn]+.[statefor]| ST + YEAR + REGION^YEAR+ sw0(ST[YEAR]), cluster = ~ST, weights = ~PWGTP, data = Forgn, family = 'logit') #
etable(reg1,reg2)

