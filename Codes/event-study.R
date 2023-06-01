# Title: Event study and parallel trend assumption
# Author: Shadi Seyedi
# Date: may 15, 2023
# Description:  This code section is written for 
# Event Study and Parrallel trend check 
 
####################
## Required Packages
####################
#install.packages("ggiplot", repos = "https://grantmcdermott.r-universe.dev")
#install.packages("ggpubr")

library(ggiplot)
library(ggpubr)

####################
## Data Preparation 
####################

NATV<- Data[Data$NATIVITY == "US-born", ]
Forgn<- Data[Data$NATIVITY == "Foregin-born", ]

#--- global variables
table(Data$NATIVITY)

controls <- c("SEX","DIS", "AGEP","MARG", "SCHLG", "RACE1","ESRG","POVPIPG")
foriegn <- c( "ENG",  "YLIU", "PLIU", "GEOR", "CULRG" )
state<-c("UnempR", "ADA")
statefor<-c("UnempR", "ADA","IPCINDX")
weights<-Data %>% select(starts_with("PWGTP")) %>% colnames


# In my data set for those state that the did not expand medicaid the expansion year is equal to 0

# I first Create a "time_to_treatment" variable for each state, so that treatment is
# relative for all treated units. For the never-treated (i.e. control) units,
# we'll arbitrarily set the "time_to_treatment" value at 0. This value 
# doesn't really matter, since it will be canceled by the treat==0 interaction
# anyway. But we do want to make sure they aren't NA, otherwise feols would drop 
# these never-treated observations at estimation time and our results will be 
# off.

Data$ttot<-ifelse(Data$expansion == "Adopted", Data$YEAR - Data$ExpansionY, 0)

#Becausae expansion was a factor variable I had problme with the feols , so I changed it back to dummy

class(Data$expansion)
table(Data$expansion)
Data$expansion<-as.numeric(Data$expansion)
Data$expansion<-ifelse(Data$expansion==2,0,Data$expansion)



############################################################
## Event study for the whole population with no control
############################################################

## Uninured rate

Event1 = feols(UNINS ~ i(ttot, expansion, ref = -1)|                 
                       ST + YEAR,                             ## FEs
               cluster = ~ST,                          ## Clustered SEs
               weights = ~PWGTP,
               data = Data)

## Medicaid take-up

Event2 = feols(HINS4 ~ i(ttot, expansion, ref = -1)|                 
                       ST + YEAR,                             ## FEs
               cluster = ~ST,                          ## Clustered SEs
               weights = ~PWGTP,
               data = Data
)

ggiplot(
        list('Uninsured rate' = Event1, 'Medicaid Take-up' = Event2),
        main = 'Event study: Staggered treatment (TWFE)',
        xlab = 'Time to treatment',
        multi_style = 'facet',
        geom_style = "ribbon",
        facet_args = list(labeller = labeller(id = \(x) gsub(".*: ", "", x))),
        theme = theme_minimal() +
                theme(
                        text = element_text(family = "HersheySans"),
                        plot.title = element_text(hjust = 0.5),
                        legend.position = "none"
                )
)

############################################################
## ###  Event study for the foreign and Native born 
############################################################

# Uninsured
Event1 = feols(UNINS ~ i(ttot, expansion, ref = -1) |                 
                         ST + YEAR,                             ## FEs
                 cluster = ~ST,                          ## Clustered SEs
                weights = ~PWGTP,
                 data = Data,
                split = ~NATIVITY)
# Medicaid Take-up
Event2 = feols(HINS4 ~ i(ttot, expansion, ref = -1) |                 
                     ST + YEAR,                             ## FEs
             cluster = ~ST,                          ## Clustered SEs
             weights = ~PWGTP,
             data = Data,
             split = ~NATIVITY)

ggiplot(
        list('Uninsured rate' = Event1, 'Medicaid Take-up' = Event2),
        main = 'Event study: Staggered treatment (TWFE)',
        xlab = 'Time to treatment',
        multi_style = 'facet',
        geom_style = "ribbon",
        facet_args = list(labeller = labeller(id = \(x) gsub(".*: ", "", x))),
        theme = theme_minimal() +
                theme(
                        text = element_text(family = "HersheySans"),
                        plot.title = element_text(hjust = 0.5),
                        legend.position = "none"
                )
        )

############################################################
## Event study for the whole population with control
############################################################

# Uninsured
Event1 = feols(UNINS ~ i(ttot, expansion, ref = -1)+.[controls]|                 
                       ST + YEAR,                             ## FEs
               cluster = ~ST,                          ## Clustered SEs
               weights = ~PWGTP,
               data = Data)

# Medicaid Take-up
Event2 = feols(HINS4 ~ i(ttot, expansion, ref = -1)+.[controls]|                 
                       ST + YEAR,                             ## FEs
               cluster = ~ST,                          ## Clustered SEs
               weights = ~PWGTP,
               data = Data
)

ggiplot(
        list('Uninsured rate' = Event1, 'Medicaid Take-up' = Event2),
        main = 'Event study: Staggered treatment (TWFE)',
        xlab = 'Time to treatment',
        multi_style = 'facet',
        geom_style = "ribbon",
        facet_args = list(labeller = labeller(id = \(x) gsub(".*: ", "", x))),
        theme = theme_minimal() +
                theme(
                        text = element_text(family = "HersheySans"),
                        plot.title = element_text(hjust = 0.5),
                        legend.position = "none"
                )
)

#################################################################
## Event study for the for the foreign and Native born with control
#################################################################

# Uninsured
Event1 = feols(UNINS ~ i(ttot, expansion, ref = -1) +.[controls]|                 
                       ST + YEAR,                             ## FEs
               cluster = ~ST,                          ## Clustered SEs
               weights = ~PWGTP,
               data = Data,
               split = ~NATIVITY)
# Medicaid Take-up
Event2 = feols(HINS4 ~ i(ttot, expansion, ref = -1) +.[controls]|                 
                       ST + YEAR,                             ## FEs
               cluster = ~ST,                          ## Clustered SEs
               weights = ~PWGTP,
               data = Data,
               split = ~NATIVITY)

ggiplot(
        list('Uninsured rate' = Event1, 'Medicaid Take-up' = Event2),
        main = 'Event study: Staggered treatment (TWFE)',
        xlab = 'Time to treatment',
        multi_style = 'facet',
        geom_style = "ribbon",
        facet_args = list(labeller = labeller(id = \(x) gsub(".*: ", "", x))),
        theme = theme_minimal() +
                theme(
                        text = element_text(family = "HersheySans"),
                        plot.title = element_text(hjust = 0.5),
                        legend.position = "none"
                )
)



#################################################################
## Event study for whole sample with control for the whole population  vs  Sun & Abraham 
#################################################################

### Event study 

df<-Data
# Following Sun and Abraham, we give our never-treated units a fake "treatment"
# date far outside the relevant study period.

df$year_treated <- ifelse(df$expansion == 0, 10000, df$ExpansionY)

# Now we re-run our model from earlier, but this time swapping out 
# `i(time_to_treat, treat, ref = -1)` for `sunab(year_treated, year)`.
# See `?sunab`.

mod_sa = feols(UNINS ~ sunab(year_treated, YEAR) +.[controls] |
                       ST + YEAR,
               cluster = ~ST,
               weights = ~PWGTP,
               data = df)



twfe = feols(UNINS ~ i(ttot, expansion, ref = -1)+.[controls]|                 
                       ST + YEAR,                             ## FEs
               cluster = ~ST,                          ## Clustered SEs
               weights = ~PWGTP,
               data = Data)


UNinPlot<-ggiplot(
        list("TWFE,Event-study" = twfe, "Sun & Abraham (2020)" = mod_sa),
        main = 'Uninsured: Event study: Staggered treatment (TWFE)',
        xlab = 'Time to treatment',
        ref.line = -1
                )


mod_saM = feols(HINS4 ~ sunab(year_treated, YEAR) +.[controls] |
                       ST + YEAR,
               cluster = ~ST,
               weights = ~PWGTP,
               data = df)




twfeM = feols(HINS4 ~ i(ttot, expansion, ref = -1)+.[controls]|                 
                       ST + YEAR,                             ## FEs
               cluster = ~ST,                          ## Clustered SEs
               weights = ~PWGTP,
               data = Data
)



MedicPlot<-ggiplot(
        list("TWFE,Event-study" = twfeM, "Sun & Abraham (2020)" = mod_saM),
        main = 'Medicaid: Event study: Staggered treatment (TWFE) ',
        xlab = 'Time to treatment',
        ref.line = -1
)

ggarrange(MedicPlot, UNinPlot , 
          ncol = 2, nrow = 1)





