# Title: Event study and parallel trend assumption
# Author: Shadi Seyedi
# Date: may 15, 2023
# Description:  This code section is written for 
# Event Study and Parallel trend check 
 
####################
## Required Packages
####################
#install.packages("ggiplot", repos = "https://grantmcdermott.r-universe.dev")
#install.packages("ggpubr")
library(ggplot2)
library(ggiplot)
library(ggpubr)
library('kableExtra')
library(fixest)

####################
## Data Preparation 
####################

 ACS <- read.csv( paste0(wd$data,"CLNACS.csv"), header = TRUE, sep = ",", fill = TRUE, stringsAsFactors = TRUE)
Data<-ACS

NATV<- Data[Data$NATIVITY == "US-born", ]
Forgn<- Data[Data$NATIVITY == "Foregin-born", ]

#Becausae expansion was a factor variable I had problme with the feols , so I changed it back to dummy
Data$expansion<-as.numeric(Data$expansion)
Data$expansion<-ifelse(Data$expansion==2,0,Data$expansion)

# need to get a new sample for foregin and native
# Forgn<- Data[Data$NATIVITY == "Foregin-born", ]
Forgn<- Data[Data$NATIVITY == "Foregin-born", ]
NATV<- Data[Data$NATIVITY == "US-born", ]



# In my data set for those state that the did not expand medicaid the expansion year is equal to 0

# I first Create a "time_to_treatment" variable for each state, so that treatment is
# relative for all treated units. For the never-treated (i.e. control) units,
# we'll arbitrarily set the "time_to_treatment" value at 0. This value 
# doesn't really matter, since it will be canceled by the treat==0 interaction
# anyway. But we do want to make sure they aren't NA, otherwise feols would drop 
# these never-treated observations at estimation time and our results will be 
# off.

#Data$ttot<-ifelse(Data$expansion == "Adopted", Data$YEAR - Data$ExpansionY, 0)

#Becausae expansion was a factor variable I had problme with the feols , so I changed it back to dummy

#class(Data$expansion)
#table(Data$expansion)
#Data$expansion<-as.numeric(Data$expansion)
#Data$expansion<-ifelse(Data$expansion==2,0,Data$expansion)



############################################################
## Fig 1: Event study for Uninsured 
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

UNINSUN<-ggiplot(list('US-born' = Event1, 'Foreign-born' = Event3),
                 ref.line = -1, main = 'Unadjusted',xlab='Event Time')
#UNINSUS<-UNINSUS+scale_y_continuous(breaks = seq(-0.10, 0.10, by = 0.05))

UNINSADJ<-ggiplot(list('US-born' = Event2, 'Foreign-born' = Event4),
                  ref.line = -1, main = 'Adjusted',xlab='Event Time')
#UNINSFOR<-UNINSFOR+scale_y_continuous(breaks = seq(-0.15, 0.10, by = 0.05))

figure1<-ggarrange(UNINSUN, UNINSADJ , 
                   ncol = 2, nrow = 1)

annotate_figure(figure1,
                top = text_grob("Uninsured Rate", face = "bold", size = 14),
                
                fig.lab = "Figure 1", fig.lab.face = "bold"
)


############################################################
## Tab 1: Event study for Uninsured 
############################################################

table1<-etable(list(Event1, Event2, Event3,Event4),
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
                       # tabular = "*",
                       model.title = "Variables",
                       depvar.title= " ",
                       var.title = "\\midrule",
                       yesNo = "yes"),
               depvar=FALSE,
               fontsize = "small",
               dict=c(ST = "State" , YEAR = "Year", ttot="year", UNINS="Uninsured Rate")# ,
               #extralines = list("^^Nativity" = c("US-Born", "US-Born", "Foreign-Born", "Foreign-Born"))
)

print(table1)

############################################################
###  Fig 2: Event study for Medicaid 
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



UNINSUN<-ggiplot(list('US-born' = Event5, 'Foregin-born' = Event7),
                 ref.line = -1,
                 main = 'Unadjusted',
                 xlab='Event Time'
                 
)
#UNINSUS<-UNINSUS+scale_y_continuous(breaks = seq(-0.10, 0.10, by = 0.05))


UNINSADJ<-ggiplot(list('US-born' = Event6, 'Foregin-born' = Event8),
                  ref.line = -1, main = 'Adjusted',xlab='Event Time')
#UNINSFOR<-UNINSFOR+scale_y_continuous(breaks = seq(-0.15, 0.10, by = 0.05))


figure2<-ggarrange(UNINSUN, UNINSADJ , 
                   ncol = 2, nrow = 1)

annotate_figure(figure2,
                top = text_grob("Medicaid Coverage Rate", face = "bold", size = 14),
                
                fig.lab = "Figure 2", fig.lab.face = "bold"
)


############################################################
###  Tab 2: Event study for Medicaid 
############################################################


table2<-etable(list(Event5, Event6, Event7,Event8),
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
                       # tabular = "*",
                       model.title = "Variables",
                       depvar.title= " ",
                       var.title = "\\midrule",
                       yesNo = "yes"),
               
               fontsize = "small",
               dict=c(ST = "State" , YEAR = "Year", ttot="year", UNINS="Uninsured Rate")# ,
               
)


#extralines = list("^^Nativity" = c("US-Born", "US-Born", "Foreign-Born", "Foreign-Born"))

print(table2)


