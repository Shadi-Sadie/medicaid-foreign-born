reg1 = feols(UNINS ~ treat*ForeginBorn  + SEX + DIS+AGEP+POVPIPG+ENG+LTINU+NonCit+ForeginBorn*(SCHLG+ MARG+ RACE1 + ESRG + ADA+UnempR+IPC) | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # Interaction with state + immigrants

reg2 = feols(HINS4 ~ treat*ForeginBorn  + SEX + DIS+AGEP+POVPIPG+ENG+LTINU+NonCit+ForeginBorn*(SCHLG+ MARG+ RACE1 + ESRG + ADA+UnempR+IPC) | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # Interaction with state + immigrants

reg3 = feols(HINS2 ~ treat*ForeginBorn  + SEX + DIS+AGEP+POVPIPG+ENG+LTINU+NonCit+ForeginBorn*(SCHLG+ MARG+ RACE1 + ESRG + ADA+UnempR+IPC) | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # Interaction with state + immigrants

reg4 = feols(HINS1 ~ treat*ForeginBorn  + SEX + DIS+AGEP+POVPIPG+ENG+LTINU+NonCit+ForeginBorn*(SCHLG+ MARG+ RACE1 + ESRG + ADA+UnempR+IPC) | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # Interaction with state + immigrants

summary(reg1, reg2, reg4,reg3 )


data$FBCIT<-ifelse(data$CIT %in% c("Naturalized-citizen", "US-citizen Born abroad "), 1,0)
data$CITSTAT<-"US-Born"
#data$CITSTAT<-ifelse(Data$CIT == 'Naturalized-citizen'| Data$CIT =="US-citizen Born abroad", "FB-Citizen", data$CITSTAT)
data$CITSTAT <- ifelse(data$CIT %in% c("Naturalized-citizen", "US-citizen Born abroad "), 
                       "FB-Citizen", data$CITSTAT)

data$CITSTAT<-ifelse(Data$CIT == "Non-citizen", "FB-Non-citizen", data$CITSTAT)
table(data$CITSTAT, data$CIT)
data$CITSTAT <- factor(data$CITSTAT, levels = c("US-Born", "FB-Citizen", "FB-Non-citizen"))
table(data$CITSTAT, data$CIT)

data$CITSTAT<-as.factor(data$CITSTAT)
table(Data$CIT)

reg1 = feols(UNINS ~ treat*CITSTAT  + SEX + DIS+AGEP+POVPIPG+ENG+LTINU+ForeginBorn*(SCHLG+ MARG+ RACE1 + ESRG + ADA+UnempR+IPC) | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # Interaction with state + immigrants

reg2 = feols(HINS4 ~ treat*CITSTAT  + SEX + DIS+AGEP+POVPIPG+ENG+LTINU+ForeginBorn*(SCHLG+ MARG+ RACE1 + ESRG + ADA+UnempR+IPC) | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # Interaction with state + immigrants

reg3 = feols(HINS2 ~ treat*CITSTAT  + SEX + DIS+AGEP+POVPIPG+ENG+LTINU+ForeginBorn*(SCHLG+ MARG+ RACE1 + ESRG + ADA+UnempR+IPC) | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # Interaction with state + immigrants

reg4 = feols(HINS1 ~ treat*CITSTAT  + SEX + DIS+AGEP+POVPIPG+ENG+LTINU+ForeginBorn*(SCHLG+ MARG+ RACE1 + ESRG + ADA+UnempR+IPC) | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # Interaction with state + immigrants

etable(reg1, reg2, reg4,reg3 )


controls   <- c("SEX","DIS", "AGEP","SCHLG", "POVPIPG", "MARG", "RACE1", "ESRG") # (SEX+DIS+AGEP+SCHLG+POVPIPG+MARG+RACE1+ESRG+ADA)
class(data$CITSTAT)
library(dplyr)
data <- data %>% filter(FBCIT != 1)

Event = feols(UNINS ~ i(ttot, ForeginBorn, expansion, ref = c(-1, 1)) |                 
                   ST + YEAR,                             ## FEs
               vcov = "hetero" ,
               weights = ~PWGTP,
               data = data)

Event1 = feols(UNINS ~ ForeginBorn*i(ttot, expansion, ref = c(-1, 1)) |                 
                  ST + YEAR,                             ## FEs
              vcov = "hetero" ,
              weights = ~PWGTP,
              data = data)

etable(Event,WTF, Event1, Event2)


fdata <- data %>% filter(NonCit != 1)
Event = feols(UNINS ~ FBCIT*i(ttot, expansion, ref = -1) +SEX + DIS+AGEP+POVPIPG+ENG+LTINU+CORIGIN+FBCIT*(SCHLG+ MARG+ RACE1 + ESRG + ADA+UnempR+IPC)|                 
                   ST + YEAR,                             ## FEs
               vcov = "hetero" ,
               weights = ~PWGTP,
               data = fdata)
summary(Event)

+ ADA +IPC+UnempR + .[conrtols]


controlwof    <- c("SEX","DIS", "AGEP","SCHLG", "POVPIPG", "MARG", "RACE1", "ESRG","ADA") # (SEX+DIS+AGEP+SCHLG+POVPIPG+MARG+RACE1+ESRG+ADA)
NICont        <- c("SEX","DIS", "AGEP","POVPIPG", "ENG", "LTINU", "CORIGIN", "NonCit")
NIContwof     <- c("SEX","DIS", "AGEP","POVPIPG") # SEX+DIS+AGEP+MARG+SCHLG+POVPIPG
Intcontrol    <- c("MARG", "RACE1", "ESRG","ADA","SCHLG" ) # (MARG + RACE1 + ESRG + ADA)
forcont       <- c("ENG", "LTINU", "CORIGIN", "NonCit") # ENG+LTINU+CORIGIN+NonCit
forcont       <- c("ENG", "LTINU", "NonCit") # ENG+LTINU+CORIGIN+NonCit



WTF<-feols(UNINS ~ FBCIT * i(ttot, expansion, ref = -1) + NonCit * i(ttot, expansion, ref = -1) + SEX + DIS+AGEP+POVPIPG+ENG+LTINU+
ForeginBorn*(SCHLG+ MARG+ RACE1 + ESRG + ADA+UnempR+IPC)| ST + YEAR,
vcov = "hetero" ,
weights = ~PWGTP,
data = data)


-0.0142

-0.0938+ 0.0119

    -0.0992+0.0268
