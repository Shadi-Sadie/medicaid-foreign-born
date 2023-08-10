## This code section is for further cleaning and subbseting that I determined later 
library(labelled)
library(dplyr)


# Read the cleaned Data
Data<-read.csv("CLNACS.csv", header = TRUE, sep ="," , fill = TRUE, stringsAsFactors = TRUE)
#Create  a copy of the data-set
ACS<-Data
Data<-ACS

#### beginning year should be 2011 to avoid effect of other provisions.
Data<- Data %>% filter(YEAR>2010) 
#Check if it's correct
table(Data$YEAR)

### Should calculate uninsured rather than insured therefore

Data<-mutate(Data, UNINS=1-HICOV)


#### Specifically for Foreign Data

Forgn$IMG<-as.numeric(Forgn$CIT)
table(as.numeric(Forgn$CIT))
Forgn$IMG<- ordered(Forgn$IMG, labels = c("Naturalized-citizen", "Non-citizen","US-citizen Born abroad" ))

Forgn$RACE1 <- factor(Forgn$RACE1, ordered = FALSE)
NATV$RACE1 <- factor(NATV$RACE1, ordered = FALSE)
Forgn$IMG <- factor(Forgn$IMG, ordered = FALSE)



require(foreign)

write.dta(Data, "CLNACS.dta")
write.dta(NATV, "NATV.dta")
write.dta(Forgn, "Forgn.dta")


table(Data$IPC)

colnames(Data)

#the new IPC code
IPCINDX<-read.csv("IPC_Final.csv", header = TRUE, sep ="," , fill = TRUE)

IPC<-read.csv("IPC.csv", header = TRUE, sep ="," , fill = TRUE)

sapply(IPC, class)
colnames(IPC) <- c("StateN", "YEAR", "IPC")

Data <- merge(Data, IPC, by = c("StateN", "YEAR"), all.x = TRUE)

colnames(Data)

# read the whole data
IPCINDX<-read.csv("IPC_Final.csv", header = TRUE, sep ="," , fill = TRUE)


# I need a new variable foregin that equalls to 1 when the nativity="Foreginborn"

Data$ForeginBorn <- ifelse(Data$NATIVITY == "Foregin-born", 1, 0)
table(Data$NATIVITY)
table(Data$ForeginBorn)
names(Data)[names(Data) == "Foregin-born"] <- "ForeginBorn"
colnames(Data)


# Make GEOR for us born United states
Data$GEOR <- factor(ifelse(Data$NATIVITY == "US-born", "United States", as.character(Data$GEOR)))
Data$CULRG <- factor(ifelse(Data$NATIVITY == "US-born", "English-speaking", as.character(Data$CULRG)))

# Make a dummy for lifetime in US this would be based on Endeshaw et al. (2018)

Data$LTINU<-ifelse(Data$PLIU>25,1,0)
Data$LTINU<-ordered(Data$LTINU, labels=c("<25%", ">25%"))

levels(b) <- c("Non-expansion","Expansion")
class(b)
b<-as.factor(b)
Data$expansion<-b

Data$CORIGIN<-b
Data$CORIGIN <- factor(ifelse(Data$GEOR == "Western Asia" | Data$GEOR == "Northern Africa" , "Middle East", as.character(Data$GEOR)))
Data$CORIGIN<- factor(ifelse(Data$GEOR == "Northen Europe" | Data$GEOR=="Southern Europe" |
                                      Data$GEOR == "Western Europe", "Western Europe", as.character(Data$CORIGIN)))
levels(b)

levels(b)[levels(b) == "Northern Americ"] <- "Canada"
levels(b)[levels(b) == "South eaastern Asia"] <- "South East Asia"

table(Data$CORIGIN, Data$GEOR)


## For ACA the assigned value for the pre-ACA is 2 while the value for the Post is 1 this is wrong. I need to change that to 0
table(Data$ACA)
class(Data$ACA)
Data$ACA<-as.integer(Data$ACA)
Data$ACA<-ifelse(Data$ACA==2,Data$ACA==0,Data$ACA)
Data$ACA<-as.factor(Data$ACA)
levels(Data$ACA) <- c("Pre-ACA","Post-ACA")

# Change GEOR based on the same to be

#Mexico, Central America, and the Caribbean were separated because of
 #the larger number of immigrants relative to those from the other regions 
#(South America, Southeast Asia, Central Asia, South Asia [Indian Subcontinent], 
 # Middle East [Western Asia], Africa, The Former Soviet Union [FSU], and Europe [excludes FSU])

# create variable ACA to have pre and post aca
df$ACA <- ifelse(df$YEAR < 2014,1, 2)
df$ACA<- ordered(df$ACA, labels = c("Pre-ACA","Post-ACA" ))
var_label(df$ACA) <- "ACA status"
#check if it's correct'

## Creating a new variable for race/ethniciy and rename it

df$RACE<-as.numeric(df$RACE)
df$RACE<-ifelse(df$RACE==1,4,df$RACE) # merge native americann to other
df$RACE<-ifelse(df$ETHN==1,1,df$RACE) # merge native americann to other

df$RACE<- ordered(df$RACE, labels = c("Hispanic", "Asian","Black", "Other", "White"))

var_label(df$RACE) <- "Race"

table(df$NATIVITY)
df$NATIVITY<- ordered(df$NATIVITY, labels = c("Native", "Foreign-born"))
var_label(df$NATIVITY) <- "Nativity"
