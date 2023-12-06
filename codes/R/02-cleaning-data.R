
##############################################################################################################
### This program's aim is to clean the data for the final use 
### last update : Nov 15, 2022
#############################################################################################################


                                                                    #### loading required library  ####
library(labelled)
library(tidyr)
library(readxl)
library(dplyr)



wd <- list()
# commonly used paths in my working directory
wd$data   <- "/home/shadi/Projects/GitHub/medicaid-foreign-born/data/"
wd$output <- "/home/shadi/Projects/GitHub/medicaid-foreign-born/output/"
wd$texts <- "/home/shadi/Projects/GitHub/medicaid-foreign-born/text/"
wd$codes <- "/home/shadi/Projects/GitHub/medicaid-foreign-born/codes/R/"

                                                                    #### Reading Dataset   ####

 Data<-read.csv("PREACS.csv", header = TRUE, sep ="," , fill = TRUE)

PREACS<-Data
                                                                    #### Cleaning the Data   ####


Varname<-colnames(Data) # Checking the names of all variable

### 1.  STATE - No cleaning needed

### 2.  TYPE - Variable not needed drop 


### 3. REGION - No cleaning needed



  
### 4. AGEP to AGEG

table(ACS$AGEP, exclude = NULL)

Data$AGEG<-0
Data$AGEG<-ifelse(Data$AGEP>25 & Data$AGEP<35,1,Data$AGEG) # 1. 25-34
Data$AGEG<-ifelse(Data$AGEP>=35 & Data$AGEP<45,2,Data$AGEG) # 2. 35-44
Data$AGEG<-ifelse(Data$AGEP>=45 & Data$AGEP<55,3,Data$AGEG) # 3. 45-54
Data$AGEG<-ifelse(Data$AGEP>=55 & Data$AGEP<65,4,Data$AGEG) # 4. 55-64

table(Data$AGEG, exclude = NULL)
Data$AGEG<- ordered(Data$AGEG, labels = c("25-34", "35-44", "45-54","55-64" ))
var_label(Data$AGEG) <- "Age (years)"


### 4. CITIZENSHIP STATUS

## in the data us-citizen born abroud are not included in foregin born. howerver, I wnat 
## this groupd to be foregin born. therefore I recode it as :

Data$NATIVITY<-replace(Data$NATIVITY,Data$CIT==3,2) 


# additionaly I want my cit to have lable
Data$CIT<- ordered(Data$CIT, labels = c("Born in US states", "Born in US Territories","US-citizen Born Abroad","Naturalized-citizen", "Non-citizen" )) # labeling the values 
var_label(Data$CIT) <- "Citizenship Status"

table(Data$NATIVITY)
#Similarly I want to have lable for nativity

Data$NATIVITY<- ordered(Data$NATIVITY, labels = c("US-born", "Foregin-born")) # labeling the values 
var_label(Data$NATIVITY) <- "Forign-born Status"



### 5. ABILITY SPEAK ENGLISH 

table(Data$ENG, Data$YEAR , exclude = NULL)   # NA .Speaks only english  # 1 .Very well # 2 .Well # 3 .Not well # 4 .Not at all

   # looking at the data, it is clear that there was a change in recoding by ACS. Prior to 2017, only english speaker 
        # were coded as 0 ,but after 2017 they were coded as NA. We need to frist fix this.

Data$ENG<-replace(Data$ENG,is.na(Data$ENG),0) #
Data$ENG<-replace(Data$ENG,Data$ENG=="b",0) #

table(Data$ENG,exclude = NULL)

Data$ENG<- ordered(Data$ENG, labels = c("Only english","Very well", "Well", "Not well", "Not at all"  ))
var_label(Data$ENG) <- "English Proficiency"



   
### 6. FER : Gave birth to child within the past 12 months, 

 # We obsesrve similar pattern as ENG here before 2017, men and not applicable interviewee were coded 0 but after 2017 there were coded NA

Data$FER<-replace(Data$FER,Data$FER==0,NA) # Change the 0 value to NA
Data$FER<-replace(Data$FER,Data$FER=="b",0) # Change the 0 value to NA

 table(Data$FER,Data$YEAR ,exclude = NULL)  # Coded 8 is only for 2012 were there was a problem gathering the data

### 7. ALL INSURANCE VARIABLES 

# HIMRKS  "HINS1"  "HINS2"  "HINS3" "HINS4"    "HINS5"    "HINS6"    "HINS7"  "HICOV"  "PRIVCOV"  "PUBCOV"
  
# HINS1 Insurance through a current or former employer or union
Data$HINS1<-replace(Data$HINS1,Data$HINS1==2,0) # Making it a dummy variable change the value of NO from 2 to 0
var_label(Data$HINS1) <- "Employer sponsored"
table(Data$HINS1)

#"HINS2" : Insurance purchased directly from an insurance company
Data$HINS2<-replace(Data$HINS2,Data$HINS2==2,0)
var_label(Data$HINS2) <- "Individual purchased"
table(Data$HINS2)

#"HINS3" : Medicare, 
Data$HINS3<-replace(Data$HINS3,Data$HINS3==2,0)
var_label(Data$HINS3) <- "Medicare"
table(Data$HINS3)

#"HINS4" Medicaid,
Data$HINS4<-replace(Data$HINS4,Data$HINS4==2,0)
var_label(Data$HINS4) <- "Medicaid"
table(Data$HINS4)

#"HINS5": TRICARE or other military health care
Data$HINS5<-replace(Data$HINS5,Data$HINS5==2,0)
var_label(Data$HINS5) <- "TRICARE"
table(Data$HINS5)

# "HINS6": VA (enrolled for VA health care)
Data$HINS6<-replace(Data$HINS6,Data$HINS6==2,0)
var_label(Data$HINS6) <- "VA health care"
table(Data$HINS6)

# "HINS7": Indian Health Service
Data$HINS7<-replace(Data$HINS7,Data$HINS7==2,0)
var_label(Data$HINS7) <- "Indian Health Service"
table(Data$HINS7)

#  HICOV Any health insurance 
Data$HICOV<-replace(Data$HICOV,Data$HICOV==2,0)
var_label(Data$HICOV) <- "Any health insurance "


### 8. MARITIAL STATUS  MAR to MARG - ( Married, widowed, divorced, Separated,Never Married)

table(Data$MAR, exclude = NULL)
Data$MARG<-1 # 1: Married
Data$MARG<-replace(Data$MARG,Data$MAR %in% c(2,3,4,5),0)  # 0: Single
table(Data$MARG, exclude = NULL)
var_label(Data$MARG) <- "Married "

### 9. EDUCATION STATUS
Data$SCHL <- as.numeric(as.character(Data$SCHL))

Data$SCHLG<-0 # creating a school group variable
Data$SCHLG<-ifelse(Data$SCHL>=1 & Data$SCHL< 16,1,Data$SCHLG) # 1. Less than High school 
Data$SCHLG<-ifelse(Data$SCHL %in% c(16,17),2,Data$SCHLG) # 2. High school
Data$SCHLG<-ifelse(Data$SCHL %in% c(18,19,20),3,Data$SCHLG) # 3. Some College Or Associate's degree 
Data$SCHLG<-ifelse(Data$SCHL %in% c(21),4,Data$SCHLG) # 4. Bachler degree
Data$SCHLG<-ifelse(Data$SCHL %in% c(22,23,24),5,Data$SCHLG) # 5. Graduate degree



Data$SCHLG<- ordered(Data$SCHLG, labels = c("Less than high school", "High school", "Some college or Associate degree", "College degree", "Graduate and beyond" ))
var_label(Data$SCHLG) <- "Education"


### 11. SEX to make it easier and be consitent wit the litriture I recoded the sex so that 0 shows male and 1 shows female previousl 1 showed male and 2 showed female

Data$SEX<-replace(Data$SEX,Data$SEX==1,0) # recode male from 1 to zero
Data$SEX<-replace(Data$SEX,Data$SEX==2,1) # recode female from 2 to 1

Data$SEX<- ordered(Data$SEX, labels = c("Male", "Female" ))
var_label(Data$SEX) <- "Sex"
table(Data$SEX)
  Data$SEX<-PREACS$SEX
    
### 12. YEAR arrived in US  "YOEP"

### 13. DISABILITY STATUS  DIS make it a dummy 

Data$DIS<-replace(Data$DIS,Data$DIS==2,0)
var_label(Data$DIS) <- "Disability"


### 14. ETHNICITY

table(Data$HISP, exclude = NULL)
Data$ETHN<-1 # creat ethnicity variable      # 1 : Hispanic
Data$ETHN<-ifelse(Data$HISP == 1,0,Data$ETHN) # 0 : Not hispanic
table(Data$ETHN,exclude = NULL) 
var_label(Data$ETHN) <- "Hispanic"


### 15. EMPLOYMENT STATUS : "ESRG" from ESR --(employed, unemployed, armed force, not in labor force)

table(Data$ESR, exclude = NULL)
Data$ESRG<-0 # creating new employment variable
Data$ESRG<-ifelse(Data$ESR %in% c(1,2,4,5),1,Data$ESRG) #1 Employed
Data$ESRG<-ifelse(Data$ESR==3,2,Data$ESRG) #2 Unemployed
Data$ESRG<-ifelse(Data$ESR==6,3,Data$ESRG) #3 Not labor force
Data$ESRG<- ordered(Data$ESRG, labels = c("Employed", "Unemployed", "Not in labor force"))
var_label(Data$ESRG) <- "Employment"
table(Data$ESRG, exclude = NULL)

### 16. INCOME To Poverty RATION : POVPIP
Data$POVPIP <- as.numeric(as.character(Data$POVPIP))


Data$POVPIPG<-1 # 1.0 to 100% poverty 
Data$POVPIPG<-ifelse(Data$POVPIP>100,2,Data$POVPIPG) #2.100 t0 138% poverty

Data$POVPIPG<- ordered(Data$POVPIPG, labels = c("Income below 100% poverty", "Income  100 to 138% poverty"))
var_label(Data$POVPIPG) <- "Income"

table(Data$POVPIP, exclude = NULL)
table(Data$POVPIPG,Data$CIT ,exclude = NULL)


### 19. RACE  

Data$RACE<-Data$RAC1P
Data$RACE<-ifelse(Data$RAC1P %in% c(3,4,5,7,8,9),9,Data$RACE)  # 8.Some Other Race alone # 9.Two or More Races
Data$RACE<-ifelse(Data$RACE==6,3,Data$RACE) # 6. Asian rename asian to 3
Data$RACE<-ifelse(Data$RACE==9,4,Data$RACE) # 9 other to 4



## Creating a new variable for race/ethnicity and rename it RACE1
Data$RACE1<-Data$RACE
Data$RACE1<-ifelse(Data$ETHN==1,5,Data$RACE1) # merge ethnicity into the race
Data$RACE1<-ifelse(Data$RACE1==4,6,Data$RACE1) # merge ethnicity into the race

Data$RACE<- ordered(Data$RACE, labels = c("White", "Black", "Asian", "Other"))
var_label(Data$RACE) <- "Race"

Data$RACE1<- ordered(Data$RACE1, labels = c("White", "Black", "Asian", "Hispanic","Other"))
var_label(Data$RACE1) <- "Race and Ethnicity"

  

# Create a variable ACA to have pre and post ACA
Data$ACA <- ifelse(Data$YEAR < 2014,1, 2)
Data$ACA<- ordered(Data$ACA, labels = c("Pre-ACA","Post-ACA" ))

# Create a variable ACA numeric binary
Data$PostACA<- ifelse(Data$YEAR < 2014,0, 1)

table(Data$PostACA)
#####  expansion status
#2015 Pennsylvania,  Indiana ,Alaska
#2016, Montana, Louisiana
#2019, Virginia, Maine
#2020 Idaho,Utah,Nebraska
#2021 Oklahoma
#2022 Missouri
# not expanded Alabama,Florida,Georgia,Kansas,Mississippi,North Carolina,South Carolina,South Dakota,Tennessee,Texas,Wisconsin,Wyoming
# Create a data frame with state FIPS codes and their corresponding expansion years

#I can use the code here to create a var for expansion and expansion year but since I have the data availble
#I'll use that instead, but if you don't want to use the data set the variable can be made using the code by removing # in the begings

#expansion_years <- data.frame(ST = c(2,4,5,6,8,9,15,17,18, 19,21,22,23,24,26,27,30,32,33,34,35,38,39,41,42,44,51,53,54),
  #                     Expan_year = c(rep(2015, 1),rep(2014, 7), rep(2015, 1),rep(2014, 2), rep(2016,1), rep(2019,1), rep(2014, 3),rep(2016,1),rep(2014, 7),rep(2015, 1),rep(2014, 1),rep(2019,1),rep(2014, 2)))
# Merge Data with expansion_years based on the STATE variable
# Data <- merge(Data, expansion_years, by = "ST", all.x = TRUE)
# Create the expansion variable based on the year of expansion
# Data$expan <- ifelse(is.na(Data$Expan_year), 0, 1)

## Substitue way using the excel file that is already there by KFF

expanData <- read_excel(paste0(wd$data,"expansion.xlsx"), sheet = "raw_data")

# this data set includes stfips code(ST), State name(StateN), expansion(expansion) if state adopted or not(treatment), the year of expansion (ExpansionY)
# and the Expan_STS which has values not expanded, expanded, late expanded.

# checking the class of data
sapply(expanData, class)

# expansion and Expan_STS are character but they need to be factor, I will change their class next
expanData$expansion<-as.factor(expanData$expansion)
expanData$Expan_STS<-as.factor(expanData$Expan_STS)

# merging data to whole data set
Data <- merge(Data, expanData, by = "ST", all.x = TRUE)
# Expansion year for the state that didn't adopt the medicaid is 0 I like to change that to NA
# I might later decide to change it to 0 again
Data$ExpansionY <- ifelse(Data$ExpansionY == 0, NA, Data$ExpansionY)
# the expansion treatment is not dynamic it only shows if the satet is among expansion or non expansion states
# Thus I need to create another variable called treatment that is dynamice
Data$treat<-ifelse(Data$YEAR>=Data$ExpansionY & !is.na(Data$ExpansionY) , 1,0)
#Change adoptet to expansion values
b<-Data$expansion
levels(b) <- c("Non-expansion","Expansion")
class(b)
b<-as.factor(b)
Data$expansion<-b

#####  percentage of life spent in the US

# there is problem that might have rised beacuse the age was rounded up in the Dataset to fix that I changed YLIU as following
Data$YLIU <- ifelse(Data$YLIU > Data$AGEP, Data$AGEP, Data$YLIU)

# now that I fixed the issue with the YLIU I can create percentage life in US 
Data$PLIU<-NA
Data$PLIU <- floor((Data$YLIU / Data$AGEP) * 100)  # i used floor to roundup the percentage 

# Country of origin

### 1. Based on Geography
# Create a new column for the geographic region (GEOR)
Data$GEOR<-NA

# Assign geographic regions based on UN sub-regions
# Africa
Data$GEOR[Data$POBP %in% c(400,414,430,436,451,456)] <- 1 #Northern Africa Done
Data$GEOR[Data$POBP %in% c(400:499) & !Data$POBP %in% c(400,414,430,436,451,456)] <- 2 #Sub-Saharan Africa Done
# America
Data$GEOR[Data$POBP %in% c(303,310:399)] <- 3 #Latin America  Done 
Data$GEOR[Data$POBP %in% c(300:302,304:309)] <- 4 #Northern America Done
# Asia
Data$GEOR[Data$POBP %in% c(200:203,210,229,231,238,218,219,246,253)] <- 5 #South  & Centeral Asia
Data$GEOR[Data$POBP %in% c(207,209,217,215,228,249,251)] <- 6 #Eastern Asia
Data$GEOR[Data$POBP %in% c(206,211,223,226,205,233,236,242,247,254,240)] <- 7 #South eaastern Asia
Data$GEOR[Data$POBP %in% c(158,159,161,212:214,216,222,224,235,239,243,245,248,208)] <- 8 #Western Asia
#Europe
Data$GEOR[Data$POBP %in% c(104,105,117,128,162,132,163,164,147, 148,149,155, 165,160,169)] <- 9 #Eastern Europe
Data$GEOR[Data$POBP %in% c(106,108,118,119,156,157,127,136,138,142,140,139)] <- 10 #Northen Europe157, 160, 162:199
Data$GEOR[Data$POBP %in% c(100,150, 151,116,120,129,168,152,154,134,167,166 )] <- 11 #Southern Europe ,1
Data$GEOR[Data$POBP %in% c(102,103,109,110,126, 137)] <- 12 #Western Europe
#Oceania and at Sea
Data$GEOR[Data$POBP %in% c(060, 130, 500:554)] <- 13 #Oceania and at Sea

Data$GEOR<- ordered(Data$GEOR, labels = c("Northern Africa", "Sub-Saharan Africa", "Latin America","Northern Americ", "South  & Centeral Asia","Eastern Asia","South eaastern Asia",
                                          "Western Asia","Eastern Europe","Northen Europe","Southern Europe","Western Europe","Oceania and at Sea"))
var_label(Data$GEOR) <- "Geographic region"
Data$GEOR <- factor(ifelse(Data$NATIVITY == "US-born", "United States", as.character(Data$GEOR)))
table(Data$GEOR,exclude = NULL)
# TO DO : maybe put cenetal Asia in another group since thera are not much observation in that group 
table(Data$ExpansionY, exclude = NULL)

### 1. Based on Culture 

# English Speaking, Protestant Europe, Catholic Europe, Confucian, Orthodox ,Latin America, South Asia, Islamic,Sub-Saharan Africa
# English-speaking, Protestant Europe,Catholic Europe, Confucian,Orthodox, Latin America, South Asian , African-Islamic, Baltic

Data$CULRG<-NA

Data$CULRG[Data$POBP %in% c(119,138,139,140,142,060, 500:554,300:302,304:309)] <- 1 #English-speaking countries
Data$CULRG[Data$POBP %in% c(106,108,110,118,126,127,136,137)] <- 2 #Protestant Europe
Data$CULRG[Data$POBP %in% c(102,103,105,109,117,120,128,129,130,134,147,148,149,151,155, 156,157)] <- 3 # Catholic Europe
Data$CULRG[Data$POBP %in% c(104,116,132,150,152,154,158,160:169,251)] <- 4 #Orthodox

Data$CULRG[Data$POBP %in% c(206,207,209,215,217,228,254,240,249 )] <- 5 # Confucian
Data$CULRG[Data$POBP %in% c(233,303,310:399)] <- 6 #Latin America
Data$CULRG[Data$POBP %in% c(203,208,210,211,214,223,226,236,238, 242,247,449)] <- 7 #South Asia
Data$CULRG[Data$POBP %in% c(159,200,202,205, 100,212,213,216,218,281,219,222,224,229, 231,235,239,243,245,246,248,253,400,414,400:448,450:499)] <- 8 #Islamic,

Data$CULRG<- ordered(Data$CULRG, labels = c("English-speaking", "Protestant Europe","Catholic Europe", "Orthodox", "Confucian", "Latin America", "South Asian" , "African-Islamic"))
var_label(Data$CULRG) <- "Inglehart-Welzel cultural Cluster"

table(Data$CULRG, exclude = NULL)



                                                              #### Export extra variables ####


                                                    ## IPC Index##

# Read an Excel file named "IPC.xlsx" from the working directory, 
# where the sheet named "Sheet1" will be read.
# This is going to be the all IPC score
IPCINDX<-read.csv(paste0(wd$data,"IPC_Final.csv"), header = TRUE, sep ="," , fill = TRUE)
IPCINDX <- IPCINDX[, c("state", "year", "annual_score")] #subset

colnames(IPCINDX) <- c("StateN", "YEAR", "IPCINDX")
sapply(IPCINDX, class)
Data <- merge(Data, IPCINDX, by = c("StateN", "YEAR"), all.x = TRUE)

# This is going to be IPC score without including Medicaid score 
IPC <- read.csv(paste0(wd$data,"IPC.csv"), header = TRUE, sep ="," , fill = TRUE)
#IPC <- read_excel(paste0(wd$data,"IPC.csv"), sheet = "Sheet1")
#
sapply(IPC, class)
colnames(IPC) <- c("StateN", "YEAR", "IPC")
Data <- merge(Data, IPC, by = c("StateN", "YEAR"), all.x = TRUE)


                                            ## Political climate ##

# Read an Excel file named "Pol.xlsx" from the working directory, 

PolClM<-read_excel(paste0(wd$data,"Pol.xlsx"), sheet = "Sheet4")
sapply(PolClM, class)

# first I drop the extra variable
#PolClM <- PolClM %>% select(-"STATE ABR")

# then I will change the var names

colnames(PolClM) <- c("YEAR", "StateN", "ST", "ADA")

Data <- merge(Data, PolClM, by = c("ST", "YEAR", "StateN"), all.x = TRUE)

table(Data$ADA,exclude=NaN)

class(Data$ADA)
#table(PolClM$Leg_Control, exclude = NaN)
#table(PolClM$Gov_Party, exclude = NaN)
#table(PolClM$State_Control, exclude = NaN)

# values Divided and split are the same need to be replaced
#PolClM$`Leg. Control` <- replace(df$Status, df$Status == "Split", "Divided")


#change charachter variable to ordered factor
#levels_order <- c("Dem", "Rep", "Divided")
#for (var in c("Leg. Control", "Gov. Party","State Control" )) {
 # PolClM[[var]] <- factor(PolClM[[var]], levels = levels_order, ordered = TRUE)
#}



                                        ## State's unemployment rate ##

UNEMPR <- read_excel(paste0(wd$data,"UnempR.xlsx"), sheet = "Sheet1")
sapply(UNEMPR, class)

# Drop the columns I don't want, Statae name and State ABR are not included in the original Dataset, 
# I might need to drop them or just keep the ABR for the final dataset

UNEMPR <- UNEMPR %>% select(-c(`STATE ABR` , `2020`, `2021`))

# Use pivot_longer to reshape the data
unemp_data_long <- pivot_longer(UNEMPR, 
                                cols = c(`2009`,`2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`), 
                                names_to = "YEAR", 
                                values_to = "UnempR")

# Print the transformed data check if everything is correct
print(unemp_data_long)
UNEMPR<-unemp_data_long 
rm(unemp_data_long)

colnames(UNEMPR) <- c("StateN","ST", "YEAR", "UnempR")
 #cheking the values again, year is charachter and need to be converted to numeric
sapply(UNEMPR, class)
UNEMPR$YEAR<- as.numeric(UNEMPR$YEAR)
table(UNEMPR$YEAR)
## Now, I try to merge my data 

Data <- merge(Data, UNEMPR, by = c("ST", "YEAR", "StateN"), all.x = TRUE)


                                     ### ADDING FURTHER CLEANING WHICH was added later on to cleaning       ### 
 

Data$ForeginBorn <- ifelse(Data$NATIVITY == "Foregin-born", 1, 0)
table(Data$NATIVITY)
table(Data$ForeginBorn)



# Make a dummy for lifetime in US this would be based on Endeshaw et al. (2018)

Data$LTINU<-ifelse(Data$PLIU>25,1,0)
Data$LTINU<-ordered(Data$LTINU, labels=c("<25%", ">25%"))

### Should calculate uninsured rather than insured therefore


Data<-mutate(Data, UNINS=1-HICOV)

### Create variable ttot for vent study later
### 
### 

Data$ttot<-ifelse(is.na(Data$ExpansionY), 0, Data$YEAR - Data$ExpansionY)


### Creating UNDOC variable 

Data$UNDOC <- 1

# Set UNDOC to 0 based on various conditions:

# The individual is a citizen.
Data$UNDOC[Data$CIT %in% c("Born in US states", "Born in US Territories", "US-citizen Born Abroad", "Naturalized-citizen")] <- 0
# The individual arrived before 1980.
Data$UNDOC[Data$YOEP < 1980] <- 0
# The individual was born in Cuba (as practically all Cuban immigrants were granted refugee status before 2017) 
Data$UNDOC[Data$POBP == 327] <- 0
# The individual is a veteran or is currently in the Armed Forces.
Data$UNDOC[Data$ESR %in% c(4,5) ] <- 0
Data$UNDOC[Data$MIL %in% c(1,2,3) ] <- 0
#e. The individual works in the government sector.
Data$UNDOC[Data$COW %in% c(3,4,5) ] <- 0
# The individual's occupation requires some form of licensing based on borjas (such as physicians, registered nurses, air traffic controllers, and lawyers)
Data$UNDOC[Data$OCCP %in% c (10, 120, 230, 350, 410, 800, 810, 820, 830, 845, 850, 860, 900, 910,
                             930, 940, 2100, 2105, 2145, 2170, 2180, 2205, 2300,
                             2310, 2320, 2330, 2350, 2360, 2400, 2435, 2440, 2545, 2555, 
                             3000, 3010, 3030, 3040, 3050, 3090, 3100, 3110, 3120, 3140,
                             3150, 3160, 3200, 3210, 3220, 3230, 3245, 3250, 3255, 3256, 
                             3258, 3261, 3270, 3300, 3310, 3321, 3322, 3323, 3324, 3330,
                             3401, 3402, 3421, 3422, 3423, 3424, 3430, 3500, 3515, 3520,
                             3545, 3550, 3700, 3710, 3720, 3725, 3740, 3750, 3801, 3802, 
                             3820, 3870, 3910, 3945, 3946, 3946, 6010, 6660, 9030, 9040, 9050, 
                             9121, 9122, 9130, 9141, 9142, 9150, 9210, 9240, 9265, 9300, 9310)] <- 0

# if spouse is legal
# 
# 

Data$good <- ifelse(Data$MAR == 1 & Data$RELP %in% c(21, 23, 1) & Data$UNDOC==0, 1,
                    ifelse(Data$MAR == 1 & Data$RELP %in% c(21, 23, 1) & Data$UNDOC==1, 0,
                           ifelse(Data$MAR == 1 & Data$RELP %in% c(20, 0) & Data$UNDOC==0, 1,
                                  ifelse(Data$MAR == 1 & Data$RELP %in% c(20, 0) & Data$UNDOC==1, 0, 0)
                           )
                    )
)


Data$slegal <- ave(Data$good , Data$YEAR, Data$SERIALNO, FUN = mean)

Data$UNDOC[Data$MAR == 1 & Data$slegal > 0 ] <- 0


                                                #### Exporting the data set into the new data sample both CSV and R ####



write.csv(Data, file = paste0(wd$data,"CLNACS.csv"), row.names = FALSE)


save(Data, file =paste0(wd$data,"CLNACS.RData"))





