library(labelled)

                                                # Reading Dataset #

Data<-read.csv("BData.csv", header = TRUE, sep ="," , fill = TRUE)

ACS2019<-Data

                                                # Treatment variable #

Data$EXPANSION<-1
Data$EXPANSION<-ifelse(Data$ST %in% c(23,51,29,31,40,49,1,12,13,20,28,37,45,46,47,48,55,56),2,Data$Expansion)
Data$EXPANSION<-replace(Data$EXPANSION,Data$EXPANSION==0,2) # 2.Not expansion

table(Data$EXPANSION)
#Data$Expansion- ordered(Data$Expansion, labels = c(" Expanision"))

Data$EXPANSION<- ordered(Data$EXPANSION, labels = c("Non expansion", "Expansion" ))
var_label(Data$EXPANSION) <- "Expansion"





#table(BRFSS$Expansion)

## State not expansion : Maine(2019), VA(2019) ID(2020), MO(2021), NE(2020), OK(2021), UT(2020)
##   AL, FL, GA, KS, MS, NC, SC, SD,  TN , TX , WI, WY 

                                                # Cleaning the Data #
table(Data$Expansion)

Varname<-colnames(Data)

### 1. REGION 

### 2.  STATE 

  
### 3. AGEP to AGEG

Data$AGEG<-0
Data$AGEG<-ifelse(Data$AGEP>=25 & Data$AGEP<35,1,Data$AGEG) # 1. 25-34
Data$AGEG<-ifelse(Data$AGEP>=35 & Data$AGEP<45,2,Data$AGEG) # 2. 35-44
Data$AGEG<-ifelse(Data$AGEP>=45 & Data$AGEP<55,3,Data$AGEG) # 3. 45-54
Data$AGEG<-ifelse(Data$AGEP>=55 & Data$AGEP<65,4,Data$AGEG) # 4. 55-64

table(Data$AGEG, exclude = NULL)
Data$AGEG<- ordered(Data$AGEG, labels = c("25-34", "35-44", "45-54","55-64" ))
var_label(Data$AGEG) <- "Age (years)"



### 4. CITIZENSHIP STATUS

Data$CITG<-0
Data$CITG<-replace(Data$CITG,Data$CIT %in% c(1,2,3),1)  # 1 born in US 2 born in portorico 3 born american family
Data$CITG<-replace(Data$CITG,Data$CIT==4,2) # 4. Naturalized citizen
Data$CITG<-replace(Data$CITG,Data$CIT==5,3) # 5. Not citizen
table(Data$CITG)

Data$CITG<- ordered(Data$CITG, labels = c("US Born", "Naturalized citizen", "Noncitizen" ))
var_label(Data$CITG) <- "Nativity"


# 1: U.S Citizen by Born 
# 2: Naturalizaed Citizen
# 3: Not Citizen

table(Data$NATIVITY, exclude = NULL)
table(Data$CIT, exclude = NULL)   
table(Data$CITG, exclude = NULL)


### 5. ABILITY SPEAK ENGLISH 

table(Data$ENG, exclude = NULL)  
# NA .Native language
# 1 .Very well
# 2 .Well
# 3 .Not well
# 4 .Not at all

### 6. FER : Gave birth to child within the past 12 months

# table(Data$FER, exclude = NULL)  

### 7. ALL INSURANCE VARIABLES 

# HIMRKS  "HINS1"    "HINS2"  "HINS3" "HINS4"    "HINS5"    "HINS6"    "HINS7"  "HICOV"  "PRIVCOV"  "PUBCOV"
  
# HINS1 Insurance through a current or former employer or union
Data$HINS1<-replace(Data$HINS1,Data$HINS1==2,0) # 1. Employer sponsoredn
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

#  HICOV Any health insurance 
Data$HICOV<-replace(Data$HICOV,Data$HICOV==2,0)
var_label(Data$HICOV) <- "Any health insurance "
table(Data$HICOV)


### 8. Language spoken at home  LANX 

# table(Data$LANX, exclude = NULL) 

### 9. MARITIAL STATUS  MAR to MARG

table(Data$MAR, exclude = NULL)
Data$MARG<-1 # 1: Married
Data$MARG<-replace(Data$MARG,Data$MAR %in% c(2,3,4,5),0)  # 0: Single
table(Data$MARG, exclude = NULL)
var_label(Data$MARG) <- "Married "

### 10. EDUCATION STATUS
 
Data$SCHLG<-0 # creating a school group variable
Data$SCHLG<-ifelse(Data$SCHL>=1 & Data$SCHL< 16,1,Data$SCHLG) # 1. Less than High school 
Data$SCHLG<-ifelse(Data$SCHL %in% c(16,17),2,Data$SCHLG) # 2. High school
Data$SCHLG<-ifelse(Data$SCHL %in% c(18,19),3,Data$SCHLG) # 3. Some College 
Data$SCHLG<-ifelse(Data$SCHL %in% c(20,21),4,Data$SCHLG) # 4. Associate or Bachler degree
Data$SCHLG<-ifelse(Data$SCHL %in% c(22,23,24),5,Data$SCHLG) # 5. Graduate degree

table(Data$SCHLG, exclude = NULL)

Data$SCHLG<- ordered(Data$SCHLG, labels = c("Less than high school", "High school", "Some college", "College", "Graduate degree" ))
var_label(Data$SCHLG) <- "Education"


### 11. SEX 
# 1. Male
# 2. Female 

Data$SEX<- ordered(Data$SEX, labels = c("Male", "Female" ))
var_label(Data$SEX) <- "Sex"
table(Data$SEX)

### 12. YEAR arrived in US  "YOEP"

### 13. ANC1P : Ancesstary 

### 14. DISABILITY STATUS  DIS

### 15. EMPLOYMENT STATUS : "ESRG" from ESR

Data$ESRG<-0 # creating new employment variable
Data$ESRG<-ifelse(Data$ESR %in% c(1,2,4,5),1,Data$ESRG) #1 Employed
Data$ESRG<-ifelse(Data$ESR==3,2,Data$ESRG) #2 Unemployed
Data$ESRG<-ifelse(Data$ESR==6,3,Data$ESRG) #3 Not labor force
table(Data$ESRG, exclude = NULL)
Data$ESRG<- ordered(Data$ESRG, labels = c("Employed", "Unemployed", "Not in labor force"))
var_label(Data$ESRG) <- "Employment"




### 16. ETHNICITY

table(Data$HISP, exclude = NULL)
Data$ETHN<-1 # creat ethnicity variable      # 1 : Hispanic
Data$ETHN<-ifelse(Data$HISP == 1,0,Data$ETHN) # 0 : Not hispanic
table(Data$ETHN,exclude = NULL) 

var_label(Data$ETHN) <- "Hispanic"

### 17.  PLACE OF BIRTH  POBP


### 18.  INCOME To Poverty RATION : POVPIP

Data$POVPIPG<-1 # 1.0 to 100% poverty 
Data$POVPIPG<-ifelse(Data$POVPIP>100,2,Data$POVPIPG) #2.100 t0 138% poverty

table(Data$POVPIP, exclude = NULL)
table(Data$POVPIPG, exclude = NULL)


Data$POVPIPG<- ordered(Data$POVPIPG, labels = c("Income below 100% poverty", "Income  100 to 138% poverty"))
var_label(Data$POVPIPG) <- "Income"


### 19. RACE  

## RACE  Modified 
# 1. White
# 2. Black or African American alone
# 3. American Indian or Alaska Native  
# 4. Asian
# 5. Native Hawaiian and Other Pacific Islander


Data$RACE<-Data$RAC1P
Data$RACE<-ifelse(Data$RAC1P %in% c(4,5),3,Data$RACE) # 4. Asian# 5. Native Hawaiian and Other Pacific Islander
Data$RACE<-ifelse(Data$RAC1P==6,4,Data$RACE) # 6. Asian
Data$RACE<-ifelse(Data$RAC1P %in% c(7,8,9),5,Data$RACE)  # 8.Some Other Race alone # 9.Two or More Races

table(Data$RACE, exclude = NULL)
table(Data$RAC1P, exclude = NULL)

Data$RACE<- ordered(Data$RACE, labels = c("White", "Black", "American Indian or Alaska Native", "Asian", "Other"))
var_label(Data$RACE) <- "Race"



write.csv(Data, file = "ACS2019A.csv", row.names = FALSE)

#require(foreign)
#write.dta(Data, "mydata.dta")
