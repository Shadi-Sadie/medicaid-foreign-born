
                                                # Reading Dataset #

Data<-read.csv("BData.csv", header = TRUE, sep ="," , fill = TRUE)

ACS2019<-Data

                                                # Treatment variable #

Data$Expansion<-1
Data$Expansion<-ifelse(Data$ST %in% c(23,51,29,31,40,49,1,12,13,20,28,37,45,46,47,48,55,56),0,Data$Expansion)
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



### 4. CITIZENSHIP STATUS

Data$CITG<-0
Data$CITG<-replace(Data$CITG,Data$CIT %in% c(1,2,3),1)  # 1 born in US 2 born in portorico 3 born american family
Data$CITG<-replace(Data$CITG,Data$CIT==4,2) # 4. Naturalized citizen
Data$CITG<-replace(Data$CITG,Data$CIT==5,3) # 5. Not citizen


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
  
# TYPE OF Insurance 
#Data$INSR<-0
#Data$INSR<-ifelse(Data$HINS1==1| Data$HINS2==1, 1,Data$INSR) # 1. Current employer
#Data$INSR<-ifelse(Data$HINS2==1,2,Data$INSR) # 2. Private from Market
#Data$INSR<-ifelse(Data$HINS3==1,3,Data$INSR) # 3. Medicaire
#Data$INSR<-ifelse(Data$HINS4==1,4,Data$INSR) # 4. Medicaid
#Data$INSR<-ifelse(Data$HINS5==1 | Data$HINS6==1,5,Data$INSR) # 5. TRICARE VA (enrolled for VA)
#Data$INSR<-ifelse(Data$HINS7==1,6,Data$INSR) #  7. Indian Health Service


table(Data$INSR, exclude = NULL)

table(Data$PRIVCOV, exclude = NULL)
table(Data$HINS6, exclude = NULL)
table(Data$HINS5, exclude = NULL)

r<-which(Data$HINS6==1 & Data$HINS6==1)

table(r)
### 8. Language spoken at home  LANX 

# table(Data$LANX, exclude = NULL) 

### 9. MARITIAL STATUS  MAR to MARG

table(Data$MAR, exclude = NULL)
Data$MARG<-0 # 0: Married
Data$MARG<-replace(Data$MARG,Data$MAR %in% c(2,3,4,5),1)  # 1: Single
table(Data$MARG, exclude = NULL)

### 10. EDUCATION STATUS
 
Data$SCHLG<-0 # creating a school group variable
Data$SCHLG<-ifelse(Data$SCHL>=1 & Data$SCHL< 16,1,Data$SCHLG) # 1. Less than High school 
Data$SCHLG<-ifelse(Data$SCHL %in% c(16,17),2,Data$SCHLG) # 2. High school
Data$SCHLG<-ifelse(Data$SCHL %in% c(18,19),3,Data$SCHLG) # 3. Some College 
Data$SCHLG<-ifelse(Data$SCHL %in% c(20,21),4,Data$SCHLG) # 4. Associate or Bachler degree
Data$SCHLG<-ifelse(Data$SCHL %in% c(22,23,24),5,Data$SCHLG) # 5. Graduate degree

table(Data$SCHLG, exclude = NULL)

### 11. SEX 
# 1. Male
# 2. Female 

### 12. YEAR arrived in US  "YOEP"

### 13. ANC1P : Ancesstary 

### 14. DISABILITY STATUS  DIS

### 15. EMPLOYMENT STATUS : "ESRG" from ESR

Data$ESRG<-0 # creating new employment variable
Data$ESRG<-ifelse(Data$ESR %in% c(1,2,4,5),1,Data$ESRG) #1 Employed
Data$ESRG<-ifelse(Data$ESR==3,2,Data$ESRG) #2 Unemployed
Data$ESRG<-ifelse(Data$ESR==6,3,Data$ESRG) #3 Not labor force
table(Data$ESRG, exclude = NULL)
### 16. ETHNICITY

table(Data$HISP, exclude = NULL)
Data$ETHN<-1 # creat ethnicity variable      # 1 : Hispanic
Data$ETHN<-ifelse(Data$HISP == 1,0,Data$ETHN) # 0 : Not hispanic
table(Data$ETHN,exclude = NULL) 


### 17.  PLACE OF BIRTH  POBP


### 18.  INCOME To Poverty RATION : POVPIP

Data$POVPIPG<-1 # 1.0 to 100% poverty 
Data$POVPIPG<-ifelse(Data$POVPIP>100,2,Data$POVPIPG) #2.100 t0 138% poverty

table(Data$POVPIP, exclude = NULL)
table(Data$POVPIPG, exclude = NULL)


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
Data$RACE<-ifelse(Data$RAC1P==7,5,Data$RACE) # 7.Native Hawaiian and Other Pacific Islander alone
Data$RACE<-ifelse(Data$RAC1P %in% c(8,9),6,Data$RACE)  # 8.Some Other Race alone # 9.Two or More Races

table(Data$RACE, exclude = NULL)
table(Data$RAC1P, exclude = NULL)

write.csv(Data, file = "ACS2019A.csv", row.names = FALSE)


