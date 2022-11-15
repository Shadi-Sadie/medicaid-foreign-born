
##############################################################################################################
### This program's aim is to clean the data for the final use 
### last update : Nov 15, 2022
#############################################################################################################



                                                # loading required library# 

library(labelled)

                                                # Reading Dataset #

# Data<-read.csv("PREACS.csv", header = TRUE, sep ="," , fill = TRUE)


                                                # Cleaning the Data #


Varname<-colnames(Data) # Checking the names of all variable

### 1.  STATE - No cleaning needed

### 2.  TYPE - Variable not needed drop 


### 3. REGION - No cleaning needed



  
### 4. AGEP to AGEG

table(Data$AGEP, exclude = NULL)

Data$AGEG<-0
Data$AGEG<-ifelse(Data$AGEP>25 & Data$AGEP<35,1,Data$AGEG) # 1. 25-34
Data$AGEG<-ifelse(Data$AGEP>=35 & Data$AGEP<45,2,Data$AGEG) # 2. 35-44
Data$AGEG<-ifelse(Data$AGEP>=45 & Data$AGEP<55,3,Data$AGEG) # 3. 45-54
Data$AGEG<-ifelse(Data$AGEP>=55 & Data$AGEP<65,4,Data$AGEG) # 4. 55-64

table(Data$AGEG, exclude = NULL)
Data$AGEG<- ordered(Data$AGEG, labels = c("25-34", "35-44", "45-54","55-64" ))
var_label(Data$AGEG) <- "Age (years)"


### 4. CITIZENSHIP STATUS

Data$CITG<-1
Data$CITG<-replace(Data$CITG,Data$CIT==4,2) # 4. Naturalized citizen
Data$CITG<-replace(Data$CITG,Data$CIT==5,3) # 5. Not citizen

Data$CITG<- ordered(Data$CITG, labels = c("US Born", "Naturalized-citizen", "Non-citizen" )) # labeling the values 
var_label(Data$CITG) <- "Citizenship Status"

        # 1: U.S Citizen by Born 
        # 2: Naturalizaed Citizen
        # 3: Not Citizen

### 5. ABILITY SPEAK ENGLISH 

table(Data$ENG, Data$YEAR , exclude = NULL)   # NA .Speaks only english  # 1 .Very well # 2 .Well # 3 .Not well # 4 .Not at all

   # looking at the data, it is clear that there was a change in recoding by ACS. Prior to 2017, only english speaker 
        # were coded as 0 ,but after 2017 they were coded as NA. We need to frist fix this.

Data$ENG<-replace(Data$ENG,is.na(Data$ENG),5) #
table(Data$ENG,exclude = NULL)

Data$ENG<- ordered(Data$ENG, labels = c("Very well", "Well", "Not well", "Not at all", "Only english" ))
var_label(Data$ENG) <- "English Proficiency"



   
### 6. FER : Gave birth to child within the past 12 months, 

 # We obsesrve similar pattern as ENG here before 2017, men and not applicable interviewee were coded 0 but after 2017 there were coded NA

Data$FER<-replace(Data$FER,Data$FER==0,NA) # Change the 0 value to NA

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
 
Data$SCHLG<-0 # creating a school group variable
Data$SCHLG<-ifelse(Data$SCHL>=1 & Data$SCHL< 16,1,Data$SCHLG) # 1. Less than High school 
Data$SCHLG<-ifelse(Data$SCHL %in% c(16,17),2,Data$SCHLG) # 2. High school
Data$SCHLG<-ifelse(Data$SCHL %in% c(18,19),3,Data$SCHLG) # 3. Some College 
Data$SCHLG<-ifelse(Data$SCHL %in% c(20,21),4,Data$SCHLG) # 4. Associate or Bachler degree
Data$SCHLG<-ifelse(Data$SCHL %in% c(22,23,24),5,Data$SCHLG) # 5. Graduate degree



Data$SCHLG<- ordered(Data$SCHLG, labels = c("Less than high school", "High school", "Some college", "College", "Graduate degree" ))
var_label(Data$SCHLG) <- "Education"


### 11. SEX : Porbably no need for the cleaning and recoding, I only labeled the values 
# 1. Male
# 2. Female 

Data$SEX<- ordered(Data$SEX, labels = c("Male", "Female" ))
var_label(Data$SEX) <- "Sex"
table(Data$SEX)

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

Data$POVPIPG<-1 # 1.0 to 100% poverty 
Data$POVPIPG<-ifelse(Data$POVPIP>100,2,Data$POVPIPG) #2.100 t0 138% poverty

Data$POVPIPG<- ordered(Data$POVPIPG, labels = c("Income below 100% poverty", "Income  100 to 138% poverty"))
var_label(Data$POVPIPG) <- "Income"

table(Data$POVPIP, exclude = NULL)
table(Data$POVPIPG,Data$CITG ,exclude = NULL)


### 19. RACE  

Data$RACE<-Data$RAC1P
Data$RACE<-ifelse(Data$RAC1P %in% c(4,5),3,Data$RACE) # 4. Asian# 5. Native Hawaiian and Other Pacific Islander
Data$RACE<-ifelse(Data$RAC1P==6,4,Data$RACE) # 6. Asian
Data$RACE<-ifelse(Data$RAC1P %in% c(7,8,9),5,Data$RACE)  # 8.Some Other Race alone # 9.Two or More Races

Data$RACE<- ordered(Data$RACE, labels = c("White", "Black", "American Indian or Alaska Native", "Asian", "Other"))
var_label(Data$RACE) <- "Race"

table(Data$RACE, exclude = NULL)
table(Data$RAC1P, exclude = NULL)



                                                #### Exporting the data set into the new data sample both CSV and Stata ####



# write.csv(Data, file = "CLNACS.csv", row.names = FALSE)

# require(foreign)
# write.dta(Data, "CLNACS.dta")
