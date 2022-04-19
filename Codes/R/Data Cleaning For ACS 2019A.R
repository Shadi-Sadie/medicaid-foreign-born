                                # Read Libraries  #                         
install.packages("sqldf")
library(tidyverse)
# library to allow to subset while reading the data
library(sqldf)

                                 # Read the Data Set #

temp <- tempfile()
temp2 <- tempfile()

fileUrl <- "https://www2.census.gov/programs-surveys/acs/data/pums/2019/1-Year/csv_pus.zip"

download.file(fileUrl, destfile= temp, method = "curl")

unzip(zipfile = temp, exdir = temp2)

Data<-read.csv.sql(file.path(temp2, "psam_pusa.csv"),"select * from file where AGEP>24 AND AGEP<65 " , header = TRUE, sep ="," ) 

unlink(c(temp, temp2))

                                # Subset the Data #
ACS2019A<-Data

#Variables subset 

#SubSet of all variable

Col.names<-c ("REGION","ST","AGEP", "CIT" , "ENG" , "FER","HIMRKS", "HINS1", 
              "HINS2", "HINS3" , "HINS4","HINS5", "HINS6", "HINS7", "LANX", 
              "MAR", "SCHL", "SEX", "YOEP", "ANC1P","DIS","HICOV", "ESR", 
              "LANP", "NATIVITY","HISP", "POBP", "POVPIP", "PUBCOV","PRIVCOV", "RAC1P")
ACS2019A<-ACS2019A[Col.names]


# Subset of Weight

ACS2019APW<-Data[c(209:288)]

# append both subset in one 
ACS2019A<-cbind(ACS2019A,ACS2019APW)


                                # Creating New Variables #

### DATE 

ACS2019A$YEAR<-2019

### YEAR LIVED IN US: ULIY 

table(ACS2019A$YOEP, exclude = NULL)
ACS2019A$YOEP<-as.numeric(ACS2019A$YOEP)

ACS2019A$YLIU<-ACS2019A$YEAR-ACS2019A$YOEP

ACS2019A$YLIU<-replace(ACS2019A$YLIU, is.na(ACS2019A$YLIU), 99)
ACS2019A$YLIU<-replace(ACS2019A$YLIU, ACS2019A$CIT %in% c(2,3),99)

table(ACS2019A$YLIU, exclude = NULL)


### MEDICAID EXPANSION 
# OR and MA
# 1. Early Expanded  CA(2010), CT(2010), CO(2012), DC(2010), MN(2010), NJ(2011), WA(2011)
# 2. Not Expanded -  AL, FL, GA, KS, MS, NC, SC, SD,  TN , TX , WI, WY 
# 3. Late Expansion- AL(2015), ID(2020), ME(2019), MO(2021), MT(2016), NE(2020), OK(2021), UT(2020),
#                      VA (2019), LA(2016), IN(2015), PA(2015)
#               
#ACS2019A$Expansion<-0
#ACS2019A$Expansion<-ifelse(ACS2019A$ST %in% c(48,12,28,1, ),1,0)
#table(BRFSS$Expansion)

# BRFSS$Expansion<- ordered(BRFSS$Expansion,
#      levels = c(0,1),
#     labels = c("Medicaid Not Expanded","Medicaid Expanded"))



                               # Cleaning the Data #
### AGE

# Grouping Age 
# 1. 25-34
# 2. 35-44
# 3. 45-54
# 4. 55-64

ACS2019A$AGEG<-0
ACS2019A$AGEG<-ifelse(ACS2019A$AGEP>=25 & ACS2019A$AGEP<35,1,ACS2019A$AGEG)
ACS2019A$AGEG<-ifelse(ACS2019A$AGEP>=35 & ACS2019A$AGEP<45,2,ACS2019A$AGEG)
ACS2019A$AGEG<-ifelse(ACS2019A$AGEP>=45 & ACS2019A$AGEP<55,3,ACS2019A$AGEG)
ACS2019A$AGEG<-ifelse(ACS2019A$AGEP>=55 & ACS2019A$AGEP<65,4,ACS2019A$AGEG)

table(ACS2019A$AGEG, exclude = NULL)


### CITIZENSHIP STATUS

# 1: U.S Citizen by Born 
# 2: Naturalizaed Citizen
# 3: Not Citizen
 table(ACS2019A$CITG, exclude = NULL)
 table(ACS2019A$CIT, exclude = NULL)
 
ACS2019A$CITG<-0
ACS2019A$CITG<-replace(ACS2019A$CITG,ACS2019A$CIT %in% c(1,2,3),1)
ACS2019A$CITG<-replace(ACS2019A$CITG,ACS2019A$CIT==4,2)
ACS2019A$CITG<-replace(ACS2019A$CITG,ACS2019A$CIT==5,3)

## Individual nativity
# 1 .Native
# 2 .Foreign born

table(ACS2019A$NATIVITY, exclude = NULL)


### IINSUARANCE STATUS

# HIMRKS
# Subsidized Marketplace Coverage

table(ACS2019A$HIMRKS, exclude = NULL)

## Health insurance coverage recode
        #1 .With health insurance coverage
        #2 .No health insurance coverage
table(ACS2019A$HICOV, exclude = NULL)
## Public health coverage 
        #1 .With public health coverage
        #2 .Without public health coverage
table(ACS2019A$PUBCOV, exclude = NULL)
## Private health insurance coverage
        #1 .With private health insurance coverage
        #2 .Without private health insurance coverage


table(ACS2019A$HICOV, exclude = NULL)



### U.S. Region
#1 .Northeast
#2 .Midwest
#3 .South
#4 .West

table(ACS2019A$REGION, exclude = NULL)


### MARTIAL STATUS
        # 0: Married
        # 1: Single

table(ACS2019A$MAR, exclude = NULL)
ACS2019A$MARG<-0
ACS2019A$MARG<-replace(ACS2019A$MARG,ACS2019A$MAR %in% c(2,3,4,5),1)
table(ACS2019A$MARG, exclude = NULL)

### EMPLOYMENT STATUS : "ESRG" from ESR

#1 Employed
#2 Unemployed
#3 Not labor force

table(ACS2019A$ESRG, exclude = NULL)

ACS2019A$ESRG<-0

ACS2019A$ESRG<-ifelse(ACS2019A$ESR %in% c(1,2,4,5),1,ACS2019A$ESRG)
ACS2019A$ESRG<-ifelse(ACS2019A$ESR==3,2,ACS2019A$ESRG)
ACS2019A$ESRG<-ifelse(ACS2019A$ESR==6,3,ACS2019A$ESRG)


### INCOME LEVEL  

#1.0 to 100% poverty 
#2.100 t0 138% poverty

#ADJINC , "OIP", "PAP", RETP, SEMP, SSP " DROP ALL of THis 

ACS2019A$POVPIPG<-1

ACS2019A$POVPIPG<-ifelse(ACS2019A$POVPIP<101,0,ACS2019A$POVPIPG)

table(ACS2019A$POVPIP, exclude = NULL)

table(ACS2019A$POVPIPG, exclude = NULL)

### EDUCATION STATUS

# 1. Less than High school 
# 2. High school
# 3. Some College 
# 4. Associate or Bachler degree
# 5. Graduate degree
# creating a school group variable
ACS2019A$SCHLG<-0
# assigning values 
ACS2019A$SCHLG<-ifelse(ACS2019A$SCHL>=1 & ACS2019A$SCHL< 16,1,ACS2019A$SCHLG)
ACS2019A$SCHLG<-ifelse(ACS2019A$SCHL %in% c(16,17),2,ACS2019A$SCHLG)
ACS2019A$SCHLG<-ifelse(ACS2019A$SCHL %in% c(18,19),3,ACS2019A$SCHLG)
ACS2019A$SCHLG<-ifelse(ACS2019A$SCHL %in% c(20,21),4,ACS2019A$SCHLG)
ACS2019A$SCHLG<-ifelse(ACS2019A$SCHL %in% c(22,23,24),5,ACS2019A$SCHLG)

table(ACS2019A$SCHLG, exclude = NULL)

### SEX 

# 1. Male
# 2. Female 

table(ACS2019A$SEX, exclude = NULL)

### RACE AND ETHINICITY 

## RACE     
# 1.White
# 2.Black or African American alone
# 3.American Indian 
# 4.Alaska Native  
# 5.American Indian and Alaska Native tribes specified; or.American Indian or Alaska Native, not specified and no other.races
# 6.Asian alone
# 7.Native Hawaiian and Other Pacific Islander alone
# 8.Some Other Race alone
# 9.Two or More Races

table(ACS2019A$RAC1P, exclude = NULL)


## RACE    Modified 
# 1. White
# 2. Black or African American alone
# 3. American Indian or Alaska Native  
# 4. Asian
# 5. Native Hawaiian and Other Pacific Islander
# 6. Other Races 

table(ACS2019A$RAC1P, exclude = NULL)

ACS2019A$RACE<-ACS2019A$RAC1P
ACS2019A$RACE<-ifelse(ACS2019A$RAC1P %in% c(4,5),3,ACS2019A$RACE)
ACS2019A$RACE<-ifelse(ACS2019A$RAC1P==6,4,ACS2019A$RACE)
ACS2019A$RACE<-ifelse(ACS2019A$RAC1P==7,5,ACS2019A$RACE)
ACS2019A$RACE<-ifelse(ACS2019A$RAC1P %in% c(8,9),6,ACS2019A$RACE)

table(ACS2019A$RACE, exclude = NULL)


## ETHNICITY

table(ACS2019A$HISP, exclude = NULL)
# creat ethnicity variable
# 0 : Not hispanic
# 1 : Hispanic

ACS2019A$ETHN<-1
ACS2019A$ETHN<-ifelse(ACS2019A$HISP == 1,0,ACS2019A$ETHN)
table(ACS2019A$ETHN,exclude = NULL)

### DISABILITY STATUS
        # 1 .With a disability
        # 2 .Without a disability

table(ACS2019A$DIS, exclude = NULL)


### Lenghth of stay LANP


# Place of birth : POBP




# Subsetting data : INCLUSIONS #


# Filter Income


ACS2019A<- ACS2019A %>% filter(POVPIP<139)

table(ACS2019A$POVPIP, exclude = NULL)


# Filter Child


# Filter Immigration 

ACS2019A<-ACS2019A[!(ACS2019A$YLIU<5 & ACS2019A$CITG==3),]


# Filter State

                                        # Save the clean DataSet #

write.csv(ACS2019A, file = "CLNACS2019A.csv", row.names = FALSE)





