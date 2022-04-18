                                # Read the Data Set #

Data<-read.csv("2012.csv", header = TRUE, sep = ",", quote = "\"",
                    dec = ".", fill = TRUE)
                                # Subset the Data #
ACS2019A<-Data

#Variables subset 

names<-("REGION", "ST","ADJINC",
        "AGEP", "CIT" , "ENG" ,"FER","HIMRKS", "HINS1", "HINS2", "HINS3" , "HINS4", "HINS5", "HINS6", "HINS7", "INTP", "LANX",
        "MAR", "OIP", "PAP", "RETP", "SCHL", "SEMP", "SEX", "SSP", "YOEP", "ANC1P",
        "DIS","HICOV", "ESR", "LANP", "NATIVITY","HISP", "NOP", "PERNP", "ADJINC", "PINCP", "POBP", "POVPIP", "PUBCOV","PRIVCOV", "RAC1P")

# Filter Age

ACS2019A<- ACS2019A %>% filter((65 > AGEP)&(AGEP>=25))#&(INCOME2<=3))

# Filter Income


# Filter Child


# Filter Immigration 


# Filter State


                                # Creating New Variables #

### DATE 

ACS2019A$YEAR<-2019

### YEAR LIVED IN US: ULIY

table(ACS2019A$YOEP, exclude = NULL)

ACS2019A$YLIU<-ACS2019A$YEAR-ACS2019A$YOEP

### MEDICAID EXPANSION 


                               # Cleaning the Data #
### AGE

# Grouping Age 
# 1. 25-34
# 2. 35-44
# 3. 45-54
# 4. 55-64

ACS2019A$AGEG<-0
ACS2019A$AGEG<-ifelse(CS2019A$AGEP>25 & ACS2019A$AGEP<34,1,ACS2019A$AGEG)
ACS2019A$AGEG<-ifelse(CS2019A$AGEP>35 & ACS2019A$AGEP<44,2,ACS2019A$AGEG)
ACS2019A$AGEG<-ifelse(CS2019A$AGEP>45 & ACS2019A$AGEP<54,3,ACS2019A$AGEG)
ACS2019A$AGEG<-ifelse(CS2019A$AGEP>55 & ACS2019A$AGEP<64,4,ACS2019A$AGEG)


### CITIZENSHIP STATUS

# 1: U.S Citizen by Born 
# 2: Naturalizaed Citizen
# 3: Not Citizen

ACS2019A$CITG<-0
ACS2019A$CITG<-replace(ACS2019A$CITG,ACS2019A$CIT %in% c(1,2,3),1)
ACS2019A$CITG<-replace(ACS2019A$CITG,ACS2019A$CIT==4,2)
ACS2019A$CITG<-replace(ACS2019A$CITG,ACS2019A$CIT==5,3)

## Individual nativity
# 1 .Native
# 2 .Foreign born

table(ACS2019A$NATIVITY, exclude = NULL)


## Parent's Nativity (probably not applicable)

table(ACS2019A$NATIVITY, exclude = NULL)

### IINSUARANCE STATUS

# HIMRKS
# Subsidized Marketplace Coverage

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


### U.S. Region
#1 .Northeast
#2 .Midwest
#3 .South
#4 .West
#9 .Puerto Rico

table(ACS2019A$REGION, exclude = NULL)


### MARTIAL STATUS
        # 0: Married
        # 1: Single

table(ACS2019A$MAR, exclude = NULL)
table(ACS2019A$MSP, exclude = NULL)
ACS2019A$MARG<-0
ACS2019A$MAR<-replace(ACS2019A$MAR,ACS2019A$MAR %in% c(3,4,5),1)


### EMPLOYMENT STATUS : "ESR"
#b .N/A (less than 16 years old)
#1 .Civilian employed, at work
#2 .Civilian employed, with a job but not at work
#3 .Unemployed
#4 .Armed forces, at work
#5 .Armed forces, with a job but not at work
#6 .Not in labor force


table(ACS2019A$MAR, exclude = NULL)



### INCOME LEVEL  

#1.0 to 100% poverty 
#2. 100 t0 138% poverty

#ADJINC , "OIP", "PAP", RETP, SEMP, SSP
#Total Earning 
#PERNP
#Total income
#PINCP
# Income to poverty Ratio: POVPIP

POVPIP
table(ACS2019A$POVPIP, exclude = NULL)

### EDUCATION STATUS
# 1. Less than High school 
# 2. High school
# 3. Some College 
# 4. College degree
# 5. More than College
table(ACS2019A$SCHL, exclude = NULL)
# creating a school group variable
ACS2019A$SCHLG<-0
# assigning values 
ACS2019A$SCHLG<-ifelse(ACS2019A$SCHL>01 & ACS2019A$SCHL< 16,1,ACS2019A$SCHLG)
ACS2019A$SCHLG<-ifelse(ACS2019A$SCHL %in% c(16,17),2,ACS2019A$SCHLG)
ACS2019A$SCHLG<-ifelse(ACS2019A$SCHL %in% c(18,19),3,ACS2019A$SCHLG)
ACS2019A$SCHLG<-ifelse(ACS2019A$SCHL %in% c(20,21),4,ACS2019A$SCHLG)
ACS2019A$SCHLG<-ifelse(ACS2019A$SCHL %in% c(22,23,24),5,ACS2019A$SCHLG)

### SEX 

table(ACS2019A$SEX, exclude = NULL)


### RACE AND ETHINICITY 

## RACE

# 1.White
# 2.Black or African American alone
# 3.American Indian alone
# 4.Alaska Native alone
# 5.American Indian and Alaska Native tribes specified; or.American Indian or Alaska Native, not specified and no other.races
# 6.Asian alone
# 7.Native Hawaiian and Other Pacific Islander alone
# 8.Some Other Race alone
# 9.Two or More Races

## ETHNICITY

table(ACS2019A$HISP, exclude = NULL)
# creat ethnicity variable
# 0 : Not hispanic
# 1 : Hispanic

ACS2019A$ETHN<-1
ACS2019A$ETHN<-ifelse(ACS2019A$HISP==0,0,ACS2019A$ACS2019A$ETHN)

### DISABILITY STATUS
        # 1 .With a disability
        # 2 .Without a disability

table(ACS2019A$DIS, exclude = NULL)


### ACCULTRATION 

"LANP"

# Place of birth : POBP






#Divide ADJINC by 1,000,000 to obtain the inflation adjustment 
#factor and multiply it to the PUMS variable value to adjust it to
#2020 dollars. Variables requiring ADJINC on the Housing Unit file 
#are FINCP and HINCP. Variables requiring ADJINC on
#the Person files are: INTP, OIP, PAP, PERNP, PINCP, RETP, SEMP, SSIP, SSP, and WAGP.