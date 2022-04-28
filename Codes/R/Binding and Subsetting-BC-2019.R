## In this file I am reading the two csv files of each year. binding them in one data set called Data 
## and subseting the variable I need from this data set and call it ACS. Finaally, I  wil change the vacant to the NA 



#### library 

library(tidyverse)
library(sqldf)          # library to allow to subset while reading the data


                                        ### 1. Reading the data from internet 

temp <- tempfile()
temp2 <- tempfile()

fileUrl <- "https://www2.census.gov/programs-surveys/acs/data/pums/2019/1-Year/csv_pus.zip"

download.file(fileUrl, destfile= temp, method = "curl")

unzip(zipfile = temp, exdir = temp2)

Data1<-read.csv.sql(file.path(temp2, "psam_pusa.csv"),"select * from file where AGEP>24 AND AGEP<65 " ,
                   header = TRUE, sep ="," , method = "raw") 

Data2<-read.csv.sql(file.path(temp2, "psam_pusb.csv"),"select * from file where AGEP>24 AND AGEP<65 " ,
                   header = TRUE, sep =",", method = "raw") 

unlink(c(temp, temp2))


                                        ### 2. Binding the data set

Data<-rbind(Data1,Data2)

                                        ### 3. Subseting the variable 

# Removing the institutionlized data 

Data<-Data[!(Data$RELSHIPP %in% c(37,38)),] 

# Choosing only var that I need for analysis

varname<-colnames(Data)  # names of all variables 

needvar<-c ("REGION","ST","AGEP", "CIT" , "ENG" , "FER","HIMRKS", "HINS1",   # Variable I would need for the analysis
              "HINS2", "HINS3" , "HINS4","HINS5", "HINS6", "HINS7", "LANX", 
              "MAR", "SCHL", "SEX", "YOEP", "ANC1P","DIS","HICOV", "ESR", 
              "LANP", "NATIVITY","HISP", "POBP", "POVPIP", "PUBCOV","PRIVCOV","RAC1P")

allneedvar<-c(which(varname %in% needvar) , grep("PWGTP", varname))   # creating a vector for all variables I want to subset 

Data<-Data[allneedvar]   # Subseting all the variable at once


# Fixing the charachter to integer

classcol<-sapply(Data, class)
table(classcol)
char_columns <- which(classcol=="character")   
for (i in char_columns){
        Data[,i] <- as.integer(Data[,i])
}


# subseting the income 

Data<- Data %>% filter(POVPIP<139)   # Subset the income level only less than <139  


# subsetting based on years of immigration

Data$YEAR<-2019  #  Creating variable year 

table(Data$YLIU, exclude = NULL) # looking at the table of years living in US
Data$YLIU<-0                      # create variable YLIU year lived in US to use for removing immigrant with less than 5 years
Data$YLIU<-Data$YEAR-Data$YOEP  
Data$YLIU<-replace(Data$YLIU, Data$CIT %in% c(2,3),NA) # born in foreign could inter to us as well but i don't want it recode them NA

Data<-Data[!(Data$YLIU<5 & Data$CIT==5),]  # Removing the data of immigrant with less than 5 years residency

# Removing the MA and OR from the data 

Data<-Data[!(Data$ST %in% c(25,41)),]  # Looked up State FIPS code MA is 25 and OR is 41
table(Data$ST, exclude=NULL)


### 4. Creating the new data sample 


write.csv(Data, file = "BACS2019.csv", row.names = FALSE)




