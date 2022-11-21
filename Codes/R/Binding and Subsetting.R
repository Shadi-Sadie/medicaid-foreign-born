
##############################################################################################################
## In this program I am subseting Raw Data set based on the inclusion criteria. 
## Additonaly, I  will change the vacant to the NA and change the variable fromat from charachteer to integer
## Updated on Nov 14, 2022
##############################################################################################################
                                                        #### loading the  Data set ####


# remove the # for the  section below if you want to read the RAWACS from the internet  

# fileUrl <- "https://www2.census.gov/programs-surveys/acs/data/pums/2018/1-Year/csv_pus.zip"
# download.file(fileUrl, destfile= temp, method = "curl")
# ACS<- reat.csv()


#
 Data<-ACS


                                                        #### loading required library ####

library(tidyverse)
library(sqldf)          # library to allow to subset while reading the data

                                

                                                       #### Subsetting the dataset ####


###### 1. Subseting based on the income  #### Already subseted ####

#Data<- Data %>% filter(POVPIP<139)   # Subset the income level only less than <139  

###### 2. Subseting based on Age  #### Already subseted ####

Data<- Data %>% filter(AGEP<65 & AGEP>25) 


###### 3. Removing the institutionlized and unistitulionalized group quarters population


Data<-Data[!(Data$TYPE %in% c(2,3)),] 

 # variable TYPE is no longer  needed can be droped 

# Data<-Data[!(names(Data)=="TYPE")]

###### 4.Subsetting based on years lived in US (for immigrants)
 
Data$YLIU<-0                      # create variable YLIU year lived in US to use for removing immigrant with less than 5 years
Data$YLIU<-Data$YEAR-Data$YOEP  
table(Data$YLIU, exclude = NULL) # looking at the table of years living in US
table(Data$CIT, Data$YLIU, exclude = NULL) # looking at the table of years living in US by citizenshio status. 

#Data$YLIU<-replace(Data$YLIU, Data$CIT %in% c(2,3),NA) # born in foreign could inter to us as well but i don't want it recode them NA

Data<-Data[!(Data$YLIU<5 & Data$CIT==5),]  # Removing the data of immigrant with less than 5 years residency

###### 5. Removing the US citizen born outside US and those born portorico and virgin island and ..

#Data<-Data[!(Data$CIT %in% c(2,3) ),]  # Re


###### 5. Removing the state adopted medicaid expansion earlier

Data<-Data[!(Data$ST %in% c(10, 11, 25,36,50)),]  # Looked up State FIPS code MA is 25 and Delaware is 10, DC is 11 , NY it 36 and Vermont is 50 



                                                #### Creating the new data sample ####


# write.csv(Data, file = "PREACS.csv", row.names = FALSE)




