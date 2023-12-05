
##############################################################################################################
### This program pulls the data for non-elderly adults with low income from ACS API and merge them in a 
### single data set called RAWACS needs further cleaning to be used for the Medicaid and Immigrant Analysis
### It was updated on Nov 11, 2022. 
##############################################################################################################

### Instaling required packages

#install.packages("tidycensus")
#install.packages("tigris")
#install.packages('Rcpp') 

### Loading Required Packages 

library(tidycensus)
library(tidyverse)

### Uploading the Key to access to the ACS's API

census_api_key("94b2e4e0fbd4ea5b12065dabc6c8b7292a615248", install = TRUE)


### Creating a for loop to dl all the annual dataset at once  ##

ldf <- list() # creates a list

for( i in 2011:2019)  {                            
        ldf[[i]] <- get_pums(
                variables = c ("REGION","AGEP", "CIT" , "ENG" , "FER","HINS1", 
                               "HINS2", "HINS3" , "HINS4","HINS5", "HINS6", "HINS7",
                               "MAR", "SCHL", "SEX", "YOEP","DIS","HICOV", "ESR", 
                                "NATIVITY","HISP", "POVPIP", "RAC1P", "TYPE","LANP","LANX","POBP","PUMA",
                               "COW","MIL","OCCP","SCH","HHT","RELP"
                               ),
                state = "all",
                year = i, 
                variables_filter = list(
                        AGEP = 19:64, POVPIP = 000:138 ## Filtering age and income #
                ),
                survey = "acs1",
                rep_weights = "person", 
                recode = FALSE,
        )
        ldf[[i]]$YEAR<-i ## Creat year and assigne value ##
        assign(paste0("ACS",i),ldf[[i]] )
}

### Bind all the annual date in a single data set 

ACS<-rbind(ACS2011,ACS2012,ACS2013,ACS2014,ACS2015,ACS2016,ACS2017,ACS2018,ACS2019)

### Change the data fromat from tbl-df to data frame 


ACS=as.data.frame(ACS)


#### Fixing the charachter to factro 

classcol<-sapply(ACS, class)
table(classcol)
char_columns <- which(classcol=="character")   
for (i in char_columns){
        ACS[,i] <- as.factor(ACS[,i])
}



## Upon checking all the variable and their value I noticed that  YOEP (Year of entry) for people borned in US is bottem coded instead of being NA , 
## thus I fix this mannaualy based on the data dictionary for each year 

# I can do it either in this way  

#ACS$YOEP<-ifelse(ACS$YEAR==2009 & ACS$YOEP==1918,NA,ACS$YOEP) #
#ACS$YOEP<-ifelse(ACS$YEAR==2010 & ACS$YOEP==1918,NA,ACS$YOEP) # 
ACS$YOEP<-ifelse(ACS$YEAR==2011 & ACS$YOEP==1918,NA,ACS$YOEP) # 
ACS$YOEP<-ifelse(ACS$YEAR==2012 & ACS$YOEP==1920,NA,ACS$YOEP) # 
ACS$YOEP<-ifelse(ACS$YEAR==2013 & ACS$YOEP==1920,NA,ACS$YOEP) # 
ACS$YOEP<-ifelse(ACS$YEAR==2014 & ACS$YOEP==1920,NA,ACS$YOEP) # 
ACS$YOEP<-ifelse(ACS$YEAR==2015 & ACS$YOEP==1920,NA,ACS$YOEP) # 
ACS$YOEP<-ifelse(ACS$YEAR==2016 & ACS$YOEP==1920,NA,ACS$YOEP) # 
ACS$YOEP<-ifelse(ACS$YEAR==2017 & ACS$YOEP==1924,NA,ACS$YOEP) # 
ACS$YOEP<-ifelse(ACS$YEAR==2018 & ACS$YOEP==1928,NA,ACS$YOEP) # 
ACS$YOEP<-ifelse(ACS$YEAR==2019 & ACS$YOEP==1928,NA,ACS$YOEP) # 


# or 

        # ACS$YOEP<-ifelse(ACS$CIT==1,NA,ACS$YOEP) # 


### Export this dataset as RAWACS for the future use 



write.csv(ACS, file = "RAWACS.csv", row.names = FALSE)



