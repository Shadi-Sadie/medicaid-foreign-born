
##############################################################################################################
### This program pulls the data for non-elderly adults with low income from ACS API and merge them in a 
### single data set called RAWACS needs further cleaning to be used for the Medicaid and Immigrant Analysis
### It was updated on Oct 16, 2025. 
##############################################################################################################

### Installing required packages

#install.packages('Rcpp') 
#install.packages("tidycensus")

### Loading Required Packages 

library(tidycensus)
library(tidyverse)


wd <- list()
# commonly used paths in my working directory  #######NOTE: UPDATE this section for the reproducibility## #################
wd$data   <- "/home/shadi/Projects/GitHub/medicaid-foreign-born/data/"
wd$output <- "/home/shadi/Projects/GitHub/medicaid-foreign-born/output/"
wd$texts <- "/home/shadi/Projects/GitHub/medicaid-foreign-born/text/"
wd$codes <- "/home/shadi/Projects/GitHub/medicaid-foreign-born/codes/R/"



### Uploading the Key to access to the ACS's API

census_api_key("94b2e4e0fbd4ea5b12065dabc6c8b7292a615248", install = TRUE)


### Creating a for loop to dl all the annual dataset at once  ##
# Note: POVPIP and AGEP are filtered at source for speed.


ldf <- list() # creates a list

for ( i in 2011:2019 )  {
        
        vars <- c("REGION","AGEP","CIT","ENG","FER",
                  "HINS1","HINS2","HINS3","HINS4","HINS5","HINS6","HINS7",
                  "MAR","SCHL","SEX","YOEP","DIS","HICOV","ESR",
                  "NATIVITY","HISP","POVPIP","RAC1P","TYPE","LANP","LANX","POBP","PUMA",
                  "COW","MIL","OCCP","SCH","HHT","RELP")
        
        if (i == 2019) vars[length(vars)] <- "RELSHIPP"  # <- only 2019 uses RELSHIPP
        
        ldf[[i]] <- get_pums(
                variables = vars,
                state = "all",
                year = i,
                variables_filter = list(AGEP = 19:64, POVPIP = 000:138),
                survey = "acs1",
                rep_weights = "person",
                recode = FALSE
        )
        
        ldf[[i]]$YEAR <- i
        assign(paste0("ACS", i), ldf[[i]])
}


## The relationship variable has a different name in 2019; rename it to RELP.
names(ACS2019)[names(ACS2019) == "RELSHIPP"] <- "RELP"




## bind all the data in one dataset 
ACS<-rbind(ACS2011,ACS2012,ACS2013,ACS2014,ACS2015,ACS2016,ACS2017,ACS2018,ACS2019)

### Change the data format from tbl-df to data frame 

ACS=as.data.frame(ACS)


#### 

write.csv(ACS, file = paste0(wd$data,"RAWACS-2025.csv"), row.names = FALSE)


save(ACS, file =paste0(wd$data,"RAWACS-2025.RData"))
