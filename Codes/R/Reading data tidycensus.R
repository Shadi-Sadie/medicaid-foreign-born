
#install.packages("tidycensus")
#install.packages("tigris")
#install.packages('Rcpp') 

library(tidycensus)
library(tidyverse)

census_api_key("94b2e4e0fbd4ea5b12065dabc6c8b7292a615248", install = TRUE)

                                ## getting the data for year 2019 ##

data <- get_pums(
        variables = c ("REGION","AGEP", "CIT" , "ENG" , "FER","HIMRKS", "HINS1", 
                       "HINS2", "HINS3" , "HINS4","HINS5", "HINS6", "HINS7", "LANX", 
                       "MAR", "SCHL", "SEX", "YOEP", "ANC1P","DIS","HICOV", "ESR", 
                       "LANP", "NATIVITY","HISP", "POBP", "POVPIP", "PUBCOV","PRIVCOV", "RAC1P"),
        state = "all",
        year = 2019, 
        survey = "acs1",
        variables_filter = list(
                AGEP = 25:64
        ),
        rep_weights = "person", 
        recode = FALSE

)



                                ## Creating New Variables ##

### DATE 








