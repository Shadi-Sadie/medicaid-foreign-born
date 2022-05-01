library(tidycensus)
install.packages("survey")
library(gtsummary)

library(survey)

install.packages("srvyr")
library("srvyr")



ACS<-Data


sum(Data$PWGTP)

297,900,179


Data %>%
        count(SEX, AGEP, wt = PWGTP) 

Data %>%
        count(SEX, AGEG, wt = PWGTP ) 




 Data %>%
        group_by(CITG, RACE) %>%
         summarise(HICOV = mean(HICOV))
 
 
 ### Chosing the survery weight #### 
 
 options( "survey.replicates.mse" = TRUE)
 pums_p.rep <- svrepdesign(repweights = pums_p[207:286],
                           weights = ~PWGTP,
                           combined.weights = TRUE,
                           type = "JK1",
                           scale = 4/80, rscales = rep(1, 80),
                           data = pums_p)
 
 pums <- svydesign(id = ~1,
                   weights = pums$PWGTP,
                   data = pums,
                   repweights = "pwgtp[0-9]+",
                   type = "JKn",
                   scale = 4/80,
                   rscales = rep(1,80),
                   combined.weights = TRUE
 )
 
 
 acs_design <-
         svrepdesign(
                 weight = ~pwgtp ,
                 repweights = 'pwgtp[0-9]+' ,
                 scale = 4 / 80 ,
                 rscales = rep( 1 , 80 ) ,
                 mse = TRUE ,
                 type = 'JK1' ,
                 data = acs_df
         )
 
 svy <- svrepdesign(data = data, weight = ~ASECWT, repweights = “REPWTP[0-9]+”, type = “JK1”, scale = 4/160, rscales = rep(1, 160), mse = TRUE)
 
 
 
 svy <- svrepdesign(data = data, weight = ~PERWT , repweights = “REPWTP[0-9
 
 
  svy <- as_survey(data, 
           weight = ASECWT, 
         repweights = matches("REPWTP[1-160]+"))
                                                                        
  svy <- as_survey(data, weight = PERWT , repweights = matches(“REPWTP[0-9]+”), 
                   type = “JK1”, scale = 4/ 80 , rscales = rep(1, 80 ), mse = TRUE)
  
  
  
 
 
Subset <- Data %>% select("CITG", "AGEG" ,"HINS1", "HINS2", "HINS3", "HINS4", "HICOV", "MARG", "SCHLG",
                          "SEX", "ESRG" , "ETHN", "POVPIPG", "RACE", "EXPANSION", grep("PWGTP", colnames(Data)))

 
## Seting the survy 

options( "survey.replicates.mse" = TRUE)
SVY <- svrepdesign(repweights = Subset[17:96],
                   weights = ~PWGTP,
                   combined.weights = TRUE,
                   type = "JK1",
                   scale = 4/80, rscales = rep(1, 80),
                   data = Subset)
colnames(Subset)



Subset %>% tbl_summary(by = EXPANSION) %>% add_p()
 


SVY %>%
        tbl_svysummary(by = EXPANSION ) %>% add_overall() 
       