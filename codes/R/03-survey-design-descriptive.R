# Title: Survey design and descriptive
# Author: Shadi Seyedi
# Date Created: Apr 17, 2023
# Description: The purpose of this script is to assign the survey weights and create multiple tables for descriptive analysis
# such as baseline equivalence tables,Comparative Table summary statistics, Frequency Distribution Table, contingency table
# Data last edit: Jun 15, 2023


ACS<-read.csv("CLNACS.csv", header = TRUE, sep ="," , fill = TRUE)

####################
## Required Packages
####################


# library(tidycensus)
library(tidyverse)
library(gtsummary)
library(survey)
#install.packages("srvyr")
#library("srvyr")

########################################
## Setting survey weights: 3 methods(pkg)
########################################

#1. Using the tidycensus package

  # svy<-to_survey(
        #         Data,
        #         type = c("person"),
        #         class = c("srvyr","survey"),
         #        design = "rep_weights"     #
#  )
#2. Using srvyr package

#  svy <- as_survey(Data, weight = PWGTP , repweights = matches("PWGTP[0-9]+"),
                   # type = "JK1", scale = 4/ 80 , rscales = rep(1, 80 ), mse = TRUE)

# 3.Using the survey package
      
      ##3.1 svydesign: Survey sample analysis.

        svy <- svydesign(
                  id=~1,
                  weights = ~ PWGTP, 
                  repweights = ~ matches("PWGTP[0-9]+"), 
                  data = Data, 
                  type = "JK1",
                  scale = 4/80,
                  rscales = rep(1, 80),
                  mse = TRUE
            )

      ##3.2 svydesign: Specify survey design with replicate weights

     # svy <- survey::svrepdesign(
                   #  repweights = "PWGTP[0-9]+",
                   #  weights = ~PWGTP,
                   #  combined.weights = TRUE,
                    # type = "JK1",
                    # scale = 4 / 80, 
                 #    rscales = rep(1, 80),
                   #  data = Data,
                   #  mse = TRUE
      #)


#######################
## Descriptive tables
######################

################# Table 1: Baseline Characteristics for states ################# 

         # create pre-aca table for states variable
t1<-Data %>% 
        filter(ACA=="Pre-ACA")%>%
        select(ADA,IPC, UnempR,expansion) %>%
            tbl_summary(
                    by = expansion,
                    label = list(ADA ~ "State's Political Liberalism",
                                 UnempR ~"State's Unemployment Rate",
                                 IPC ~ "Immigration Policy Climate"
                                  ),
                     statistic = list(all_continuous() ~ "{mean} ({sd})")
                     )%>%
                      add_difference() %>%
                      add_significance_stars(hide_ci = TRUE, hide_p = FALSE)%>%
                      modify_caption("**Table 1. Baseline Comparison of States**") %>%
                     # modify_spanning_header(all_stat_cols() ~ "**Table 1.Baseline Comparison of States**") %>%
                      modify_header(
                                  label = "**Variable**",
                                  stat_1 = '**Non Expansion**',
                                   stat_2 = '**Expansion**'
                             )
# tab1 # check if all is correct

# create post-aca table for states variable

t2<-Data %>% 
  filter(ACA=="Post-ACA")%>%
  select(ADA,IPC, UnempR,expansion) %>%
  tbl_summary(
    by = expansion,
    label = list(ADA ~ "State's Political Liberalism",
                 UnempR ~"State's Unemployment Rate",
                 IPC ~ "Immigration Policy Climate"
    ),
    statistic = list(all_continuous() ~ "{mean} ({sd})")
  )%>%
  add_difference() %>%
  add_significance_stars(hide_ci = TRUE, hide_p = FALSE)%>%
  modify_caption("**Table 1. Baseline Comparison of States**") %>%
  # modify_spanning_header(all_stat_cols() ~ "**Table 1.Baseline Comparison of States**") %>%
  modify_header(
    label = "**Variable**",
    stat_1 = '**Non Expansion**',
    stat_2 = '**Expansion**'
  )


#tab2 # check if everything is correct

## Stack two tables to create table 1

table1 <-
  tbl_stack(list(t1, t2), group_header = c("**Pre-ACA**", "**Post-ACA**"))

table1

############ Table 2.Baseline Characteristics by Nativity. #########
  
# unweighted table2
  tableA1<-Data %>%  filter(ACA=="Pre-ACA")%>%
  select(UNINS, HINS4, AGEP, SEX,  DIS, ESRG, MARG ,SCHLG , RACE1,POVPIPG,
            CIT,LTINU,ENG, CULRG,CORIGIN,expansion, NATIVITY ) %>%
            tbl_strata(
                       strata = expansion,
                      .tbl_fun = ~ .x %>% 
                        tbl_summary(
                               by = NATIVITY,
                               missing = "no",
                               label = list(AGEP ~ "Age", UNINS ~ "Uninsured",HINS4 ~ "Medicaid coverage", SEX ~ "Sex", DIS ~"Disability",
                                            ESRG~"Current employment status", MARG ~"Marital status",SCHLG~"Education" , RACE1~"Race/ethnicity",
                                            POVPIPG ~ "Federal poverty",CIT~"Citizenship status",LTINU ~"Lifetime in US",
                                            ENG~"Self-rated English proficiency", CORIGIN~"Country/Region of birth", CULRG~"Cultural clusters"
                                           )
                                    ) %>%
                       #add_p() 
                        add_p(
                          # perform t-test for all variables
                          test = list(all_continuous() ~ "t.test", all_categorical() ~ "chisq.test"),
                          # assume equal variance in the t-test
                         # test.args = all_tests("t.test") ~ list(var.equal = TRUE)
                        )
                      %>%
                       #add_significance_stars(hide_ci = TRUE, hide_p = FALSE)%>%
                       italicize_labels() %>%
                       bold_labels() %>%
                        modify_caption("**Table 2.Baseline Characteristics by Nativity**")
                      #modify_spanning_header(stat_1 ~ "**Non Expansion**", stat_2 ~ "**Expansion**")
                      
                      #.header = "**{strata}**, N = {n}"
                       )
  tableA1
  
  
  
  table2<-svy %>% subset(ACA=="Pre-ACA")%>%
      tbl_strata(
           strata = expansion,
          .tbl_fun = ~ .x %>% 
                tbl_svysummary(
                         by = NATIVITY,
                         include = c(
                                 UNINS, HINS4, AGEP, SEX,  DIS, 
                                 ESRG, MARG ,SCHLG , RACE1,POVPIPG,
                                 CIT,LTINU,ENG,CULRG,CORIGIN 
                                     ),
                         missing = "no",
                         label = list(
                               AGEP ~ "Age", UNINS ~ "Uninsured",
                               HINS4 ~ "Medicaid coverage", SEX ~ "Sex", DIS ~"Disability",
                               ESRG~"Current employment status", MARG ~"Marital status",
                               SCHLG~"Education" , RACE1~"Race/ethnicity",POVPIPG ~ "Federal poverty",
                               CIT~"Citizenship status",LTINU ~"Lifetime in US",
                               ENG~"Self-rated English proficiency", 
                               CORIGIN~"Country/Region of birth", CULRG~"Cultural clusters"
                                     )
                              ) %>%
                add_p()                  %>%
                  #add_p (
                  # perform t-test for all variables
                  #test = list(all_continuous() ~ "t.test", all_categorical() ~ "chisq.test"),
                  # assume equal variance in the t-test
                  # test.args = all_tests("t.test") ~ list(var.equal = TRUE)
                        #)                 %>%
               add_significance_stars(hide_ci = TRUE, hide_p = FALSE)%>%
               italicize_labels() %>%
               bold_labels() %>%
               modify_caption("**Table 2.Baseline Characteristics by Nativity**")
              #modify_spanning_header(stat_1 ~ "**Non Expansion**", stat_2 ~ "**Expansion**")
            )
  
  print(table2)
  
  
################# Table 3: Uninsured and Medicaid coverage rate before and after ACA for expansion and non-expansion state by characteristics*

  ## Uninsured Section 

 t1<- Data %>% 
      filter(expansion=="expansion")%>%
      select(UNINS, CIT, LTINU,  RACE1, SEX,ESRG, MARG, SCHLG,POVPIPG, ENG,CULRG, CORIGIN, DIS,expansion,ACA) %>%  
      tbl_strata(
           strata = ACA,
          .tbl_fun = ~ .x %>%  
                  tbl_summary(
                        by = UNINS,
                        include = c(CIT, LTINU,  RACE1,SEX,ESRG, MARG, SCHLG,POVPIPG, ENG, CORIGIN, DIS,CULRG) ,
                        label = list(
                                     RACE1~"Race/ethnicity", CIT~"Citizenship status",LTINU ~"Lifetime in US",
                                     SEX ~ "Sex", DIS ~"Disability", ESRG~"Current employment status", MARG ~"Marital status",
                                     SCHLG~"Education" , POVPIPG ~ "Federal poverty",ENG~"Self-rated English proficiency",
                                     CORIGIN~"Country/Region of birth", CULRG~"Cultural clusters"
                                     ), 
                        percent = "row",
                        statistic = list(all_categorical() ~ "{p}%")
                             ) %>%
                    modify_column_hide(columns = stat_1)%>%
                    italicize_labels() #%>%
                    # modify_header(stat_2 = "N = {n} ({style_percent(p)}%)") %>%
                    # modify_footnote(update = everything() ~ NA) # if you'd like no footnote remove #
                )

 t2<- Data %>% 
   filter(expansion=="Non-expansion")%>%
   select(UNINS, CIT, LTINU,  RACE1, SEX,ESRG, MARG, SCHLG,POVPIPG, ENG,CULRG, CORIGIN, DIS,expansion,ACA) %>%  
   tbl_strata(
       strata = ACA,
      .tbl_fun = ~ .x %>%  
           tbl_summary(
                      by = UNINS,
                      include = c(CIT, LTINU,  RACE1,SEX,ESRG, MARG, SCHLG,POVPIPG, ENG, CORIGIN, DIS,CULRG) ,
                      label = list (
                                RACE1~"Race/ethnicity", CIT~"Citizenship status",LTINU ~"Lifetime in US",
                                SEX ~ "Sex", DIS ~"Disability", ESRG~"Current employment status", MARG ~"Marital status",
                                SCHLG~"Education" , POVPIPG ~ "Federal poverty",ENG~"Self-rated English proficiency",
                                CORIGIN~"Country/Region of birth", CULRG~"Cultural clusters"
                                   ), 
                      percent = "row",
                      statistic = list(all_categorical() ~ "{p}%")
                     ) %>%
          modify_column_hide(columns = stat_1) %>%
          italicize_labels() #%>
            )
 
# creating the merged table out of the t1 and t2
 tab1<-tbl_merge(
              tbls = list(t1, t2),
              tab_spanner = c("**Expansion**", "**Non-expansion**")
                 )%>%
               modify_header(
                 stat_2_1_1 = "**Pre-ACA** ({style_percent(p)}%)",
                 stat_2_2_1 = "**Post-ACA** ({style_percent(p)}%)",
                 stat_2_1_2 = "**Pre-ACA** ({style_percent(p)}%)",
                 stat_2_2_2 = "**Post-ACA** ({style_percent(p)}%)"
                 ) 


##### Medicaid coverage section

 t1<- Data %>% 
   filter(expansion=="expansion")%>%
   select(HINS4, CIT, LTINU,  RACE1, SEX,ESRG, MARG, SCHLG,POVPIPG, ENG,CULRG, CORIGIN, DIS,expansion,ACA) %>%  
   tbl_strata(
     strata = ACA,
     .tbl_fun = ~ .x %>%  
       tbl_summary(
         by = HINS4,
         include = c(CIT, LTINU,  RACE1,SEX,ESRG, MARG, SCHLG,POVPIPG, ENG, CORIGIN, DIS,CULRG) ,
         label = list(
           RACE1~"Race/ethnicity", CIT~"Citizenship status",LTINU ~"Lifetime in US",
           SEX ~ "Sex", DIS ~"Disability", ESRG~"Current employment status", MARG ~"Marital status",
           SCHLG~"Education" , POVPIPG ~ "Federal poverty",ENG~"Self-rated English proficiency",
           CORIGIN~"Country/Region of birth", CULRG~"Cultural clusters"
         ), 
         percent = "row",
         statistic = list(all_categorical() ~ "{p}%")
       ) %>%
       modify_column_hide(columns = stat_1)%>%
       italicize_labels() #%>%
     # modify_header(stat_2 = "N = {n} ({style_percent(p)}%)") %>%
     # modify_footnote(update = everything() ~ NA) # if you'd like no footnote remove #
   )
 
 t2<- Data %>% 
   filter(expansion=="Non-expansion")%>%
   select(HINS4, CIT, LTINU,  RACE1, SEX,ESRG, MARG, SCHLG,POVPIPG, ENG,CULRG, CORIGIN, DIS,expansion,ACA) %>%  
   tbl_strata(
     strata = ACA,
     .tbl_fun = ~ .x %>%  
       tbl_summary(
         by = HINS4,
         include = c(CIT, LTINU,  RACE1,SEX,ESRG, MARG, SCHLG,POVPIPG, ENG, CORIGIN, DIS,CULRG) ,
         label = list (
           RACE1~"Race/ethnicity", CIT~"Citizenship status",LTINU ~"Lifetime in US",
           SEX ~ "Sex", DIS ~"Disability", ESRG~"Current employment status", MARG ~"Marital status",
           SCHLG~"Education" , POVPIPG ~ "Federal poverty",ENG~"Self-rated English proficiency",
           CORIGIN~"Country/Region of birth", CULRG~"Cultural clusters"
         ), 
         percent = "row",
         statistic = list(all_categorical() ~ "{p}%")
       ) %>%
       modify_column_hide(columns = stat_1) %>%
       italicize_labels() #%>
   )
 
 
 tab2 <- tbl_merge(
                tbls = list(t1, t2),
                tab_spanner = c("**Expansion**", "**Non-expansion**")
                  )%>%
                 modify_header(
                        stat_2_1_1 = "**Pre-ACA** ({style_percent(p)}%)",
                        stat_2_2_1 = "**Post-ACA** ({style_percent(p)}%)",
                        stat_2_1_2 = "**Pre-ACA** ({style_percent(p)}%)",
                        stat_2_2_2 = "**Post-ACA** ({style_percent(p)}%)"

                 )

 
 ####  Merge Medicaid & Uninsured to create table 3
 
 
 table3<-tbl_merge(list(tab1,tab2),tab_spanner = c('Uninsured','Medicaid'))
 

 
 
 ################# Table 5: Uninsured Rate and Medicaid Coverage rate by Socio-Demographic Factors, Across Citizenship Status
 
 
 # Create a vector of variables
 variables <- c("UNINS", "HINS4")
 
 # Create an empty list to store the tables
 tables <- list()
 
 # Iterate over the variables
 for (var in variables) {
   # Create an empty list to store the tables for each value of CIT
   cit_tables <- list()
   
   # Get unique values of CIT
   cit_values <- unique(Data$CIT)
   
   # Iterate over the values of CIT
   for (cit_value in cit_values) {
     # Filter the data based on CIT
     filtered_data <- Data %>%
       filter(CIT == cit_value) %>%
       select({{ var }}, SEX, DIS, ESRG, MARG, SCHLG, RACE1, POVPIPG,LTINU, ENG, CULRG, CORIGIN)
     
     # Create the table using tbl_summary
     table <- filtered_data %>%
       tbl_summary(
         by = all_of(var),
         missing = "no",
         label = list(
           SEX ~ "Sex",
           DIS ~ "Disability",
           ESRG ~ "Current employment status",
           SCHLG ~ "Education",
           RACE1 ~ "Race/ethnicity",
           CULRG ~ "Cultural clusters",
           POVPIPG ~ "Federal poverty",
           LTINU ~ "Lifetime in US",
           MARG ~"Married",
           ENG ~ "Self-rated English proficiency",
           CORIGIN ~ "Country/Region of birth"
         ),
         percent = "row",
         statistic = list(all_categorical() ~ "{p}%")
       ) %>%
       modify_column_hide(columns = stat_1) %>%
       bold_labels()%>%
     modify_footnote(update = everything() ~ NA)
     # Modify the table header
     if (var == "UNINS") {
       table <- table %>%
         modify_header(stat_2 = "Uninsured {style_percent(p)}%")
     } else if (var == "HINS4") {
       table <- table %>%
         modify_header(stat_2 = "Medicaid {style_percent(p)}%")
     }
     
     # Add the table to the list for the specific value of CIT
     cit_tables[[cit_value]] <- table
   }
   
   # Add the list of tables for the variable to the main list
   tables[[var]] <- cit_tables
 }
 
 
 
 t <- list()
 
 for (cit_value in cit_values) {
   
   t[[cit_value]]  <- tbl_merge(list(tables$UNINS[[cit_value]], tables$HINS4[[cit_value]]))
   
 }
 
 tables<-tbl_merge(list(t$`Non-citizen`,t$`Naturalized-citizen`,t$`US-citizen Born abroad `,
                        t$`Born in US states`,t$`Born in US Territories`),
                   tab_spanner = c("Non-citizen","Naturalized-citizen",'Citizen born abroad',
                                   'Born in US states','Born in territories')
                  ) %>%
 modify_caption("Uninsured/Medicaid Rate by Socio-Demographic Factors, Across Citizenship Status")
 
 tables 
   

  
#####################
# Extra helpful codes
#####################

######## A. export the table to latex ######## 
#   as_gt(table2) %>% 
#   gt::as_latex()
##############################################


################ B. creating table using aggregate and goupby ################  
# insurance_mean <- aggregate(UNINS ~ RACE1 + expansion+NATIVITY, data=Data, FUN=mean)
# aggregate(UNINS ~ CIT + RACE1, data=Data, FUN=mean)
#
# b<-Data %>%  group_by(CIT)%>% 
# summarise(mean = mean(UNINS), n = n())
# aggregate(UNINS ~  CIT , data=Data, FUN=length)
################################################################################     


################ C. Another way to create table 3 ################  

# Data %>%  select(UNINS, CIT, LTINU,  RACE1, SEX,ESRG, MARG, SCHLG,POVPIPG, ENG, CORIGIN, DIS,  expansion,ACA) %>%
#  tbl_strata(
#    strata = c(expansion,ACA),
#    .tbl_fun = ~ .x %>% 
#               tbl_summary(
#                 by = UNINS,
#                 include = c(CIT, LTINU,  RACE1, SEX,ESRG, MARG, SCHLG,POVPIPG, ENG, CORIGIN, DIS) ,
#               percent = "row"
#             ) %>%
#              modify_column_hide(columns = stat_1) 

# )

  ################################################################################      

