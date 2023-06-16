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


library(tidycensus)
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
  
  
################# Table 3: Uninsured rate before and after ACA for expansion and non-expansion state by characteristics*


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
 table3<-tbl_merge(
              tbls = list(t1, t2),
              tab_spanner = c("**Expansion**", "**Non-expansion**")
                 )%>%
               modify_header(
                 stat_2_1_1 = "**Pre-ACA** ({style_percent(p)}%)",
                 stat_2_2_1 = "**Post-ACA** ({style_percent(p)}%)",
                 stat_2_1_2 = "**Pre-ACA** ({style_percent(p)}%)",
                 stat_2_2_2 = "**Post-ACA** ({style_percent(p)}%)"
                 ) %>%
               modify_caption("**Table 3.Uninsured rate before and after ACA for expansion and non-expansion by characteristics**")

 
print(table3)


 ################# Table 4: Medicaid coverage rate before and after ACA for expansion and non-expansion state by characteristics*
 
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
 
 
 table4 <- tbl_merge(
                tbls = list(t1, t2),
                tab_spanner = c("**Expansion**", "**Non-expansion**")
                  )%>%
                 modify_header(
                        stat_2_1_1 = "**Pre-ACA** ({style_percent(p)}%)",
                        stat_2_2_1 = "**Post-ACA** ({style_percent(p)}%)",
                        stat_2_1_2 = "**Pre-ACA** ({style_percent(p)}%)",
                        stat_2_2_2 = "**Post-ACA** ({style_percent(p)}%)"
                              ) %>%
                 modify_caption("**Table 4. Medicaid coverage rate before/after ACA for expansion/non-expansion state by characteristics**")
 
 print(table4)
 
 ################# Table 5: Uninsured Rate by Socio-Demographic Factors, Across Citizenship Status
 
  table5<-Data %>%  select(UNINS,SEX, DIS, ESRG, MARG ,SCHLG , RACE1,POVPIPG,
                           CIT,LTINU,ENG, CULRG,CORIGIN ) %>%
    tbl_strata(
      strata = CIT,
      .tbl_fun = ~ .x %>% 
        tbl_summary(
          by = UNINS,
          missing = "no" ,
          label = list(SEX ~ "Sex", DIS ~"Disability",
                      ESRG~"Current employment status",
                      SCHLG~"Education" , RACE1~"Race/ethnicity",CULRG~"Cultural clusters",
                      POVPIPG ~ "Federal poverty",LTINU ~"Lifetime in US",
                      ENG~"Self-rated English proficiency", CORIGIN~"Country/Region of birth"
                      ),
          percent = "row",
          statistic = list(all_categorical() ~ "{p}%")
                   ) %>%
         
            # add_p() %>%
            # add_significance_stars(hide_ci = TRUE, hide_p = TRUE)%>%
            italicize_labels() %>%
             bold_labels() %>%
             modify_column_hide(columns = stat_1) %>%
             modify_header(stat_2 = "{style_percent(p)}%") %>%
             modify_spanning_header(stat_2 ~ "**Expansion**")%>%
        
              modify_caption("**Table 5. Uninsured Rate by Socio-Demographic Factors, Across Citizenship Status**")
      
      
      #.header = "**{strata}**, N = {n}"
    )
  print(table5)
  
  ################# Table 6: Medicaid Coverage Rate by Socio-Demographic Factors, Across Citizenship Status
  
  table6<-Data %>%  select(HINS4, SEX, DIS, ESRG, MARG ,SCHLG , RACE1,POVPIPG,
                           CIT,LTINU,ENG, CULRG,CORIGIN ) %>%
    tbl_strata(
      strata = CIT,
      .tbl_fun = ~ .x %>% 
        tbl_summary(
          by = HINS4,
          missing = "no" ,
          label = list(SEX ~ "Sex", DIS ~"Disability",
                       ESRG~"Current employment status",
                       SCHLG~"Education" , RACE1~"Race/ethnicity",CULRG~"Cultural clusters",
                       POVPIPG ~ "Federal poverty",LTINU ~"Lifetime in US",
                       ENG~"Self-rated English proficiency", CORIGIN~"Country/Region of birth"
          ),
          percent = "row",
          statistic = list(all_categorical() ~ "{p}%")
        ) %>%
        
        # add_p() %>%
        # add_significance_stars(hide_ci = TRUE, hide_p = TRUE)%>%
        italicize_labels() %>%
        bold_labels() %>%
        modify_column_hide(columns = stat_1) %>%
        modify_header(stat_2 = "{style_percent(p)}%") %>%
        modify_caption("**Table 6. Medicaid Coverage Rate by Socio-Demographic Factors, Across Citizenship Status**")
      
      
      #.header = "**{strata}**, N = {n}"
    )
  print(table6)

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

