
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


# Data for table

df<-Data
df$ACA <- factor(df$ACA, levels = c("Pre-ACA", "Post-ACA"))

dummy<- subset(Data, select=c(RACE1, SEX ,ESRG,SCHLG ,POVPIPG)  
               )
library(dummy)
dummy<-dummy(dummy)
dummy <- dummy %>% mutate_all(as.numeric)
colnm<-colnames(dummy)
colnm<- gsub("RACE1_|SCHLG_|SEX_|ESRG_|POVPIPG_", "", colnm)
colnm <- gsub("\\.", " ", colnm)
colnm <- gsub("\\s+", " ", colnm)
colnm
names(dummy)<-colnm
head(dummy)
df <- cbind(dummy, df)

colnm<-c("UNINS","HINS4","HINS2","HINS1","AGEP", colnm, "MARG", "DIS")
al<-c( NATIVITY,ACA, expansion,UNINS,HINS4,HINS2,HINS1,AGEP, Asian,Black,Hispanic,Other, White, Female ,Male,  MARG ,DIS )
      
    
tab<- df %>% filter(ACA=="Pre-ACA")%>%
    select(NATIVITY,ACA, expansion,UNINS,HINS4,HINS2,HINS1,AGEP, Asian,Black,Hispanic,Other, White, Female ,Male,  MARG ,DIS ) %>%  
    tbl_strata(
        strata = NATIVITY,
        .tbl_fun = ~ .x %>% 
            tbl_summary(
                by = expansion,
                include = c(UNINS,HINS4,HINS2,HINS1,AGEP, Asian,Black,Hispanic,Other, White, Female ,Male,  MARG ,DIS) ) %>%  
                    tbl_strata( Female ,Male,  MARG ,DIS),
                missing = "no",
                label = list(
                    UNINS ~ "Uninsured",
                    HINS4 ~ "Medicaid coverage",
                    HINS1 ~ "Employer Sponsered",
                    HINS2 ~ "Individualy Purchased",
                    AGEP ~ "Age", 
                    MARG ~"Married",
                    DIS ~"Disability"
                ),
                statistic = list(all_categorical() ~ "{p}",  all_continuous() ~ "{mean} ({sd})")
                
            ) %>%
            add_difference()                  %>%
            add_significance_stars(hide_ci = TRUE,
                                   hide_p = TRUE, 
                                   hide_se =TRUE,
                                   pattern = "{estimate}{stars}"
                                  ) 
            )
#%>%
#  add_p()                  %>%
#  add_stat_label()        %>%

#add_p (
# perform t-test for all variables
#test = list(all_continuous() ~ "t.test", all_categorical() ~ "chisq.test"),
# assume equal variance in the t-test
# test.args = all_tests("t.test") ~ list(var.equal = TRUE)
#)                 %>%
# add_significance_stars(hide_ci = TRUE, hide_p = FALSE)%>%
#bold_labels() %>%
#  modify_caption("Baseline Characteristics by Nativity") %>%
# modify_header(all_stat_cols() ~ "**{level}**, \nN = {n}"


 `Graduate and beyond` ,   `High school`, `Less than high school` , `Some college or Associate degree` ,`Income 100 to 138 poverty`,`Income below 100 poverty`        



df <- cbind(dummy, df)


 tab<- df %>% filter(ACA=="Pre-ACA")%>%
        select( NATIVITY,ACA, expansion,UNINS,HINS4,HINS2,HINS1,AGEP, Asian,Black,Hispanic,Other, White, Female ,Male, Employed,Unemployed,`Not in labor force`,`College degree` ,
                `Graduate and beyond` ,   `High school`, `Less than high school` , `Some college or Associate degree` ,`Income 100 to 138 poverty`,`Income below 100 poverty` ,       
                MARG ,DIS  ) %>%  
         tbl_strata(
                 strata = expansion,
                 .tbl_fun = ~ .x %>% 
                        tbl_summary(
                                 by = NATIVITY,
                                 include = c(UNINS,HINS4,HINS2,HINS1,AGEP, Asian,Black,Hispanic,Other, White, Female ,Male,Employed , Unemployed,`Not in labor force`,`College degree` ,
                                             `Graduate and beyond` ,   `High school`, `Less than high school` , `Some college or Associate degree` ,`Income 100 to 138 poverty`,`Income below 100 poverty`  ,      
                                             MARG ,DIS),
                                missing = "no",
                                label = list(
                                       UNINS ~ "Uninsured",
                                        HINS4 ~ "Medicaid coverage",
                                        HINS1 ~ "Employer Sponsered",
                                        HINS2 ~ "Individualy Purchased",
                                        AGEP ~ "Age", 
                                       MARG ~"Married",
                                       DIS ~"Disability"
                                   ),
                                 statistic = list(all_categorical() ~ "{p}",  all_continuous() ~ "{mean} ({sd})")
                                 
                                 ) %>%
                         add_difference()                  %>%
                    add_significance_stars(hide_ci = TRUE,
                                                                                    hide_p = TRUE, 
                                                                                    hide_se =TRUE,
                                                                                    pattern = "{estimate}{stars}"
                                                                                  ) 
                    )
 
 tab %>%
 
 as_kable_extra(booktabs = TRUE, format = "latex",
                linesep = "") %>% 
     kableExtra::kable_styling(latex_options= c("scale_down","HOLD_position"), font_size= 6.5 ) %>% # ,font_size=6.5) 
     
     pack_rows("Insurance coverage, %",1,4) %>%
     pack_rows("Race, %",6,10)%>%
     pack_rows("Gender, %",11,12)%>%
     pack_rows("Empoyment Status, %",13,15)%>%
     pack_rows("Education, %",16,20)%>%
     pack_rows("Poverty, %",21,22)
 
)
 