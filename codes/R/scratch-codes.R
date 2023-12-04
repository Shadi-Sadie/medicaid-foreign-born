#Visualizing
library(ggplot2)
library(dplyr)

utils::write.csv(Data, file = "CLNACS.csv", row.names = FALSE)
write.csv(Data, file = "CLNACS.csv", row.names = FALSE)

Data <- Data %>% mutate_if(is.character, as.factor)

df <- haven::read_dta("https://raw.githubusercontent.com/Mixtape-Sessions/Advanced-DID/main/Exercises/Data/ehec_data.dta")

Data$ExpansionY <- ifelse(Data$ExpansionY == 0, NA, Data$ExpansionY)

write.csv(Data, file = "CLNACS.csv", row.names = FALSE)


Data$treat<- ifelse(Data$ExpansionY==0, 0,2)
Data$t<-3

NATV <- NATV[, -which(names(NATV)=="t")]

Data<-read.csv("CLNACS.csv", header = TRUE, sep ="," , fill = TRUE)
ACS<-Data

sum(is.na(PREASC$SERIALNO))
table(Data$SEX)

tab<- Forgn %>% group_by(RACE) %>% summarize(Freq=n()) %>% mutate(Prop=Freq/sum(Freq)) %>%
        arrange(desc(Prop))

levels(Data$CIT)[levels(Data$CIT) == "Born in US state"] <- "Born in US states"

ssubset_data <- Data[, c( "ST", "YEAR","ExpansionY", "ttot", "expansion", "Post", "t", "treat", "ACA" ,"Expan_STS"  )]

subset_data <- filter(ssubset_data, Expan_STS=="Late Expansion")
Data <- Data[, -which(names(Data)=="t")]
colnames(Data)
ggplot(data=tab, mapping = aes(x=RACE, y=Prop))+
        geom_col()+
        coord_flip()+
        scale_x_discrete(limits=tab$RACE)
Chee
table(Data$NATIVITY, Data$REGION, exclude = NULL)

PREACS<-read.csv("PREACS.csv", header = TRUE, sep ="," , fill = TRUE)


# Result for unisured foregin-born

#Normal OLS Regression
model <- lm(UNINS ~ treat, data = Data)

#probit regression
myprobit <- glm(UNINS ~ expansion, family = binomial(link = "probit"), 
                data = Data)
#Logit regression
myprobit <- glm(UNINS ~ expansion, family = binomial(link = "logit"), 
                data = Data)
## Fixed effect
felm_model <- felm(UNINS ~ expansion |ST + YEAR | 0 | ST, data = Data)

## using a fixed effect package

pdata <- pdata.frame(Data, index = c("SERIALNO", "YEAR"))
fatal_fe_mod <- plm(UNINS ~ expansion, 
                    data = pdata,
                    index = c("ST", "YEAR"), 
                    model = "within")

coeftest(fatal_fe_mod, vcov. = vcovHC, type = "HC1")


### Check the correlation of IPC and Other
# I want to take average of all my data for 
# for creatng dummey 
data<-dummy_data
data <- subset(data, select = -c(ENG,GEOR,SEX,SCHLG,SEX_Male,ESRG,RACE1,ESRG_Employed,CIT))
data <- subset(data, select = -c(CULRG))

data <- data %>% 
        mutate_if(is.character, as.numeric)

##to have it as matrix or data frame
maj<-as.data.frame(MajVotingF)        #########
colnames(maj)<-variables               #########
rownames(maj)<-variables               #########
maj[maj < 4] <- 0                     #########
maj[maj > 3] <- 1                   #########
##############################################
finalDag<-dagitty("dag{
        ADA 
        AGEP 
        CIT 
        DIS 
        ENG 
        ESRG 
        IPCINDX 
        MARG 
        POVPIP
        RACE1 
        REGION 
        SCHLG 
        SEX 
        UNINS 
        UnempR 
        treat 
        ADA -> IPCINDX
        ADA -> UNINS
        ADA -> treat
        ADA <-> CIT
        ADA <-> UnempR
        AGEP -> ESRG
        AGEP -> POVPIP
        AGEP -> UNINS
        AGEP -> DIS
        CIT -> ENG
        CIT -> ESRG
        CIT -> POVPIP
        CIT -> UNINS
        DIS -> ESRG
        DIS -> SCHLG
        DIS -> UNINS
        ENG -> ESRG
        ENG -> UNINS
        ESRG -> UNINS
        ESRG -> POVPIP
        IPCINDX -> POVPIP
        IPCINDX -> UNINS
        IPCINDX -> treat
        MARG -> ESRG
        MARG -> POVPIP
        MARG -> UNINS
        POVPIP -> UNINS
        SCHLG -> ENG
        SCHLG -> ESRG
        SCHLG -> POVPIP
        SCHLG -> UNINS
        SEX -> ESRG
        SEX -> SCHLG
        SEX -> UNINS
        UnempR -> ESRG
        UnempR -> UNINS
        UnempR -> treat
        treat -> MARG
        treat -> POVPIP
        treat -> UNINS}")

print(finalDag)

adjustmentSets(
        finalDag,
        exposure = "treat",
        outcome = "UNINS")

fordag<-dag
nativedag<-dag

table(data$X.5)
backDoorGraph(dag)

all_objects <- ls()
objects_to_remove <- setdiff(all_objects, c(ACS, Data,Forgn,NATV ))

### Change my categorical variable to binary variables:

# change factor variables to dummy


dummy_data <- dummy(data)
colnames(dummy_data)
data <- cbind(dummy_data, data)

dummy_data

library(dummy)

install.packages("dummy")


table(dummy_data$ENG_Not.at.all)

table(data$ENG)

##Zotero to R markdown
remotes::install_github("paleolimbot/rbbt")




reg1 = feols(UNINS ~ treat ,  weights = ~PWGTP, data = NATV) # state and year fixed effect
reg2 = feols(UNINS ~ treat ,  weights = ~PWGTP, data = Forgn) # state and year fixed effect
reg3 = feols(UNINS ~ treat/ForeginBorn, weights = ~PWGTP, data = Data) # state and year fixed effect
reg4 = feols(UNINS ~ treat*ForeginBorn,  weights = ~PWGTP, data = Data) # state and year fixed effect
reg5 = feols(UNINS ~ i(treat, i.ForeginBorn,0),  weights = ~PWGTP, data = Data) # state and year fixed effect
reg6 = feols(UNINS ~ i(treat, i.ForeginBorn), weights = ~PWGTP, data = Data) # state and year fixed effect

reg6 = feols(UNINS ~ treat*ForeginBorn, weights = ~PWGTP, data = Data) # state and year fixed effect

etable(reg1, reg2, reg3,reg4,reg5,reg6)

reg2 = feols(UNINS ~ treat + .[foriegn],  weights = ~PWGTP, data = Forgn) # state and year fixed effect


.[foriegn]

PLIU

r<-feols(UNINS ~ treat + GEOR,  weights = ~PWGTP, data = Forgn) 
b<-feols(UNINS ~ treat*ForeginBorn + .[controls]+.[foriegn],  weights = ~PWGTP, data = data) # state and year fixed effect
etable(r,b)


b<-feols(UNINS ~ treat+ ForeginBorn +.[adj12]+ .[controls]+.[foriegn],  weights = ~PWGTP, data = data) # state and year fixed effect


reg1 = feols(UNINS ~ treat ,  weights = ~PWGTP, data = data) # state and year fixed effect



### how to correlation matrix
install.packages("corrplot")


cor_matrix <- cor(model.matrix(~UNINS+ POVPIPG+ESRG+SCHLG+ RACE1- 1, data = Data))

SEX +POVPIPG+ AGEP+ESRG+MARG+SCHLG+ RACE1 
ENG+YLIU+PLIU+GEOR+ CULRG+IPCINDX
#heatmap
#method 1
library(corrplot)
corrplot(cor_matrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#method 2

col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = cor_matrix, col = col, symm = TRUE)


var(Data$CIT)
cor_matrix

controls


library(car)

# Calculate the VIF for variables in the model
vif_values <- vif(b)

# Print the VIF values
print(vif_values)

####################
## remove extra data from global
####################
all_objects <- ls()

# Specify the variables to keep
variables_to_keep <- c("data", "NATV", "Forgn","ACS","Data", "adj1","adj12","controls")

# Identify the objects to remove
objects_to_remove <- setdiff(all_objects, variables_to_keep)

# Remove the objects from the global environment
rm(list = objects_to_remove)



svyf<-to_survey(
        Data,
        type = c("person"),
        class = c("srvyr", "survey"),
        design = "rep_weights"
)




bdsd <- svyby(~ DIS, ~  expansion, design = svy, FUN = svymean)


svyN %>% 
        survey_count(expansion)

# IPC for north Carolina in year 2016 was missing USed the year after for the 2016 as well
Data$IPC<-ifelse(Data$ST==37 & Data$YEAR==2016,-3,Data$IPC)

table(Data$IPC,exclude=NaN)




save(Data, file = paste0(wd$data,'Data.Rdata'))


colnames(Data)


#### Removed From DAG
#### 
#### ibrary(gsl)
library(energy)
library(devtools)
library(pcalg)
library(kpcalg)
#library(CAM)
library(bnlearn)
#library(AlphaPart)
#library(tabuSearch)
#library(dplyr)
library(Rgraphviz)
library(AlphaPart)







feols(UNINS ~ i(ttot, treat, ref = -1) |   ST + YEAR,  cluster = ~ST,   data = Data)



## To closs ll the plot
## 
dev.off()

dev.off(dev.list()["RStudioGD"])

data$B<-log(data$UNINS)

## Checking the result if unique using other methods
## 
## 
 library(survey)

reg<-svyglm(UNINS ~treat*ForeginBorn +factor(ST)+ factor(YEAR), 
       family="binomial", 
       design= svydesign(
                id=~1,
                weights = ~ PWGTP, 
                repweights = ~ matches("PWGTP[0-9]+"), 
                data = data, 
                type = "JK1",
                scale = 4/80,
                rscales = rep(1, 80),
                mse = TRUE
                 ),
       
)

# check the table
# 

tab_model(reg6, p.style = "numeric_stars", transform = NULL)



# check risdiuals 
par(mfrow=c(1,2))
    
plot(residuals(reg6,type = "deviance"))
plot(residuals(reg))


# just want to plot logestic regression curve 


plot (data$UNINS, data$treat)
abline(reg)

plot(UNINS ~ treat, data=data, col="steelblue")
lines(UNINS ~ treat, newdata, lwd=2)

newdata <- data.frame(treat=data$treat,ForeginBorn=data$ForeginBorn, ST=data$ST, YEAR=data$YEAR )
newdata$UNINS <- predict(reg, newdata, type = "response")
dev.off()
plot(UNINS ~ treat, data=data)
lines(UNINS ~ treat, newdata, lwd=2, col="green")


etable(list(reg1,reg2,reg3, reg4,reg5,reg6,reg7,reg8, reg9,reg0),  fitstat = ~ n +r2 +ar2 + pr2+  aic + bic )
       

### Agregated data by st and year

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




agrdata  <- aggregate(cbind(UNINS, treat) ~ ST + YEAR, data = Data, FUN = mean)


#aggregated_data <- svyby(~ UNINS + treat, ~ ST + YEAR, design = svy,FUN = svymean)


df_bacon1 <-bacon(UNINS ~ treat,
         data = agrdata,
        id_var = "ST",
         time_var = "YEAR")


dfplot1<- ggplot(df_bacon1) +
    aes(x = weight, y = estimate, shape = factor(type)) +
    labs(x = "Weight", y = "Estimate", shape = "Type") +
    geom_hline(yintercept = dd_estimate, color = "A94064") +
    
    geom_point()


garrange(dfplot1, dfplot2 , 
         ncol = 2, nrow = 1)

agrdata  <- aggregate(cbind(HINS4, treat) ~ ST + YEAR, data = Data, FUN = mean)


#aggregated_data <- svyby(~ UNINS + treat, ~ ST + YEAR, design = svy,FUN = svymean)



df_bacon2 <-bacon(HINS4 ~ treat,
                 data = agrdata,
                 id_var = "ST",
                 time_var = "YEAR")

df_bacon2

bacon_summary(df_bacon2)


dfplot2<- ggplot(df_bacon2) +
    aes(x = weight, y = estimate, shape = factor(type)) +
    labs(x = "Weight", y = "Estimate", shape = "Type") +
    
    geom_point()

dd_estimate <- sum(df_bacon1$estimate*df_bacon1$weight)


ggplot(data = df_bacon1) +
    geom_point(aes(x = weight, y = estimate, 
                   color = type, shape = type), size = 2) +
    xlab("Weight") +
    ylab("2x2 DD Estimate") +
    geom_hline(yintercept = dd_estimate, color = "red") +
    theme_minimal() + 
    theme(
        legend.title = element_blank(),
        legend.background = element_rect(
            fill="white", linetype="solid"),
        legend.justification=c(1,1), 
        legend.position=c(1,1)
    )


type <- c("Earlier vs Later Treated", "Later vs Earlier Treated", "Treated vs Untreated")
Coeff <- c("0.14610", "0.08782", "0.14452")
Weight <- c("0.11871","0.08771","0.79358")
bc1 <- data.frame(type, Coeff,Weight )

bc<- table(bc1$type, bc1$Coeff,bc1$Weight)


### Aug 10, 2023
### 
CIT,LTINU,ENG,CULRG,CORIGIN

CIT~"Citizenship status",LTINU ~"Lifetime in US",
ENG~"Self-rated English proficiency", 
CORIGIN~"Country/Region of birth", CULRG~"Cultural clusters"


   print(table2) 


## 4.Method

4.1 Used Causal Structure Discovery to find the Directed acyclic graph and minimal adjustment set


4.2 Difference-in-Differences (DID) Estimation:
    
    DID is used to estimate the impact of Medicaid expansion on Uninsured and Medicaid Coverage.
Utilizing a two-way fixed effects model with differential timing, which enables the incorporation of the Medicaid expansion timeline into the DID analysis.

Model Specification:
    
    $$ 
    \begin{equation}
Y_{ist} = \alpha + \delta_s + \lambda_t + \gamma_s \cdot T_{st} + \beta_1 (FB_{ist} \times T_{st}) + FB_{ist} + \beta  X_{ist} + \theta(FB_{ist} \times C_{ist}) + \epsilon_{ist}
\end{equation}

$$
    
    
    In this equation:
    
    - \(Y_{ist}\) represents the outcome of individual \(i\) in year \(t\) in state \(s\).
- \(\delta_s\) represents state fixed effects.
- \(\lambda_t\) represents year fixed effects.
- \(\gamma\) represents the average treatment effect of Medicaid expansion for state \(s\).
- \(T_{st}\) is a dummy variable that equals 1 if individual \(i\) in state \(s\) is exposed to the treatment in year \(t\), and 0 otherwise. For states that expanded Medicaid in 2014, \(T_{ist}\) is equal to 1 for all years after 2014. For states that expanded Medicaid later, \(T_{ist}\) is equal to 1 for all years after the year of expansion.
- \(\beta_1\) represents the coefficient for the interaction term between \(FB_{ist}\) and \(T_{st}\), capturing the differential treatment effect for foreign-born individuals.
X represents vector of control variables, including racial composition, age distribution, gender distribution, employment, and personal income 

\(\theta_k \cdot (FB_{ist} \times C_{ist,k})\) represents the interaction terms between the binary variable \(FB_{ist}\) and the control variables specifice to foreign-born individuals (\(C_{ist,k}\)). These interaction terms capture the differential effects of the control variables for foreign-born individuals compared to native-born individuals. This includes English proficiency, region of origin, acculturation.

The inclusion of the interaction term between \(FB_{ist}\) and the treatment variable is important to account for potential differences in the treatment effect between foreign-born and US-born individuals.

Clustered standard errors at the state-level are used for robustness.
spital-specific linear time trends




    
    
    4.3 Event Study Estimation:
    
    Additionally, I estimate this effect using an event study model that allows us to assess the evolution of relative outcomes while controlling for fixed differences across states and national trends over time.

event study model allows us to test the parallel trends assumption and it provides valuable insights into the temporal dynamics of the treatment effect, enhancing our understanding of the causal relationship between the treatment and the outcome variable.


Model Specification:
    
    $$ 
    \begin{equation}
Y_{ist} = \alpha + \delta_s + \lambda_t + \gamma_s \cdot T_{st} + \beta_1 (FB_{ist} \times T_{st}) + \sum_{j=2}^{J} \beta_j  X_{ist,j} + \sum_{k=1}^{K} \theta_k (FB_{ist} \times C_{ist,k}) + \epsilon_{ist}
\end{equation}
$$



    
    
    *Equation Components:**
    
    - \(Y_{ist}\): Outcome for individual \(i\) each year and state.
- \(\delta_s\): State fixed effect.
- \(\lambda_t\): Year fixed effects.
- \(X_{idt}\): Vector of control variables.

**Dynamic Policy Effects:**
    
    - \(\sum_{n=-3}^{5} \beta_y D_s D_{it}^n\): Captures dynamic effects of policy.
- \(D_{k,its}\): Lead and lag dummy variables when state adopts expansion.
- \({\beta}_y\): Estimates change in outcomes in expansion vs. non-expansion states during year \(y\).

**Omitted Category:**
    
    - \(k=-1\): Year prior to expansion, excluded as omitted category.










LTINU ~"Lifetime in US",

ENG~"Self-rated English proficiency",
CORIGIN~"Country/Region of birth", CULRG~"Cultural clusters"

print(levels(Data$expansion))



table(Data$expansion)
print(levels(Data$expansion))
levels(Data$expansion)[levels(Data$expansion) == "expansion"] <- "Expansion"

table2<-Data %>% subset(ACA=="Pre-ACA")%>%
    tbl_strata(
        strata = expansion,
        .tbl_fun = ~ .x %>% 
            tbl_summary(
                by = NATIVITY,
                include = c(
                    UNINS, HINS4, AGEP, RACE1, SEX,  MARG  ,
                    ESRG,SCHLG ,POVPIPG,
                    DIS),
                missing = "no",
                label = list(
                    AGEP ~ "Age", UNINS ~ "Uninsured",
                    HINS4 ~ "Medicaid coverage", SEX ~ "Sex", DIS ~"Disability",
                    ESRG~"Employment status", MARG ~"Married",
                    SCHLG~"Education" , RACE1~"Race/ethnicity",POVPIPG ~ "Federal poverty"
                ),
                statistic = list(all_categorical() ~ "{p}",  all_continuous() ~ "{mean} ({sd})")
                
            ) %>%
            add_p()                  %>%
            add_stat_label()        %>%
            
            #add_p (
            # perform t-test for all variables
            #test = list(all_continuous() ~ "t.test", all_categorical() ~ "chisq.test"),
            # assume equal variance in the t-test
            # test.args = all_tests("t.test") ~ list(var.equal = TRUE)
            #)                 %>%
            # add_significance_stars(hide_ci = TRUE, hide_p = FALSE)%>%
            bold_labels() %>%
            modify_caption("Baseline Characteristics by Nativity") %>%
            modify_header(all_stat_cols() ~ "**{level}**, \nN = {n}"
                          
            )
        #modify_spanning_header(stat_1 ~ "**Non Expansion**", stat_2 ~ "**Expansion**")
    )


NATV<- Data[Data$NATIVITY == "US-born", ]
IMG<- Data[Data$CIT == "Non-citizen", ]

NCIT<- Data[Data$CIT == "Naturalized-citizen", ]
CIT<- Data[Data$CIT == "US-citizen Born abroad ", ]

# I want to add another dummy variable for cit that only shows if the person is citizen or not
data$NonCit<- ifelse(data$CIT == "Non-citizen",1,0)

alcontol   <- c("SEX","DIS", "AGEP","SCHLG", "POVPIPG", "MARG", "RACE1", "ESRG", "ENG", "LTINU" )
controlwof    <- c("SEX","DIS", "AGEP","SCHLG", "POVPIPG", "MARG", "RACE1", "ESRG","ADA") # (SEX+DIS+AGEP+SCHLG+POVPIPG+MARG+RACE1+ESRG+ADA)
NICont        <- c("SEX","DIS", "AGEP","POVPIPG", "ENG", "LTINU", "CORIGIN", "NonCit")
NIContwof     <- c("SEX","DIS", "AGEP", "POVPIPG") # SEX+DIS+AGEP+MARG+POVPIPG
Intcontrol    <- c("MARG", "RACE1", "ESRG","ADA", "SCHLG" ) # (MARG + RACE1 + ESRG + ADA)
forcont       <- c("ENG", "LTINU", "CORIGIN", "NonCit") # ENG+LTINU+CORIGIN+NonCit


reg1 = feols(UNINS ~treat*ForeginBorn | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # no control
reg7 = feols(UNINS ~ treat*ForeginBorn + .[alcontol] | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # control for all normal thin

reg7

reg2 = feols(UNINS ~treat*ForeginBorn + .[controlswf] | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # state and year fixed effect

reg3 = feols(UNINS ~treat*ForeginBorn + .[controlswf] + ForeginBorn*(MARG + RACE1 + ESRG + ADA)| ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # state and year fixed effect

reg4 = feols(UNINS ~treat*ForeginBorn + .[controls] | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # state and year fixed effect

reg5 = feols(UNINS ~treat*ForeginBorn + .[controls] + ForeginBorn*(MARG + RACE1 + ESRG + ADA) | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # state and year fixed effect

reg6 = feols(UNINS ~treat*ForeginBorn + .[controlswf] + ForeginBorn*(MARG + RACE1 + ESRG + ADA+ENG+LTINU+CORIGIN+ NonCit )| ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # state and year fixed effect

reg7 = feols(UNINS ~ treat*ForeginBorn + .[alcontol] | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # state and year fixed effect

reg8 = feols(UNINS ~ treat*ForeginBorn +  ForeginBorn*(SEX+DIS+AGEP+MARG+SCHLG+RACE1+ESRG+POVPIPG+ENG+LTINU+CORIGIN+NonCit+ADA)| ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # state and year fixed effect

etable(list(reg1, reg2,reg3,reg4,reg5,reg6,reg7,reg8),    fitstat = ~ n +ll+r2 +ar2 + pr2+  aic + bic )  




allcontrols   <- c("SEX","DIS", "AGEP","SCHLG", "POVPIPG", "MARG", "RACE1", "ESRG","ADA", "ENG", "LTINU", "CORIGIN", "NonCit")
controlwof    <- c("SEX","DIS", "AGEP","SCHLG", "POVPIPG", "MARG", "RACE1", "ESRG","ADA") # (SEX+DIS+AGEP+SCHLG+POVPIPG+MARG+RACE1+ESRG+ADA)
NICont        <- c("SEX","DIS", "AGEP","POVPIPG", "ENG", "LTINU", "NonCit")
NIContwof     <- c("SEX","DIS", "AGEP","POVPIPG") # SEX+DIS+AGEP+MARG+SCHLG+POVPIPG
Intcontrol    <- c("MARG", "RACE1", "ESRG","SCHLG" ) # (MARG + RACE1 + ESRG + ADA)
forcont       <- c("ENG", "LTINU", "CORIGIN", "NonCit") # ENG+LTINU+CORIGIN+NonCit


reg1 = feols(UNINS ~treat*ForeginBorn | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # no control


# reg2 = feols(UNINS ~ treat*ForeginBorn + SEX+DIS+AGEP+SCHLG+POVPIPG+MARG+RACE1+ESRG | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # control for all normal things

#reg3 = feols(UNINS ~ treat*ForeginBorn + SEX+DIS+AGEP+SCHLG+POVPIPG+MARG+RACE1+ESRG+ ENG+LTINU+NonCit| ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # Control for demographic + immigrants

reg4 = feols(UNINS ~ treat*ForeginBorn + SEX+DIS+AGEP+SCHLG+POVPIPG+MARG+RACE1+ESRG+ ENG+LTINU+NonCit+ADA+UnempR+IPC| ST + YEAR  , vcov = "hetero", weights = ~PWGTP, data = data) # Control for demographic + immigrants + state

#reg5 = feols(UNINS ~ treat*ForeginBorn + SEX + DIS+AGEP+ POVPIPG+ ForeginBorn*(SCHLG+ MARG+ RACE1 + ESRG ) | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # Interaction with state 


#reg6 = feols(UNINS ~ treat*ForeginBorn + SEX + DIS+AGEP+ POVPIPG+ENG+LTINU+NonCit+ ForeginBorn*(SCHLG+ MARG+ RACE1 + ESRG ) | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # Interaction with state + immigrants

reg7 = feols(UNINS ~ treat*ForeginBorn + SEX + DIS+AGEP+ POVPIPG+ENG+LTINU+NonCit+ ADA+UnempR+IPC+ ForeginBorn*(SCHLG+ MARG+ RACE1 + ESRG ) | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # Interaction with state + immigrants


reg8 = feols(UNINS ~ treat*ForeginBorn + SEX + DIS+AGEP+ POVPIPG+ENG+LTINU+NonCit+ ForeginBorn*(SCHLG+ MARG+ RACE1 + ESRG + ADA+UnempR+IPC)  | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # Interaction with state + immigrants
reg9 = feols(UNINS ~ treat*ForeginBorn + SEX + DIS+AGEP+ POVPIPG+ENG+LTINU+NonCit+ ForeginBorn*(SCHLG+ MARG+ RACE1 + ESRG + ADA+UnempR+IPC)  | ST + YEAR + REGION^YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # Interaction with state + immigrants

# reg9 = feols(UNINS ~ treat*ForeginBorn + SEX + DIS+AGEP+ POVPIPG+ENG+LTINU+NonCit+ ForeginBorn*(SCHLG+ MARG+ RACE1 + ESRG +IPC+ UnempR )  | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # Interaction with state + immigrants

# reg9 = feols(UNINS ~ treat*ForeginBorn + ForeginBorn*(SCHLG+ MARG+ RACE1 + ESRG + SEX + DIS+AGEP+ POVPIPG+ENG+ADA+IPC+UnempR+NonCit+LTINU) | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # Interaction with all (+state+ img)
# reg = feols(UNINS ~ treat*ForeginBorn + ForeginBorn*(SCHLG+ MARG+ RACE1 + ESRG + SEX + DIS+AGEP+ POVPIPG+ENG+LTINU+NonCit) | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # Interaction with all(+immigrants)
# reg0 = feols(UNINS ~ treat*ForeginBorn + ForeginBorn*(SCHLG+ MARG+ RACE1 + ESRG + SEX + DIS+AGEP+ POVPIPG) | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # Interaction with all 


# reg7= feols(UNINS ~ treat*ForeginBorn + SEX+DIS+AGEP+SCHLG+POVPIPG+MARG+RACE1+ESRG+ ADA+ IPC +ENG+LTINU+NonCit| ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # state and 

# reg8 = feols(UNINS ~ treat*ForeginBorn + SEX + DIS+AGEP+ POVPIPG+ENG+LTINU+NonCit+ ADA+ IPC +ForeginBorn*(SCHLG+ MARG+ RACE1 + ESRG ) | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # state and 

# reg9 = feols(UNINS ~ treat*ForeginBorn +  SEX + DIS+AGEP+ POVPIPG+ENG+LTINU+NonCit + ForeginBorn*(SCHLG+ MARG+ RACE1 + ESRG + ADA) | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # state and 

#reg5 = feols(UNINS ~ treat*ForeginBorn + .[NICont] + ForeginBorn*(SCHLG+ MARG+ RACE1 + ESRG + ADA) | ST + YEAR + REGION^YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # state and 
etable(list(reg1, reg4,reg7, reg8,reg9),    fitstat = ~ n +ll+r2 +ar2 + pr2+  aic + bic )  

etable(list(reg1, reg2,reg3, reg4))




t1<- Data %>% 
    filter(expansion=="Expansion")%>%
    select(HINS4, NATIVITY,RACE1, SEX,ESRG, MARG, SCHLG,POVPIPG, DIS,expansion,ACA) %>%  
    tbl_strata(
        strata = ACA,
        .tbl_fun = ~ .x %>%  
            tbl_summary(
                by = HINS4,
                include = c(NATIVITY,RACE1,SEX,ESRG, MARG, SCHLG,POVPIPG,DIS) ,
                label = list(
                    RACE1~"Race/ethnicity", NATIVITY~"Nativity",
                    SEX ~ "Sex", DIS ~"Disability", ESRG~"Employment status", MARG ~"Marital status",
                    SCHLG~"Education" , POVPIPG ~ "Federal poverty"
                ), 
               # percent = "row",
                statistic = list(all_categorical() ~ "{p}")
            ) %>%
         #   modify_column_hide(columns = stat_1)%>%
            bold_labels() %>%
            add_stat_label()        %>%
            modify_header(label = "Characteristic")
    )


reg = feglm(UNINS ~ treat*ForeginBorn +.[NICont] + ForeginBorn*(MARG + RACE1 + ESRG + ADA) | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data, family = 'logit') # state and year fixed effect
reg = feols(UNINS ~treat*ForeginBorn + .[NICont] + ForeginBorn*(MARG + RACE1 + ESRG + ADA)| ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # state and year fixed effect

reg = feglm(UNINS ~treat*ForeginBorn | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data, family = 'logit') # state and year fixed effect
reg = feols(UNINS ~treat*ForeginBorn | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # state and year fixed effect
reg = feols(UNINS ~treat*ForeginBorn + .[allcontrols] | ST + YEAR  , vcov = "hetero", weights = ~PWGTP, data = data) # state and year fixed effect
reg = feols(UNINS ~treat*ForeginBorn + .[allcontrols] | ST + YEAR + REGION^YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # state and year fixed effect


-

reg = feglm(UNINS ~ treat*ForeginBorn + SEX+DIS+AGEP+POVPIPG+ ForeginBorn*(SCHLG+ MARG+ RACE1 + ESRG + ADA) | ST + YEAR , vcov = "hetero", weights = ~PWGTP, data = data) # state and 

extract_eq(reg)
equatiomatic::extract_eq(reg)


"SEX","DIS", "AGEP","POVPIPG", "ENG", "LTINU", "CORIGIN", "NonCit"

library(sjPlot)
library(sjmisc)
library(equatiomatic)





trend_data <- aggregate(UNINS ~ YEAR + StateN, data, mean)
trend_data$UNINS <- trend_data$UNINS * 100

state_trend_plots <- trend_data %>%
    ggplot(aes(x = YEAR, y = UNINS, color = StateN)) +
    geom_line() +
    labs(title = "State-Specific Uninsured Rate Trends Over Time",
         x = "Year", y = "Uninsured Rate",
         color = "State") +
    scale_x_continuous(breaks = trend_data$YEAR, labels = trend_data$YEAR) +
    
    theme_minimal() +
    theme(legend.position = "right")

plotly::ggplotly(state_trend_plots)


```{r}

trend_data <- aggregate(UNINS ~ YEAR + REGION, data, mean)
trend_data$UNINS <- trend_data$UNINS * 100

heatmap_plot <- trend_data %>%
    ggplot(aes(x = YEAR, y = REGION, fill = UNINS)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "blue") +
    labs(title = "Uninsured Rate Across Regions and Years",
         x = "Year", y = "Region",
         fill = "Uninsured Rate") +
    scale_x_continuous(breaks = trend_data$YEAR, labels = trend_data$YEAR) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(heatmap_plot)


```
class(data$ACA)


tab<-svy %>% subset(ACA=="Pre-ACA")%>%
    tbl_strata(
        strata = NATIVITY,
        .tbl_fun = ~ .x %>% 
            tbl_svysummary(
                by = expansion,
                include = c(
                    UNINS, HINS4, AGEP, RACE1, SEX,  MARG  ,
                    ESRG,SCHLG ,POVPIPG,
                    DIS),
                missing = "no",
                label = list(
                    AGEP ~ "Age", UNINS ~ "Uninsured",
                    HINS4 ~ "Medicaid coverage", SEX ~ "Sex", DIS ~"Disability",
                    ESRG~"Employment status", MARG ~"Married",
                    SCHLG~"Education" , RACE1~"Race/ethnicity",POVPIPG ~ "Federal poverty"
                ),
                statistic = list(all_categorical() ~ "{p}",  all_continuous() ~ "{mean} ({sd})")
                
            ) %>%
            add_difference()                  %>%
            add_significance_stars(hide_ci = TRUE,
                                   hide_p = TRUE, 
                                   hide_se =TRUE,
                                   pattern = "{estimate}{stars}"
                                   ) %>%
          #  add_p()                  %>%
          #  add_stat_label()        %>%
            
            #add_p (
            # perform t-test for all variables
            #test = list(all_continuous() ~ "t.test", all_categorical() ~ "chisq.test"),
            # assume equal variance in the t-test
            # test.args = all_tests("t.test") ~ list(var.equal = TRUE)
            #)                 %>%
            # add_significance_stars(hide_ci = TRUE, hide_p = FALSE)%>%
            bold_labels() %>%
            modify_caption("Baseline Characteristics by Nativity") %>%
            modify_header(all_stat_cols() ~ "**{level}**, \nN = {n}"
                          
            )
    )
        #modify_spanning_header(