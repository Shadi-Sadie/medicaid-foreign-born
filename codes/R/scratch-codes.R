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
