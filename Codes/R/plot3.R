
## This is the code to create plot 3 for Employer insurance 


library(ggplot2)
library(tidycensus)
#install.packages("survey")
library(survey)
#install.packages("srvyr")
library("srvyr")
library(labelled)
library(dplyr)



Data<-read.csv("CLNACS.csv", header = TRUE, sep ="," , fill = TRUE)
Temp<-Data

df <- subset(Data, select=c(CIT, expansion, YEAR, HINS1))
# create variable ACA to have pre and post aca
df$ACA <- ifelse(df$YEAR < 2014,1, 2)
df$ACA<- ordered(df$ACA, labels = c("Pre-ACA","Post-ACA" ))
var_label(df$ACA) <- "ACA status"
#check if it's correct'
table(df$YEAR, df$ACA)
df$CIT<-as.numeric(df$CIT)
df$CITG<-1
df$CITG<-replace(df$CITG,df$CIT==2,2) # 4. Naturalized citizen
df$CITG<-replace(df$CITG,df$CIT==3,3) # 5. Not citizen

df$CITG<- ordered(df$CITG, labels = c("US Born", "Naturalized-citizen", "Non-citizen" )) # labeling the values 
var_label(df$CITG) <- "Citizenship Status"
# Calculate the mean insurance rate by ACA status, citizenship status, and expansion status
insurance_mean <- aggregate(HINS1 ~ ACA + CITG + expansion, data=df, FUN=mean)
# Create the bar graph
plot3<-ggplot(data=insurance_mean, aes(x=CITG, y=HINS1, fill=ACA)) +
        geom_bar(stat="identity", position=position_dodge()) +
        facet_grid( ~ expansion) +
        scale_y_continuous(limits=c(0,1), expand=c(0,0), breaks=seq(0,1,0.1))+
        scale_fill_brewer(palette="PuBu")+ # specify the colors you want
        labs(x="Citizenship Status", y="Mean Employer-sponsored Insurance", title="Insurance Rates by Citizenship and ACA Status and Expansion Status") +
        #theme_classic()
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(color = 'black'),
              axis.text = element_text(size=12),
              axis.title = element_text(size=14))

## Weighted graph

# a subset of the dataset
df_sub <- Data[, c("CIT", "expansion", "YEAR", "HINS1", grep("PWGTP", names(Data), value = TRUE))]

# Create needed varibale

df_sub$ACA <- ifelse(df_sub$YEAR < 2014,1, 2)
df_sub$ACA<- ordered(df_sub$ACA, labels = c("Pre-ACA","Post-ACA" ))
var_label(df_sub$ACA) <- "ACA status"
#check if it's correct'
table(df_sub$YEAR, df_sub$ACA)
df_sub$CIT<-as.numeric(df_sub$CIT)
df_sub$CITG<-1
df_sub$CITG<-replace(df_sub$CITG,df_sub$CIT==2,2) # 4. Naturalized citizen
df_sub$CITG<-replace(df_sub$CITG,df_sub$CIT==3,3) # 5. Not citizen

df_sub$CITG<- ordered(df_sub$CITG, labels = c("US Born", "Naturalized-citizen", "Non-citizen" )) # labeling the values 
var_label(df_sub$CITG) <- "Citizenship Status"



# Create survey design object for this subset

svy_sub <- as_survey(df_sub, weight = PWGTP, 
                     repweights = matches("PWGTP[0-9]+"),
                     type = "JK1", 
                     scale = 4/80, 
                     rscales = rep(1, 80), 
                     mse = TRUE)

# Calculate survey-weighted mean insurance rate by ACA status, citizenship status, and expansion status


insurance_mean <- svyby(~ HINS1, ~ ACA + CITG + expansion, design = svy_sub, FUN = svymean)


# Create the bar graph
plot3w<-ggplot(data=insurance_mean, aes(x=CITG, y=HINS1, fill=ACA)) +
        geom_bar(stat="identity", position=position_dodge()) +
        facet_grid( ~ expansion) +
        scale_y_continuous(limits=c(0,1), expand=c(0,0), breaks=seq(0,1,0.1))+
        scale_fill_brewer(palette="Pastel1")+ # specify the colors you want
        labs(x="Citizenship Status", y="Insurance Rate", title="Insurance Rates by Citizenship and ACA Status and Expansion Status") +
        #theme_classic()
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(color = 'black'),
              axis.text = element_text(size=12),
              axis.title = element_text(size=14))




