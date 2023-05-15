
### This is the code to creat graph 2 



library(ggplot2)
library(tidycensus)
#install.packages("survey")
library(survey)
#install.packages("srvyr")
library("srvyr")
library(labelled)
library(dplyr)

### Unweighted plot 2 df <- subset(Data, select=c(CIT, expansion, YEAR, HICOV))
# create variable ACA to have pre and post aca
df <- Data[, c("CIT", "expansion", "YEAR", "HINS4", grep("PWGTP", names(Data), value = TRUE))]


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
insurance_mean <- aggregate(HINS4 ~ ACA + CITG + expansion, data=df, FUN=mean)
# Create the bar graph
plot2<-ggplot(data=insurance_mean, aes(x=CITG, y=HINS4, fill=ACA)) +
        geom_bar(stat="identity", position=position_dodge()) +
        facet_grid( ~ expansion) +
        scale_y_continuous(limits=c(0,1), expand=c(0,0), breaks=seq(0,1,0.1))+
        scale_fill_brewer(palette="PuBu")+ # specify the colors you want
        labs(x="Citizenship Status", y="Medicaid Insurance Rate", title="Medicaid Insurance Rates by Citizenship and ACA Status and Expansion Status") +
        #theme_classic()
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(color = 'black'),
              axis.text = element_text(size=12),
              axis.title = element_text(size=14))
print(plot2)

## Weighted graph


insurance_mean1 <- df %>%
        group_by(ACA, CITG, expansion) %>%
        summarize(weighted_mean = weighted.mean(HINS4, w = PWGTP))


# Create the bar graph
plot2w<-ggplot(data=insurance_mean1, aes(x=CITG, y=weighted_mean, fill=ACA)) +
        geom_bar(stat="identity", position=position_dodge()) +
        facet_grid( ~ expansion) +
        scale_y_continuous(limits=c(0,1), expand=c(0,0), breaks=seq(0,1,0.1))+
        scale_fill_brewer(palette="Pastel2")+ # specify the colors you want
        labs(x="Citizenship Status", y="Medicaid Rate", title="Medicaid Insurance Rates by Citizenship and ACA Status and Expansion Status") +
        #theme_classic()
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(color = 'black'),
              axis.text = element_text(size=12),
              axis.title = element_text(size=14))







