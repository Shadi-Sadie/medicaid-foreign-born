# Title: Graphs
# Date created: may 15, 2023
# Description:  This scripts include the codes for all the graphs that I created for descriptive analysis
# originally there were 10 separated scripts for 10 graph I created! However, later it was decided to change 
# the graph to tables thus I gathered all the graphs in on script
# Date edited: Jun 15, 2023

####################
## Required Packages
####################

library(ggplot2)
library(tidycensus)
#install.packages("survey")
library(survey)
#install.packages("srvyr")
library("srvyr")
library(labelled)
library(dplyr)
library(ggpubr)


####################
## Data Preparation 
####################

# Read the data

# Data<-read.csv("CLNACS.csv", header = TRUE, sep ="," , fill = TRUE)

# Subset for un-weighted
df <- subset(Data, select=c(CIT, ACA, expansion, YEAR, RACE1,NATIVITY, HINS1,HICOV, HINS4,HINS2))

# Subset for weighted
df_sub <- Data[, c("CIT", "expansion", "ACA" ,"YEAR","HINS1" ,"HICOV", "HINS4","HINS2", grep("PWGTP", names(Data), value = TRUE))]

# Create a new variable CITG in un-weighted sample

df$CIT<-as.numeric(df$CIT)
df$CITG<-1
df$CITG<-replace(df$CITG,df$CIT==2,2) # 4. Naturalized citizen
df$CITG<-replace(df$CITG,df$CIT==3,3) # 5. Not citizen
df$CITG<- ordered(df$CITG, labels = c("US Born", "Naturalized-citizen", "Non-citizen" )) # labeling the values 
var_label(df$CITG) <- "Citizenship Status"

# Add the new variable to the weighted sample

df_sub$CITG<-df$CITG

# Create survey design object from the weighted sample

svy_sub <- as_survey(df_sub, weight = PWGTP, 
                     repweights = matches("PWGTP[0-9]+"),
                     type = "JK1", 
                     scale = 4/80, 
                     rscales = rep(1, 80), 
                     mse = TRUE)

################################################################################
## plot 1: Mean Insurance by ACA status, expansion status and citizenship status
################################################################################

########### Plot 1: Panel A #####################

# Calculate the mean insurance rate by ACA status, citizenship status, and expansion status
insurance_mean <- aggregate(HICOV ~ ACA + CITG + expansion, data=df, FUN=mean)

# Create the bar graph
panelA<-ggplot(data=insurance_mean, aes(x=CITG, y=HICOV, fill=ACA)) +
  geom_bar(stat="identity", position=position_dodge()) +
  facet_grid( ~ expansion) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0), breaks=seq(0,1,0.1))+
  scale_fill_brewer(palette="PuBu")+ # specify the colors you want
  labs( x="Citizenship Status",y="Mean Insurance Rate") +  # , x="Citizenship Status", title="Insurance Rates by Citizenship and ACA Status and Expansion Status") +
  #theme_classic()
   theme(plot.title = element_text(hjust = 0.5),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       panel.background = element_blank(),
       axis.line = element_line(color = 'black'),
       axis.text = element_text(size=12),
       axis.title = element_text(size=14))

########### Plot 1: Panel B #####################

# Calculate the mean medicaid coverage by ACA status, citizenship status, and expansion status
insurance_mean <- aggregate(HINS4 ~ ACA + CITG + expansion, data=df, FUN=mean)

# Create the bar graph
panelB<-ggplot(data=insurance_mean, aes(x=CITG, y=HINS4, fill=ACA)) +
  geom_bar(stat="identity", position=position_dodge()) +
  facet_grid( ~ expansion) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0), breaks=seq(0,1,0.1))+
  scale_fill_brewer(palette="PuBu")+ # specify the colors you want
  labs(  x="Citizenship Status",y="Mean Medicaid Coverage Rate") + #, x="Citizenship Status", title="Medicaid Insurance Rates by Citizenship and ACA Status and Expansion Status") +
  #theme_classic()
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14))

########### Plot 1: Panel C #####################

# Calculate the mean insurance rate by ACA status, citizenship status, and expansion status
insurance_mean <- aggregate(HINS1 ~ ACA + CITG + expansion, data=df, FUN=mean)
# Create the bar graph
panelC<-ggplot(data=insurance_mean, aes(x=CITG, y=HINS1, fill=ACA)) +
  geom_bar(stat="identity", position=position_dodge()) +
  facet_grid( ~ expansion) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0), breaks=seq(0,1,0.1))+
  scale_fill_brewer(palette="PuBu")+ # specify the colors you want
  labs(  x="Citizenship Status",y="Mean Employer-sponsored Insurance")+  #, x="Citizenship Status", title="Insurance Rates by Citizenship and ACA Status and Expansion Status") +
  #theme_classic()
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14))

########### Plot 1: Panel D #####################

# Calculate the mean insurance rate by ACA status, citizenship status, and expansion status
insurance_mean <- aggregate(HINS2 ~ ACA + CITG + expansion, data=df, FUN=mean)
# Create the bar graph
panelD<-ggplot(data=insurance_mean, aes(x=CITG, y=HINS2, fill=ACA)) +
  geom_bar(stat="identity", position=position_dodge()) +
  facet_grid( ~ expansion) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0), breaks=seq(0,1,0.1))+
  scale_fill_brewer(palette="PuBu")+ # specify the colors you want
  labs(  x="Citizenship Status",y="Mean Individual purchased Insurance")+ # , title="Insurance Rates by Citizenship and ACA Status and Expansion Status") +
  #theme_classic()
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14))

##################### Append all the panels in one plot #####################


plot1<-ggarrange(panelA+ font("x.text", size = 7), panelB + font("x.text", size = 7), panelC+ font("x.text", size = 7), panelD + font("x.text", size = 7) ,
          ncol = 2, nrow =2,common.legend=TRUE)

plot1

#######################################################################################################
## plot 1-W: Mean Insurance by ACA status, expansion status and citizenship status (Weighted graph)
######################################################################################################

########### Plot 1-W: Panel A #####################

# Calculate survey-weighted mean insurance rate by ACA status, citizenship status, and expansion status

insurance_mean <- svyby(~ HICOV, ~ ACA + CITG + expansion, design = svy_sub, FUN = svymean)

# Create the bar graph
panelA<-ggplot(data=insurance_mean, aes(x=CITG, y=HICOV, fill=ACA)) +
  geom_bar(stat="identity", position=position_dodge()) +
  facet_grid( ~ expansion) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0), breaks=seq(0,1,0.1))+
  scale_fill_brewer(palette="Pastel1")+ # specify the colors you want
  labs(x="Citizenship Status", y="Mean Insurance Rate") +#, title="Insurance Rates by Citizenship and ACA Status and Expansion Status") +
  #theme_classic()
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14))

############# Plot 1-W: Panel B #####################


insurance_mean1 <- df %>%
  group_by(ACA, CITG, expansion) %>%
  summarize(weighted_mean = weighted.mean(HINS4, w = PWGTP))

# Create the bar graph
panelB<-ggplot(data=insurance_mean1, aes(x=CITG, y=weighted_mean, fill=ACA)) +
  geom_bar(stat="identity", position=position_dodge()) +
  facet_grid( ~ expansion) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0), breaks=seq(0,1,0.1))+
  scale_fill_brewer(palette="Pastel2")+ # specify the colors you want
  labs(x="Citizenship Status", y="Mean Medicaid Coverage Rate")+ #, title="Medicaid Insurance Rates by Citizenship and ACA Status and Expansion Status") +
  #theme_classic()
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14))


############# Plot 1-W: Panel C #####################

insurance_mean <- svyby(~ HINS1, ~ ACA + CITG + expansion, design = svy_sub, FUN = svymean)

# Create the bar graph
panelC<-ggplot(data=insurance_mean, aes(x=CITG, y=HINS1, fill=ACA)) +
  geom_bar(stat="identity", position=position_dodge()) +
  facet_grid( ~ expansion) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0), breaks=seq(0,1,0.1))+
  scale_fill_brewer(palette="Pastel1")+ # specify the colors you want
  labs(x="Citizenship Status", y="Mean Employer-sponsored Insurance")+ #, title="Insurance Rates by Citizenship and ACA Status and Expansion Status") +
  #theme_classic()
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14))

############# Plot 1-W: Panel D #####################


insurance_mean <- svyby(~ HINS2, ~ ACA + CITG + expansion, design = svy_sub, FUN = svymean)

# Create the bar graph
panelD<-ggplot(data=insurance_mean, aes(x=CITG, y=HINS2, fill=ACA)) +
  geom_bar(stat="identity", position=position_dodge()) +
  facet_grid( ~ expansion) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0), breaks=seq(0,1,0.1))+
  scale_fill_brewer(palette="Pastel1")+ # specify the colors you want
  labs(x="Citizenship Status", y="Mean Individual purchased Insurance")+ #, title="Insurance Rates by Citizenship and ACA Status and Expansion Status") +
  #theme_classic()
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14))

##################### Append all the panels in one plot #####################


plot1-w<-ggarrange(panelA+ font("x.text", size = 7), panelB + font("x.text", size = 7), panelC+ font("x.text", size = 7), panelD + font("x.text", size = 7) ,
                 ncol = 2, nrow =2,common.legend=TRUE)

plot1-w


######################################################################
## plot 2: Mean Insurance by RACE, ACA status, and Nativity status
#######################################################################

########### Plot 2: Panel A #####################

insurance_mean <- aggregate(HICOV ~ NATIVITY + RACE1 + expansion, data=df, FUN=mean)
# Create the bar graph
panelA<-ggplot(data=insurance_mean, aes(x=RACE1, y=HICOV, fill=NATIVITY)) +
  geom_bar(stat="identity", position=position_dodge()) +
  facet_grid( ~ expansion) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0), breaks=seq(0,1,0.1))+
  scale_fill_brewer(palette="Pastel1")+ # specify the colors you want
  labs(x="Race", y="Insurance Rate")+#, title="Insurance Rates by Race and Nativity Status and Expansion Status") +
  #theme_classic()
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14))

########### Plot 2: Panel B #####################
# Calculate the mean insurance rate by ACA status, citizenship status, and expansion status

insurance_mean <- aggregate(HINS4 ~ NATIVITY + RACE1 + expansion, data=df, FUN=mean)
# Create the bar graph
panelB<-ggplot(data=insurance_mean, aes(x=RACE1, y=HINS4, fill=NATIVITY)) +
  geom_bar(stat="identity", position=position_dodge()) +
  facet_grid( ~ expansion) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0), breaks=seq(0,1,0.1))+
  scale_fill_brewer(palette="Pastel1")+ # specify the colors you want
  labs(x="Race", y="Medicaid Coverage Rate")+ #, title="Insurance Rates by Race and Nativity Status and Expansion Status") +
  #theme_classic()
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14))

########### Plot 2: Panel C #####################
# Calculate the mean insurance rate by ACA status, citizenship status, and expansion status
insurance_mean <- aggregate(HINS1 ~ NATIVITY + RACE1 + expansion, data=df, FUN=mean)

# Create the bar graph
panelC<-ggplot(data=insurance_mean, aes(x=RACE1, y=HINS1, fill=NATIVITY)) +
  geom_bar(stat="identity", position=position_dodge()) +
  facet_grid( ~ expansion) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0), breaks=seq(0,1,0.1))+
  scale_fill_brewer(palette="Pastel1")+ # specify the colors you want
  labs(x="Race", y="Employer-sponsored Insurance Rate")+#, title="Insurance Rates by Race and Nativity Status and Expansion Status") +
  #theme_classic()
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14))

########### Plot 2: Panel D #####################

# Calculate the mean insurance rate by ACA status, citizenship status, and expansion status
insurance_mean <- aggregate(HINS2 ~ NATIVITY + RACE1 + expansion, data=df, FUN=mean)

# Create the bar graph
panelD<-ggplot(data=insurance_mean, aes(x=RACE1, y=HINS2, fill=NATIVITY)) +
  geom_bar(stat="identity", position=position_dodge()) +
  facet_grid( ~ expansion) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0), breaks=seq(0,1,0.1))+
  scale_fill_brewer(palette="Pastel1")+ # specify the colors you want
  labs(x="Race", y="Self Purchased Insurance Rate")+ #, title="Insurance Rates by Race and Nativity Status and Expansion Status") +
  #theme_classic()
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14))

##################### Append all the panels in one plot #####################


plot2<-ggarrange(panelA+ font("x.text", size = 7), panelB + font("x.text", size = 7), panelC+ font("x.text", size = 7), panelD + font("x.text", size = 7) ,
                   ncol = 2, nrow =2,common.legend=TRUE)

plot2


######################################################################
## plot 3: self purchased insurance rate by race and nativity
#######################################################################

# Code for creating plot 9  self purchased insurance rate by race and nativity

## Insurance thriygh employer
Forgn<- Data[Data$NATIVITY == 2, ]

df <- subset(Forgn, select=c(GEOR,CIT,expansion, YEAR, HICOV))


df$CIT<-as.numeric(df$CIT)
df$CIT<- ordered(df$CIT, labels = c("Naturalized-citizen", "Immigrant"))


# Calculate the mean insurance rate by ACA status, citizenship status, and expansion status
insurance_mean <- aggregate(HICOV ~ CIT + GEOR + expansion, data=df, FUN=mean)


# Create the bar graph
plot9_a<-ggplot(data=insurance_mean, aes(x=GEOR, y=HICOV, fill=CIT)) +
  geom_bar(stat="identity", position=position_dodge()) +
  facet_wrap( ~ expansion) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0), breaks=seq(0,1,0.1))+
  scale_fill_brewer(palette="Paired")+ # specify the colors you want
  labs(x="Birthplace", y="Mean Insurance Rate", fill = "Immigration Status",title="Insurance Rates by Birthplace, Immigration Status for forign-born people") +
  #theme_classic()
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14),
        axis.text.x = element_text(angle = 45, hjust = 1))

# rotate and align x-axis labels

print(plot9_a)



# Create grouped bar chart

p1<-ggplot(insurance_mean,                         # Draw barplot with grouping & stacking
           aes(x = expansion,
               y = HICOV,
               fill = CIT)) + 
  geom_bar(stat = "identity",
           position = "stack") +
  facet_wrap(~ GEOR, strip.position = "bottom")+
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Expansion Status",
       y = "Insurance Rate",
       fill = "Immigration Status",
       title = "Insurance Rates by Expansion and Immigration Status") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14),
        axis.text.x = element_blank(),
        strip.placement = "outside")
# axis.text.x = element_text(angle = 45, hjust = 1))
print(p1)


#

p1<-ggplot(data=subset(insurance_mean, expansion == "Adopted"), aes(x=GEOR, y=HICOV, fill=CIT)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0), breaks=seq(0,1,0.1)) +
  scale_fill_brewer(palette="Paired") + # specify the colors you want
  labs(x="Birthplace", y="Mean Insurance Rate", fill = "Immigration Status",title="Insurance Rates by Birthplace, Immigration Status for forign-born people (Expanded)") +
  theme_classic()

p2<-ggplot(data=subset(insurance_mean, expansion == "Not Adopted"), aes(x=GEOR, y=HICOV, fill=CIT)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0), breaks=seq(0,1,0.1)) +
  scale_fill_brewer(palette="Paired") + # specify the colors you want
  labs(x="Birthplace", y="Mean Insurance Rate", fill = "Immigration Status",title="Insurance Rates by Birthplace, Immigration Status for forign-born people (Expanded)") +
  theme_classic()


######################################################################
## plot 4 : self purchased insurance rate by race and nativity
#######################################################################

df <- subset(Forgn, select=c(GEOR,CIT,expansion, YEAR, HINS4))


df$CIT<-as.numeric(df$CIT)
df$CIT<- ordered(df$CIT, labels = c("Naturalized-citizen", "Immigrant"))


# Calculate the mean insurance rate by ACA status, citizenship status, and expansion status
insurance_mean <- aggregate(HINS4 ~ CIT + GEOR + expansion, data=df, FUN=mean)


# Create the bar graph
plot10<-ggplot(data=insurance_mean, aes(x=GEOR, y=HINS4, fill=CIT)) +
  geom_bar(stat="identity", position=position_dodge()) +
  #facet_wrap( ~ expansion) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0), breaks=seq(0,1,0.1))+
  scale_fill_brewer(palette="Paired")+ # specify the colors you want
  labs(x="Birthplace", y="Mean Medicaid Take-up Rate", fill = "Immigration Status",title="Medicaid Take-up Rate by Birthplace, Immigration Status for forign-born people") +
  #theme_classic()
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14),
        axis.text.x = element_text(angle = 45, hjust = 1))

# rotate and align x-axis labels

print(plot10)

