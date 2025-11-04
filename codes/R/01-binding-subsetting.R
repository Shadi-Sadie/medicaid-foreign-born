
##############################################################################################################
## In this program I am subseting Raw Data set based on the inclusion criteria. 
## Additonaly, I  will change the vacant to the NA and change the variable fromat from charachteer to integer
## Updated on Nov 14, 2022
##############################################################################################################

### Loading Required Packages 

#install.packages("sqldf")
library(tidyverse)
library(sqldf)          # library to allow to subset while reading the data
library(readxl)



### Loading the paths 
wd <- list()
# commonly used paths in my working directory  #######NOTE: UPDATE this section for the reproducibility## #################
wd$data   <- "/home/shadi/Projects/GitHub/medicaid-foreign-born/data/"
wd$output <- "/home/shadi/Projects/GitHub/medicaid-foreign-born/output/"
wd$texts <- "/home/shadi/Projects/GitHub/medicaid-foreign-born/text/"
wd$codes <- "/home/shadi/Projects/GitHub/medicaid-foreign-born/codes/R/"



                                                        #### loading the  Data set ####


# remove the # for the  section below if you want to read the RAWACS from the pc   

#Data<-read.csv( paste0(wd$data,"RAWACS-2025.csv"), header = TRUE, sep = ",", fill = TRUE, stringsAsFactors = TRUE)


Data<-RAWACS


                                                       #### Subsetting the dataset ####


###### 1. Subseting based on the income  #### Already subseted ####

#Data<- Data %>% filter(POVPIP<139)   # Subset the income level only less than <139  

###### 2. Subseting based on Age  #### Already subseted ####

Data<- Data %>% filter(AGEP<65 & AGEP>25) 


###### 3. Removing the institutionlized and unistitulionalized group quarters population


Data<-Data[!(Data$TYPE %in% c(2,3)),] 

 # variable TYPE is no longer  needed can be dropped 

# Data<-Data[!(names(Data)=="TYPE")]

###### 4.Sub-seting based on years lived in US (for immigrants)
 
Data$YLIU<-0                      # create variable YLIU year lived in US to use for removing immigrant with less than 5 years
Data$YLIU<-Data$YEAR-Data$YOEP  
table(Data$YLIU, exclude = NULL) # looking at the table of years living in US
table(Data$CIT, Data$YLIU, exclude = NULL) # looking at the table of years living in US by citizenship status. 

#Data$YLIU<-replace(Data$YLIU, Data$CIT %in% c(2,3),NA) # born in foreign could inter to us as well but i don't want it recode them NA

Data<-Data[!(Data$YLIU<5 & Data$CIT==5),]  # Removing the data of immigrant with less than 5 years residency

###### 5. Removing the US citizen born outside US and those born portorico and virgin island and ..

#Data<-Data[!(Data$CIT %in% c(2,3) ),]  # Re





                                                #### Merging with other data sets ####



#### Export extra variables ####

#####  expansion status


expanData <- read_excel(paste0(wd$data,"expansion.xlsx"), sheet = "raw_data")

# this data set includes stfips code(ST), State name(StateN), expansion(expansion) if state adopted or not(treatment), the year of expansion (ExpansionY)
# and the Expan_STS which has values not expanded, expanded, late expanded.

# checking the class of data
sapply(expanData, class)

# expansion and Expan_STS are character but they need to be factor, I will change their class next
expanData$expansion<-as.factor(expanData$expansion)
expanData$Expan_STS<-as.factor(expanData$Expan_STS)

# merging data to whole data set
Data <- merge(Data, expanData, by = "ST", all.x = TRUE)
# Expansion year for the state that didn't adopt the medicaid is 0 I like to change that to NA
# I might later decide to change it to 0 again
Data$ExpansionY <- ifelse(Data$ExpansionY == 0, NA, Data$ExpansionY)
# the expansion treatment is not dynamic it only shows if the satet is among expansion or non expansion states
# Thus I need to create another variable called treatment that is dynamice
Data$treat<-ifelse(Data$YEAR>=Data$ExpansionY & !is.na(Data$ExpansionY) , 1,0)
#Change adoptet to expansion values
b<-Data$expansion
levels(b) <- c("Non-expansion","Expansion")
class(b)
b<-as.factor(b)
Data$expansion<-b





## IPC Index##

# Read an Excel file named "IPC.xlsx" from the working directory, 
# where the sheet named "Sheet1" will be read.
# This is going to be the all IPC score
IPCINDX<-read.csv(paste0(wd$data,"IPC_Final.csv"), header = TRUE, sep ="," , fill = TRUE)
IPCINDX <- IPCINDX[, c("state", "year", "annual_score")] #subset

colnames(IPCINDX) <- c("StateN", "YEAR", "IPCINDX")
sapply(IPCINDX, class)
Data <- merge(Data, IPCINDX, by = c("StateN", "YEAR"), all.x = TRUE)

# This is going to be IPC score without including Medicaid score 
IPC <- read.csv(paste0(wd$data,"IPC.csv"), header = TRUE, sep ="," , fill = TRUE)
#IPC <- read_excel(paste0(wd$data,"IPC.csv"), sheet = "Sheet1")
#
sapply(IPC, class)
colnames(IPC) <- c("StateN", "YEAR", "IPC")
Data <- merge(Data, IPC, by = c("StateN", "YEAR"), all.x = TRUE)


## Political climate ##

# Read an Excel file named "Pol.xlsx" from the working directory, 

PolClM<-read_excel(paste0(wd$data,"Pol.xlsx"), sheet = "Sheet4")
sapply(PolClM, class)

# first I drop the extra variable
#PolClM <- PolClM %>% select(-"STATE ABR")

# then I will change the var names

colnames(PolClM) <- c("YEAR", "StateN", "ST", "ADA")

Data <- merge(Data, PolClM, by = c("ST", "YEAR", "StateN"), all.x = TRUE)

table(Data$ADA,exclude=NaN)

class(Data$ADA)
#table(PolClM$Leg_Control, exclude = NaN)
#table(PolClM$Gov_Party, exclude = NaN)
#table(PolClM$State_Control, exclude = NaN)

# values Divided and split are the same need to be replaced
#PolClM$`Leg. Control` <- replace(df$Status, df$Status == "Split", "Divided")


#change charachter variable to ordered factor
#levels_order <- c("Dem", "Rep", "Divided")
#for (var in c("Leg. Control", "Gov. Party","State Control" )) {
# PolClM[[var]] <- factor(PolClM[[var]], levels = levels_order, ordered = TRUE)
#}



## State's unemployment rate ##

UNEMPR <- read_excel(paste0(wd$data,"UnempR.xlsx"), sheet = "Sheet1")
sapply(UNEMPR, class)

# Drop the columns I don't want, state name and State ABR are not included in the original Dataset, 
# I might need to drop them or just keep the ABR for the final dataset

UNEMPR <- UNEMPR %>% select(-c(`STATE ABR` , `2020`, `2021`))

# Use pivot_longer to reshape the data
unemp_data_long <- pivot_longer(UNEMPR, 
                                cols = c(`2009`,`2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`), 
                                names_to = "YEAR", 
                                values_to = "UnempR")

# Print the transformed data check if everything is correct
print(unemp_data_long)
UNEMPR<-unemp_data_long 
rm(unemp_data_long)

colnames(UNEMPR) <- c("StateN","ST", "YEAR", "UnempR")
#cheking the values again, year is charachter and need to be converted to numeric
sapply(UNEMPR, class)
UNEMPR$YEAR<- as.numeric(UNEMPR$YEAR)
table(UNEMPR$YEAR)
## Now, I try to merge my data 




Data <- merge(Data, UNEMPR, by = c("ST", "YEAR", "StateN"), all.x = TRUE)






 # write.csv(Data, file = "PREACS-2025.csv", row.names = FALSE)

  PREACS<-Data


