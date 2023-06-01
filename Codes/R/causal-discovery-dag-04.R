# Title: DAG causal discovery 
# Author: Shadi Seyedi
# Date: May 31, 2023
# Description: 
# This code script constructs a Directed Acyclic Graph (DAG) using the dagitty package and visualizes it using ggdag.
# The resulting DAG represents the causal relationships between variables inferred from the data. It provides valuable insights into the underlying  
# causal structure of the variables involved in the analysis. The intial DAG is generated based on algorithms (GES, GDS, PC, RFCI) and majority voting 
# it may not represent the true underlying causal structure with absolute certainty. However, it serves as a useful tool for generating hypotheses and 
# informing the modeling process.I manually edit the DAG by removing paths that don't align with my domain knowledge or seem implausible in the context
# of the analysis. This results in the final DAG representation.
# To improve clarity and simplicity, I further grouped related variables into composite nodes. For instance, variables like
# "education," "income," and "employment" are combined into a single node called "Socioeconomics." This grouping can help
# streamline the analysis and provide a more concise representation of the underlying relationships.
# The simplified DAG helps to highlight key relationships and pathways of interest, making it easier to identify potential
# confounding factors and interpret the effects of variables on the outcome of interest.

####################
## Required Packages
####################

#install.packages('gsl ')
#install.packages('brew')
#install.packages('AlphaPart')
#install.packages('utils')
#install.packages('Rgraphviz')
#install.packages('libgsl-dev')
#install.packages("energy")
#install.packages("devtools")
#install.packages("pcalg")
#install.packages("kpcalg")
#install.packages("tabuSearch")
#install.packages("graph")
#install.packages("dagitty")
#install.packages("ggdag")
#tabuSearch
# install.packages("CAM") not availble for R 3.6.3
#install.packages("bnlearn")
#install.packages("rcausal") not availble for R 3.6.3
## install needed package graph
#if (!require("BiocManager", quietly = TRUE))
#        install.packages("BiocManager")
#BiocManager::install("graph")
## install needed package RGBL
#if (!require("BiocManager", quietly = TRUE))
 #       install.packages("BiocManager")
#BiocManager::install("RBGL")
# Install Rgraphviz
#if (!require("BiocManager", quietly = TRUE))
        #install.packages("BiocManager")
#BiocManager::install("Rgraphviz")

#library(gsl)
#library(utils)
library(gsl)
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
library(csv)
library(brew)
library(dplyr)
library(dagitty)
library(ggdag)

####################
## Data Preparation 
####################

# Data <- read.csv("CLNACS.csv", header = TRUE, sep = ",", fill = TRUE, stringsAsFactors = TRUE)
# Forgn<- Data[Data$NATIVITY == "Foregin-born", ]

data1 <- subset(Data, select = c(IPC,IPCINDX,ENG,GEOR,YLIU,PLIU,CULRG, SEX,DIS,AGEP, SCHLG, MARG, ESRG, RACE1,POVPIP,treat,UnempR,ADA,CIT,HINS4))
data2 <- subset(Forgn, select = c(IPC,IPCINDX,ENG,GEOR,YLIU,PLIU,CULRG, SEX,DIS,AGEP, SCHLG, MARG, ESRG, RACE1,POVPIP,treat,UnempR,ADA,CIT,HINS4))
data3 <- subset(Data, select = c(IPC,IPCINDX,ENG,GEOR,YLIU,PLIU,CULRG, SEX,DIS,AGEP, SCHLG, MARG, ESRG, RACE1,POVPIP,treat,UnempR,ADA,CIT,UNINS))
data4 <- subset(Forgn, select = c(IPC,IPCINDX,ENG,GEOR,YLIU,PLIU,CULRG, SEX,DIS,AGEP, SCHLG, MARG, ESRG, RACE1,POVPIP,treat,UnempR,ADA,CIT,UNINS))

####################
## Identify Causal Graph
####################

# Create an empty list to store the matrices

matMajVotingList <- list()

# Loop through data1 to data4
for (i in 1:4) {
        # Get the current data
        currentData <- get(paste0("data", i))
        
        # Convert character variables to factors
        currentData <- currentData %>% mutate_if(is.character, as.factor)
        
        # Convert factor variables to numeric
        currentData <- currentData %>% mutate_if(is.factor, as.numeric)
        
        # 1.Perform the GES algorithm
        score <- new("GaussL0penObsScore", currentData)
        ges1.fit <- ges(score, fixedGaps = NULL, adaptive = "vstructures", phase = "forward", maxDegree = integer(0), verbose = FALSE)
        
        # Create the matrix for GES algorithm
        p <- dim(currentData)[2]
        matGES <- matrix(0, nrow = p, ncol = p)
        row <- 1
        for (list in ges1.fit$essgraph$.in.edges) {
                for (node in list) {
                        matGES[node, row] <- 1
                }
                row <- row + 1
        }
        
        # 2. Perform the GDS algorithm
        gds.fit <- gds(score, fixedGaps = NULL, adaptive = "triples", phase = "forward", maxDegree = integer(0), verbose = FALSE)
        
        # Create the matrix for GDS algorithm
        matGDS <- matrix(0, nrow = p, ncol = p)
        row <- 1
        for (list in gds.fit$essgraph$.in.edges) {
                for (node in list) {
                        matGDS[node, row] <- 1
                }
                row <- row + 1
        }
        
        # 3. Perform the PC algorithm
        SuffStat <- list(C = cor(currentData), n = nrow(currentData))
        result.pc <- pc(suffStat = SuffStat, indepTest = gaussCItest, labels = colnames(currentData), alpha = 0.01)
        
        # Create the matrix for PC algorithm
        matPC <- matrix(0, nrow = p, ncol = p)
        row <- 1
        for (list in result.pc@graph@edgeL) {
                for (node in list) {
                        matPC[node, row] <- 1
                }
                row <- row + 1
        }
        
        # 4.Perform the RFCI algorithm
        result.rfci <- rfci(SuffStat, indepTest = gaussCItest, p = ncol(currentData), alpha = 0.01)
        
        # Create the matrix for RFCI algorithm
        matRFCI <- matrix(0, nrow = p, ncol = p)
        item <- 0
        for (l in result.rfci@amat) {
                row <- item %% p + 1
                column <- as.integer(item / p) + 1
                if (l == 2)
                        matRFCI[row, column] <- 1
                if (l == 3)
                        matRFCI[column, row] <- 1
                item <- item + 1
        }
        
        # Majority voting of the DAG
        matMajVoting <- matGES + matGDS + matPC + matRFCI
        
        # Store the matrix in the list
        matMajVotingList[[i]] <- matMajVoting
}

# Access the matrices from the list
matMajVoting1 <- matMajVotingList[[1]]
matMajVoting2 <- matMajVotingList[[2]]
matMajVoting3 <- matMajVotingList[[3]]
matMajVoting4 <- matMajVotingList[[4]]

MajVoting <- matMajVoting1 + matMajVoting2 + matMajVoting3 + matMajVoting4

#MajVotingF <- matMajVoting2 + matMajVoting4

# create a DAG based on majority voting 

digraph <- "dag {"  # Initialize the digraph as an empty string

for(i in 1:p){
        for(j in 1:p){
                if(MajVoting[i, j] > 10){ # change the value 2 to an appropriate value (threshold)
                        #print(variables[i])
                        digraph <- paste0(digraph, variables[i], "->", variables[j],";")
                }
        }
}

digraph <- gsub(";$", "}", digraph) # change the last ; to } to make it compatible for Dagitty function 

# create DAG object
dag <- dagitty(digraph)

# DAG representation
ggdag(dag)+  # Turn off legend
        theme_dag()

####################
## Edit the identified DAG
####################

# Manually attempting to remove paths that don't align with my domain knowledge or seem implausible in the context
# of the analysis. This results in the final DAG representation.


# create a dag object

findag<-dagitty('dag{
 bb="-5.958,-4.932,7.16,5.438"
 "Life Lived in US" [pos="-2.079,1.854"]
 "State Political Ideology" [pos="-4.930,0.020"]
 "Age" [pos="1.789,-1.856"]
 "Citizenship Status" [pos="-2.079,2.842"]
 "Disability" [pos="1.789,-2.675"]
 "English Proficiency" [pos="-2.079,3.801"]
 "Employment" [pos="-2.079,-3.112"]
 "Immigration Policy Climate" [pos="-4.554,2.786"]
 "Marital Status" [pos="1.811,-1.080"]
 "Income-to-Poverty Ratio" [pos="-2.068,-0.657"]
 "Race/Ethnicity" [pos="1.789,-0.107"]
 "Cultural Grouping" [pos="-2.090,4.930"]
 "Education" [pos="-2.046,-2.068"]
 "Sex" [pos="1.800,-3.676"]
 "Medicaid Take-Up" [outcome,pos="1.822,1.304"]
 "State unemployment rate" [pos="-3.858,0.006"]
 "Medicaid Expansion" [exposure,pos="-4.389,1.276"]
 "Life Lived in US" -> "English Proficiency"
 "State Political Ideology" -> "Immigration Policy Climate"
 "State Political Ideology" -> "State unemployment rate"
 "State Political Ideology" -> "Medicaid Expansion"
 "Age" -> "Life lived in US"
 "Age" -> "Disability"
 "Age" -> "Employment"
 "Age" -> "Medicaid Take-Up"
 "Citizenship Status" -> "State Political Ideology"
 "Citizenship Status" -> "English Proficiency"
 "Citizenship Status" -> "Marital Status"
 "Citizenship Status" -> "Income-to-Poverty Ratio"
 "Citizenship Status" -> "Education" 
 "Citizenship Status" -> "Medicaid Take-Up"
 "Citizenship Status" -> "Employment"
 "Disability" -> "Employment"
 "Disability" -> "Education" 
 "Disability" -> "Medicaid Take-Up"
 "English Proficiency" -> "Employment"
 "English Proficiency" -> "Medicaid Take-Up"
 "Employment" -> "Income-to-Poverty Ratio"
 "Employment" -> "Medicaid Take-Up"
 "Immigration Policy Climate"  -> "Income-to-Poverty Ratio"
 "Immigration Policy Climate"  -> "Medicaid Take-Up"
 "Marital Status" -> "Income-to-Poverty Ratio"
 "Marital Status" -> "Medicaid Take-Up"
 "Income-to-Poverty Ratio" -> "Medicaid Take-Up"
 "Race/Ethnicity" -> "Education"  
 "Cultural Grouping" -> "English Proficiency"
 "Cultural Grouping" -> "Employment"
 "Cultural Grouping" -> "Race/Ethnicity"
 "Cultural Grouping" -> "Education" 
 "Cultural Grouping" -> "Medicaid Take-Up"
 "Education" -> "English Proficiency"
 "Education" -> "Employment"
 "Education" -> "Medicaid Take-Up"
 "Sex" -> "Employment"
 "Sex" -> "Medicaid Take-Up"
 "State unemployment rate" -> "Employment"
 "State unemployment rate" -> "Education"
 "State unemployment rate" -> "Medicaid Take-Up"
 "State unemployment rate" -> "Medicaid Expansion"
 "Medicaid Expansion" -> "Medicaid Take-Up"
}')

FINDAG<-ggdag_status(findag,node_size = 10,use_labels = "name", text = FALSE)+
        guides(color = FALSE) +  # Turn off legend
        theme_dag()

FINDAG$layers[[3]]$mapping <- aes(colour = status)
FINDAG + scale_color_manual(na.value ="#ffc300" , values = c(exposure ="#A94064" , outcome = "#44AA99")) +
        scale_fill_manual(na.value ="#ffc300", values = c(exposure ="#A94064" , outcome = "#44AA99"))

# check what should be the adjustment set based on this graph
adjustmentSets(findag)



# A more simplified DAG 

# for this purpose I grouped related variables into composite nodes. For instance, variables like
# "education," "income," and "employment" are combined into a single node called "Socioeconomics."

digraph <- "dag {"  # Initialize the digraph as an empty string

for(i in 1:p){
        for(j in 1:p){
                if(matMajVoting[i, j] > 2){ # change the value 2 to an appropriate value (threshold)
                        #print(variables[i])
                        digraph <- paste0(digraph, variables[i], "->", variables[j],";")
                }
        }
}

digraph <- gsub(";$", "}", digraph)
dag <- dagitty(digraph)
ggdag(dag, layout = "circle")
print(dag)
table(data$IPC)


shaddag<-dagitty('dag {
        bb="-3.828,-4.241,3.728,3.459"
        "State Political Ideology" [pos="-3.274,-2.020"]
         Demographics [pos="0.049,-1.978"]
        "Medicaid Expansion" [exposure,pos="-2.803,-0.805"]
        "Immigration Policy Climate" [pos="-2.771,0.180"]
        "Foregin-Born" [pos="-1.294,0.128"]
        "Socioeconomic" [pos="-1.288,-2.010"]
        "Medicaid Take-Up" [outcome,pos="0.049,-0.763"]
        "State unemployment rate" [pos="-2.478,-2.020"]
        "State Political Ideology" -> "Medicaid Expansion"
        "State Political Ideology" -> "Immigration Policy Climate"
        "State Political Ideology" -> "State unemployment rate"
        "State Political Ideology" -> "Medicaid Take-Up"
        Demographics -> "Socioeconomic"
        Demographics -> "Medicaid Take-Up"
        "Medicaid Expansion" -> "Medicaid Take-Up"
        "Immigration Policy Climate" -> "Foregin-Born"
        "Immigration Policy Climate" -> "Medicaid Take-Up"
        "Foregin-Born" -> "Socioeconomic"
        "Foregin-Born" <-> "Demographics"
        "Foregin-Born" -> "Medicaid Take-Up"
        "Socioeconomic" -> "Medicaid Take-Up"
        "State unemployment rate" -> "Medicaid Expansion"
        "State unemployment rate" -> "Immigration Policy Climate"
        "State unemployment rate" -> "Socioeconomic"
        "State unemployment rate" -> "Medicaid Take-Up"
}')

####################
## Create simplified  DAG
####################

DAGSHADI<-ggdag_status(shaddag,node_size = 10,use_labels = "name", text = FALSE)+
        guides(color = FALSE) +  # Turn off legend
        theme_dag()

DAGSHADI$layers[[3]]$mapping <- aes(colour = status)
DAGSHADI + scale_color_manual(na.value ="#222D5a" , values = c(exposure ="#A94064" , outcome = "#44AA99")) +
        scale_fill_manual(na.value ="#222D5a", values = c( exposure ="#A94064" , outcome = "#44AA99")) # other coloer = "#ffc300",  


# check the adjustment set from this DAG

adjustmentSets(
        shaddag)



