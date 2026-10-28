# Title: Figure 6. Simplified DAG
# Author: Shadi Seyedi
# Description: Draws the simplified directed acyclic graph with fixed node positions. It needs no
#              data. Copied from codes/R/04-causal-discovery-dag.R lines 310-349 (now in
#              archive/); the causal-discovery part of that script is not needed for the paper.
# Output: output/figures/fig6.pdf

source("code/00_setup.R")

library(ggplot2)
library(dagitty)
library(ggdag)


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
fig6 <- DAGSHADI + scale_color_manual(na.value ="#222D5a" , values = c(exposure ="#A94064" , outcome = "#44AA99")) +
        scale_fill_manual(na.value ="#222D5a", values = c( exposure ="#A94064" , outcome = "#44AA99")) # other coloer = "#ffc300",

ggsave(file.path(figure_dir, "fig6.pdf"), fig6, width = 8, height = 8)
