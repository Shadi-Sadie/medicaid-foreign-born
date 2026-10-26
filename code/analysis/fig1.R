# Title: Figure 1. Adjusted event study, pooled sample (US-born vs foreign-born)
# Author: Shadi Seyedi
# Description: Event-study model with foreign-born interactions and controls, for four outcomes.
#              The foreign-born effect is the US-born coefficient plus the interaction.
#              Copied from codes/R/08-function-event-plot-adj.R lines 1-176, with the data
#              preparation from codes/R/06-did-regressions.R lines 19-28 that it ran on
#              (both now in archive/).
# Output: output/figures/fig1.pdf

source("code/00_setup.R")
load(data_file)

library(ggplot2)
library(fixest)
library(ggpubr)


####################
## Data Preparation (from 06-did-regressions.R)
####################

Data$ForeginBorn <- ifelse(Data$NATIVITY == "Foregin-born", 1, 0)
# Make sure data are factor
data<-Data
factor_columns <- sapply(data, is.factor)
data[factor_columns] <- lapply(data[factor_columns], as.character)

data <- replace(data, is.na(data), 99999999)

data[factor_columns] <- lapply(data[factor_columns], as.factor)


################## Starting the Function with variable  ###########################

outcome_vars <- c("UNINS", "HINS4", "HINS1", "HINS2")
outcome_labels <- c("UNINS" = "Uninsured", "HINS4" = "Medicaid", "HINS1" = "Employer-Sponsored", "HINS2" = "Directly Purchased")

treatment_var<- c("ForeginBorn")
plotleglable<- c('US-Born','Foregin-born')
Dataset<-data
### Changing the graph colors
cbPalette <- c("#232424", "#CC6677", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


#################################### Regression tables  ########################################


regression_models <- list()
plot_list <- list()

for (outcome_var in outcome_vars) {
  formula <- as.formula(paste(outcome_var, "~",treatment_var," * i(ttot, expansion, ref = -1)+ SEX + DIS+AGEP+POVPIPG+ENG+LTINU+NonCit+ForeginBorn*(SCHLG+ MARG+ RACE1 + ESRG + ADA+UnempR+IPC)| ST + YEAR")) # if adjusted need to be added #

  Event= feols( formula   ,                        ## FEs
                vcov="hetero",
                weights = ~PWGTP,
                data = Dataset)
  regression_models[[outcome_var]] <- Event


  ################ Get info from regression to create basic for est and sd calculation ####################

  draft<-coeftable(Event)
  draft<-as.data.frame(draft)

  event_rows <- grepl("ttot::", rownames(draft))
  draft <- draft[event_rows, ]

  checkmark<-any(grepl(treatment_var,rownames(draft)))
  if (checkmark) {
    plot<-draft[1:(nrow(draft)/2),1:2]
  } else {
    plot<-draft[,1:2]
  }

  plot$id<-1
  plot$x_year<-as.numeric(gsub("[^0-9.-]", "", rownames(plot))) # extracting year variable from row names

  colnams<-c("estimate","sd","id", "x_year") #assigning names
  colnames(plot)<- colnams
  ############## Calculating the est of interaction #############
  #
  est1<-draft[1:(nrow(draft)/2),1]
  est2<-draft[((nrow(draft)/2)+1):nrow(draft),1]
  est3<-est2+est1

  ############## # Calculate standard error for each pair of rows and columns#############

  cov<-vcov(Event, "hetero")

  event_rows <- grepl("ttot::", rownames(cov))
  cov <- cov[event_rows, event_rows]

  if (checkmark) {
    i <- 1
    j <- nrow(cov)/2
    se_list <-list()  # initiating a vector for calculating SE for the desired plot
    while (i <= j) {
      se_list[[i]] <- sqrt(cov[i, i] + cov[i+j, i+j] + 2 * cov[i, i+j])
      i <- i + 1
    }

    cosd<-data.frame(estimate= est3)
    cosd$se<-unlist(se_list)
    cosd$id<-2
    cosd$x_year<-as.numeric(gsub("[^0-9.-]", "", rownames(plot)))

    colnames(cosd)<- colnams

    plot<-rbind(plot,cosd)

  }


  ############## Calculating the est of interaction #############

  plot$prms.ci_low<- plot$estimate - qnorm(0.975) * plot$sd
  plot$prms.ci_high<- plot$estimate + qnorm(0.975) * plot$sd


  #############################Prepare the table for ploting ##################################

  plot$label<-plot$x_year
  nrplot<-nrow(plot)
  plot$x_year<-ifelse(1:nrplot < nrplot/2 , plot$x_year-0.1, plot$x_year+0.1)


  ########################## Now plot it #######################################

  plot<-ggplot(plot,
               aes(
                 x =x_year, y =estimate,group =as.factor(id),
                 ymin = prms.ci_low , ymax = prms.ci_high ,col=as.factor(id)
               ))+
    labs(title = outcome_labels[outcome_var], x="Time to Treatment", y="Estimate and 95% Conf. Int")+
    geom_hline(yintercept = 0)+
    geom_point(aes(shape=as.factor(id)),size=3) +
    geom_errorbar(width = 0.07) +
    geom_vline(xintercept = -1, lty =  "dashed")+
    scale_colour_manual(labels=plotleglable,name="Population",values=cbPalette)+
    scale_shape_discrete(labels=plotleglable,name="Population")+
    scale_y_continuous(breaks = scales::breaks_pretty()) +
    theme(panel.background = element_rect(fill = "white", colour = "grey50"),legend.position="bottom", plot.title = element_text(hjust = 0.5))

  plot_list[[outcome_var]] <- plot

}


fig1 <- ggarrange(plotlist = plot_list, ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")
ggsave(file.path(figure_dir, "fig1.pdf"), fig1, width = 10, height = 7)
