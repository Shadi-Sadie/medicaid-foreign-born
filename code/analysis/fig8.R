# Title: Figure 8. Unadjusted event study, pooled sample (US-born vs foreign-born)
# Author: Shadi Seyedi
# Description: Same event-study model as Figure 1 without control variables.
#              Copied from codes/R/08-function-event-plot-unad.R (now in archive/).
#              It ran on `Data` as loaded from CLNACS2.RData (ForeginBorn is already in the file).
# Output: output/figures/fig8.pdf
# Note: the colours differ from the paper's Figure 8 because the palette in the original script
#       was changed after the paper's figure was made.

source("code/00_setup.R")
Data <- make_sample(readRDS(data_file), sample_name)

library(ggplot2)
library(fixest)
library(ggpubr)

##This is the code for creating Unadjsted event study for all outcome variables.

################## Starting the Function with variable  ###########################


treatment_var<- c("ForeginBorn")
plotleglable<- c('US-Born','Foregin-born')
Dataset<-Data
### Changing the graph colors
cbPalette <- c("#888888", "#999933", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


#################################### Regression tables  ########################################


regression_models <- list()
plot_list <- list()

for (outcome_var in outcome_vars) {
    # FIX: fixed effects and clustering as in Figure 1 (fig1.R).
    formula <- as.formula(paste(outcome_var, "~",treatment_var," * i(ttot, expansion, ref = -1) | ST^", treatment_var, "+ YEAR^", treatment_var)) # if adjusted need to be added .[controlvar]#

    Event= feols( formula   ,                        ## FEs
                  vcov= ~ST,
                  weights = ~PWGTP,
                  data = Dataset)
    regression_models[[outcome_var]] <- Event


    ################ Get info from regression to create basic for est and sd calculation ####################

    draft<-coeftable(Event)
    draft<-as.data.frame(draft)

    event_rows <- grepl("ttot::", rownames(draft))   # keep only the event-time coefficients
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

    cov<-vcov(Event)

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


fig8 <- ggarrange(plotlist = plot_list, ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")
ggsave(file.path(figure_dir, "fig8.pdf"), fig8, width = 10, height = 7)
