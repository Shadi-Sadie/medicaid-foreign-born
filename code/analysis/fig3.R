# Title: Figure 3. Adjusted event study by citizenship status
# Author: Shadi Seyedi
# Description: Event-study model with interactions for foreign-born citizens and non-citizens
#              (reference group: US-born), with controls, for four outcomes.
#              Copied from codes/R/10-event-plot-cit-adj.R (now in archive/).
# Output: output/figures/fig3.pdf

source("code/00_setup.R")
Data <- make_sample(readRDS(data_file), sample_name)

library(ggplot2)
library(fixest)
library(ggpubr)


treatment_var<-c("FBCIT", "NonCit")
plotleglable<- c('US-Born' ,'FB-Citizen', 'FB-Non-citizen' )
Dataset<-Data
### Changing the graph colors 
cbPalette <- c("#000021", "#88CCEE", "#661100", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


#################################### Regression tables  ########################################

regression_models <- list()
plot_list <- list()

for (outcome_var in outcome_vars) {
    formula <- as.formula(paste(outcome_var, "~", treatment_var[1]," * i(ttot, expansion, ref = -1) +" ,   treatment_var[2]," * i(ttot, expansion, ref = -1) + SEX + DIS+AGEP+POVPIPG+ENG+LTINU+NonCit+",
                                treatment_var[1],"*(SCHLG+ MARG+ RACE1 + ESRG + ADA+UnempR+IPC) +",
                                treatment_var[2],"*(SCHLG+ MARG+ RACE1 + ESRG + ADA+UnempR+IPC)  |",
                                # FIX: fixed effects and clustering as in Figure 2 (fig2.R).
                                "ST^", treatment_var[1], "^", treatment_var[2], "+ YEAR^", treatment_var[1], "^", treatment_var[2]))

    Event= feols( formula   ,
                  vcov= ~ST,
                  weights = ~PWGTP,
                  data = Dataset)

    regression_models[[outcome_var]] <- Event


    ################ Get info from regression to create basic for est and sd calculation ####################

    draft<-coeftable(Event)
    draft<-as.data.frame(draft)

    event_rows <- grepl("ttot::", rownames(draft))   # keep only the event-time coefficients
    draft <- draft[event_rows, ]

    checkmark<-any(grepl(treatment_var,rownames(draft)))    # check if the string stored in treatment_var is  in any of the row names of the draft
    if (checkmark) {
        plot<-draft[1:(nrow(draft)/3),1:2]
    } else {
        plot<-draft[,1:2]
    }

    plot$id<-1
    plot$x_year<-as.numeric(gsub("[^0-9.-]", "", rownames(plot))) # extracting year variable from row names
    colnams<-c("estimate","sd","id", "x_year") #assigning names
    colnames(plot)<- colnams
    ############## Calculating the est of interaction #############
    #
    est1<-draft[1:(nrow(draft)/3),1]
    est2<-draft[((nrow(draft)/3)+1):(2*nrow(draft)/3),1]
    est3<-est2+est1
    est4<-draft[((2*nrow(draft)/3)+1): nrow(draft),1]
    est5<-est4+est1
    ############## # Calculate standard error for each pair of rows and columns#############

    cov<-vcov(Event)

    event_rows <- grepl("ttot::", rownames(cov))
    cov <- cov[event_rows, event_rows]

    if (checkmark) {
        i <- 1
        j <- nrow(cov)/3
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
    }


    ### Second set of calcuation

    if (checkmark) {
        i <- 1
        j <- 2*nrow(cov)/3
        se_list <-list()  # initiating a vector for calculating SE for the desired plot
        while (i <= 13) {
            se_list[[i]] <- sqrt(cov[i, i] + cov[i+j, i+j] + 2 * cov[i, i+j])
            i <- i + 1
        }

        cosd2<-data.frame(estimate= est5)
        cosd2$se<-unlist(se_list)
        cosd2$id<-3
        cosd2$x_year<-as.numeric(gsub("[^0-9.-]", "", rownames(plot)))

        colnames(cosd2)<- colnams
    }


    plot<-rbind(plot,cosd,cosd2)
    ############## Calculating the est of interaction #############

    plot$prms.ci_low<- plot$estimate - qnorm(0.975) * plot$sd
    plot$prms.ci_high<- plot$estimate + qnorm(0.975) * plot$sd


    #############################Prepare the table for ploting ##################################
    plot$label<-plot$x_year
    nrplot<-nrow(plot)
    plot$x_year<-ifelse(1:nrplot < (nrplot/3)+1 , plot$x_year-0.2, plot$x_year)
    plot$x_year[14:26] <- plot$x_year[14:26] + 0.2

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


fig3 <- ggarrange(plotlist = plot_list, ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")
ggsave(file.path(figure_dir, "fig3.pdf"), fig3, width = 10, height = 7)
