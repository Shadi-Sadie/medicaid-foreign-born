# Title: Figure 3. Adjusted event study by citizenship status
# Author: Shadi Seyedi
# Description: Event-study model with interactions for foreign-born citizens and non-citizens
#              (reference group: US-born), with controls, for four outcomes.
#              Copied from codes/R/10-event-plot-cit-adj.R (now in archive/).
# Output: output/figures/fig3.pdf

source("code/00_setup.R")
load(data_file)

library(ggplot2)
library(fixest)
library(ggpubr)

# FIX: FBCIT was coded from CIT with the misspelled level "US-citizen Born abroad " (see table6.R),
# which left 16,187 foreign-born citizens in the US-born group. Foreign-born citizens are now
# foreign-born adults who are not non-citizens.
Data$FBCIT <- ifelse(Data$ForeginBorn == 1 & Data$NonCit == 0, 1, 0)

outcome_vars <- c("UNINS", "HINS4", "HINS1", "HINS2")
outcome_labels <- c("UNINS" = "Uninsured", "HINS4" = "Medicaid", "HINS1" = "Employer-Sponsored", "HINS2" = "Directly Purchased")

treatment_var<-c("FBCIT", "NonCit")
variable_list <-c("FBCIT", "NonCit",
                  "UnempR" , "SEX"   ,  "DIS"   ,  "AGEP"    ,"SCHLG.L", "SCHLG.Q" ,"SCHLG.C", "SCHLG^4", "MARG",
                    "ESRG.L" , "ESRG.Q",  "ENG.L" ,  "ENG.Q"  , "RACE1.L", "RACE1.Q", "RACE1.C", "RACE1^4",
                    "ENG.C" ,  "ENG^4" ,  "LTINU" , "NonCit", "IPC", "POVPIPG.L", "ADA",

"FBCIT:LTINU", "FBCIT:RACE1.L", "FBCIT:RACE1.Q", 
"FBCIT:RACE1.C", "FBCIT:RACE1^4", "FBCIT:SCHLG.L", 
"FBCIT:SCHLG.Q" ,"FBCIT:SCHLG.C", "FBCIT:SCHLG^4", 
'FBCIT:MARG',"FBCIT:IPC", "FBCIT:ESRG.L" , 
"FBCIT:ESRG.Q","FBCIT:ADA","FBCIT:UnempR",

"NonCit:LTINU", "NonCit:RACE1.L", "NonCit:RACE1.Q", 
"NonCit:RACE1.C", "NonCit:RACE1^4", "NonCit:SCHLG.L", 
"NonCit:SCHLG.Q" ,"NonCit:SCHLG.C", "NonCit:SCHLG^4", 
'NonCit:MARG',"NonCit:IPC", "NonCit:ESRG.L" , 
"NonCit:ESRG.Q","NonCit:ADA","NonCit:UnempR") ########## To be removed from the draft table
plotleglable<- c('US-Born' ,'FB-Citizen', 'FB-Non-citizen' )
# FIX: the original ran on `data`, the copy made in 06-did-regressions.R that turns ordered factors
# into plain factors. The coefficient names then no longer matched `variable_list` and the script
# crashed. It now runs on `Data` as loaded.
Dataset<-Data
### Changing the graph colors 
cbPalette <- c("#000021", "#88CCEE", "#661100", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


#################################### Regression tables  ########################################

regression_models <- list()
plot_list <- list()

for (outcome_var in outcome_vars) {
    formula <- as.formula(paste(outcome_var, "~", treatment_var[1]," * i(ttot, expansion, ref = -1) +" ,   treatment_var[2]," * i(ttot, expansion, ref = -1) + SEX + DIS+AGEP+POVPIPG+ENG+LTINU+NonCit+",
                                treatment_var[1],"*(SCHLG+ MARG+ RACE1 + ESRG + ADA+UnempR+IPC) +",
                                treatment_var[2],"*(SCHLG+ MARG+ RACE1 + ESRG + ADA+UnempR+IPC)  | ST + YEAR"))

    Event= feols( formula   ,
                  vcov="hetero",
                  weights = ~PWGTP,
                  data = Dataset)

    regression_models[[outcome_var]] <- Event


    ################ Get info from regression to create basic for est and sd calculation ####################

    draft<-coeftable(Event)
    draft<-as.data.frame(draft)

    existing_rows <- variable_list[variable_list %in% rownames(draft)]  # give the row number in darft that their names is among variable list

    if (length(existing_rows) > 0) {
        dropOutFtab <- which(rownames(draft) %in% existing_rows)
        draft <- draft[-dropOutFtab, ]
    }

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

    cov<-vcov(Event, "hetero")

    if (length(existing_rows) > 0) {
        dropOutFtab <- which(rownames(cov) %in% existing_rows)
        cov<-cov[-dropOutFtab,-dropOutFtab]
    }

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
