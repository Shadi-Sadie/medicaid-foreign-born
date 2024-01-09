library(ggplot2)
library(fixest)
library(ggpubr)
##This is the code for creating Unadjsted event study for all outcome variables.
table(Data$CITSTAT)

################## Creating new variable for this function  ###########################
Data$CITSTAT<-0
Data$CITSTAT<-ifelse(Data$CIT == "Born in US states" | Data$CIT =="Born in US Territories", "US-Born", Data$CITSTAT)
Data$CITSTAT<-ifelse(Data$CIT == "US-citizen Born Abroad" | Data$CIT == 'Naturalized-citizen', "FB-Citizen", Data$CITSTAT)
Data$CITSTAT<-ifelse(Data$CIT == "Non-citizen", "FB-Non-citizen", Data$CITSTAT)
table(Data$CITSTAT)
Data$CITSTAT<-as.factor(Data$CITSTAT)

# Create a dummy for foreign born citzien
Data$FBCIT<-0
Data$FBCIT<-ifelse(Data$CIT == "US-citizen Born Abroad" | Data$CIT == 'Naturalized-citizen', 1, Data$FBCIT)

Data$FBCIT<-0
Data$FBCIT<-ifelse(Data$CIT == "US-citizen Born Abroad" | Data$CIT == 'Naturalized-citizen', 1, Data$FBCIT)

Data$FBLEG<-55
Data$FBLEG<-ifelse(Data$ForeginBorn==1 & Data$UNDOC == 0, 1, 0)


################## Starting the Function with variable  ###########################

outcome_vars <- c("UNINS", "HINS4", "HINS1", "HINS2")
outcome_labels <- c("UNINS" = "Uninsured", "HINS4" = "Medicaid", "HINS1" = "Employer-Sponsored", "HINS2" = "Directly Purchased")

treatment_var<-c("FBCIT", "NonCit")
controlvar<-("UnempR","SEX","DIS", "AGEP","SCHLG", "MARG", "RACE1", "ESRG","ENG", "LTINU")
variable_list <- c("FBCIT", "NonCit") ########## To be removed from the draft table
plotleglable<- c('FB-Citizen', 'FB-Non-citizen', 'US-Born'  )
Dataset<-Data
### Changing the graph colors 
#cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

cbPalette <- c("#888888", "#999933", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# cbPalette  <- c("#000000","#B64074", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00" )

safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                             "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")


#################################### Regression tables  ########################################


#regression_models <- list()
#plot_list <- list()

#for (outcome_var in outcome_vars) {
    #formula <- as.formula(paste(outcome_var, "~",treatment_var," * i(ttot, expansion, ref = -1) | ST + YEAR")) # if adjusted need to be added .[controlvar]#
  #  
    ##### Need to change #########
    Event= feols(UNINS ~ FBCIT*i(ttot, expansion, ref = -1) + NonCit*i(ttot, expansion, ref = -1) | ST + YEAR,
                  vcov="hetero",
                  #  cluster = "ST",
                  weights = ~PWGTP,
                  data = Dataset)

Event= feols(UNINS ~ FBLEG*i(ttot, expansion, ref = -1) + UNDOC*i(ttot, expansion, ref = -1) +.[controlvar] | ST + YEAR,
             vcov="hetero",
             #  cluster = "ST",
             weights = ~PWGTP,
             data = Dataset)



   # regression_models[[outcome_var]] <- Event
    

    ################ Get info from regression to create basic for est and sd calculation ####################
    
    draft<-coeftable(Event) 
    draft<-as.data.frame(draft)
    
    existing_rows <- variable_list[variable_list %in% rownames(draft)]  # give the row number in darft that their names is among variable list
    
    if (length(existing_rows) > 0) {
        dropOutFtab <- which(rownames(draft) %in% existing_rows)
        draft <- draft[-dropOutFtab, ]
    }
    
    #grep("ttot",rownames(draft)))
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
    
    #se_treat_male <- sqrt(cov[1, 1] + cov[2, 2] + 2 * cov[1, 2]) ->

    if (checkmark) { 
        i <- 1    
        j <- nrow(cov)/3
        se_list <-list()  # numeric(nrow(cov)/2) ## initiating a vector for calculating SE for the desired plot
        while (i <= j) {
            #for (i in 1:row(cov)/2) {
            se_list[[i]] <- sqrt(cov[i, i] + cov[i+j, i+j] + 2 * cov[i, i+j])
            i <- i + 1
        }
        cosd<-data.frame(se = unlist(se_list))
        cosd<-data.frame(estimate= est3)
        cosd$se<-unlist(se_list)
        cosd$id<-2
        cosd$x_year<-as.numeric(gsub("[^0-9.-]", "", rownames(plot)))
        
        
        colnames(cosd)<- colnams
        
       
        
    }
    
    
   ### Second set of calcuation 
    
    
    #se_treat_male <- sqrt(cov[1, 1] + cov[2, 2] + 2 * cov[1, 2]) ->
    if (checkmark) { 
        i <- 1    
        j <- 2*nrow(cov)/3
        se_list <-list()  # numeric(nrow(cov)/2) ## initiating a vector for calculating SE for the desired plot
        while (i <= 13) {
            #for (i in 1:row(cov)/2) {
            se_list[[i]] <- sqrt(cov[i, i] + cov[i+j, i+j] + 2 * cov[i, i+j])
            i <- i + 1
        }
        
        cosd2<-data.frame(se = unlist(se_list))
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
    
    
    
    #plot$ci_treat_male <- est1 + c(-1,1) * qnorm(0.975) * 0.001735147
    
    
    #############################Prepare the table for ploting ################################## 
    #############################
   # plot$x_year<-plot$label
    plot$label<-plot$x_year
    nrplot<-nrow(plot)
    plot$x_year<-ifelse(1:nrplot < (nrplot/3)+1 , plot$x_year-0.1, plot$x_year)
   # plot$x_year<-ifelse(2*nrplot/3:nrplot, plot$x_year+0.1, plot$x_year)
    plot$x_year[14:26] <- plot$x_year[14:26] + 0.1
    
    ########################## Now plot it ####################################### 
    
    plots<-ggplot(plot,
                 aes(
                     x =x_year, y =estimate,group =as.factor(id),
                     ymin = prms.ci_low , ymax = prms.ci_high ,col=as.factor(id)
                 ))+
        #labs(title = outcome_labels[outcome_var], x="Time to Treatment", y="Estimate and 95% Conf. Int")+
        geom_hline(yintercept = 0)+
        geom_point(aes(shape=as.factor(id)),size=3) +
        geom_errorbar(width = 0.07) +
        geom_vline(xintercept = -1, lty =  "dashed")+
        # geom_ribbon(alpha = 0.2) +
        #labs(title = "Your Title", x = "X-axis Label", y = "Y-axis Label") +
       # scale_colour_manual(labels=plotleglable,name="Population",values=cbPalette)+
       # scale_shape_discrete(labels=plotleglable,name="Population")+
        scale_y_continuous(breaks = scales::breaks_pretty()) +
        theme(panel.background = element_rect(fill = "white", colour = "grey50"),legend.position="bottom", plot.title = element_text(hjust = 0.5))
    
    plot_list[[outcome_var]] <- plot
}       #  geom_style
# geom_vline(xintercept =-1, col = ref.line.par$col, lwd = ref.line.par$lwd, lty = ref.line.par$lty)        geom_ribbon()
################################################################# Other


ggarrange(plotlist = plot_list, ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")
