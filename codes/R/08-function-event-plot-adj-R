library(ggplot2)
library(fixest)

###########
###########
###########
etable(EventsUNAds[[1]],EventAdj1[[1]],EventAdj2[[1]],regression_models[[1]])
############## Regressions ####################
EventAdj2<-regression_models
#+ADA+ IPC+ UnempR+.[controlwof] 
###############COntrol variables #####################
forcont       <- c("ENG", "LTINU") # ENG+LTINU+CORIGIN+NonCit  "NonCit"
controlwof    <- c("SEX","DIS", "AGEP","SCHLG", "MARG", "RACE1", "ESRG") # (SEX+DIS+AGEP+SCHLG+POVPIPG+MARG+RACE1+ESRG+ADA) "POVPIPG"
################## Starting the Function ###########################
outcome_vars <- c("UNINS", "HINS4", "HINS1", "HINS2")
regression_models <- list()
plot_list <- list()
variable_labels <- c("UNINS" = "Uninsured", "HINS4" = "Medicaid", "HINS1" = "Employer-Sponsored", "HINS2" = "Directly Purchased")

for (outcome_var in outcome_vars) {
  formula <- as.formula(paste(outcome_var, "~ NonCit * i(ttot, expansion, ref = -1) + UnempR+.[controlwof] + .[forcont]  | ST + YEAR"))
    
##### Need to change #########
Event= feols( formula   ,                        ## FEs
              vcov="hetero",
              #  cluster = "ST",
              weights = ~PWGTP,
              data = Forgn)
regression_models[[outcome_var]] <- Event

############## Get info from regression to create basic for est and sd calculation ####################

draft<-coeftable(Event) #### Where need changes ####
draft<-as.data.frame(draft)

variable_list <- c("NonCit","ADA", "IPC", "UnempR", "SEX", "DIS", "AGEP", "SCHLG.L", "SCHLG.Q", "SCHLG.C",
                   "SCHLG^4", "POVPIPG.L", "MARG", "RACE1.L", "RACE1.Q", "RACE1.C", "RACE1^4", 
                   "ESRG.L", "ESRG.Q", "ENG.L", "ENG.Q", "ENG.C", "ENG^4", "LTINU", 
                   "CORIGINEastern Europe", "CORIGINLatin America", "CORIGINMiddle East",
                   "CORIGINNorthern Americ", "CORIGINOceania and at Sea", "CORIGINSouth  & Centeral Asia",
                   "CORIGINSouth eaastern Asia", "CORIGINSub-Saharan Africa", "CORIGINWestern Europe", "NonCit")

existing_rows <- variable_list[variable_list %in% rownames(draft)]

if (length(existing_rows) > 0) {
    dropOutFtab <- which(rownames(draft) %in% existing_rows)
    draft <- draft[-dropOutFtab, ]
}

#grep("ttot",rownames(draft)))
checkmark<-any(grepl("UNDOC",rownames(draft)))    #### Where need changes ####

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

if (length(existing_rows) > 0) {
    dropOutFtab <- which(rownames(cov) %in% existing_rows)
    cov<-cov[-dropOutFtab,-dropOutFtab]
}

        #se_treat_male <- sqrt(cov[1, 1] + cov[2, 2] + 2 * cov[1, 2]) ->
if (checkmark) { 
i <- 1    
j <- nrow(cov)/2
se_list <-list()  # numeric(nrow(cov)/2) ## initiating a vector for calculating SE for the desired plot
while (i <= j) {
#for (i in 1:row(cov)/2) {
        se_list[[i]] <- sqrt(cov[i, i] + cov[i+j, i+j] + 2 * cov[i, i+j])
        i <- i + 1
}

cosd<-data.frame(se = unlist(se_list))

table(Data$UNINS)


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



#plot$ci_treat_male <- est1 + c(-1,1) * qnorm(0.975) * 0.001735147


#############################Prepare the table for ploting ################################## 

plot$label<-plot$x_year
nrplot<-nrow(plot)

plot$x_year<-ifelse(1:nrplot < nrplot/2 , plot$x_year-0.1, plot$x_year+0.1)




########################## Now plot it ####################################### 
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# cbPalette <- c("#44AA99", "#999933", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

 #cbPalette  <- c("#000000","#B64074", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00" )

safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                            "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")

plot<-ggplot(plot,
    aes(
        x =x_year, y =estimate,group =as.factor(id),
        ymin = prms.ci_low , ymax = prms.ci_high ,col=as.factor(id)
        ))+
        labs(title = variable_labels[outcome_var], x="Time to Treatment", y="Estimate and 95% Conf. Int")+
        geom_hline(yintercept = 0)+
        geom_point(aes(shape=as.factor(id)),size=3) +
        geom_errorbar(width = 0.07) +
        geom_vline(xintercept = -1, lty =  "dashed")+
       # geom_ribbon(alpha = 0.2) +
        #labs(title = "Your Title", x = "X-axis Label", y = "Y-axis Label") +
        scale_colour_manual(labels=c('Legal','NonCit'),name="Population",values=cbPalette)+
        scale_shape_discrete(labels=c('Legal','NonCit'),name="Population")+
        scale_y_continuous(breaks = scales::breaks_pretty()) +
        theme(panel.background = element_rect(fill = "white", colour = "grey50"),legend.position="bottom", plot.title = element_text(hjust = 0.5))

plot_list[[outcome_var]] <- plot
}       #  geom_style
        # geom_vline(xintercept =-1, col = ref.line.par$col, lwd = ref.line.par$lwd, lty = ref.line.par$lty)        geom_ribbon()
################################################################# Other


ggarrange(plotlist = plot_list, ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")


