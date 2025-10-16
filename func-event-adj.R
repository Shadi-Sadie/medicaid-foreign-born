library(ggplot2)
library(fixest)



################################################################################
#### Fig 1
#### Adjusted Event study plot without spliting
#### (OLS Weighted) for all low-income adult aged 26-65
################################################################################



################## Starting the Function with variable  ###########################

outcome_vars <- c("UNINS", "HINS4", "HINS1", "HINS2")
outcome_labels <- c("UNINS" = "Uninsured", "HINS4" = "Medicaid", "HINS1" = "Employer-Sponsored", "HINS2" = "Directly Purchased")

controlvar<-c("UnempR","SEX","DIS", "AGEP","SCHLG", "MARG", "RACE1", "ESRG","ENG", "LTINU","NonCit", "IPC","POVPIPG","ADA") 
variable_list<- c("ForeginBorn"   , "UnempR" , "SEX"   ,  "DIS"   ,  "AGEP"    ,"SCHLG.L", "SCHLG.Q" ,"SCHLG.C", "SCHLG^4", "MARG",
                  "ESRG.L" , "ESRG.Q",  "ENG.L" ,  "ENG.Q"  , "RACE1.L", "RACE1.Q", "RACE1.C", "RACE1^4",
                  "ENG.C" ,  "ENG^4" ,  "LTINU" , "NonCit", "IPC", "POVPIPG.L", "ForeginBorn:LTINU", "ForeginBorn:RACE1.L", "ForeginBorn:RACE1.Q", 
                  "ForeginBorn:RACE1.C", "ForeginBorn:RACE1^4", "ForeginBorn:SCHLG.L", "ForeginBorn:SCHLG.Q" ,"ForeginBorn:SCHLG.C", "ForeginBorn:SCHLG^4", 'ForeginBorn:MARG',
                  "ForeginBorn:IPC", "ForeginBorn:ESRG.L" , "ForeginBorn:ESRG.Q","ADA","ForeginBorn:ADA","ForeginBorn:UnempR" )
#treatment_var<- c("ForeginBorn")
treatment_var<- c("ForeginBorn")

level<- c("")
# controlvar<-C("UnempR","SEX","DIS", "AGEP","SCHLG", "MARG", "RACE1", "ESRG","ENG", "LTINU")
# plotleglable<- c('US-Born','Foregin-born')
plotleglable<- c('US-Born','Foregin-born')

Dataset<-data
### Changing the graph colors 
cbPalette <- c("#232424", "#CC6677", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# cbPalette <- c("#888888", "#F0E442", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#cbPalette  <- c("#000000","#B64074", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00" )

safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                             "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")


#################################### Regression tables  ########################################


regression_models <- list()
plot_list <- list()

for (outcome_var in outcome_vars) {
    formula <- as.formula(paste(outcome_var, "~",treatment_var," * i(ttot, expansion, ref = -1)+ + SEX + DIS+AGEP+POVPIPG+ENG+LTINU+NonCit+ForeginBorn*(SCHLG+ MARG+ RACE1 + ESRG + ADA+UnempR+IPC)| ST + YEAR")) # if adjusted need to be added #
    #formula <- as.formula(paste(outcome_var, "~",treatment_var," * i(ttot, expansion, ref = -1)+ SEX + DIS+AGEP+POVPIPG+ENG+LTINU+SCHLG+ MARG+ RACE1 + ESRG + ADA+UnempR+IPC| ST + YEAR")) # if adjusted need to be added #
    
    ##### Need to change #########
    Event= feols( formula   ,                        ## FEs
                  vcov="hetero",
                  #  cluster = "ST",
                  weights = ~PWGTP,
                  data = Dataset)
    regression_models[[outcome_var]] <- Event
    
    
    ################ Get info from regression to create basic for est and sd calculation ####################
    
    draft<-coeftable(Event) 
    draft<-as.data.frame(draft)
    
    # existing_rows <- variable_list[variable_list %in% rownames(draft)]
    event_rows <- grepl("ttot::", rownames(draft))
    draft_event <- draft[event_rows, ]
    
    # if (length(existing_rows) > 0) {
    #   dropOutFtab <- which(rownames(draft) %in% existing_rows)
    #  draft <- draft[-dropOutFtab, ]
    # }
    
    #grep("ttot",rownames(draft)))
    
    baseline_rows <- grepl("^ttot::", rownames(draft_event))
    interaction_rows <- grepl(paste0("^", treatment_var), rownames(draft_event))
    # Example: interaction_levels = c("Level2", "Level3")
    interaction_terms <- rownames(draft_event[interaction_rows, ])
    interaction_levels <- unique(sub(":ttot.*", "", interaction_terms))
    

    est_baseline <- draft_event[baseline_rows, 1]
    sd_baseline  <- draft_event[baseline_rows, 2]
    years <- as.numeric(gsub("[^0-9.-]", "", rownames(draft_event[baseline_rows, ])))
    
    # 6️⃣ Prepare plot data for baseline:
    df_plot <- data.frame(estimate = est_baseline,
                          sd = sd_baseline,
                          id = "Baseline",
                          x_year = years)
    
    for (level in interaction_levels) {
        
        # Match rows for this level:
        level_rows <- grepl(paste0("^", level, ":ttot::"), rownames(draft_event))
        
        est_inter <- draft_event[level_rows, 1] + est_baseline
    

        cov_mat <- vcov(Event, "hetero")
        
        # Subset covariance matrix:
        event_names <- rownames(draft_event)
        cov_mat <- cov_mat[event_names, event_names]
        
        idx_baseline <- which(baseline_rows)
        idx_level <- which(level_rows)
        
        se_inter <- sqrt(diag(cov_mat)[idx_baseline] + diag(cov_mat)[idx_level] +
                             2 * diag(cov_mat[idx_baseline, idx_level]))
        
        # Add to plot df:
        df_tmp <- data.frame(estimate = est_inter,
                             sd = se_inter,
                             id = level,
                             x_year = years)
        
        df_plot <- rbind(df_plot, df_tmp)
    }
    
    ############## Calculating the est of interaction #############
    
    df_plot$prms.ci_low <- df_plot$estimate - qnorm(0.975) * df_plot$sd
    df_plot$prms.ci_high <- df_plot$estimate + qnorm(0.975) * df_plot$sd
    
    
    
    #plot$ci_treat_male <- est1 + c(-1,1) * qnorm(0.975) * 0.001735147
    
    
    #############################Prepare the table for ploting ################################## 
    
    # Number of groups:
    n_groups <- length(unique(df_plot$id))
    
    # Create jitter offsets automatically:
    jitter_offsets <- seq(from = -0.15, to = 0.15, length.out = n_groups)
    
    # Assign x:
    df_plot$x_year <- rep(years, times = n_groups) +
        rep(jitter_offsets, each = length(years))
    
    
    ########################## Now plot it ####################################### 
    
    p<-ggplot(df_plot,
              aes(
                  x =x_year, y =estimate,group =as.factor(id),
                  ymin = prms.ci_low , ymax = prms.ci_high ,col=as.factor(id)
              ))+
        labs(title = outcome_labels[outcome_var], x="Time to Treatment", y="Estimate and 95% Conf. Int")+
        geom_hline(yintercept = 0)+
        geom_point(aes(shape=as.factor(id)),size=3) +
        geom_errorbar(width = 0.07) +
        geom_vline(xintercept = -1, lty =  "dashed")+
        # geom_ribbon(alpha = 0.2) +
        #labs(title = "Your Title", x = "X-axis Label", y = "Y-axis Label") +
        scale_colour_manual(labels=plotleglable,name="Population",values=cbPalette)+
        scale_shape_discrete(labels=plotleglable,name="Population")+
        scale_y_continuous(breaks = scales::breaks_pretty()) +
        theme(panel.background = element_rect(fill = "white", colour = "grey50"),legend.position="bottom", plot.title = element_text(hjust = 0.5))
    
    
    plot_list[[outcome_var]] <- p
    
}      #  geom_style
# geom_vline(xintercept =-1, col = ref.line.par$col, lwd = ref.line.par$lwd, lty = ref.line.par$lty)        geom_ribbon()
################################################################# Other



ggarrange(plotlist = plot_list, ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")


