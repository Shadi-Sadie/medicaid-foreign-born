# Title: Robustness Check
# Author: Shadi Seyedi
# Date Created: July 17, 2023
# Description: The purpose of this script is to check the robustness of the result from main analysis
# Data last edit: Aug 7, 2023



####################
## Required Packages
####################

library(fixest)
library(ggpubr)
library(ggiplot)
library('kableExtra')
library(bacondecomp)


####################
## Data Preparation 
####################

#Becausae expansion was a factor variable I had problme with the feols , so I changed it back to dummy

Data$expansion<-as.numeric(Data$expansion)
Data$expansion<-ifelse(Data$expansion==2,0,Data$expansion)

# need to get a new sample for foregin and native
# Forgn<- Data[Data$NATIVITY == "Foregin-born", ]
Forgn<- Data[Data$NATIVITY == "Foregin-born", ]
NATV<- Data[Data$NATIVITY == "US-born", ]


df<-Data
# Following Sun and Abraham, we give our never-treated units a fake "treatment"
# date far outside the relevant study period.

df$year_treated <- ifelse(df$expansion == 0, 10000, df$ExpansionY)

dfF<- df[df$NATIVITY == "Foregin-born", ]
dfN<- df[df$NATIVITY == "US-born", ]



############################
##  Sun & Abraham 
############################

#plot 1 unin native
san = feols(UNINS ~ sunab(year_treated, YEAR) +.[controls] |
                ST + YEAR,
            vcov = "hetero" ,
            weights = ~PWGTP,
            data = dfN)


fen= feols(UNINS ~ i(ttot, expansion, ref = -1)+.[controls]|                 
               ST + YEAR,                             ## FEs
           weights = ~PWGTP,
           vcov = "hetero" ,
           data = NATV)
Plota<-ggiplot(
    list("Event-study" = fen, "Sun & Abraham (2020)" = san),
    main = 'Adjusted. US-born Uninsured',
    xlab = 'Time to treatment',
    ref.line = -1,
    theme  = theme_minimal() + theme(legend.position = "none",
                                     plot.title = element_text(hjust = 0.5, size=11))
)

#plot 1 unin foreg

saf = feols(UNINS ~ sunab(year_treated, YEAR) +.[controls] |
                ST + YEAR,
            vcov = "hetero" ,
            weights = ~PWGTP,
            data = dfF)


fef= feols(UNINS ~ i(ttot, expansion, ref = -1)+.[controls]|                 
               ST + YEAR,                             ## FEs
           weights = ~PWGTP,
           vcov = "hetero" ,
           data = Forgn)

Plotb<-ggiplot(
    list("Event-study" = fef, "Sun & Abraham (2020)" = saf),
    main = 'Adjusted. Foreign-born Uninsured',
    xlab = 'Time to treatment',
    ref.line = -1,
    theme  = theme_minimal() + theme(legend.position = "none",
                                     plot.title = element_text(hjust = 0.5, size=11))
)

#plot 3 uni med nat


msan = feols(HINS4 ~ sunab(year_treated, YEAR) +.[controls] |
                 ST + YEAR,
             vcov = "hetero" ,
             weights = ~PWGTP,
             data = dfN)


mfen= feols(HINS4 ~ i(ttot, expansion, ref = -1)+.[controls]|                 
                ST + YEAR,                             ## FEs
            weights = ~PWGTP,
            vcov = "hetero" ,
            data = NATV)



Plotc<-ggiplot(
    list("Event-study" = mfen, "Sun & Abraham (2020)" = msan),
    main = 'Adjusted. US-born Medicaid Coverage',
    xlab = 'Time to treatment',
    ref.line = -1,
    theme  = theme_minimal() + theme(legend.position = "none",
                                     plot.title = element_text(hjust = 0.5, size=11))
)



#plot 4 uni med for



msaf = feols(HINS4 ~ sunab(year_treated, YEAR) +.[controls] +.[foriegn] |
                 ST + YEAR,
             vcov = "hetero" ,
             weights = ~PWGTP,
             data = dfF)


mfef= feols(HINS4 ~ i(ttot, expansion, ref = -1)+.[controls]+.[foriegn]|                 
                ST + YEAR,                             ## FEs
            weights = ~PWGTP,
            vcov = "hetero" ,
            data = Forgn)

Plotd<-ggiplot(
    list("Event-study" = mfef, "Sun & Abraham (2020)" = msaf),
    main = 'Adjusted. Foreign-born Medicaid Coverage',
    ref.line = -1,
    xlab = "Time to treatment",
    theme  = theme_minimal() + theme(legend.position = "none",
                                     plot.title = element_text(hjust = 0.5, size=11))
)



figure3<-ggarrange(Plota, Plotb ,Plotc,Plotd ,
                   ncol = 2, nrow = 2,
                   common.legend = T,
                   legend=   "bottom",
                   font.label= 'plain'
                   
)

annotate_figure(figure3,
                top = text_grob("Medicaid Expansions Effect Accounting for Variation in Treatment Timing", face = "bold", size = 10),
                
                fig.lab = "Figure 3"
)


##############################
## Bacon Decomposition theorem
##############################


agrdata  <- aggregate(cbind(UNINS, treat) ~ ST + YEAR, data = Data, FUN = mean)

#aggregated_data <- svyby(~ UNINS + treat, ~ ST + YEAR, design = svy,FUN = svymean)

df_bacon1 <- bacon(UNINS ~ treat,
                   data = agrdata,
                   id_var = "ST",
                   time_var = "YEAR")

dd_estimate1 <-  sum(df_bacon1$estimate*df_bacon1$weight)



agrdata  <- aggregate(cbind(HINS4, treat) ~ ST + YEAR, data = Data, FUN = mean)

df_bacon2 <- bacon(HINS4 ~ treat,
                   data = agrdata,
                   id_var = "ST",
                   time_var = "YEAR")

dd_estimate2 <- sum(df_bacon2$estimate*df_bacon2$weight)


## Bacon table 

Comparison <- c("Earlier vs Later Treated", "Later vs Earlier Treated", "Treated vs Untreated","Earlier vs Later Treated", "Later vs Earlier Treated", "Treated vs Untreated" )
Coefficient <- c('-0.09309','-0.05521' , "-0.08462", "0.14610", "0.08782", "0.14452")
Weight <- c("0.11871","0.08771", "0.79358","0.11871","0.08771","0.79358")
bc <- data.frame(Comparison, Coefficient, Weight)

kbl(bc,caption="Bacon Decomposition" , booktabs=T) %>%
    kable_styling()%>%
    pack_rows("Uninsured",1,3,latex_gap_space="1em")%>% pack_rows("Medicaid",4,6)

### Bacon Plot


library(cowplot)

dfplot1<- ggplot(df_bacon1) +
    aes(x = weight, y = estimate, shape = factor(type)) +
    labs(x = "Weight", y = "Estimate", shape = "Type") +
    geom_hline(yintercept = dd_estimate1, color = "#A94064") +
    theme_bw()+
    ggtitle("Uninsured")+
    geom_point(show.legend = FALSE)

dfplot2<- ggplot(df_bacon2) +
    aes(x = weight, y = estimate, shape = factor(type)) +
    labs(x = "Weight", y = "Estimate", shape = "Type") +
    geom_hline(yintercept = dd_estimate2, color = "#A94064") +
    theme_bw()+
    theme(legend.position ="bottom")+
    ggtitle("Medicaid Coverage")+
    geom_point()

# Combine the plots vertically
combined_plot <- plot_grid(dfplot1, dfplot2, ncol = 1, align = "v")

# Add a common legend
combined_plot_with_legend <- cowplot::plot_grid(combined_plot, get_legend(dfplot1), ncol = 2, rel_widths = c(0.9, 0.1))

# Display the combined plot with the common legend
combined_plot_with_legend