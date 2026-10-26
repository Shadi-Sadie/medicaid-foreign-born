# Title: Figure 5. TWFE event study vs Sun & Abraham (2020), split by nativity
# Author: Shadi Seyedi
# Description: Adjusted event-study (TWFE) and Sun & Abraham estimates for uninsured and Medicaid
#              coverage; panel (a) US-born, panel (b) foreign-born.
#              Copied from codes/R/09-sun&abraham-vs-event-fig3.R lines 73-210 (now in archive/).
# Output: output/figures/fig5.pdf
# Note: uses `controlvar` from code/00_setup.R.

source("code/00_setup.R")
load(data_file)

library(ggplot2)
library(fixest)
library(ggiplot)
library(ggpubr)

# FIX: year_treated was only created in 07-robustness-check.R (on a copy of the data), so this
# script failed when run on its own. Following Sun and Abraham, never-treated states get a fake
# treatment year far outside the study period.
Data$year_treated <- ifelse(Data$expansion == 0, 10000, Data$ExpansionY)

#############################       Model 2                      #############################

Forgn<- Data[Data$NATIVITY == "Foregin-born", ]
NATV<- Data[Data$NATIVITY == "US-born", ]

san = feols(UNINS ~ sunab(year_treated, YEAR) +.[controlvar] |
                ST + YEAR,
            vcov = "hetero" ,
            weights = ~PWGTP,
            data = NATV)


fen= feols(UNINS ~ i(ttot, expansion, ref = -1)+.[controlvar]|
               ST + YEAR,                             ## FEs
           weights = ~PWGTP,
           vcov = "hetero" ,
           data = NATV)



saf = feols(UNINS ~ sunab(year_treated, YEAR) +.[controlvar] |
                ST + YEAR,
            vcov = "hetero" ,
            weights = ~PWGTP,
            data = Forgn)


fef= feols(UNINS ~ i(ttot, expansion, ref = -1)+.[controlvar]  |
               ST + YEAR,                             ## FEs
           weights = ~PWGTP,
           vcov = "hetero" ,
           data = Forgn)



msan = feols(HINS4 ~ sunab(year_treated, YEAR) +.[controlvar] |
                ST + YEAR,
            vcov = "hetero" ,
            weights = ~PWGTP,
            data = NATV)


mfen= feols(HINS4 ~ i(ttot, expansion, ref = -1)+.[controlvar]|
               ST + YEAR,                             ## FEs
           weights = ~PWGTP,
           vcov = "hetero" ,
           data = NATV)



msaf = feols(HINS4 ~ sunab(year_treated, YEAR) +.[controlvar] |
                ST + YEAR,
            vcov = "hetero" ,
            weights = ~PWGTP,
            data = Forgn)


mfef= feols(HINS4 ~ i(ttot, expansion, ref = -1)+.[controlvar]  |
               ST + YEAR,                             ## FEs
           weights = ~PWGTP,
           vcov = "hetero" ,
           data = Forgn)



plot1<-ggiplot(list('TWFE'=fen,'Sun & Abraham (2020)'=san), pt.pch=20, pt.size=2,
               ref.line = -1,
               main = "Uninsured",
               xlab = "Time to treatment",
      theme =
theme(panel.background = element_rect(fill = "white", colour = "grey50"),
                     axis.title= element_text(size=8),
      legend.position=c(.85, .85), legend.box.background = element_rect(colour = "grey",linetype='solid'), legend.title = element_text(colour="white",size=1),legend.text = element_text(size=7))
)+
    scale_color_manual(values=c('black','gray'))


# FIX: panel (b) is labelled "Foreign-Born" but plotted the US-born models (fen/san). It now
# plots the foreign-born models (fef/saf).
plot2<-ggiplot(list('TWFE'=fef,'Sun & Abraham (2020)'=saf), pt.pch=17, pt.size=2,
               ref.line = -1,
               main = "Uninsured",
               xlab = "Time to treatment",
        theme = theme(panel.background = element_rect(fill = "white", colour = "grey50"),
                      axis.title= element_text(size=8),
                     legend.position=c(.85, .85), legend.box.background = element_rect(colour = "grey",linetype='solid'), legend.title = element_text(colour="white",size=1),legend.text = element_text(size=7))

)+
    scale_color_manual(values=c('#B64074','#f7c3da'))



plot3<-ggiplot(list('TWFE'=mfen,'Sun & Abraham (2020)'=msan), pt.pch=20, pt.size=2,
               ref.line = -1,
               main = "Medicaid Coverage",
               xlab = "Time to treatment",
             theme = theme(panel.background = element_rect(fill = "white", colour = "grey50"),
                     axis.title= element_text(size=8) ,
                    legend.position='none' )
        )+
    scale_color_manual(values=c('black','gray'))


# FIX: as for plot2, panel (b) now plots the foreign-born models (mfef/msaf), not mfen/msan.
plot4<-ggiplot(list('TWFE'=mfef,'Sun & Abraham (2020)'=msaf), pt.pch=17, pt.size=2,
               ref.line = -1,
               main = "Medicaid Coverage",
               xlab = "Time to treatment",
        theme = theme(panel.background = element_rect(fill = "white", colour = "grey50"),
                     axis.title= element_text(size=8),
                     legend.position='none' )
        )+
    scale_color_manual(values=c('#B64074','#f7c3da'))




ar1<-ggarrange(plot1, plot3 ,
          ncol = 1, nrow = 2 )
ar1<-annotate_figure(ar1,
                top = text_grob("(a) US-Born", face = "bold", size = 14)
)


ar2<-ggarrange(plot2, plot4,
          ncol = 1, nrow = 2)
ar2<-annotate_figure(ar2,
                top = text_grob("(b) Foreign-Born", face = "bold", size = 14)
)



fig<-ggarrange(ar1,ar2,common.legend = TRUE)
fig<-annotate_figure(fig,
               top = text_grob("Staggered treatment: Split mutli-sample", face = "bold", size = 14)
)

ggsave(file.path(figure_dir, "fig5.pdf"), fig, width = 10, height = 7)
