
########################################
########### Sun & Abraham and TWFE adjusted #################
#############################

#############################       Model 1                         #############################


## Uninsured Sun & Abraham and TWFE adjusted

evntaladj= feols(UNINS ~ i(ttot, expansion, ref = -1)+ .[controlvar]  |                 
                  ST + YEAR,                             ## FEs
              weights = ~PWGTP,
              vcov = "hetero" ,
              data = Data,
              split= ~NATIVITY)
sanadj = feols(UNINS ~ sunab(year_treated, YEAR) +.[controlvar] |
                ST + YEAR,
            vcov = "hetero" ,
            weights = ~PWGTP,
            data = Data,
            split= ~NATIVITY)

plot1<-ggiplot(list('TWFE' = evntaladj, 'Sun & Abraham (2020)' = sanadj),
    ref.line = -1,
    main = "Staggered treatment: Split mutli-sample",
    xlab = "Time to treatment",
    multi_style = "facet",
    geom_style = "ribbon",
    facet_args = list(labeller = labeller(id = \(x) gsub(".*: ", "", x))),
    theme = theme(panel.background = element_rect(fill = "white", colour = "grey50"),legend.position="bottom", plot.title = element_text(hjust = 0.5))
)+
    scale_fill_brewer(palette = 'Pastel1', aesthetics = c('colour', 'fill'))
  

## Medicaid Sun & Abraham and TWFE adjusted

evntaladj= feols(HINS4 ~ i(ttot, expansion, ref = -1)+ .[controlvar]  |                 
                     ST + YEAR,                             ## FEs
                 weights = ~PWGTP,
                 vcov = "hetero" ,
                 data = Data,
                 split= ~NATIVITY)

sanadj = feols(HINS4 ~ sunab(year_treated, YEAR) +.[controlvar] |
                   ST + YEAR,
               vcov = "hetero" ,
               weights = ~PWGTP,
               data = Data,
               split= ~NATIVITY)

plot2<-ggiplot(list('TWFE' = evntaladj, 'Sun & Abraham (2020)' = sanadj), # Plot for medicaid
        ref.line = -1,
        main = "Staggered treatment: Split mutli-sample",
        xlab = "Time to treatment",
        multi_style = "facet",
        geom_style = "ribbon",
        facet_args = list(labeller = labeller(id = \(x) gsub(".*: ", "", x))),
        theme = theme(panel.background = element_rect(fill = "white", colour = "grey50"),legend.position="bottom", plot.title = element_text(hjust = 0.5))
)+
    scale_fill_brewer(palette = 'Pastel1', aesthetics = c('colour', 'fill'))


# Mix both plot

figureA1<-ggarrange(plot11, plot2 ,
                    ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom")





#############################       Model 2                      #############################
#############################       
     
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



plot2<-ggiplot(list('TWFE'=fen,'Sun & Abraham (2020)'=san), pt.pch=17, pt.size=2,
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
                    #legend.box.background = element_rect(colour = "grey",linetype='solid'), legend.title = element_text(colour="white",size=1))
        )+ 
    scale_color_manual(values=c('black','gray'))



plot4<-ggiplot(list('TWFE'=mfen,'Sun & Abraham (2020)'=msan), pt.pch=17, pt.size=2,
               ref.line = -1,
               main = "Medicaid Coverage",
               xlab = "Time to treatment",
        theme = theme(panel.background = element_rect(fill = "white", colour = "grey50"),
                     axis.title= element_text(size=8),
                     legend.position='none' )
                     #legend.position=c(.88, .88), legend.box.background = element_rect(colour = "grey",linetype='solid'), legend.title = element_text(colour="white",size=1))
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
