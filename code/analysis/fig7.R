# Title: Figure 7. Adjusted event study, split sample (US-born and foreign-born)
# Author: Shadi Seyedi
# Description: Event-study models with controls, estimated separately by nativity
#              (split = ~NATIVITY), for four outcomes.
#              Copied from the commented-out block in codes/R/08-function-event-plot-adj.R
#              lines 190-245 (now in archive/), uncommented.
# Output: output/figures/fig7.pdf

source("code/00_setup.R")
load(data_file)

library(ggplot2)
library(fixest)
library(ggiplot)
library(ggpubr)


Event1= feols( UNINS ~ i(ttot, expansion, ref = -1) +
                SEX+DIS+AGEP+SCHLG+POVPIPG+MARG+RACE1+ESRG+ENG+LTINU+NonCit+ADA+UnempR+IPC |
                ST + YEAR   ,                        ## FEs
              vcov="hetero",
              weights = ~PWGTP,
              data = Data,
              split= ~NATIVITY)

plot1<-ggiplot(Event1,
               ref.line = -1, main = 'Uninsured',xlab='Event Time')+
  scale_color_manual(values=c('black','#B64074'))



Event2= feols( HINS4 ~ i(ttot, expansion, ref = -1) +
                 SEX+DIS+AGEP+SCHLG+POVPIPG+MARG+RACE1+ESRG+ENG+LTINU+NonCit+ADA+UnempR+IPC |
                 ST + YEAR   ,                        ## FEs
               vcov="hetero",
               weights = ~PWGTP,
               data = Data,
               split= ~NATIVITY)

plot2<-ggiplot(Event2, ref.line = -1, main = 'Medicaid',xlab='Event Time')+
  scale_color_manual(values=c('black','#B64074'))


Event3= feols( HINS1 ~ i(ttot, expansion, ref = -1) +
                 SEX+DIS+AGEP+SCHLG+POVPIPG+MARG+RACE1+ESRG+ENG+LTINU+NonCit+ADA+UnempR+IPC |
                 ST + YEAR   ,                        ## FEs
               vcov="hetero",
               weights = ~PWGTP,
               data = Data,
               split= ~NATIVITY)

plot3<-ggiplot(Event3,ref.line = -1, main = 'Employer-Sponsored',xlab='Event Time')+
  scale_color_manual(values=c('black','#B64074'))


Event4= feols( HINS2 ~ i(ttot, expansion, ref = -1) +
                 SEX+DIS+AGEP+SCHLG+POVPIPG+MARG+RACE1+ESRG+ENG+LTINU+NonCit+ADA+UnempR+IPC |
                 ST + YEAR   ,                        ## FEs
               vcov="hetero",
               weights = ~PWGTP,
               data = Data,
               split= ~NATIVITY)

plot4<-ggiplot(Event4,ref.line = -1, main = 'Directly Purchased',xlab='Event Time')+
  scale_color_manual(values=c('black','#B64074'))


fig7 <- ggarrange(plot1,plot2,plot3, plot4 ,
          ncol = 2, nrow = 2,common.legend = TRUE, legend = "bottom")
ggsave(file.path(figure_dir, "fig7.pdf"), fig7, width = 10, height = 7)
