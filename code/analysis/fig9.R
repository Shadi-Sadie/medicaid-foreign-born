# Title: Figure 9. Unadjusted event study, split sample (US-born and foreign-born)
# Author: Shadi Seyedi
# Description: Event-study models without controls, estimated separately by nativity
#              (split = ~NATIVITY), for four outcomes.
# Output: output/figures/fig9.pdf
#
# RECONSTRUCTED: no code for Figure 9 existed in the project. This is the Figure 7 code
# (fig7.R) with the control variables removed, which reproduces the paper's Figure 9.

source("code/00_setup.R")
load(data_file)

library(ggplot2)
library(fixest)
library(ggiplot)
library(ggpubr)


Event1= feols( UNINS ~ i(ttot, expansion, ref = -1) |
                ST + YEAR   ,                        ## FEs
              vcov="hetero",
              weights = ~PWGTP,
              data = Data,
              split= ~NATIVITY)

plot1<-ggiplot(Event1,
               ref.line = -1, main = 'Uninsured',xlab='Event Time')+
  scale_color_manual(values=c('black','#B64074'))



Event2= feols( HINS4 ~ i(ttot, expansion, ref = -1) |
                 ST + YEAR   ,                        ## FEs
               vcov="hetero",
               weights = ~PWGTP,
               data = Data,
               split= ~NATIVITY)

plot2<-ggiplot(Event2, ref.line = -1, main = 'Medicaid',xlab='Event Time')+
  scale_color_manual(values=c('black','#B64074'))


Event3= feols( HINS1 ~ i(ttot, expansion, ref = -1) |
                 ST + YEAR   ,                        ## FEs
               vcov="hetero",
               weights = ~PWGTP,
               data = Data,
               split= ~NATIVITY)

plot3<-ggiplot(Event3,ref.line = -1, main = 'Employer-Sponsored',xlab='Event Time')+
  scale_color_manual(values=c('black','#B64074'))


Event4= feols( HINS2 ~ i(ttot, expansion, ref = -1) |
                 ST + YEAR   ,                        ## FEs
               vcov="hetero",
               weights = ~PWGTP,
               data = Data,
               split= ~NATIVITY)

plot4<-ggiplot(Event4,ref.line = -1, main = 'Directly Purchased',xlab='Event Time')+
  scale_color_manual(values=c('black','#B64074'))


fig9 <- ggarrange(plot1,plot2,plot3, plot4 ,
          ncol = 2, nrow = 2,common.legend = TRUE, legend = "bottom")
ggsave(file.path(figure_dir, "fig9.pdf"), fig9, width = 10, height = 7)
