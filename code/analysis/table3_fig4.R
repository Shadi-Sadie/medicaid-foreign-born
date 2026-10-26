# Title: Table 3 and Figure 4. Goodman-Bacon decomposition
# Author: Shadi Seyedi
# Date Created: July 17, 2023
# Description: Bacon decomposition of the two-way fixed-effects DiD estimate for uninsured and
#              Medicaid coverage, on state-year means. Copied from codes/R/07-robustness-check.R
#              lines 168-236 (now in archive/).
# Output: output/tables/table3.tex, table3.csv and output/figures/fig4.pdf

source("code/00_setup.R")
load(data_file)

library(ggplot2)
library(kableExtra)
library(bacondecomp)
library(cowplot)


##############################
## Bacon Decomposition theorem
##############################

agrdata  <- aggregate(cbind(UNINS, treat) ~ ST + YEAR, data = Data, FUN = mean)

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

# FIX: the original table typed the coefficients and weights in by hand. They are now computed
# from the decomposition: weighted mean estimate and total weight for each comparison type.
bacon_by_type <- function(df_bacon) {
    agg <- aggregate(cbind(wx = estimate * weight, weight) ~ type, data = df_bacon, FUN = sum)
    data.frame(Comparison = agg$type,
               Coefficient = sprintf("%.5f", agg$wx / agg$weight),
               Weight = sprintf("%.5f", agg$weight))
}

bc <- rbind(bacon_by_type(df_bacon1), bacon_by_type(df_bacon2))
write.csv(cbind(Outcome = rep(c("Uninsured", "Medicaid"), each = 3), bc,
                Overall = rep(c(dd_estimate1, dd_estimate2), each = 3)),
          file.path(table_dir, "table3.csv"), row.names = FALSE)

kbl(bc, format = "latex", caption="Bacon Decomposition" , booktabs=T) %>%
    kable_styling()%>%
    pack_rows("Uninsured",1,3,latex_gap_space="1em")%>% pack_rows("Medicaid",4,6) %>%
    save_kable(file.path(table_dir, "table3.tex"))


### Bacon Plot (Figure 4)

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

ggsave(file.path(figure_dir, "fig4.pdf"), combined_plot_with_legend, width = 10, height = 7)
