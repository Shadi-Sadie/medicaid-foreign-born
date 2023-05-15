# Code for creating plot 9  self purchased insurance rate by race and nativity

## Insurance thriygh employer
Forgn<- Data[Data$NATIVITY == 2, ]

df <- subset(Forgn, select=c(GEOR,CIT,expansion, YEAR, HICOV))


df$CIT<-as.numeric(df$CIT)
df$CIT<- ordered(df$CIT, labels = c("Naturalized-citizen", "Immigrant"))


# Calculate the mean insurance rate by ACA status, citizenship status, and expansion status
insurance_mean <- aggregate(HICOV ~ CIT + GEOR + expansion, data=df, FUN=mean)


# Create the bar graph
plot9_a<-ggplot(data=insurance_mean, aes(x=GEOR, y=HICOV, fill=CIT)) +
        geom_bar(stat="identity", position=position_dodge()) +
        facet_wrap( ~ expansion) +
        scale_y_continuous(limits=c(0,1), expand=c(0,0), breaks=seq(0,1,0.1))+
        scale_fill_brewer(palette="Paired")+ # specify the colors you want
        labs(x="Birthplace", y="Mean Insurance Rate", fill = "Immigration Status",title="Insurance Rates by Birthplace, Immigration Status for forign-born people") +
        #theme_classic()
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(color = 'black'),
              axis.text = element_text(size=12),
              axis.title = element_text(size=14),
              axis.text.x = element_text(angle = 45, hjust = 1))
             
        # rotate and align x-axis labels

print(plot9_a)



# Create grouped bar chart

p1<-ggplot(insurance_mean,                         # Draw barplot with grouping & stacking
       aes(x = expansion,
           y = HICOV,
           fill = CIT)) + 
        geom_bar(stat = "identity",
                 position = "stack") +
        facet_wrap(~ GEOR, strip.position = "bottom")+
scale_fill_brewer(palette = "Set2") +
        labs(x = "Expansion Status",
             y = "Insurance Rate",
             fill = "Immigration Status",
             title = "Insurance Rates by Expansion and Immigration Status") +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(color = 'black'),
              axis.text = element_text(size=12),
              axis.title = element_text(size=14),
              axis.text.x = element_blank(),
                strip.placement = "outside")
             # axis.text.x = element_text(angle = 45, hjust = 1))
print(p1)


#

p1<-ggplot(data=subset(insurance_mean, expansion == "Adopted"), aes(x=GEOR, y=HICOV, fill=CIT)) +
        geom_bar(stat="identity", position=position_dodge()) +
        scale_y_continuous(limits=c(0,1), expand=c(0,0), breaks=seq(0,1,0.1)) +
        scale_fill_brewer(palette="Paired") + # specify the colors you want
        labs(x="Birthplace", y="Mean Insurance Rate", fill = "Immigration Status",title="Insurance Rates by Birthplace, Immigration Status for forign-born people (Expanded)") +
        theme_classic()

p2<-ggplot(data=subset(insurance_mean, expansion == "Not Adopted"), aes(x=GEOR, y=HICOV, fill=CIT)) +
        geom_bar(stat="identity", position=position_dodge()) +
        scale_y_continuous(limits=c(0,1), expand=c(0,0), breaks=seq(0,1,0.1)) +
        scale_fill_brewer(palette="Paired") + # specify the colors you want
        labs(x="Birthplace", y="Mean Insurance Rate", fill = "Immigration Status",title="Insurance Rates by Birthplace, Immigration Status for forign-born people (Expanded)") +
        theme_classic()
