df <- subset(Forgn, select=c(GEOR,CIT,expansion, YEAR, HINS4))


df$CIT<-as.numeric(df$CIT)
df$CIT<- ordered(df$CIT, labels = c("Naturalized-citizen", "Immigrant"))


# Calculate the mean insurance rate by ACA status, citizenship status, and expansion status
insurance_mean <- aggregate(HINS4 ~ CIT + GEOR + expansion, data=df, FUN=mean)


# Create the bar graph
plot10<-ggplot(data=insurance_mean, aes(x=GEOR, y=HINS4, fill=CIT)) +
        geom_bar(stat="identity", position=position_dodge()) +
        #facet_wrap( ~ expansion) +
        scale_y_continuous(limits=c(0,1), expand=c(0,0), breaks=seq(0,1,0.1))+
        scale_fill_brewer(palette="Paired")+ # specify the colors you want
        labs(x="Birthplace", y="Mean Medicaid Take-up Rate", fill = "Immigration Status",title="Medicaid Take-up Rate by Birthplace, Immigration Status for forign-born people") +
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

print(plot10)
