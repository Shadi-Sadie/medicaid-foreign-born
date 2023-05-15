df <- subset(Data, select=c(RACE,ETHN ,NATIVITY,expansion, YEAR, HINS4))


## Creating a new variable for race/ethniciy and rename it

df$RACE<-as.numeric(df$RACE)
df$RACE<-ifelse(df$RACE==1,4,df$RACE) # merge native americann to other
df$RACE<-ifelse(df$ETHN==1,1,df$RACE) # merge native americann to other

df$RACE<- ordered(df$RACE, labels = c("Hispanic", "Asian","Black", "Other", "White"))

var_label(df$RACE) <- "Race"

table(df$NATIVITY)
df$NATIVITY<- ordered(df$NATIVITY, labels = c("Native", "Foreign-born"))
var_label(df$NATIVITY) <- "Nativity"

# Calculate the mean insurance rate by ACA status, citizenship status, and expansion status
insurance_mean <- aggregate(HINS4 ~ NATIVITY + RACE + expansion, data=df, FUN=mean)


# Create the bar graph
plot6<-ggplot(data=insurance_mean, aes(x=RACE, y=HINS4, fill=NATIVITY)) +
        geom_bar(stat="identity", position=position_dodge()) +
        facet_grid( ~ expansion) +
        scale_y_continuous(limits=c(0,1), expand=c(0,0), breaks=seq(0,1,0.1))+
        scale_fill_brewer(palette="Pastel1")+ # specify the colors you want
        labs(x="Race", y="Insurance Rate", title="Insurance Rates by Race and Nativity Status and Expansion Status") +
        #theme_classic()
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(color = 'black'),
              axis.text = element_text(size=12),
              axis.title = element_text(size=14))
print(plot6)