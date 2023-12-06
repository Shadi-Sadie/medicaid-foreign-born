# creating Undocumented status following Borjas
#
# Generate undocumented immigrant variable
Data$UNDOC <- 1

# Set UNDOC to 0 based on various conditions:

    # The individual is a citizen.
Data$UNDOC[Data$CIT %in% c("Born in US states", "Born in US Territories", "US-citizen Born Abroad", "Naturalized-citizen")] <- 0
    # The individual arrived before 1980.
Data$UNDOC[Data$YOEP < 1980] <- 0
    # The individual was born in Cuba (as practically all Cuban immigrants were granted refugee status before 2017) 
Data$UNDOC[Data$POBP == 327] <- 0
    # The individual is a veteran or is currently in the Armed Forces.
Data$UNDOC[Data$ESR %in% c(4,5) ] <- 0
Data$UNDOC[Data$MIL %in% c(1,2,3) ] <- 0
    #e. The individual works in the government sector.
Data$UNDOC[Data$COW %in% c(3,4,5) ] <- 0
    # The individual's occupation requires some form of licensing based on borjas (such as physicians, registered nurses, air traffic controllers, and lawyers)
Data$UNDOC[Data$OCCP %in% c (120, 230, 350, 410, 800, 810, 820, 830, 845, 850, 860, 900, 910,
                             930, 940, 2100, 2105, 2145, 2170, 2180, 2205, 2300,
                             2310, 2320, 2330, 2350, 2360, 2400, 2435, 2440, 2545, 2555, 
                             3000, 3010, 3030, 3040, 3050, 3090, 3100, 3110, 3120, 3140,
                             3150, 3160, 3200, 3210, 3220, 3230, 3245, 3250, 3255, 3256, 
                             3258, 3261, 3270, 3300, 3310, 3321, 3322, 3323, 3324, 3330,
                             3401, 3402, 3421, 3422, 3423, 3424, 3430, 3500, 3515, 3520,
                             3545, 3550, 3700, 3710, 3720, 3725, 3740, 3750, 3801, 3802, 
                             3820, 3870, 3910, 3945, 3946, 3946, 6010, 6660, 9030, 9040, 9050, 
                             9121, 9122, 9130, 9141, 9142, 9150, 9210, 9240, 9265, 9300, 9310)] <- 0

 # if spouse is citize
 # 
 


Data$good <- ifelse(Data$MAR == 1 & Data$RELP %in% c(21, 23, 1) & Data$CIT %in% c("Born in US states", "Born in US Territories", "US-citizen Born Abroad", "Naturalized-citizen"), 1,
                    ifelse(Data$MAR == 1 & Data$RELP %in% c(21, 23, 1) & Data$CIT == "Non-citizen", 0,
                           ifelse(Data$MAR == 1 & Data$RELP %in% c(20, 0) & Data$CIT %in% c("Born in US states", "Born in US Territories", "US-citizen Born Abroad", "Naturalized-citizen"), 1,
                                  ifelse(Data$MAR == 1 & Data$RELP %in% c(20, 0) & Data$CIT == "Non-citizen", 0, NaN)
                           )
                    )
)

table(data$slegal,exclude = NA)
table(Data$slegal,exclude = NA)


Data$slegal <- ave(Data$good , Data$YEAR, Data$SERIALNO, FUN = mean)

data$UNDOC[data$slegal > 0 ] <- 0




table(data$CIT,data$ESR)
table(Data$CIT, Data$slegal)

b<-data[is.na(data$good)&data$MAR==1,]
