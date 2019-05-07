## create time series of low, mid, high medians

library(dygraphs)
library(robustbase)

## collect District names and accountability grades from master_table
ms_account_years <- master_table[which(c(master_table$`2016`)!="NA"),match(c("District","2014","2015","2016","2017","2018"),colnames(master_table))]

## convert to numeric for comparing
ms_account_years$`2014` <- as.numeric_version(gsub("E", 488, ms_account_years$`2014`))
ms_account_years$`2015` <- as.numeric_version(gsub("E", 488, ms_account_years$`2015`))
ms_account_years$`2016` <- as.numeric_version(gsub("E", 488, ms_account_years$`2016`))
ms_account_years$`2017` <- as.numeric_version(gsub("E", 488, ms_account_years$`2017`))
ms_account_years$`2018` <- as.numeric_version(gsub("E", 488, ms_account_years$`2018`))

## break up into low, mid, high from 2014

## low performing schools 2014
ms_account_low2014 <- ms_account_years[which(ms_account_years$`2014` <= 535),]

## high performing schools 2014
ms_account_high2014 <- ms_account_years[which(ms_account_years$`2014` >= 599),]

## mid level schools 2014
ms_account_mid2014 <- ms_account_years[which((ms_account_years$`2014` > 535) & (ms_account_years$`2014` < 599)),]

## get medians

## median from low
ms_account_low2014_median <- c(median(ms_account_low2014$`2014`),median(ms_account_low2014$`2015`),median(ms_account_low2014$`2016`),median(ms_account_low2014$`2017`),median(ms_account_low2014$`2018`))

## median from mid
ms_account_mid2014_median <- c(median(ms_account_mid2014$`2014`),median(ms_account_mid2014$`2015`),median(ms_account_mid2014$`2016`),median(ms_account_mid2014$`2017`),median(ms_account_mid2014$`2018`))

## median from high
ms_account_high2014_median <- c(median(ms_account_high2014$`2014`),median(ms_account_high2014$`2015`),median(ms_account_high2014$`2016`),median(ms_account_high2014$`2017`),median(ms_account_high2014$`2018`))

## medians table
ms_account_medians <- rbind.data.frame(ms_account_low2014_median,ms_account_mid2014_median,ms_account_high2014_median)

colnames(ms_account_medians) <- c("2014-10-1","2015-10-1","2016-10-1","2017-10-1","2018-10-1")
rownames(ms_account_medians) <- c("low","mid","high")

## transpose
ms_account_medians_t <- data.frame(t(ms_account_medians))

## convert to time series object
ms_account_medians_ts <- as.ts(ms_account_medians_t)

## set tsp for objects
tsp(ms_account_medians_ts) <- c(2014,2018,1)

## plot time series
tsplot_ms_account_median <- dygraph(ms_account_medians_ts, main = "District Performance 2014-2018",
                                    ylab = "Accountability Score", xlab = "Date") %>%
  dyLimit(441, label = "'F' District", labelLoc = "right",
          color = "black", strokePattern = "solid") %>%
  
  dyLimit(489, label = "'D' District", labelLoc = "right",
          color = "black", strokePattern = "solid") %>%

  dyLimit(536, label = "'C' District", labelLoc = "right",
        color = "black", strokePattern = "solid") %>%

  dyLimit(599, label = "'B' District", labelLoc = "right",
        color = "black", strokePattern = "solid") %>%
  
  dyLimit(668, label = "'A' District", labelLoc = "right",
          color = "black", strokePattern = "solid") %>%
  
  dyEvent("2015-1-01", "ESSA", labelLoc = "bottom", strokePattern = "solid", color = "blue") %>%

  dySeries("low", label = "Low Perform", color = "red", strokePattern = "dotted", strokeBorderWidth = 1, strokeWidth = 3) %>%
  
  dySeries("mid", label = "Mid Perform", color = "darkorange", strokePattern = "dashed", strokeBorderWidth = 1, strokeWidth = 3) %>%
  
  dySeries("high", label = "High Perform", color = "green", strokePattern = "dotdash", strokeBorderWidth = 1, strokeWidth = 3) %>%
  
  dyAxis("y", valueRange = c(440, 700)) %>%
  
  dyLegend(width = 400)

tsplot_ms_account_median


## break up low by race
ms_account_race <- master_table[which(c(master_table$`2016`)!="NA"),match(c("District","Percent Black","2014","2015","2016","2017","2018"),colnames(master_table))]

## convert to numeric for comparing
ms_account_race$`2014` <- as.numeric_version(gsub("E", 488, ms_account_race$`2014`))
ms_account_race$`2015` <- as.numeric_version(gsub("E", 488, ms_account_race$`2015`))
ms_account_race$`2016` <- as.numeric_version(gsub("E", 488, ms_account_race$`2016`))
ms_account_race$`2017` <- as.numeric_version(gsub("E", 488, ms_account_race$`2017`))
ms_account_race$`2018` <- as.numeric_version(gsub("E", 488, ms_account_race$`2018`))

ms_account_race$race <- ms_account_race$`Percent Black` > .5

ms_account_race$race <- gsub(TRUE, "majority Black", ms_account_race$race)
ms_account_race$race <- gsub(FALSE,"majority White", ms_account_race$race)

## create low2014
ms_account_race_low2014 <- ms_account_race[which(ms_account_race$`2014` <= 535),]

## create low2014 majority black
ms_account_race_low2014_black <- ms_account_race_low2014[which(ms_account_race_low2014$race=="majority Black"),c(3:7)]

## create low2014 majority white
ms_account_race_low2014_white <- ms_account_race_low2014[which(ms_account_race_low2014$race=="majority White"),c(3:7)]

## black to numeric
ms_account_race_low2014_black[] <- lapply(ms_account_race_low2014_black, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})

## white to numeric
ms_account_race_low2014_white[] <- lapply(ms_account_race_low2014_white, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})

## create low2014 black and white medians
ms_account_race_low2014_black_median <- c(median(as.numeric(ms_account_race_low2014_black$`2014`)),median(as.numeric(ms_account_race_low2014_black$`2015`)),median(as.numeric(ms_account_race_low2014_black$`2016`)),median(as.numeric(ms_account_race_low2014_black$`2017`)),median(as.numeric(ms_account_race_low2014_black$`2018`)))
ms_account_race_low2014_white_median <- c(median(as.numeric(ms_account_race_low2014_white$`2014`)),median(as.numeric(ms_account_race_low2014_white$`2015`)),median(as.numeric(ms_account_race_low2014_white$`2016`)),median(as.numeric(ms_account_race_low2014_white$`2017`)),median(as.numeric(ms_account_race_low2014_white$`2018`)))

## standard dev
low2014_black_sd <- sapply(ms_account_race_low2014_black, sd)
low2014_white_sd <- sapply(ms_account_race_low2014_white, sd)

## black lower and upper
ms_account_race_low2014_black_lower <- ms_account_race_low2014_black_median - 1*low2014_black_sd
ms_account_race_low2014_black_upper <- ms_account_race_low2014_black_median + 1*low2014_black_sd

## white lower and upper
ms_account_race_low2014_white_lower <- ms_account_race_low2014_white_median - 1*low2014_white_sd
ms_account_race_low2014_white_upper <- ms_account_race_low2014_white_median + 1*low2014_white_sd

## low2014 race medians table
ms_account_race_low2014_medians <- rbind.data.frame(ms_account_race_low2014_black_median,ms_account_race_low2014_white_median,ms_account_race_low2014_black_lower,ms_account_race_low2014_black_upper,ms_account_race_low2014_white_lower,ms_account_race_low2014_white_upper)

colnames(ms_account_race_low2014_medians) <- c("2014-10-1","2015-10-1","2016-10-1","2017-10-1","2018-10-1")
rownames(ms_account_race_low2014_medians) <- c("black","white","black_lower","black_upper","white_lower","white_upper")

## transpose
ms_account_race_low2014_medians_t <- data.frame(t(ms_account_race_low2014_medians))

## convert to time series object
ms_account_race_low2014_medians_ts <- as.ts(ms_account_race_low2014_medians_t)

## set tsp for objects
tsp(ms_account_race_low2014_medians_ts) <- c(2014,2018,1)

## plot time series
tsplot_ms_account_race_low2014_median <- dygraph(ms_account_race_low2014_medians_ts[,c(1,2)], main = "Low District Performance by Race 2014-2018",
                                    ylab = "Accountability Score", xlab = "Date") %>%
  dyLimit(401, label = "'F' District", labelLoc = "left",
          color = "purple", strokePattern = "solid") %>%
  
  dyLimit(489, label = "'D' District", labelLoc = "left",
          color = "red", strokePattern = "solid") %>%
  
  dyLimit(536, label = "'C' District", labelLoc = "left",
          color = "orange", strokePattern = "solid") %>%
  
  dyEvent("2015-1-01", "ESSA", labelLoc = "bottom", strokePattern = "solid", color = "blue") %>%
  
  dyAxis("y", valueRange = c(400, 600)) %>%
  
  dySeries("black", label = "majority black", color = "black", strokePattern = "dotted", strokeBorderWidth = 1, strokeWidth = 2) %>%
  
  dySeries("white", label = "majority white", color = "grey", strokePattern = "dashed", strokeBorderWidth = 1, strokeWidth = 2) %>%
  
  dyLegend(width = 400)

tsplot_ms_account_race_low2014_median


