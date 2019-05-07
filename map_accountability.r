## To read this code:
## 1. import libraries
## 2. import local files
##     local files:
##       - district points: https://docs.google.com/spreadsheets/d/1khG5t9MmyOz0u60D7zhmokW3hnUjrKK2sYNXQTpmv6A/edit#gid=836757860
##       - tract outcomes: https://opportunityinsights.org/paper/the-opportunity-atlas/
## 3. set census api key
## 4. get ms districts from acs data:
##     a. school districts, total pop <- "B00001_001E", black <- "B02001_003"
## 5. clean data
## 6. append information
## 7. compare names from grades file to acs names
## 8. combine tables
## 9. append county GEOIDs to master table
## 10. get geometry data for MS counties
## 11. append geomtery data to master table
## 12. map income by county
## 13. map accountability by county

##################################################
## 1. import libraries
library(data.table)
library(ggplot2)
library(tidyverse)
library(tidycensus)
library(tigris)
library(leaflet)
library(scales)
library(stringr)
library(sf)
library(dplyr)
library(readxl)

##################################################
## 2. import numeric MS district point totals 
## F = 1, D = 2, C = 3, B = 4, A = 5
dist_points <- read.csv("/Users/asahialexisnino/Ground/code/r/census_data/data_files/district total points.csv", header = TRUE)
colnames(dist_points) <- c("District",as.numeric(2014),as.numeric(2015),as.numeric(2016),as.numeric(2017),as.numeric(2018))

## import tract outcomes
## https://opportunityinsights.org/wp-content/uploads/2018/10/Codebook-for-Table-4.pdf
## This code is commented because data pulled from API was written to a local csv so it could be accessed faster in later runs
# tract_outcomes = fread("/Users/asahialexisnino/Ground/code/r/census_data/data_files/tract_outcomes_early.csv", header = TRUE)
# stay <- tract_outcomes %>% select(matches('(staytract_pooled_pooled)'))
# ms_stay <- tract_outcomes[which(tract_outcomes$state == 28),c(1,2,5254)]

## write ms_stay to csv
# write.csv(ms_stay, file = "/Users/asahialexisnino/Ground/code/r/census_data/data_files/ms_stay.csv",row.names=FALSE)
ms_stay <- read.csv("/Users/asahialexisnino/Ground/code/r/census_data/data_files/ms_stay.csv", header = TRUE)

ms_stay$GEOID <- as.character(ms_stay$state * 1000 + ms_stay$county)
ms_stay <- ms_stay[,c(4,3)]

ms_stay_county_avg <- aggregate(as.numeric(as.character(ms_stay$staycz_pooled_pooled_mean)), by = list(ms_stay$GEOID), FUN = mean, na.rm = TRUE)
colnames(ms_stay_county_avg) <- c("GEOID","stay_avg")

## clean workspace
#rm(ms_stay)

##################################################
## 3. set api key
api_key <- "d8be0f9922b97970583f64d62e7e4116d6f37dc9"
census_api_key(api_key, install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")

##################################################
## 4. get MS school districts
## variables:
## B02001_001E = Estimate!!Total
## B02001_003E = Estimate!!Total!!Black or African American alone
## B19013_001E = Estimate!!Median household income in the past 12 months (in 2017 inflation-adjusted dollars)
## This code is commented because data pulled from API was written to a local csv so it could be accessed faster in later runs
# acs_sch_dist <- get_acs(geography = "school district (unified)",
#                       variables = c("B02001_001E","B02001_003E","B19013_001E"),
#                      output = "wide",
#                     state = "MS",
#                     keep_geo_vars = TRUE)
# write.csv(acs_sch_dist, file = "/Users/asahialexisnino/Ground/code/r/census_data/data_files/acs_sch_dist.csv",row.names=FALSE)

acs_sch_dist <- read.csv("/Users/asahialexisnino/Ground/code/r/census_data/data_files/acs_sch_dist.csv", header = TRUE)

###################################################
## 5. clean data
## get rid of ", Mississippi" in acs_sch_dist$NAME
clean_acs_NAME <- gsub("\\,.*","",acs_sch_dist$NAME)
## replace NAME column
acs_sch_dist$NAME <- clean_acs_NAME
## add "Consolidated" to East Jasper and East Tallahatchie
acs_sch_dist$NAME <- gsub("East Jasper School District","East Jasper Consolidated School District", acs_sch_dist$NAME)
acs_sch_dist$NAME <- gsub("East Tallahatchie School District","East Tallahatchie Consolidated School District", acs_sch_dist$NAME)
## sort acs_sch_dist by NAME
sorted_acs_dist <- acs_sch_dist[order(acs_sch_dist$NAME),]
## rename district columns
colnames(sorted_acs_dist)[colnames(sorted_acs_dist)=="NAME"] <- "District"
## rename variable columns
colnames(sorted_acs_dist)[colnames(sorted_acs_dist)=="B02001_001E"] <- "Total"
colnames(sorted_acs_dist)[colnames(sorted_acs_dist)=="B02001_001M"] <- "Total MOE"
colnames(sorted_acs_dist)[colnames(sorted_acs_dist)=="B02001_003E"] <- "Black or AA"
colnames(sorted_acs_dist)[colnames(sorted_acs_dist)=="B02001_003M"] <- "Black or AA MOE"
colnames(sorted_acs_dist)[colnames(sorted_acs_dist)=="B19013_001E"] <- "Median Income"
colnames(sorted_acs_dist)[colnames(sorted_acs_dist)=="B19013_001M"] <- "Median Income MOE"

###################################################
## 6. Append information
## calculate % append %black
percent_black <- signif(sorted_acs_dist$`Black or AA`/sorted_acs_dist$Total, digits = 2)
sorted_acs_dist$`Percent Black` <- percent_black
## append consolidated status
dist_points$Consolidated <- ifelse(grepl("Consolidated", dist_points$District),1,0)
sorted_acs_dist$Consolidated <- ifelse(grepl("Consolidated", sorted_acs_dist$District),1,0)

###################################################
## 7. compare names from grades file to acs names
dist_points$District[! dist_points$District %in% sorted_acs_dist$District]

###################################################
## 8. combine tables
master_table <- merge(sorted_acs_dist, dist_points, by=c("District","Consolidated"))
colnames(master_table)[11:15] <- c(2014,2015,2016,2017,2018)

write.csv(master_table, file = "/Users/asahialexisnino/Ground/code/r/census_data/data_files/master_table.csv",row.names=FALSE)

## clean workspace
#rm(acs_sch_dist)
#rm(dist_points)
#rm(sorted_acs_dist)

###################################################
## 9. append county to master table
## read file from https://www.mdek12.org/dd with all district info
ms_dist_counties <- read.table("/Users/asahialexisnino/Ground/code/r/census_data/data_files/ms_dist_counties.txt", sep = "\n")

## get rows with district names
district_rows <- data.frame(ms_dist_counties[grep("District", ms_dist_counties$V1),])
rm(ms_dist_counties)

## change data type to character
district_rows$District_Raw <- as.character(district_rows$ms_dist_counties.grep..District...ms_dist_counties.V1....)

## get rid of everything before '('
district_rows$County <- gsub('.*\\(', '', district_rows$District_Raw)
## get rid of after ')'
district_rows$County <- gsub('\\).*', '', district_rows$County)

## replace those without 'County'
district_rows$County[!grepl('County',district_rows$County)]
district_rows$County <- gsub('East Tallahatchie Consolidated School District', 'Tallahatchie County', district_rows$County)
district_rows$County <- gsub('North Bolivar Consolidated School District', 'Bolivar County', district_rows$County)
district_rows$County <- gsub('Pascagoula-Gautier School District ', 'Jackson County', district_rows$County)
district_rows$County <- gsub('Poplarville Special Municipal Separate School District', 'Pearl River County', district_rows$County)
district_rows$County <- gsub('Tippah', 'Tippah County', district_rows$County)
district_rows$County <- gsub('West Bolivar Consolidated School District ', 'Bolivar County', district_rows$County)
district_rows$County <- gsub('Winona-Montgomery Consolidated School District', 'Montgomery County', district_rows$County)
district_rows$County <- gsub('RankinCounty', 'Rankin County', district_rows$County)
## get rid of everything after 'County'
district_rows$County <- gsub('(County).*', '\\1, Mississippi', district_rows$County)

district_rows <- district_rows[,c(1,3)]
colnames(district_rows) <- c("District", "County")

## clean District
district_rows$District <- gsub('\\(.*', '', district_rows$District)
district_rows$District <- trimws(district_rows$District, which = "right")

## make unique ids for districts
unique_id <- function(x) {
  first_word <- word(x, 1)
  first2_words <- word(x, 1, 2)
  
  i <- 1
  
  for (word in first_word) {
    if (length(grep(word, x)) > 1) {
      first_word[i] <- first2_words[i]
    }
    
    i <- i + 1
  }
  return(first_word)
}

## append to district_rows and master_table
district_rows$Unique <- unique_id(district_rows$District)
master_table$Unique <- unique_id(master_table$District)

master_table <- left_join(master_table, district_rows[,c(2,3)], by = "Unique")

# clean workspace
#rm(district_rows)

## find non-matching Districts and add missing Counties
master_table[is.na(master_table$County),which(colnames(master_table)=="Unique")]
master_table[which(master_table$Unique=="Durant"),which(colnames(master_table)=="County")] <- "Holmes County, Mississippi"
master_table[which(master_table$Unique=="Greenville"),which(colnames(master_table)=="County")] <- "Washington County, Mississippi"
master_table[which(master_table$Unique=="Lumberton"),which(colnames(master_table)=="County")] <- "Lamar County, Mississippi"
master_table[which(master_table$Unique=="Meridian"),which(colnames(master_table)=="County")] <- "Lauderdale County, Mississippi"
master_table[which(master_table$Unique=="Montgomery"),which(colnames(master_table)=="County")] <- "Montgomery County, Mississippi"
master_table[which(master_table$Unique=="Perry"),which(colnames(master_table)=="County")] <- "Perry County, Mississippi"
master_table[which(master_table$Unique=="Pontotoc City"),which(colnames(master_table)=="County")] <- "Pontotoc County, Mississippi"
master_table[which(master_table$Unique=="Pontotoc County"),which(colnames(master_table)=="County")] <- "Pontotoc County, Mississippi"
master_table[which(master_table$Unique=="Vicksburg"),which(colnames(master_table)=="County")] <- "Warren County, Mississippi"
master_table[which(master_table$Unique=="Winona"),which(colnames(master_table)=="County")] <- "Montgomery County, Mississippi"

## average points for each county with more than one district
county_account_avg_2014 <- aggregate(as.numeric(as.character(master_table$`2014`)), by = list(master_table$County), FUN = mean, na.rm = TRUE)
county_account_avg_2015 <- aggregate(as.numeric(as.character(master_table$`2015`)), by = list(master_table$County), FUN = mean, na.rm = TRUE)
county_account_avg_2016 <- aggregate(as.numeric(as.character(master_table$`2016`)), by = list(master_table$County), FUN = mean, na.rm = TRUE)
county_account_avg_2017 <- aggregate(as.numeric(as.character(master_table$`2017`)), by = list(master_table$County), FUN = mean, na.rm = TRUE)
county_account_avg_2018 <- aggregate(as.numeric(as.character(master_table$`2018`)), by = list(master_table$County), FUN = mean, na.rm = TRUE)

## set colnames for county accountability averages
colnames(county_account_avg_2014) <- c("County","account_avg_2014")
colnames(county_account_avg_2015) <- c("County","account_avg_2015")
colnames(county_account_avg_2016) <- c("County","account_avg_2016")
colnames(county_account_avg_2017) <- c("County","account_avg_2017")
colnames(county_account_avg_2018) <- c("County","account_avg_2018")

## join county averages to master_table
master_table <- left_join(master_table, county_account_avg_2014, by = "County")
master_table <- left_join(master_table, county_account_avg_2015, by = "County")
master_table <- left_join(master_table, county_account_avg_2016, by = "County")
master_table <- left_join(master_table, county_account_avg_2017, by = "County")
master_table <- left_join(master_table, county_account_avg_2018, by = "County")

##################################################
## 10. get geometry data for MS counties
## This code is commented because data pulled from API was written to a local csv so it could be accessed faster in later runs
ms_income <- get_acs(geography = "county",
                  variables = "B19013_001E",
                  state = "MS",
                  geometry = TRUE)

# write.csv(ms_income, file = "/Users/asahialexisnino/Ground/code/r/census_data/data_files/ms_income.csv",row.names=FALSE)
# 
# ms_income <- read.csv("/Users/asahialexisnino/Ground/code/r/census_data/data_files/ms_income.csv", header = TRUE)

ms_counties_geom <- ms_income[,c(1,2,6)]

##################################################
## 11. append geomtery data to master table data
colnames(ms_counties_geom)[2] <- "County"
ms_account <- left_join(master_table[,which(colnames(master_table)==c("County","account_avg_2018"))], ms_counties_geom, by = "County")
colnames(ms_account) <- c("NAME","account_avg_2018","GEOID","geometry")

## convert to sf object
## sf: simple features, a standardized way to encode spatial vector data
ms_account <- st_as_sf(ms_account, sf_column_name = "geometry")
class(ms_account)

## clean workspace
#rm(county_account_avg_2014)
#rm(county_account_avg_2015)
#rm(county_account_avg_2016)
#rm(county_account_avg_2017)
#rm(county_account_avg_2018)

##################################################
# 12. map income by county
pal <- colorQuantile(palette = "viridis", domain = ms_income$estimate, n = 10)

ms_income$label <- gsub(", Mississippi",": $",paste(ms_income$NAME, ms_income$estimate))

ms_income %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ label,
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal(estimate)) %>%
  addLegend("bottomright",
            pal = pal,
            values = ~ estimate,
            title = "Income Percentiles",
            opacity = 1)

##################################################
## 13. map accountability by county

ms_account$str_est <- as.character(ms_account$account_avg)

ms_account$letter_grade <- cut(ms_account$account_avg_2018, c(0,489,535,599,669,1000), labels = c("F","D","C","B","A"))
ms_account$grade_num <- as.numeric(cut(ms_account$account_avg_2018, c(0,489,535,599,669,1000), labels = c(1:5)))

ms_account$label <- paste(gsub(", Mississippi","",ms_account$NAME),":",ms_account$letter_grade)

pal <- colorQuantile(palette = "viridis", domain = ms_account$account_avg_2018, n = 5)

pal <- colorFactor(palette = "viridis", domain = ms_account$letter_grade)


ms_account %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(label, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal(letter_grade)) %>%
  addLegend("bottomright",
            pal = pal,
            values = ~ letter_grade,
            title = "2018 School Grades",
            opacity = 1)

##################################################
## 14. map stay avg by county

## join county to stay
ms_stay_county_avg <- left_join(ms_stay_county_avg,ms_counties_geom,by = "GEOID")

## convert estimate to percentage
ms_stay_county_avg$estimate_percent <- percent(ms_stay_county_avg$stay_avg)
ms_stay_county_avg$str_est <- as.character(ms_stay_county_avg$estimate_percent)

## convert ms_stay_county_avg to sf object
ms_stay_county_avg <- st_as_sf(ms_stay_county_avg, sf_column_name = "geometry")

pal <- colorQuantile(palette = "viridis", domain = ms_stay_county_avg$stay_avg, n = 10)

pal <- colorNumeric(palette = "viridis", domain = ms_stay_county_avg$stay_avg)

ms_stay_county_avg %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(County, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal(stay_avg)) %>%
  addLegend("bottomright",
            pal = pal,
            values = ~ stay_avg,
            title = "Rate of Stay (Commuting Zone)",
            opacity = 1)

##################################################
## 15. plot income vs. stay
ms_income_stay <- left_join(ms_stay_county_avg[,c(1,2)],as.data.frame(ms_income[,c(1,4)]), by = "GEOID")

income_stay <- ggplot(ms_income_stay, aes(x = ms_income_stay$estimate,
                                           y = ms_income_stay$stay_avg)) +
  geom_point()

income_stay

##################################################
## 16. get 2018 Community Health Rankings data
chr_2018 <- read_xls("/Users/asahialexisnino/Ground/code/r/census_data/data_files/2018 County Health Rankings Mississippi Data - v3.xls", sheet = 4, skip = 1, col_names = TRUE)

chr_2018$County <- gsub(" ,",",",paste(chr_2018$County, ",", chr_2018$State))
chr_2018 <- chr_2018[,which(colnames(chr_2018) != "State")]

## build a "basic needs" set
## food, water, shelter
# basic_needs_18 <- chr_2018[,c(1,)]

# colnames(chr_2018) <- c("GEOID","County",)

##################################################
## 17. plot income vs. stay and race
## append race to ms_income_stay
## variables:
## B02001_001E = Estimate!!Total
## B02001_003E = Estimate!!Total!!Black or African American alone
## B19013_001E = Estimate!!Median household income in the past 12 months (in 2017 inflation-adjusted dollars)
## This code is commented because data pulled from API was written to a local csv so it could be accessed faster in later runs
# ms_county_race <- get_acs(geography = "county",
#                       variables = c("B02001_001E","B02001_003E"),
#                      output = "wide",
#                     state = "MS",
#                     keep_geo_vars = TRUE)
# 
# write.csv(ms_county_race, file = "/Users/asahialexisnino/Ground/code/r/census_data/data_files/ms_county_race.csv",row.names=FALSE)

ms_county_race <- read.csv("/Users/asahialexisnino/Ground/code/r/census_data/data_files/ms_county_race.csv", header = TRUE)

colnames(ms_county_race)[colnames(ms_county_race)=="B02001_001E"] <- "Total"
colnames(ms_county_race)[colnames(ms_county_race)=="B02001_001M"] <- "Total MOE"
colnames(ms_county_race)[colnames(ms_county_race)=="B02001_003E"] <- "Black or AA"
colnames(ms_county_race)[colnames(ms_county_race)=="B02001_003M"] <- "Black or AA MOE"

## calculate % append %black
percent_black <- signif(ms_county_race$`Black or AA`/ms_county_race$Total, digits = 2)
ms_county_race$`Percent Black` <- percent_black

ms_county_race$race <- ms_county_race$`Percent Black` > .5

ms_county_race$race <- gsub(TRUE, "majority Black", ms_county_race$race)
ms_county_race$race <- gsub(FALSE,"majority White", ms_county_race$race)

## join ms_income_stay with race
ms_county_race$GEOID <- as.character(ms_county_race$GEOID)
ms_income_stay_race <- left_join(ms_income_stay, ms_county_race[,c(1,8)], by = "GEOID")

plot_ms_income_stay_race <- ggplot() + geom_point(data = ms_income_stay_race, aes(x = estimate, y = stay_avg, color = race)) + 
    xlab("Median Income") + ylab("Rate of Stay") + ggtitle("Which communities keep their children (by race)") +
  theme(plot.title = element_text(hjust = .5))

plot_ms_income_stay_race

## join account to stay with race
df_ms_stay_county_avg <- ms_stay_county_avg
st_geometry(df_ms_stay_county_avg) <- NULL

ms_account_stay <- left_join(ms_account[,c(2,3)], df_ms_stay_county_avg[,c(1,2)], by = "GEOID")

ms_account_stay_race <- left_join(ms_account_stay,ms_county_race[,c(1,8)], by = "GEOID")

plot_ms_account_stay_race <-ggplot() + geom_point(data = ms_account_stay_race, aes(x = account_avg_2018, y = stay_avg, color = race)) + 
  xlab("Average Accountability Score") + ylab("Rate of Stay") + ggtitle("Which communities keep their children (by race)") +
  theme(plot.title = element_text(hjust = .5))

plot_ms_account_stay_race

##################################################
## 18. map of growth
## join geom with county and growth

# lm on each row of district points

years <- as.numeric(colnames(dist_points)[2:6])

f = function(x, output) {
  name = x[1]
  
  total_points = x[2:6]
  
  non_na_points = total_points[!(is.na(total_points) | total_points=="E")]
  non_na_years = years[!(is.na(total_points) | total_points=="E")]
  
  year_points_lm <- lm(non_na_points ~ non_na_years)
  
  coef <- year_points_lm$coefficients
  
  intercept <- ifelse(is.na(coef[1]), 0, coef[1])
  slope <- ifelse(is.na(coef[2]), 0, coef[2])
  
  # plot(non_na_points ~ non_na_years, main = name)
  # abline(a = intercept, b = slope)
  
  #cat(name,"intercept: ", coef[1],"- slope: ", coef[2], "\n")
  # cat(name, " ", non_na_years, " ", non_na_points, "\n")
  
  slope
}

master_table$growth <- apply(dist_points[,1:6], 1, f)


master_table$GEOID <- as.character(master_table$GEOID)

ms_growth_county_avg <- aggregate(as.numeric(as.character(master_table$growth)), by = list(master_table$County), FUN = mean, na.rm = TRUE)
colnames(ms_growth_county_avg) <- c("County","growth_avg")

ms_growth_geom <- left_join(ms_growth_county_avg,ms_counties_geom,by = "County")

ms_growth_geom$popup <- paste(gsub(", Mississippi","",ms_growth_geom$County),": ", signif(ms_growth_geom$growth_avg,2))

## convert ms_growth_geom to sf object
ms_growth_geom <- st_as_sf(ms_growth_geom, sf_column_name = "geometry")

pal <- colorQuantile(palette = "viridis", domain = ms_growth_geom$growth_avg, n = 10)
pal <- colorNumeric(palette = "viridis", domain = ms_growth_geom$growth_avg)

ms_growth_geom %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ popup,
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal(growth_avg)) %>%
  addLegend("bottomright",
            pal = pal,
            values = ~ growth_avg,
            title = "Accountability Growth Coef. (2014-18)",
            opacity = 1)

# Plot %black vs. dist grade 2018
black_grade18 <- ggplot(master_table, aes(x = master_table$`Percent Black`,
                                          y = master_table$`2018`)) + 
  geom_point() + 
  xlab("Percent Black") + ylab("Accountability Score") + ggtitle("How do schools perform (by race) (2018)") +
  theme(plot.title = element_text(hjust = .5))

black_grade18

# Plot %black vs. dist grade 2018
income_grade18 <- ggplot(master_table, aes(x = master_table$`Median Income`,
                                          y = master_table$`2018`)) + 
  geom_point() + 
  xlab("Median Income") + ylab("Accountability Score") + ggtitle("How do schools perform (by income) (2018)") +
  theme(plot.title = element_text(hjust = .5))

income_grade18

# Plot %black vs. growth
black_growth <- ggplot(master_table, aes(x = master_table$`Percent Black`,
                                         y = master_table$growth)) +
  geom_point() + 
  xlab("Percent Black") + ylab("Growth of Accountability Score") + ggtitle("How do schools grow (by race)") +
  theme(plot.title = element_text(hjust = .5))
black_growth

# Plot %black vs. growth
income_growth <- ggplot(master_table, aes(x = master_table$`Median Income`,
                                         y = master_table$growth)) +
  geom_point() + 
  xlab("Median Income") + ylab("Growth of Accountability Score") + ggtitle("How do schools grow (by income)") +
  theme(plot.title = element_text(hjust = .5))
income_growth
