install.packages(c("ggplot2", "dplyr", "sf"))
library(ggplot2)
library(dplyr)
library(sf)

#load in data files
biodiversity <- read.csv("/cloud/project/biodiversity.csv")
agriculture <- read.csv("/cloud/project/agriculture.csv")
counties <- st_read("/cloud/project/Counties_Shoreline.shp")

#get area of counties
counties.area <- st_area(counties)

#select county and ag acre
agriculture <- agriculture %>%
  select("County", "Total.Acres.in.Agricultural.Districts")
#sum all ag acreage by county
agriculture <- agriculture %>%
  group_by(County) %>%
  summarize(ag.acre = sum(Total.Acres.in.Agricultural.Districts))

#select county name and area
counties <- counties %>%
  select("NAME", "CALC_SQ_MI")
#convert county area to acres
counties$total.acre <- counties$CALC_SQ_MI * 640

#rename county name column
counties <- counties %>%
  rename(County = NAME)
#make counties County column uppercase
counties <- counties %>%
  mutate(County = toupper(County))
#remove all periods in names
agriculture$County <- gsub("\\.", "", agriculture$County)


#join counties and agricultural datasets
ag.join <- left_join(agriculture,
                     counties,
                     by = c("County"))
#select relevant columns
ag.join <- ag.join %>%
  select("County", "ag.acre", "total.acre")

#add percent column
#convert county area to acres
ag.join$ag.percent <- (ag.join$ag.acre / ag.join$total.acre) * 100
#round percentage to two decimals
ag.join$ag.percent <- round(ag.join$ag.percent, 2)

