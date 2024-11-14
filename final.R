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
counties$acre <- counties$CALC_SQ_MI * 640
#rename county name column
counties <- counties %>%
  rename(County = NAME)

#join counties and agricultural datasets
agriculture <- full_join(agriculture,
                         counties,
                         by = "County")

