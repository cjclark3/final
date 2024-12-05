install.packages(c("ggplot2", "dplyr", "sf"))
install.packages("stringr")
install.packages("tidyr")
library(stringr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(sf)

#load in data files
biodiversity <- read.csv("/cloud/project/biodiversity.csv")
agriculture <- read.csv("/cloud/project/agriculture.csv")
counties <- st_read("/cloud/project/Counties_Shoreline.shp")

###prepare agriculture and county datasets
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

###prepare biodiversity dataset
unique(biodiversity$State.Conservation.Rank)

biodiversity <- biodiversity %>%
  select(County, Category, NY.Listing.Status, State.Conservation.Rank, Global.Conservation.Rank)

#filter out ranking that conservation/diverisyt would not apply
biodiversity <- biodiversity %>%
  filter(!State.Conservation.Rank %in% c("SH", "SX", "SU", "SNR", "SNA"))

biodiversity$State.Conservation.Rank <- str_replace(biodiversity$State.Conservation.Rank, "\\?","")

#split the column into two new columns
biodiversity <- biodiversity %>%
  separate(State.Conservation.Rank, into = c("Rank1", "Rank2", "Rank3"), sep = "S", fill = "right")

conversation_ranks <- separate(biodiversity, State.Conservation.Rank, into = c("Rank0", "Rank1", "Rank2","Rank3"), sep = "S", fill = "right", remove = FALSE) %>%
  select(State.Conservation.Rank, Rank1, Rank2, Rank3)
conversation_ranks <- conversation_ranks[!duplicated.data.frame(conversation_ranks),]
write.csv(conversation_ranks, "conversation_ranks.csv")

conversation_ranks <- read.csv("conversation_ranks.csv")
biodiversity <- left_join(biodiversity, conversation_ranks, by = "State.Conservation.Rank")

#biodiversity <- biodiversity %>%
  #filter(str_detect(State.Conservation.Rank, "SH|SX|SU|SNR|SNA"))
