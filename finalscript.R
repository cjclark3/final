install.packages(c("ggplot2", "dplyr", "sf", "stringr", "tidyr", "PerformanceAnalytics", "olsrr"))
library(stringr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(sf)
library(PerformanceAnalytics)
library(olsrr)

#load in data files
biodiversity <- read.csv("/Users/clark/Downloads/biodiversity.csv")
agriculture <- read.csv("/Users/clark/Downloads/agriculture.csv")
counties <- st_read("/Users/clark/Downloads/NYS_Civil_Boundaries.shp/Counties_Shoreline.shp")

###prepare agriculture and county datasets
#get area of counties
counties.area <- st_area(counties)

#select county and ag acre
agriculture.filtered <- agriculture %>%
  select("County", "Total.Acres.in.Agricultural.Districts")
#sum all ag acreage by county
agriculture.filtered <- agriculture.filtered %>%
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
agriculture.filtered$County <- gsub("\\.", "", agriculture$County)

#join counties and agricultural datasets
ag.join <- left_join(agriculture.filtered,
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

#keep only needed columnns
biodiversity.filtered <- biodiversity %>%
  select(County, Category, State.Conservation.Rank)

#make County column uppercase
biodiversity.filtered <- biodiversity.filtered %>%
  mutate(County = toupper(County))

#sepaprate listed ranks for each species into different columns + save to new df
conservation_ranks <- separate(biodiversity.filtered, State.Conservation.Rank, into = c("Rank0", "Rank1", "Rank2","Rank3"), sep = "S", fill = "right", remove = FALSE) %>%
  select(State.Conservation.Rank, Rank1, Rank2, Rank3)
#eliminate duplicate ranks and save to csv
conservation_ranks <- conservation_ranks[!duplicated.data.frame(conservation_ranks),]
write.csv(conservation_ranks, "conservation_ranks.csv")

#manually clean rank csv in excel, read in clean rank csv
conservation_ranks_clean <- read.csv("/cloud/project/conservation_ranks_clean.csv")

#rename rank columns
conservation_ranks_clean <- rename(conservation_ranks_clean, BS.rank = Breeding.Species)
conservation_ranks_clean <- rename(conservation_ranks_clean, NBS.rank = NonBreeding.Species)

#join cleaned rank df and filtered biodiversity df
biodiversity.filtered.ranks <- left_join(biodiversity.filtered, conservation_ranks_clean, by = "State.Conservation.Rank")

#calculate weighted biodiversity score by county
BFR.counties <- biodiversity.filtered.ranks %>%
  group_by(County) %>%
  summarize(county.biodiversity = mean(BS.rank + NBS.rank, na.rm = T))

###join biodiversity and agricultural land use df by county, filter out counties without data for both
bio.ag <- full_join(BFR.counties, ag.join, by = "County") %>%
  filter(!is.na(ag.percent))

###run simple linear analysis
bio.ag.lm <- lm(county.biodiversity ~ ag.percent, data = bio.ag)
summary(bio.ag.lm)

#find residuals and fitted values
res.bio.ag <- rstandard(bio.ag.lm)
fit.bio.ag <- fitted.values(bio.ag.lm)

#test for normality using shapiro.wilkes
shapiro.test(res.bio.ag)

#test for normality using qq plot and line
qqnorm(res.bio.ag, pch=19)
qqline(res.bio.ag)

#plot residuals and fitted values
plot(fit.bio.ag,res.bio.ag, pch=19)
abline(h=0)

#plot linear regression
ggplot(bio.ag, aes(x = ag.percent, y = county.biodiversity, color = County)) +
  geom_point() + 
  geom_smooth(method = "lm", se = TRUE, color = "red") + 
  labs(x = "Agricultural Land Use (%)", y = "Biodiversity Score") +
  theme_minimal()

###check if plant, animal, and natural community diversity effect each other
#separate animal, plant, and natural community observations
biodiversity.cat <- biodiversity.filtered %>%
  mutate(
    animal = if_else(Category == "Animal", 1, 0),
    plant = if_else(Category == "Plant", 1, 0),
    natural_community = if_else(Category == "Natural Community", 1, 0))

#join cleaned rank df and filtered biodiversity df
biodiversity.cat.ranks <- left_join(biodiversity.cat, conservation_ranks_clean, by = "State.Conservation.Rank")

#calculate weighted biodiversity score by county
biodiversity.cat.ranks <- biodiversity.cat.ranks %>%
  group_by(County) %>%
  mutate(county.biodiversity = mean(BS.rank + NBS.rank, na.rm = TRUE))

###join category biodiversity and agricultural land use df by county, filter out counties without data for both
bio.cat.ag <- full_join(biodiversity.cat.ranks, ag.join, by = "County") %>%
  filter(!is.na(ag.percent))

###run multiple regression to see if category impacts biodiversity
bio.cat.ag.lm <- lm(county.biodiversity ~ animal +
                      plant + natural_community +
                      ag.percent,  data = bio.cat.ag) 
summary(bio.cat.ag.lm)

#find residuals and fitted values
res.bio.cat.ag <- rstandard(bio.cat.ag.lm)
fit.bio.cat.ag <- fitted.values(bio.cat.ag.lm)

#test for normality using qq plot and line
qqnorm(res.bio.cat.ag, pch=19)
qqline(res.bio.cat.ag)

#plot residuals and fitted values
plot(fit.bio.cat.ag,res.bio.cat.ag, pch=19)
abline(h=0)

#test for multicollinearity
BCA.cat <- data.frame(bio.cat.ag$animal,
                      bio.cat.ag$plant,
                      bio.cat.ag$natural_community,
                      bio.cat.ag$ag.percent)

#make a correlation matrix 
chart.Correlation(BCA.cat, histogram=TRUE, pch=19)

#run stepwise to asses importance of variables
BCA.step <- ols_step_forward_aic(bio.cat.ag.lm)
# view table
BCA.step 

#plot linear regression
ggplot(bio.ag, aes(x = ag.percent, y = county.biodiversity, color = County)) +
  geom_point() + 
  geom_smooth(method = "lm", se = TRUE, color = "red") + 
  labs(x = "Agricultural Land Use (%)", y = "Biodiversity Score") +
  theme_minimal()