#loading Libraries
library(dplyr)
library(tidyr)
library(stringi)
install.packages("cdlTools")
library(cdlTools)
install.packages("stargazer")
library("stargazer")
library(reshape2)

#importing data
gov_elec_1999 <- read.csv("~/14.33 Final Project/gov_elec_1999.csv", header=FALSE)
gov_elec_2009 <- read.csv("~/14.33 Final Project/gov_elec_2009.csv", header=FALSE)
gov_elec_2019 <- read.csv("~/14.33 Final Project/gov_elections_2019.csv", header=FALSE)
state_GDP <- read.csv("~/14.33 Final Project/SAGDP1__ALL_AREAS_1997_2019 2.csv")
state_unemployment <- read.delim("~/14.33 Final Project/la.data.3.AllStatesS.txt")

#cleaning governor elections and combining
gov_elec_1999_clean <- gov_elec_1999 %>% filter(V1=="Governor")
gov_elec_2009_clean <- gov_elec_2009 %>% filter(V1=="Governor")
gov_elec_2019_clean <- gov_elec_2019 %>% filter(V1=="Governor")
#combining
gov_elec_2019_clean <- rbind(gov_elec_2019_clean, gov_elec_2009_clean)
gov_elec_2019_clean <- rbind(gov_elec_2019_clean, gov_elec_1999_clean)
#reassigning headers
colnames(gov_elec_2019_clean) <- c("Office", "CensusPop", "raceYear", "RaceTypeName", "state", "RepVotes", "RepCandidate", "RepStatus", "DemVotes", "DemCandidate", "DemStatus", "ThirdParty", "ThirdVotes","ThirdCandidate", "ThirdStatus", "OtherVotes", "PluralityVotes", "PluralityParty", "ThirdVotesTotalPercent", "RepVotesMajorPercent", "DemVotesMajorPercent", "RaceNotes")
#cleaning the year column
gov_elec_2019_clean$year <- stri_sub(gov_elec_2019_clean$raceYear, 1,4)
#final = gov_elec_2019_clean

#cleaning unemployment
state_unemployment$state_num<- substr(state_unemployment$series_id, 6, 7)
#add state name
state_unemployment$state<- substr(state_unemployment$series_id, 6, 7)
state_unemployment$state <- fips(state_unemployment$state, to="Name")
#get correct unemployment rate
state_unemployment$is_3<- substr(state_unemployment$series_id, 20, 20)
state_unemployment <- state_unemployment %>% filter(is_3 == 3)
colnames(state_unemployment)[colnames(state_unemployment) == "value"] <- "unemployment"
#making unemployment a numeric instead of factor
state_unemployment$unemployment <- levels(state_unemployment$unemployment)[state_unemployment$unemployment]
state_unemployment$unemployment <- as.numeric(state_unemployment$unemployment)
#filter by data, get change from jan to oct and filter October
state_unemployment <- state_unemployment %>% filter(period == "M10" | period == "M01")
state_unemployment <- state_unemployment %>%
  group_by(state) %>% 
  arrange(state, year, period, .by_group = TRUE) %>%
  mutate(change_jan_oct = (unemployment/lag(unemployment) - 1) * 100)
state_unemployment <- state_unemployment %>% filter(period == "M10")


#final = state_unemployment

#cleaning GDP
state_GDP <- state_GDP %>% filter(LineCode == 1)
state_GDP <- melt(state_GDP, id.vars = c("GeoFIPS", "GeoName"), measure.vars = c("X1998", "X1999", "X2000", "X2001", "X2002", "X2003", "X2004", "X2005", "X2006", "X2007", "X2008", "X2009", "X2010", "X2011", "X2012", "X2013", "X2014", "X2015", "X2016", "X2017", "X2018", "X2019"), variable.name = "year", value.name = "GDP")
state_GDP$year <- gsub("X", "", state_GDP$year)
state_GDP <- filter(state_GDP, GeoName != "District of Columbia" & GeoName != "United States" & GeoName != "Southeast"  & GeoName != "New England" & GeoName != "Far West" & GeoName != "Plains" & GeoName != "Rocky Mountain" & GeoName != "Great Lakes" & GeoName != "Southwest" & GeoName != "Mideast")
colnames(state_GDP)[colnames(state_GDP) == "GeoName"] <- "state"
#Add GDP Percent change column
state_GDP <- state_GDP %>%
  group_by(state) %>% 
  arrange(year, .by_group = TRUE) %>%
  mutate(GDP_percent_change = (GDP/lag(GDP) - 1) * 100)
#final = state_GDP

#combining all the data
mergedData <- merge(gov_elec_2019_clean, state_unemployment, by=c("year", "state"))
mergedData <- merge(mergedData, state_GDP, by=c("year", "state"))

#filter out if 3rd party has over 30% of the votes
mergedData$ThirdVotesTotalPercent <- levels(mergedData$ThirdVotesTotalPercent)[mergedData$ThirdVotesTotalPercent]
mergedData$ThirdVotesTotalPercent <- as.numeric(mergedData$ThirdVotesTotalPercent)
mergedData <- filter(mergedData, ThirdVotesTotalPercent < 30)

#adding victory margin percent column (RepVotes - DemVotes)
#year is already a character
#Rep
mergedData$RepVotesMajorPercent <- levels(mergedData$RepVotesMajorPercent)[mergedData$RepVotesMajorPercent]
mergedData$RepVotesMajorPercent <- as.numeric(mergedData$RepVotesMajorPercent)
#Dem
mergedData$DemVotesMajorPercent <- levels(mergedData$DemVotesMajorPercent)[mergedData$DemVotesMajorPercent]
mergedData$DemVotesMajorPercent <- as.numeric(mergedData$DemVotesMajorPercent)
#Combine
mergedData$RepMinusDem <- (mergedData$RepVotesMajorPercent - mergedData$DemVotesMajorPercent)

#run a regression and make table 1
linearModel1 <- lm(RepMinusDem ~ GDP_percent_change + unemployment + state + year + RepStatus + DemStatus, data = mergedData)
linearModel2 <- lm(RepMinusDem ~ unemployment + state + year + RepStatus + DemStatus, data = mergedData)
linearModel3 <- lm(RepMinusDem ~ GDP_percent_change + unemployment + change_jan_oct + state + year + RepStatus + DemStatus, data = mergedData)

#linearModel3 <- lm(RepMinusDem ~ GDP_percent_change + state + year + RepStatus + DemStatus, data = mergedData)

stargazer(linearModel1, linearModel2, linearModel3,omit = c("state", "year"), title="Regression Results",
          align=TRUE, no.space=TRUE, dep.var.labels   = "RepMargin")

#make figure 1
mergedData <- mergedData %>% add_count(year)
plot(mergedData$year, mergedData$n, main = "Figure 1: Number of Elections per Year", xlab = "Year", ylab = "Number of Elections", xaxt = "none")
lines(mergedData$year, mergedData$n, xlab = "Year", ylab = "Number of Elections")
axis(1, seq(1996,2019,2),las=2)


#run a regression comparing incumbents to challengers
#make new merged data that only includes when there is an incumbent
mergedDataInc <- mergedData %>% filter(RepStatus == "Incumbent" | DemStatus == "Incumbent")
#calc incumbent vote share
mergedDataInc <- mutate(mergedDataInc, IncVotesDem = ifelse(DemStatus == "Incumbent", DemVotesMajorPercent, 0))
mergedDataInc <- mutate(mergedDataInc, IncVotesRep = ifelse(RepStatus == "Incumbent", RepVotesMajorPercent, 0))
mergedDataInc <- mutate(mergedDataInc, ChaVotesDem = ifelse(DemStatus == "Challenger", DemVotesMajorPercent, 0))
mergedDataInc <- mutate(mergedDataInc, ChaVotesRep = ifelse(RepStatus == "Challenger", RepVotesMajorPercent, 0))
mergedDataInc$IncMinusCha <- mergedDataInc$IncVotesDem + mergedDataInc$IncVotesRep - mergedDataInc$ChaVotesDem - mergedDataInc$ChaVotesRep
#run the regression
linearModelA <- lm(IncMinusCha ~ GDP_percent_change + unemployment + RepStatus + state + year, data = mergedDataInc)
linearModelB <- lm(IncMinusCha ~ unemployment + state + year + RepStatus, data = mergedDataInc)
linearModelC <- lm(IncMinusCha ~ GDP_percent_change + unemployment + change_jan_oct + state + year + RepStatus, data = mergedDataInc)
stargazer(linearModelA, linearModelB, linearModelC,omit = c("state", "year"), title="Regression Results",
          align=TRUE, no.space=TRUE, dep.var.labels   = "Incumbent Margin")
