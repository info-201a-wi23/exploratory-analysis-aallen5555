library("dplyr")
library("tidyverse")
library("stringr")
library("knitr")
library("kableExtra")

# read csv
full_dataset <- read.csv("fulldataframe.csv")

# make tcolumn for number of city per state and the most common city
Cities <- full_dataset %>% group_by(State) %>%
  summarize(CityCount = n(), MostCommonCity = names(which.max(table(City))))

# make column for unique zip codes amount and the most common zip codes per city
ZipCodes <- full_dataset %>% group_by(State) %>%
  summarize(UniqueZipCount = n(), MostCommonZIP = names(which.max(table(ZIP))))

# find number of business
NumberOfBusiness <- full_dataset %>% group_by(State) %>% summarise(NumberOfBusiness = n())

# count the total minority
MinorityCount <- full_dataset %>% group_by(State) %>%
  summarise(non_minority_count = sum(Ethnicity == "NON-MINORITY"), 
    total_count = n()) %>% 
  mutate(minority_count = (total_count - non_minority_count))

# count the percentage minority
PctMinority <- full_dataset %>% group_by(State) %>%
  summarise(non_minority_count = sum(Ethnicity == "NON-MINORITY"), 
    total_count = n()) %>% 
  mutate(minority_pct = ((total_count - non_minority_count) / total_count) * 100)
  PctMinority <- PctMinority[, c("State", "minority_pct")]

# find out which ethnicity is the majority
Majority <- full_dataset %>%
  group_by(State) %>%
  summarize(MajorityEthnicity = names(which.max(table(Ethnicity))))

# find the number of WBE owner
WBECount <- full_dataset %>% group_by(State) %>%  
  summarise(WBECount = sum(str_detect(certification, "WBE")))

# find the number of WBE percentage
WBEPercentage <- full_dataset %>% group_by(State) %>%
  summarise(WBECount = sum(str_detect(certification,"WBE")),
    total_count = n()) %>%
  mutate(WBE_pct = (WBECount / total_count) * 100)
  WBEPercentage <- WBEPercentage[, c("State", "WBE_pct")]
                   
# find the number of MBE owner
MBECount <- full_dataset %>% group_by(State) %>%  
  summarise(MBECount = sum(str_detect(certification, "MBE")))

# find the number of MBE percentage
MBEPercentage <- full_dataset %>% group_by(State) %>%
  summarise(MBECount = sum(str_detect(certification, "MBE")),
    total_count = n()) %>%
  mutate(MBE_pct = (MBECount / total_count) * 100)
  MBEPercentage <- MBEPercentage[, c("State", "MBE_pct")]

# add all to table
Final <- left_join(Cities, ZipCodes, PctMinority, by = "State") %>%
  left_join(NumberOfBusiness, by = "State") %>% 
  left_join(MinorityCount, by = "State") %>% 
  left_join(PctMinority, by = "State") %>%
  left_join(Majority, by = "State") %>%
  left_join(WBECount, by = "State") %>%
  left_join(WBEPercentage, by = "State") %>%
  left_join(MBECount, by = "State") %>%
  left_join(MBEPercentage, by = "State")

Final %>% kbl()

