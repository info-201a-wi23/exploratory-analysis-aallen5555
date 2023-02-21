library("dplyr")
library("tidyverse")
library("stringr")


full_dataset <- read.csv("fulldataframe.csv")

WBE_count <- nrow(full_dataset[full_dataset$certification == "WBE", ])

Cities <- full_dataset %>% group_by(State) %>% summarise(Cities = paste(unique(na.omit(City)), collapse = ", "))

ZipCodes <- full_dataset %>% group_by(State) %>% summarise(ZipCodes = paste(unique(na.omit(ZIP)), collapse = ", "))

# Group data by state and find most common zip code
most_common_zip <- full_dataset %>%
  group_by(State) %>%
  summarize(most_common_zip = names(which.max(table(na.omit(ZIP))))) %>%
  ungroup()

NumberOfBusiness <- full_dataset %>% group_by(State) %>% summarise(NumberOfBusiness = n())

MinorityCount <- full_dataset %>% group_by(State) %>%
  summarise(non_minority_count = sum(Ethnicity == "NON-MINORITY"), 
    total_count = n()) %>% 
  mutate(minority_count = (total_count - non_minority_count))

PctMinority <- full_dataset %>% group_by(State) %>%
  summarise(non_minority_count = sum(Ethnicity == "NON-MINORITY"), 
    total_count = n()) %>% 
  mutate(minority_pct = ((total_count - non_minority_count) / total_count) * 100)
  PctMinority <- PctMinority[, c("State", "minority_pct")]

Majority <- full_dataset %>%
  group_by(State) %>%
  summarize(MajorityEthnicity = names(which.max(table(Ethnicity))))

WBECount <- full_dataset %>% group_by(State) %>%  
  summarise(WBECount = sum(str_detect(certification, "WBE")))

WBEPercentage <- full_dataset %>% group_by(State) %>%
  summarise(WBECount = sum(str_detect(certification,"WBE")),
    total_count = n()) %>%
  mutate(WBE_pct = (WBECount / total_count) * 100)
  WBEPercentage <- WBEPercentage[, c("State", "WBE_pct")]
                            
MBECount <- full_dataset %>% group_by(State) %>%  
  summarise(MBECount = sum(str_detect(certification, "MBE")))

MBEPercentage <- full_dataset %>% group_by(State) %>%
  summarise(MBECount = sum(str_detect(certification, "MBE")),
    total_count = n()) %>%
  mutate(MBE_pct = (MBECount / total_count) * 100)
  MBEPercentage <- MBEPercentage[, c("State", "MBE_pct")]

Final <- left_join(Cities, ZipCodes, PctMinority, by = "State") %>%
  left_join(NumberOfBusiness, by = "State") %>% 
  left_join(most_common_zip, by = "State") %>% 
  left_join(MinorityCount, by = "State") %>% 
  left_join(PctMinority, by = "State") %>%
  left_join(Majority, by = "State") %>%
  left_join(WBECount, by = "State") %>%
  left_join(WBEPercentage, by = "State") %>%
  left_join(MBECount, by = "State") %>%
  left_join(MBEPercentage, by = "State")

