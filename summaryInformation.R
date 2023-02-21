library("dplyr")
library("tidyverse")
library("stringr")

# make list
summary_info <- list()

# Number of States Observed
summary_info$num_States <- nrow(Final)

# Number of business observed
summary_info$TotalBusiness <- sum(Final$NumberOfBusiness)

# Number of minority total
summary_info$TotalMinority <- sum(Final$minority_count)

# Most common Majority Ethnicity
summary_info$CommonEthnicity <- Final %>%
  group_by(MajorityEthnicity) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  slice(1) %>%
  select(MajorityEthnicity)

# Number of Women business owner based on certificaton
summary_info$WomenBusinessOwner <- Final %>%
  filter(WBECount == max(WBECount, na.rm = T)) %>%
  select(WBECount)
