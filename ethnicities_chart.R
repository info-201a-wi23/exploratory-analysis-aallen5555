# Load relevant libraries
library("dplyr")
library("ggplot2")

# Load data frame
full_dataset <- read.csv("fulldataframe.csv", stringsAsFactors = F)

# Create datta frame with number of vendors for each ethnicity
ethnicities <- full_dataset %>%
  group_by(Ethnicity) %>%
  summarise(num_vendors = n_distinct(Vendor_Formal_Name, na.rm = T))

# Create a bar chart to observe trends
ggplot(data = ethnicities) +
  geom_col(mapping = aes(
    x = Ethnicity,
    y = num_vendors,
    fill = Ethnicity
  )) +
  scale_fill_manual(
    values =
      c("#bc6c25", "#606c38", "#dda15e", "#e76f51", "#283618")
  ) +
  labs(
    title = "Representation of Ethnicities among NYC Businesses",
    x = "Ethnicity",
    y = "Number of Vendors"
  ) +
  theme(panel.background = element_rect(fill = "#f5ebe0"))
