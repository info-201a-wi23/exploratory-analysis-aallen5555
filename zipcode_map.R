# Install maps and zipcode package
install.packages("maps", repos = "maps_3.4.1.tar.gz")
install.packages("zipcodeR", repos = "zipcodeR_0.3.5.tar.gz")

# Load libraries
library("ggplot2")
library("maps")
library("dplyr")
library("zipcodeR")

full_dataset <- read.csv("fulldataframe.csv", colClasses = c(ZIP = "character"))

zipcodes <- full_dataset %>% group_by(Vendor_Formal_Name) %>% summarize(lat_long = reverse_zipcode(ZIP)[,c(8,9)])

full_dataset <- left_join(full_dataset, zipcodes, by = "Vendor_Formal_Name")

#load base map
state_shape <- map_data("state")

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    axis.title = element_blank(), 
    plot.background = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(), 
  )

#creating ny ny specific map
ggplot(data = state_shape) +
  geom_polygon(aes(x = long, 
                   y = lat, 
                   group = group)) +
  coord_map(xlim = c(-75, -71.6),ylim = c(41.8, 40)) +
  geom_point(data = full_dataset,
             aes(x = lat_long$lng,
                 y = lat_long$lat,
                 color = certification)) +
  blank_theme

#	Longitude: 71° 47' 25" W to 79° 45' 54" W Latitude: 40° 29' 40" N to 45° 0' 42" N

