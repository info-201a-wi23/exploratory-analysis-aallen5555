library(tidyverse)
library(IRdisplay)
library(dplyr)

full_dataset <- read.csv("fulldataframe.csv")

colors <- c( "#27A695", "#136037","#1A8049")

data <- full_dataset %>% 
        group_by(certification) %>% 
        summarize(counts = n(),
                  percentage = n()/nrow(full_dataset)) %>% 
        arrange(desc(counts)) %>% 
        slice(1:3)


pie <- ggplot(data = data, aes(x="", y = percentage, fill = certification)) + 
      coord_polar("y", start=0) +
      geom_col(color = "black") + 
      geom_text(aes(label = paste0(round(percentage*100), "%")),
                position = position_stack(vjust = 0.5)) +
      theme(panel.background = element_blank(),
            axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 18)) + 
      ggtitle("Pie chart of NY Certifications") +
      scale_fill_manual(values = colors)

pie
  