# Read in filtered data, format a bit, and plot detection histories
# Created by Pete Moniz - winter 2023
# Updated by Dan Scurfield - Feb 2024
# Updated by Dan Scurfield - June 2025

# Initial Setup ---------------------------------------------------------------

# Remove any objects from old R sessions
rm(list=ls(all=TRUE))

# Load packages
library(tidyverse)
library(IFRthemes)
library(lubridate)
library(ggforce)

# Make all times UTC and avoid auto re-display of timezone
Sys.setenv(TZ = "UTC") 

# Don't allow display of scientific notation
options(scipen = 999)

# Read In Data ----------------------------------------------------------------

# Read in mobile filtered telemetry data. 
plotData <- read.csv("Data Output/099_AllData_FinalCleaned_2024.csv", 
                              header = TRUE, 
                              stringsAsFactors = FALSE) %>%
  # Only use variables we need.
  dplyr::select(dateTime, freqCode, rkm, method, waterbody) %>%
  mutate(dateTime = as.POSIXct(dateTime),
        # rkm = ifelse(waterbody == "Morice Lake", 201, rkm), # This makes the plots look cleaner for mobile detections way upstream in the lake -REVIEW THIS
         location = case_when(rkm == -0.7 ~ "Fallback",
                              waterbody == "Tagging" ~ "Tagging",
                              waterbody == "Bulkley River" ~ "Bulkley River", 
                              waterbody == "Upper Bulkley River" ~ "Upper Bulkley\nRiver", 
                              waterbody == "Morice River" ~ "Morice River",
                              waterbody == "Atna River" ~ "Atna River",
                              waterbody == "Morice Lake" ~ "Morice Lake",
                              waterbody == "Nanika River" ~ "Nanika River"),
         location = as.character(location)) %>%
  mutate(freqCode = ifelse(str_length(str_extract(freqCode, "\\d+$")) == 2,
                         str_replace(freqCode, "(\\s)(\\d{2}$)", "\\10\\2"),
                         freqCode)) %>%
  arrange(freqCode)



# Plotting --------------------------------------------------------------------

# Set up variables for faceting and setting the number of pages and plots 
rows <- 3
cols <- 3
pages <- ceiling(length(unique(plotData$freqCode))/(rows*cols))


#Need to add IFR color palette
plot <- ggplot(plotData, aes(x = dateTime, y = rkm)) + 
  geom_line(size = 1) +
  geom_point(aes(color = location, shape = method, size = 0.5)) +
  scale_shape_manual(values=c(4, 16, 8, 17, 15))+ #order: death, fixed, lost tag, mobile, tagging
  scale_color_manual(values = c("#76a9f0", #atna
                                "#A8AABC", #bulkley
                              "#a40c01", #fallback
                              "#104692", #morice lake
                              "#fd5c50", #nanika
                              "#747687")) + #tagging
                              
  theme_ifr() +
  ylim(-5,250) +
  xlab("") +  
  ylab("River Kilometer") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 0.6, size = 12)) +
  scale_size(guide = 'none')

#TEST DELETE
# plot <- ggplot(plotData, aes(x = dateTime, y = rkm)) + 
#   geom_line(size = 1) +
#   geom_point(aes(color = location, shape = method, size = 0.5)) +
#   scale_shape_manual(values=c(15, 16, 7, 17, 13))+
#   scale_color_manual(values = c("#104692", #atna - tagging
#                                 "#A8AABC", #bulkley - bulkley
#                                 "#a40c01", #fallback - fallback
#                                 "#76a9f0", #morice lake - morice lake
#                                 "#747687", #morice river - atna river
#                                #nanika river - nanika river
#                                 "#fd5c50", #tagging
#                               )) + #upper bulkley
#   theme_ifr() +
#   ylim(-5,250) +
#   xlab("") +  
#   ylab("River Kilometer") +
#   theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 0.6, size = 12)) +
#   scale_size(guide = 'none')
# #scale_size(guide = 'none') + guides(color = guide_legend(override.aes = list(size = 2)), shape = guide_legend(override.aes = list(size = 2)))

for(i in 1:19) { #removed warn
  plot <- plot +
    ggforce::facet_wrap_paginate(~plotData$freqCode, 
                                 nrow = rows, 
                                 ncol = cols, 
                                 page = i) 
  ggsave(filename = sprintf("Figures and Tables/099_DetectionHistoryPlots_Page%s_2024.png", i),
         plot = plot, width=6.5, height=8.2)
}


  