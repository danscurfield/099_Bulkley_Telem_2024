# Read in, format, and plot Bulkley tagging data
# Created by Pete Moniz - winter 2023
# Updated by Dan Scurfield - Jan 2024

# Initial Setup ---------------------------------------------------------------

# Remove any objects from old R sessions
rm(list=ls(all=TRUE))

# Load packages
library(tidyverse)
library(IFRthemes)

# Make all times UTC and avoid auto re-display of timezone
Sys.setenv(TZ = "UTC") 

# Read In Data ----------------------------------------------------------------

# Read in tagging data and format dates and times
tagData <- read.csv("Data Input/099_Tag_Metadata_2023.csv", 
                    header = TRUE, 
                    stringsAsFactors = FALSE, na.strings = c("","unknown", "NA")) %>%
  #mutate_all(~str_replace_all(., "unknown", "")) %>%
  mutate(date = mdy(date),
         tagDateTime = as.POSIXct(paste0(date, time, sep = " "), 
                                  format="%Y-%m-%d %H:%M"),
         freq = paste("149.", freq, sep = ""),
         freqCode = paste(freq, code, sep = " ")) %>%
  dplyr::select(tagDateTime, freqCode, sex, forkLength) %>%
  # filter out recaptured fish
  filter(!(tagDateTime == "2023-07-10 09:15:00" & freqCode == "149.48 41"),
         !(tagDateTime == "2023-07-10 09:15:00" & freqCode == "149.48 42"),
         !(tagDateTime == "2023-07-10 09:15:00" & freqCode == "149.48 45"),
         !(tagDateTime == "2023-07-10 09:15:00" & freqCode == "149.48 46"),
         !(tagDateTime == "2023-07-10 09:15:00" & freqCode == "149.48 45"),
         !(tagDateTime == "2023-07-10 10:56:00" & freqCode == "149.48 50")) %>%
  #filter out tags with missing data
  filter(!(freqCode == "149.42 NA"),
         !(freqCode == "149.48 3?"),
         !(freqCode == "149.48 49?"),
         !(freqCode == "149.48 NA")) %>%
  #filter our tags not assigned
  filter(!(is.na(tagDateTime)))  %>%
  distinct(freqCode, .keep_all = TRUE) 

# Quick Summaries -------------------------------------------------------------

tagData %>% group_by(sex) %>%
  summarise(n = n(),
            meanFL = mean(forkLength, na.rm = TRUE),
            medianFL = median(forkLength, na.rm = TRUE),
            minFL = min(forkLength, na.rm = TRUE),
            maxFL = max(forkLength, na.rm = TRUE),
            sdFL = sd(forkLength, na.rm = TRUE))

# Plot Data -------------------------------------------------------------------

# We'll make two plots for male and female instead of faceting and stack them
# on top of each other.

freqPlot <- tagData %>%
  mutate(sex = case_when(sex == "m" ~ "Male",
                         sex == "f" ~ "Female", 
                         is.na(sex) ~ "Unknown")) %>%
  ggplot(aes(x = forkLength,
             # use the line below to remove the annoying borders on bins with no data
             y = ifelse(after_stat(count) > 0, after_stat(count), NA),
             fill = sex)) +
  geom_histogram(binwidth = 1, color = "black") +
  xlim(46, 66) +
  xlab("Fork Length (cm)") +
  ylab("Count") +
  facet_grid(~sex) +
  scale_fill_ifr() +
  theme_ifr() +
  theme(legend.position = "none")
  
freqPlot

ggsave(plot = freqPlot,
       "99_FreqPlot_2023.png",
       path = "Figures and Tables/",
       height = 4, width = 6.5, units = "in", dpi = 600)

