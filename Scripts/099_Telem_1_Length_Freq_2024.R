# Read in, format, and plot Bulkley tagging data
# Created by Pete Moniz - winter 2023
# Updated by Dan Scurfield - Jan 2024
# updated by Dan Scurfield - Apr 2025

# Initial Setup ---------------------------------------------------------------

# Remove any objects from old R sessions
rm(list=ls(all=TRUE))

# Load packages
library(tidyverse)
library(IFRthemes)
library(lubridate)

# Make all times UTC and avoid auto re-display of timezone
Sys.setenv(TZ = "UTC") 

# Read In Data ----------------------------------------------------------------

# Read in tagging data and format dates and times
tagData <- read.csv("Data Input/099_Tag_Metadata_2024.csv", 
                    header = TRUE, 
                    stringsAsFactors = FALSE, na.strings = c("","unkn", "NA")) %>%
  mutate(date = Date,
         time = ("00:00:00"),
         freq = Frequency,
         code = Tag.Number,
         sex = Gender,
         forkLength = Length) %>%
  #Make all sex data upper case
  mutate(sex = str_to_upper(sex)) %>%
  #Manually input tags with missing frequency data
  mutate(freq = case_when(
    code == "19" ~ 500,  #code 19 only detected on 149.500
    code == "88" ~ 500,  #code 88 only detected on 149.500
    code == "199" ~ 320,  #code 199 only detected on 149.320
    code == "193" ~ 320,  #code 193 only detected on 149.320
    code == "192" ~ 320,  #code 192 only detected on 149.320
    code == "191" ~ 320,  #code 191 only detected on 149.320
    code == "197" ~ 320,  #code 197 only detected on 149.320
    code == "194" ~ 320,  #code 194 only detected on 149.320
    TRUE ~ freq)) %>%
  #Manually input date for tags with missing tagging date
  # Tag 149.500 13
  mutate(date = case_when(
    code == "13" & freq == "500" ~ "2024-07-12", #earliest date of tagging
    TRUE ~ date)) %>%
  #Make tagDateTime and freqCode columns
  mutate(tagDateTime = as.POSIXct(paste0(date, time, sep = " "), 
                                  format="%Y-%m-%d %H:%M"),
         freq = paste("149.", freq, sep = ""),
         freqCode = paste(freq, code, sep = " ")) %>%
  dplyr::select(tagDateTime, freqCode, sex, forkLength) %>%
  # filter out recaptured fish - no recaptured fish in 2024
  #filter out tags with missing data - solved by sleuthing detection data
  #filter our tags not assigned
  filter(!(is.na(tagDateTime)))  %>%
  distinct(freqCode, .keep_all = TRUE) 

#Save cleaned tag data
write_csv(tagData, "Data Input/tagData.csv")

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
  mutate(sex = case_when(sex == "M" ~ "Male",
                         sex == "F" ~ "Female", 
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
       "99_FreqPlot_2024.png",
       path = "Figures and Tables/",
       height = 4, width = 6.5, units = "in", dpi = 600)

