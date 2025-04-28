# Read in, format, and run initial filtering of mobile and fixed telemetry data
# Created by Pete Moniz - winter 2023
# Updated by Dan Scurfield - Feb 2024

# Initial Setup ---------------------------------------------------------------

# Remove any objects from old R sessions
rm(list=ls(all=TRUE))

# Load packages
library(tidyverse)
library(IFRthemes)
library(lubridate)

# Make all times UTC and avoid auto re-display of timezone
Sys.setenv(TZ = "UTC") 


# Functions -------------------------------------------------------------------

# Modification of function used for reading in 2021 Orion data.
# Chose not to use the start.date argument and will filter by 
# tag datetime for each tag specifically later on.
uploadOrion <- function(path) {
  tempfile <- dir(path=path, full.names = TRUE)
  tempdat <- tempfile %>% 
    map(read.table, header=TRUE, stringsAsFactors=FALSE) %>% 
    reduce(rbind) %>%
    mutate(station = as.character(Site),
           date = lubridate::ymd(Date),
           dateTime = as.POSIXct(paste0(date, Time, sep = " ")),
           code = ifelse(Code < 10, paste0("0",Code), as.character(Code)),
           freqCode = paste(Freq, code, sep = " ")) %>%
    distinct(freqCode, dateTime, .keep_all = TRUE) %>%
    filter(date >= "2023-07-04") %>%
    dplyr::select(date, dateTime, station, freqCode, code = Code, power = Power)
}

# Read In Data ----------------------------------------------------------------

# Read in tagging data and format dates and times
tagData <- read.csv("Data Input/099_Tag_Metadata_2023.csv", 
                    header = TRUE, 
                    stringsAsFactors = FALSE) %>%
  mutate_all(~str_replace_all(., "unknown", "")) %>%
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
         !(freqCode == "149.48 NA"),
         !freqCode == "149.42 ")%>%
  #filter our tags not assigned
  filter(!(is.na(tagDateTime)))

# Read in mobile telemetry data. 
# This data was manually converted from txt to csv using Excel.
# All the extra info was also deleted using Excel so that just the detection data was left.
## Remove test tag data to not skew detection accuracy calculation

mobileData <- read.csv("Data Input/Radio Downloads/Mobile/099_Mobile_Telemetry_formatted_2023.csv", 
                       header = TRUE, 
                       stringsAsFactors = FALSE) %>%
  mutate(date = mdy(Date),
       dateTime = as.POSIXct(paste0(date, Time, sep = " "), format="%Y-%m-%d %H:%M"),
       code = ifelse(TagID.BPM < 10, paste0("0",TagID.BPM), as.character(TagID.BPM)),
       freqCode = paste(Freq..MHz., code, sep = " "),
       station = "Mobile",
       method = "Mobile") %>%
  filter(dateTime >= "2023-07-04" & dateTime <= "2023-10-26") %>%
  filter(!(freqCode == "149.5 212")) %>% #remove test tag (149.5 212)
  dplyr::select(date, dateTime, station, method, freqCode, freq = Freq..MHz., code = TagID.BPM, 
         power = RSSI, lat = Latitude, long = Longitude)


# Read in fixed station data, combine, and add location and rkm info
station1 <- uploadOrion(path = "Data Input/Radio Downloads/Station1")
station2 <- uploadOrion(path = "Data Input/Radio Downloads/Station2")
station4 <- uploadOrion(path = "Data Input/Radio Downloads/Station4")
station5 <- uploadOrion(path = "Data Input/Radio Downloads/Station5") %>%
  mutate(station = 5)
station6 <- uploadOrion(path = "Data Input/Radio Downloads/Station6") %>%
  mutate(station = 6)

# Bind and format to match mobile data
## Remove test tag data to not skew detection accuracy calculation

fixedData <- rbind(station1, station2, station4, station5, station6) %>%
  filter(!(freqCode == "149.5 212")) %>% #remove test tag (149.5 212)
  mutate(waterbody = case_when(station == "1" ~ "Bulkley River",
                              station == "2" ~ "Bulkley River",
                              station == "4" ~ "Nanika River",
                              station == "5" ~ "Atna River",
                              station == "6" ~ "Morice Lake"),
         # updated rkms using ArcGIS and the BC Freshwater Atlas shapefile
         rkm = case_when(station == "1" ~ -0.7,
                         station == "2" ~ 42,
                         station == "4" ~ 214,
                         station == "5" ~ 226.5,
                         station == "6" ~ 201),
         method = "Fixed")

# rm(station1, station2, station6, station5, station4)

# Mobile Tracking Data Filtering ----------------------------------------------


mobileDataCleaned <- mobileData %>% # Started with 9538 detections
  left_join(tagData, by = "freqCode", relationship = "many-to-many")  %>%
  filter(!(is.na(tagDateTime))) %>% # Remove detections with no tag info. Down to 1382
  filter(dateTime > tagDateTime) %>% # Remove detections before tag date and time (none). Still 1382
  filter(!(is.na(lat))) %>% # Remove detections with no lat, long. Down to 1080
  arrange(freqCode, dateTime) %>%
  filter(!(freqCode == lag(freqCode) & dateTime == lag(dateTime))) %>% # Remove repeated detections. Down to 659
  dplyr::select(-freq) # Remove freq column to reduce redundancy

# Write out csv of cleaned mobile tracking data
# Will assign rkms, combine with fixed station data, and clean further.
write.csv(mobileDataCleaned, 
          file = "Data Output/099_MobileTrackingData_1_InitialClean_2023.csv")

# Fixed Station Data Filtering ------------------------------------------------
fixedDataCleaned <- fixedData %>% #Started with 302,443 detections
  left_join(tagData, by = "freqCode", relationship = "many-to-many")  %>%
  filter(!(is.na(tagDateTime))) %>% # Remove detections with no tag info. Down to 220,551
  filter(dateTime > tagDateTime) # Remove detections before tag date and time. Down to 164,539



# Write out csv of initial clean fixed station data.
# Will combine with mobile data after rkms are assigned and cleaned further.
write.csv(fixedDataCleaned, 
          file = "Data Output/099_FixedStationData_InitialClean_2023.csv")


# False Positive Rates --------------------------------------------------------

# Calculate % of false positives after initial cleaning. Report this in the Array Performance section
#number of detections per station

fixedData %>%
  count(station)

# Lower Bulkley
fixedDataCleaned %>% filter(station == "1") %>% count() # 130228 remaining of 267184 (48.7%); so 51.3% removed.

# Lower Bulkley
fixedDataCleaned %>% filter(station == "2") %>% count() # 12215 remaining of 13054 (93.6%); so 6.4% removed.

# Morice Lake Outlet
fixedDataCleaned %>% filter(station == "6") %>% count() # 1 remaining of 6 (16.7%); so 83.3% removed.

# Nanika River
fixedDataCleaned %>% filter(station == "4") %>% count() # 21467 remaining of 21571 (99.9); so 0.1% removed.

# Atna River
fixedDataCleaned %>% filter(station == "5") %>% count() # 628 remaining of 628 (100%); so 0% removed.


# Additional Stats for recommendations ------------------------------------


# Check lag times of mobile data to see if there are any lags of 13 seconds.
mobileDataCleanedLagTimes <- mobileDataCleaned %>%
  arrange(dateTime) %>%
  group_by(freqCode) %>%
  mutate(lag = difftime(dateTime, lag(dateTime), units = "secs"))
# There are none.

fixedDataCleanedLagTimes <- fixedDataCleaned %>%
  arrange(dateTime) %>%
  group_by(freqCode) %>%
  mutate(lag = difftime(dateTime, lag(dateTime), units = "secs")) 
  
lostTags <- fixedDataCleanedLagTimes %>%
  filter(lag == 13)  %>%
  dplyr::select(!(lag)) %>%
  summarize(unique_freqCode = n_distinct(freqCode)            ,
            earliestDate = min(dateTime))
#18 lost tags detected on fixed stations

write.csv(lostTags, 
          file = "Data Output/099_LostTags_2023.csv")

#this is probably because scanning multiple frequencies - can pick up on 13s intervals. 

# Number of frequencies scanned per mobile survey. This should only be 2 but was 3 for
# most surveys. Lots of wasted scan time :(
freqMobileData <- mobileData %>%
  group_by(date) %>%
  count(freq)
  
