# Read in, format, and run initial filtering of mobile and fixed telemetry data
# Created by Pete Moniz - winter 2023
# Major updates by Dan Scurfield - Feb 2024
# Updated by Dan Scurfield - June 2025

# Initial Setup ---------------------------------------------------------------

# Remove any objects from old R sessions
rm(list=ls(all=TRUE))

# Load packages
library(tidyverse)
library(IFRthemes)
library(lubridate)
library(dplyr)
library(purrr)

# Make all times UTC and avoid auto re-display of timezone
Sys.setenv(TZ = "UTC") 


# Functions -------------------------------------------------------------------

#Function to read-in Orion data

# Modification of function used for reading in 2021 Orion data.
# Chose not to use the start.date argument and will filter by 
# tag datetime for each tag specifically later on.

uploadOrion <- function(path) {
  tempfile <- dir(path = path, full.names = TRUE)
  
  tempdat <- map2(tempfile, basename(tempfile), ~ {
    read.table(.x, header = TRUE, stringsAsFactors = FALSE) %>%
      mutate(file = .y)
  }) %>%
    reduce(rbind) %>%
    mutate(Freq = format(Freq, nsmall = 3),
           station = as.character(Site),
           date = lubridate::ymd(Date),
           dateTime = as.POSIXct(paste0(date, Time, sep = " ")),
           code = sprintf("%03d", Code),
           freqCode = paste(Freq, code, sep = " ")) %>%
    distinct(freqCode, dateTime, .keep_all = TRUE) %>%
    filter(date >= "2023-07-04") %>%
    dplyr::select(date, dateTime, station, freqCode, code = Code, power = Power, file)
}

#Function to read-in SRX1200 or SRX800 data

# Function to load radio telemetry data from a daily download folder
# Created 2020 by Annika then modified over the years by Katrina and Pete
# Modified 20240116 to work for both SRX800 and 1200 

# Works with SRX800 and SRX1200
# Works for both .TXT and .DTA files (can even be at in the same folder)

# Function Inputs:
# data: the folder with the radio files to be uploaded


# Required packages
#library(dplyr) - included above

srxUpload <- function(path) {
  
  files <- list.files(path = path)
  full.files <- list.files(path = path,
                           full.names = TRUE)
  
  # Pull the station number from the file names "F39_2022..." becomes "39"
  stations <- substr(files, 
                     start = 1,
                     stop = 6)
  
  # Determine the lines containing header data by finding "ID Only Records"
  skip <- full.files %>% 
    purrr::map(readLines) %>% 
    # Determine which lines to skip by selecting the line with Date (the row with col names)
    purrr::map(grep, pattern = "*ID Only Records*") %>% 
    unlist
  
  skipLines <- skip + 2
  
  # Create a function to read in the data files and skip the file-specific line
  ReadFun <- function(a, b, c, d) {
    read.table(a,
               skip = b,
               header = FALSE,
               fill = TRUE,
               stringsAsFactors = FALSE,
               skipNul = TRUE,
               col.names = c("Date",
                             "Time",
                             "Channel",
                             "TagID",
                             "Antenna",
                             "Power",
                             "Latitude",
                             "Longitude")) %>% 
      mutate(Station = c,
             File = d) 
  }
  
  # Use pmap to apply the ReadFun to all files 
  # pmap essentially creates a list where:
  # element 1: ReadFun(a = full.files[1], b = skipLines[1], c = stations[1], d = files[1])
  # element 2: ReadFun(a = full.files[2], b = skipLines[2], c = stations[2], d = files[2]), etc.
  dat0 <- purrr::pmap(list(full.files, skipLines, stations, files),
                      ReadFun)
  
  # Turn the list into a data.frame
  dat1 <- dat0 %>% 
    data.table::rbindlist(., fill = TRUE)  
  
  # Manipulate the data frame
  dat2 <- dat1 %>% 
    # Remove the column containing: "End of data" by filtering for NA TagIDs
    filter(!is.na(TagID)) %>% 
    mutate(Date = lubridate::mdy(Date, tz = "MST")) %>% 
    mutate(Hour = lubridate::hour(lubridate::hms(Time))) %>% 
    mutate(datetime = paste(Date, Time)) %>% 
    mutate(TagID = sprintf("%03d", TagID)) %>%
    # Rename stations
    # mutate(site.desc = suppressWarnings(forcats::fct_recode(as.factor(Station),
    #                                                         "33: Mainstem 2" = "33",
    #                                                         "34: Approach RB" = "34",
    #                                                         "35: Approach LB" = "35",
    #                                                         "36: Cofferdam" = "36",
    #                                                         "37: Tunnel Outlet" = "37",
    #                                                         "38: Entrance Aerial" = "38",
    #                                                         "39: Entrance Dipole" = "39",
    #                                                         "40: Entrance Pool Dipole" = "40",
    #                                                         "42: Cell 8 Dipole" = "42",
    #                                                         "41: Turning Basin Dipole" = "41",
    #                                                         "43: Vee-Trap Dipole" = "43",
    #                                                         "46: Tunnel Inlet" = "46",
    #                                                         "48: Entrance Dipole" = "48",
    #                                                         "49: Entrance Pool Dipole" = "49",
    #                                                         "99: TEST" = "99")))      %>% 
    # Remove error codes and test tags
    filter(!(TagID == 999))
  # filter(!(TagID == 728| TagID == 727))
  
  return(dat2)
  
}


# Read In Data ----------------------------------------------------------------

# Read in tagging data and format dates and times
tagData <- read_csv("Data Input/tagData.csv")


# Read in mobile telemetry data. 
# This data was manually converted from txt to csv using Excel.
# All the extra info was also deleted using Excel so that just the detection data was left.
## Remove test tag data to not skew detection accuracy calculation

mobileData <- srxUpload(path = "Data Input/Radio Downloads/Mobile") %>%
  rename(date = Date,
         time = Time,
         dateTime = datetime,
         code = TagID,
         power = Power, 
         station = Station,
         file = File,
         latitude = Latitude,
         longitude = Longitude) %>%
  mutate(frequency = case_when( #convert channel numbers to frequency values
    Channel == 1 ~ "149.500",
    Channel == 2 ~ "149.320",
    Channel == 3 ~ "149.340",
    TRUE ~ "other")) %>%
  mutate(freqCode = paste(frequency, code, sep = " ")) %>% #cr4eate frewqcode column among others. 
  dplyr::select(date, dateTime, station, freqCode, code, power, file, latitude, longitude)
         
  
  #Unfortunately it appears the mobile scans were not monitoring the 149.340 frequency

# Read in fixed station data, combine, and add location and rkm info
station1 <- uploadOrion(path = "Data Input/Radio Downloads/Station1") #2024-07-04 to 2024-09-09
station2 <- uploadOrion(path = "Data Input/Radio Downloads/Station2") #2024-07-04 to 2024-10-22
station4 <- uploadOrion(path = "Data Input/Radio Downloads/Station4") #2024-07-05 to 2024-09-10
station5 <- uploadOrion(path = "Data Input/Radio Downloads/Station5") #2024-07-05 to 2024-08-12 - data gap
station6 <- uploadOrion(path = "Data Input/Radio Downloads/Station6") #2024-07-05 to 2024-09-27 

#Error in the data discovered
#There is only one download for station 5 with zero detections: bulkley_station_5_10172024.txt
#Station 5 data is found in file bulkley_station_1_08122024.txt 2024-07-05 to 2024-08-12
#There is data from station 1 in file bulkley_station_1_08142024.txt from 2024-07-04 to 2024-08-14
#this makes me beleive the data labeled site 5 in bulkley_station_1_08122024.txt is correct
#this means there is missing data from 2024-08-12 to 2024-10-24.
#there may be a misnamed file with detection data from 2024-08-12 to 2024-10-24. 
#this file could be named like bulkley_station_1_08122024.txt but with 5's in 
#the Site column, the first 151 detections on this reader say site 5, until it 
#was changed in the field July 5, 2024, making the misnaming easy.

#Only Station 1 & 2 are in frequency 149.340
#10 tags on this frequency
#149.340 001
#149.340 002
#149.340 003
#149.340 004
#149.340 005
#149.340 006
#149.340 006
#149.340 007
#149.340 008
#149.340 009
#149.340 010

# Bind and format to match mobile data
## Remove test tag data to not skew detection accuracy calculation

fixedData <- rbind(station1, station2, station4, station5, station6) %>%
  filter(!(freqCode == "149.500 212")) %>% #remove test tag 212
  filter(!(freqCode == "149.500 211")) %>% #remove test tag 211
  filter(!(date <= "2024-07-03")) %>%
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

fixedData %>%
  distinct(freqCode)

#remove unneeded items from environment
rm(station1, station2, station6, station5, station4)


# Fixed Station Data Filtering ------------------------------------------------
fixedDataCleaned <- fixedData %>% #Started with 1,687,017 detections
  left_join(tagData, by = "freqCode", relationship = "many-to-many")  %>%
  filter(!(is.na(tagDateTime))) %>% # Remove detections with no tag info - 474,942 detections
  filter(dateTime > tagDateTime) # Remove detections before tag date and time - 465,634 detections



# Write out csv of initial clean fixed station data.
# Will combine with mobile data after rkms are assigned and cleaned further.
write.csv(fixedDataCleaned, 
          file = "Data Output/099_FixedStationData_InitialClean_2024.csv")


# Mobile Tracking Data Filtering ----------------------------------------------

#initial clean

mobileDat0 <- mobileData %>% # Started with 563 detections
  left_join(tagData, by = "freqCode", relationship = "many-to-many")  %>%
  # mutate(dateTime = as.POSIXct(date),
  #        tagDateTime = as.POSIXct(tagDateTime)) %>%
  filter(!(freqCode == "149.500 211")) %>% #removed test tag - 304 detections
  filter(!(is.na(tagDateTime))) %>% # Remove detections with no tag info - 275 detections
  filter(dateTime >= tagDateTime) %>% # Remove detections before tag date and time (none) - 275 detections
  #filter(!(is.na(lat))) %>% # Remove detections with no lat, long. Down to 1080
  arrange(dateTime, freqCode) %>%
  mutate(dateTime = as.POSIXct(dateTime)) %>%
  # filter(!(freqCode == lag(freqCode) & dateTime == lag(dateTime))) %>% # Remove repeated detections. Down to 659
  dplyr::select(-c("date", "file")) #Remove date and file column

#second clean
##include lat/long for detections with NA's by assigning nearest values in time

# Create a separate table with known lat/lon
known_locs <- mobileDat0 %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  select(dateTime, latitude, longitude)

# Helper function to find nearest known location
get_nearest_location <- function(dt, known_locs) {
  # Find the row in known_df with minimum time difference
  nearest <- known_locs %>%
    slice(which.min(abs(difftime(dateTime, dt, units = "secs"))))
  return(nearest %>% select(latitude, longitude))
}

mobileDat1 <- mobileDat0 %>%
  rowwise() %>%
  mutate(
    nearest = if (is.na(latitude) | is.na(longitude)) list(get_nearest_location(dateTime, known_locs)) else list(NULL),
    latitude = if (is.na(latitude)) nearest$latitude else latitude,
    longitude = if (is.na(longitude)) nearest$longitude else longitude
  ) %>%
  ungroup() %>%
  select(-nearest)

mobileDataCleaned <- mobileDat1

#remove unneeded items in environment
rm(known_locs, mobileDat0, mobileDat1)


# Write out csv of cleaned mobile tracking data
# Will assign rkms, combine with fixed station data, and clean further.
write.csv(mobileDataCleaned, 
          file = "Data Output/099_MobileTrackingData_1_InitialClean_2024.csv")


# False Positive Rates --------------------------------------------------------

# Calculate % of false positives after initial cleaning. Report this in the Array Performance section
#number of detections per station

fixedData %>%
  count(station)

# Lower Bulkley
fixedDataCleaned %>% filter(station == "1") %>% count() # 33324 remaining of 40170 (83.0%); so 17.0% removed.

# Lower Bulkley
fixedDataCleaned %>% filter(station == "2") %>% count() # 305290 remaining of 1513306 (20.2%); so 79.8% removed.

# Morice Lake Outlet
fixedDataCleaned %>% filter(station == "6") %>% count() # 64286 remaining of 70059 (91.8%); so 8.2% removed.

# Nanika River
fixedDataCleaned %>% filter(station == "4") %>% count() # 61682 remaining of 62402 (98.8); so 1.2% removed.

# Atna River
fixedDataCleaned %>% filter(station == "5") %>% count() # 1052  remaining of 1080 (97.4%); so 2.6% removed.


# Additional Stats for recommendations ------------------------------------


# Check lag times of mobile data to see if there are any lags of 13 seconds.
mobileDataCleanedLagTimes <- mobileDataCleaned %>%
  arrange(dateTime) %>%
  group_by(freqCode) %>%
  mutate(lag = difftime(dateTime, lag(dateTime), units = "secs"))

fishDeath <- mobileDataCleanedLagTimes %>%
  filter(lag == 13 | lag == 26)  %>%
  dplyr::select(!(lag)) %>%
  summarize(unique_freqCode = n_distinct(freqCode)            ,
            earliestDate = min(dateTime))


# 13s tags (fish death)
# 149.500 008 at 2024-10-01 17:58:23
# 149.500 025 at 2024-10-01 17:59:17 (2x)

#26s tags (fish death)
# 149.500 071 at 2024-10-01 19:29:55
# 149.500 033 at 2024-10-01 19:38:50
# 149.500 034
# 149.500 038 at 2024-10-01 19:30:59
# 149.500 014 at 2024-10-01 19:32:21

write.csv(fishDeath, 
          file = "Data Output/099_fishDeath_2024.csv")


fixedDataCleanedLagTimes <- fixedDataCleaned %>%
  arrange(dateTime) %>%
  group_by(freqCode) %>%
  mutate(lag = difftime(dateTime, lag(dateTime), units = "secs")) 


lostTags <- fixedDataCleanedLagTimes %>%
  filter(lag == 13)  %>%
  dplyr::select(!(lag)) %>%
  summarize(unique_freqCode = n_distinct(freqCode)            ,
            earliestDate = min(dateTime))

#15 lost tags detected on fixed stations
#I am going to look at the fixed data to see if they are seen upstream and 
#determine whether they are lost (or dead) or alive fish.
#alive
#	149.320 127 seen upstream (S2)
# 149.340 001 seen upstream (S2)
# 149.340 002 seen upstream (S2)
# 149.340 003 seen upstream (S2)
# 149.340 004 seen upstream (S2)
# 149.340 006 seen upstream (S2)
# 149.340 008 seen upstream (S2)
# 149.500 001 seen upstream (S5 Atna)
# 149.500 002 seen upstream (S5 Atna)
# 149.500 018 seen upstream (S4 Nanika)
# 149.500 034 seen upstream (S6 Morice L)
# 149.500 044 seen upstream (S4 Nanika)
# 149.500 071 seen upstream (S4 Nanika)


#lost (dead)
#149.320 146 only seen at S2 2024-08-24 10:10:14
#149.500 100 only seen at S2 since 2024-07-31 10:50:28



write.csv(lostTags, 
          file = "Data Output/099_LostTags_2024.csv")

#I'm not going to rule out these fish entirely because many of them are later
#detected upstream - meaning the "death signal" may have failed. 

# the mobile survey recorded data on 2 frequncies only:
# 149.500 & 149.320

# tags also put out in 149.340
