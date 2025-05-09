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
library(dplyr)

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
      mutate(File = .y)
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
    dplyr::select(date, dateTime, station, freqCode, code = Code, power = Power, File)
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
         latitude = Latitude,
         longitude = Longitude) %>%
  mutate(frequency = case_when( #convert channel numbers to frequency values
    Channel == 1 ~ "149.500",
    Channel == 2 ~ "149.320",
    Channel == 3 ~ "149.340",
    TRUE ~ "other")) %>%
  mutate(freqCode = paste(frequency, code, sep = " ")) %>% #cr4eate frewqcode column among others. 
  dplyr::select(date, dateTime, station, freqCode, code, power, latitude, longitude)
         
  
  #Unfortunately it appears the mobile scans were not monitoring the 149.340 frequency

# Read in fixed station data, combine, and add location and rkm info
station1 <- uploadOrion(path = "Data Input/Radio Downloads/Station1")
station2 <- uploadOrion(path = "Data Input/Radio Downloads/Station2")
station4 <- uploadOrion(path = "Data Input/Radio Downloads/Station4")
station5 <- uploadOrion(path = "Data Input/Radio Downloads/Station5")
station6 <- uploadOrion(path = "Data Input/Radio Downloads/Station6") %>%
  mutate(station = 6)

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
  
