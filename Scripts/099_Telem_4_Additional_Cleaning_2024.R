# Read in, run additional filtering of mobile and fixed telemetry data
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

# Don't allow display of scientific notation
options(scipen = 999)


# Read In Data ----------------------------------------------------------------

# Read in mobile filtered telemetry data. 
mobileDataCleaned <- read.csv("Data Output/099_MobileTrackingData_2_InitialClean_RKM_2023.csv", 
                       header = TRUE, 
                       stringsAsFactors = FALSE) %>%
  mutate(date = ymd(date),
         dateTime = ymd_hms(dateTime),
         tagDateTime = ymd_hms(tagDateTime)) %>%
  dplyr::select(-X)

# Read in fixed station data
fixedDataCleaned <- read.csv("Data Output/099_FixedStationData_InitialClean_2023.csv", 
                      header = TRUE, 
                      stringsAsFactors = FALSE) %>%
  mutate(date = ymd(date),
         dateTime = ymd_hms(dateTime),
         tagDateTime = ymd_hms(tagDateTime),
         station = as.character(station),
         lat = case_when(station == "1" ~ 55.01835,
                         station == "2" ~ 54.78871,
                         station == "4" ~ 54.04742,
                         station == "5" ~ 54.014545,
                         station == "6" ~ 54.10783,),
         long = case_when(station == "1" ~ -127.3189,
                          station == "2" ~ -127.1463,
                          station == "4" ~ -127.4264,
                          station == "5" ~ -127.74321,
                          station == "6" ~ -127.4250)) %>%
  dplyr::select(-X)

# Read in tagging data, same as before, except we'll also make some additional 
# variables so that we can combine these with the fixed station data.
# This will allow us to more easily calculate movement rates after release and
# filter out improbable movement rates.
tagData <- read.csv("Data Input/099_Tag_Metadata_2023.csv", 
                    header = TRUE, 
                    stringsAsFactors = FALSE, na.strings = c("","unknown", "NA")) %>%
  mutate(date = mdy(date),
         tagDateTime = as.POSIXct(paste0(date, time, sep = " "), 
                                  format="%Y-%m-%d %H:%M"),
         #code = ifelse(code < 10, paste0("0",code), as.character(code)),
         freq = paste("149.", freq, sep = ""),
         freqCode = paste(freq, code, sep = " "),
         code = as.numeric(code), # 2 tag code numbers have "?"
         dateTime = tagDateTime,
         rkm = 0,
         waterbody = "Tagging",
         method = "Tagging",
         station = "Tagging",
         power = NA,
         lat = NA,
         long = NA) %>%
  dplyr::select(date, dateTime, freqCode, code, power, waterbody, rkm, method, station,
                tagDateTime, sex, forkLength, lat, long) %>%
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
          !(freqCode == "149.48 NA")) 


lostTags <- read.csv("Data Output/099_LostTags_2023.csv", 
                    header = TRUE, 
                    stringsAsFactors = FALSE)  %>%
  mutate(method = "Lost Tag") %>%
  dplyr::select(-X)

# Additional Filtering --------------------------------------------------------

# First let's remove erroneous detections when the difference between detections 
# for the same tag were less than the tag’s pulse rate (5 seconds).
# This removes 10,770 detections.
allData <- rbind(fixedDataCleaned, mobileDataCleaned, tagData) %>%
  arrange(dateTime) %>%
  group_by(freqCode) %>%  arrange(dateTime) %>%
  group_by(freqCode) %>%
  mutate(lag = difftime(dateTime, lag(dateTime), units = "secs")) %>%
  # mutate(lag = as.numeric(difftime(dateTime, lag(dateTime), units = "secs"))) %>%
  filter((lag >= 5 | is.na(lag))) %>% # is.na(lag) keeps the tagging data since those don't have a previous detection.
  mutate(method = ifelse(lag == 13 & (method == "Fixed" | method == "Mobile"), "Lost Tag", method)) %>% #these are lost tags / dead fish
  ungroup() %>%
  filter(!(freqCode == "149.42 01" & rkm == 214 & dateTime >= "2023-07-08 18:34:51"),
         !(freqCode == "149.42 02" & rkm == 214 & dateTime == "2023-07-07 19:19:16"), 
         !(freqCode == "149.48 24" & dateTime == "2023-09-06 08:09:00"), 
         !(freqCode == "149.48 29" & station == "Mobile" & dateTime >= "2023-09-06 08:02:00" & dateTime <= "2023-09-06 08:09:00")) #these removed because falsely detected immediately


# Next we'll remove detections of tags downstream of Nanika River once the fish 
# has already made it to Nanika River. The Nanika River fixed station had a 
# relatively low FP rate and high efficiency, so I have pretty high confidence that
# we can consider these real detections.
# This process removes 1954 detections.
allData1 <- allData %>%
  arrange(dateTime) %>% 
  group_by(freqCode, waterbody) %>%
  mutate(nanikaSwitch = ifelse(waterbody == "Nanika River" & row_number() == 1, 1,0)) %>%
  ungroup() %>%
  arrange(dateTime) %>%
  group_by(freqCode) %>%
  mutate(reachedNanika = cumsum(nanikaSwitch)) %>%
  filter(!(reachedNanika == 1 & waterbody != "Nanika River")) %>%
  ungroup() %>%
  dplyr::select(-nanikaSwitch, -reachedNanika)

# Next we'll remove detections of tags downstream of Atna River once the fish 
# has already made it to Atna River. The Atna River fixed station had an 
# extremely low FP (false positive) rate but poor efficiency, so it can be
# assumed that the fishdetected are all true.
# we can consider these real detections.
# This process removes 338 detections.
allData2 <- allData1 %>%
  arrange(dateTime) %>% 
  group_by(freqCode, waterbody) %>%
  mutate(atnaSwitch = ifelse(waterbody == "Atna River" & row_number() == 1, 1,0)) %>%
  ungroup() %>%
  arrange(dateTime) %>%
  group_by(freqCode) %>%
  mutate(reachedAtna = cumsum(atnaSwitch)) %>%
  filter(!(reachedAtna == 1 & waterbody != "Atna River")) %>%
  ungroup() %>%
  dplyr::select(-atnaSwitch, -reachedAtna)

# Now lets add a distance traveled, lag in days between detections, and a rate
# in rkm/day to manually filter out detections with unrealistic rates of travel
allData3 <- allData2 %>%
  arrange(dateTime) %>%
  group_by(freqCode) %>%
  mutate(lag = as.numeric(difftime(dateTime, lag(dateTime), units = "secs"))) %>%
  filter((lag >= 5 | is.na(lag))) %>% # is.na(lag) keeps the tagging data since those don't have a previous detection.
  ungroup() %>%
  arrange(dateTime) %>%
  group_by(freqCode) %>%
  mutate(distance = abs(rkm - lag(rkm)),
         lagDays = as.numeric(difftime(dateTime, lag(dateTime), units = "days")),
         rate = distance/lagDays) %>%
  ungroup()

impossible_rates <- allData3 %>%
  filter(rate > 100 & distance > 100)

# Now that we've removed a few bogus detections and calculated a rate in rkm/day,
# we can use those rates to filter out detections that suggest unrealistic movement rates.

# I did this manually since there weren't that many tags to look through, 
# but I'm sure it could be automated with a "while" loop where you:
#   1) Calculate distance, lag in days, and rates of movement between detections for each tag;
#   2) Remove the very first detection for each tag with an impossible rate (ex: movement of > 100 rkm at > 100 rkm/day);
#   3) Re-calculate distance, lag in days, and rates of movement between detections after first detection removed;
#   4) Remove next detection for each tag with an impossible rate;
#   5) Then keep doing those steps until no more detections are removed from any tags.
  
# Most of the obvious false detections with impossible movement rates are from the Morice Lake Outlet fixed (rkm 201) station in 2022. 
# This isn't surprising considering how noisy this station was (99% of dets removed during initial cleaning step).
# Seems to be lots of false detections at Morice lake outlet (83%) and lower bulkley (93%) station in 2023. 


# This step removes __ detections.
## Currently very messy but keep this adding to this
## this removes 9629 detections
allData4 <- allData3 %>%
  arrange(dateTime) %>%
  group_by(freqCode) %>%
  ungroup() %>%
  arrange(dateTime) %>%
  group_by(freqCode) %>%
  mutate(distance = abs(rkm - lag(rkm)),
         lagDays = as.numeric(difftime(dateTime, lag(dateTime), units = "days")),
         rate = distance/lagDays) %>%
  #filter our lost tags
  filter(
!(freqCode == "149.42 01" & (method == "Fixed" | method == "Mobile") & dateTime >= "2023-09-13 16:07:59"),
!(freqCode == "149.42 02" & (method == "Fixed" | method == "Mobile") & dateTime >= "2023-10-09 10:52:19"),
!(freqCode == "149.42 02" & (method == "Fixed" | method == "Mobile") & dateTime == "2023-10-09 10:52:06"), #detection during lost tag signal
!(freqCode == "149.42 03" & (method == "Fixed" | method == "Mobile") & dateTime >= "2023-10-09 13:55:12"),
!(freqCode == "149.42 09" & (method == "Fixed" | method == "Mobile") & dateTime >= "2023-08-15 15:47:45"),
!(freqCode == "149.42 100" & (method == "Fixed" | method == "Mobile") & dateTime >= "2023-09-15 14:39:00"),
!(freqCode == "149.42 100" & (method == "Fixed" | method == "Mobile") & dateTime >= "2023-09-15 14:02:59"), #detection during lost tag signal
!(freqCode == "149.42 150" & (method == "Fixed" | method == "Mobile") & dateTime >= "2023-09-09 19:57:39"),
!(freqCode == "149.42 150" & (method == "Fixed" | method == "Mobile") & dateTime >= "2023-09-08 19:40:57"), #detection during lost tag signal
!(freqCode == "149.42 62" & (method == "Fixed" | method == "Mobile") & dateTime >= "2023-10-09 16:02:59"),
!(freqCode == "149.42 68" & (method == "Fixed" | method == "Mobile") & dateTime >= "2023-09-15 16:50:01"),
!(freqCode == "149.42 68" & (method == "Fixed" | method == "Mobile") & dateTime >= "2023-09-13 15:07:26"), #detection during lost tag signal
!(freqCode == "149.42 77" & (method == "Fixed" | method == "Mobile") & dateTime >= "2023-08-06 13:57:42"),
!(freqCode == "149.42 77" & (method == "Fixed" | method == "Mobile") & dateTime >= "2023-08-06 13:55:32"), #detection during lost tag signal
!(freqCode == "149.48 101" & (method == "Fixed" | method == "Mobile") & dateTime >= "2023-08-05 00:04:51"),
!(freqCode == "149.48 109" & (method == "Fixed" | method == "Mobile") & dateTime >= "2023-08-15 04:59:56"),
# !(freqCode == "149.48 116" & (method == "Fixed" | method == "Mobile") & dateTime >= "2023-08-16 12:14:05"),
!(freqCode == "149.48 22" & (method == "Fixed" | method == "Mobile") & dateTime >= "2023-07-14 11:12:46"),
#!(freqCode == "149.48 29" & (method == "Fixed" | method == "Mobile") & dateTime >= "2023-07-22 11:31:49"),
!(freqCode == "149.48 34" & (method == "Fixed" | method == "Mobile") & dateTime >= "2023-08-25 12:19:06"),
!(freqCode == "149.48 44" & (method == "Fixed" | method == "Mobile") & dateTime >= "2023-07-14 11:58:21"), #sketchy tag
!(freqCode == "149.48 46" & (method == "Fixed" | method == "Mobile") & dateTime >= "2023-07-29 09:37:06"), #sketchy tag
!(freqCode == "149.48 47" & (method == "Fixed" | method == "Mobile") & dateTime >= "2023-09-13 12:32:53")) %>%
  #filter out odd detections - removes 1816 detections
  filter(
         # !(freqCode == "149.42 01" & rkm == 42.0 & dateTime == "2023-10-09 11:16:11"), #very suspect tag
         !(freqCode == "149.42 02" & station == "Mobile"),
         # !(freqCode == "149.42 02" & rkm == 42.0 & dateTime == "2023-10-09 10:52:06"), #very suspect tag - 13s interval = dead
         !(freqCode == "149.42 03" & rkm == -0.7 & dateTime >= "2023-08-08 14:51:16"), #03 removing out some possibly real detections
         !(freqCode == "149.42 06" & dateTime == "2023-08-30 05:43:00"),
         !(freqCode == "149.42 06" & dateTime == "2023-09-25 09:14:00"),
         !(freqCode == "149.42 08" & dateTime == "2023-09-27 12:09:00"),
         !(freqCode == "149.42 15" & dateTime >= "2023-09-22 09:52:00"),  
         !(freqCode == "149.42 15" & dateTime <= "2023-09-26 06:08:00"), #removes many (~15) mobile detections
         !(freqCode == "149.42 17" & dateTime == "2023-09-06 08:07:00"),
         !(freqCode == "149.42 18" & dateTime == "2023-08-02 15:44:55"),
         !(freqCode == "149.42 19" & dateTime == "2023-08-02 15:44:55"),
         !(freqCode == "149.42 21" & dateTime == "2023-08-28 15:30:53"),
         !(freqCode == "149.48 22" & dateTime == "2023-09-22 03:35:00"),
         !(freqCode == "149.48 22" & dateTime == "2023-09-25 06:02:00"),
         !(freqCode == "149.48 23" & dateTime == "2023-08-30 05:40:00"),
         !(freqCode == "149.48 24" & rkm == -0.7 & dateTime >= "2023-09-01 10:55:28"),
         !(freqCode == "149.48 24" & station == "Mobile" & dateTime >= "2023-09-22 03:49:00"),
         !(freqCode == "149.48 26" & dateTime == "2023-09-28 11:03:00"),
         #!(freqCode == "149.48 27" & rkm == -0.7 & dateTime >= "2023-07-10 01:19:46"), #all singular detections
         !(freqCode == "149.48 28" & rkm == -0.7 & dateTime >= "2023-08-10 21:43:05"), #all singular detections
         !(freqCode == "149.48 29" & rkm == -0.7 & dateTime >= "2023-09-15 16:57:14"),
         !(freqCode == "149.48 29" & station == "Mobile" & dateTime == "2023-09-22 03:46:00"),
         !(freqCode == "149.48 29" & station == "Mobile" & dateTime >= "2023-09-22 04:46:00" & dateTime <= "2023-09-22 04:58:00"), 
         !(freqCode == "149.48 30" & rkm == -0.7 & dateTime >= "2023-08-28 15:32:22"),
         !(freqCode == "149.48 34" & rkm == -0.7 & dateTime >= "2023-08-19 18:36:32"),
         !(freqCode == "149.48 38" & rkm == -0.7 & dateTime >= "2023-07-13 18:28:55"),
         !(freqCode == "149.48 39" & rkm == -0.7 & dateTime >= "2023-08-02 11:51:46"),
         !(freqCode == "149.48 40" & rkm == -0.7 & dateTime >= "2023-08-23 17:57:40"),
         !(freqCode == "149.48 41" & rkm == -0.7 & dateTime >= "2023-08-03 13:15:54"),
         !(freqCode == "149.48 44" & dateTime == "2023-07-09 17:10:17"),
         !(freqCode == "149.48 44" & rkm == 42.0 & dateTime == "2023-07-06 09:30:47"), #44 is a suspect tag
         !(freqCode == "149.48 46" & dateTime == "2023-07-08 18:46:49"),
         !(freqCode == "149.48 46" & rkm == 42.0 & dateTime >= "2023-07-06 09:32:13"), #46 is a suspect tag
         !(freqCode == "149.48 47" & rkm == -0.7 & dateTime >= "2023-08-15 08:49:40"),
         !(freqCode == "149.48 49" & rkm == -0.7 & dateTime >= "2023-08-26 20:06:42"),
         !(freqCode == "149.48 51" & rkm == 42.0 & dateTime == "2023-07-24 09:41:02"),
         !(freqCode == "149.48 51" & rkm == -0.7 & dateTime == "2023-07-23 17:02:59"),
         !(freqCode == "149.48 53" & rkm == -0.7 & dateTime >= "2023-07-30 08:54:26"),
         !(freqCode == "149.48 54" & rkm == -0.7 & dateTime >= "2023-08-03 15:40:43"),
         !(freqCode == "149.48 56" & rkm == -0.7 & dateTime >= "2023-08-02 11:59:40"),
         !(freqCode == "149.48 57" & rkm == -0.7 & dateTime >= "2023-07-30 08:58:30"),
         !(freqCode == "149.48 58" & dateTime >= "2023-09-22 05:08:00" & dateTime <= "2023-09-25 06:43:00"),
         !(freqCode == "149.48 59" & station == "Mobile" & dateTime >= "2023-09-22 04:24:00" & dateTime == "2023-09-25 09:14:00"),
         !(freqCode == "149.48 59" & rkm == -0.7 & dateTime >= "2023-08-21 17:00:30"),
         !(freqCode == "149.42 61" & dateTime == "2023-09-06 14:48:03"),
         !(freqCode == "149.42 62" & dateTime == "2023-07-11 03:35:42"),
         !(freqCode == "149.42 63" & dateTime == "2023-09-06 08:07:00"), #check and 64
         !(freqCode == "149.42 67" & rkm == -0.7 & dateTime >= "2023-09-13 16:07:45"),
         !(freqCode == "149.42 73" & station == "Mobile" & dateTime <= "2023-09-22 04:46:00"),
         !(freqCode == "149.42 77" & rkm == -0.7 & dateTime <= "2023-08-31 14:35:09"), #check
         !(freqCode == "149.42 78" & rkm == -0.7 & dateTime >= "2023-08-23 14:31:18"),
         !(freqCode == "149.42 82" & dateTime == "2023-09-06 08:06:00"),
         !(freqCode == "149.42 82" & dateTime == "2023-09-06 08:07:00"),
         !(freqCode == "149.42 83" & dateTime == "2023-09-13 16:08:03"),
         !(freqCode == "149.42 84" & rkm == -0.7 & dateTime >= "2023-08-30 15:40:46"),
         !(freqCode == "149.42 86" & rkm == -0.7 & dateTime >= "2023-07-18 16:01:13"),
         !(freqCode == "149.42 88" & rkm == -0.7 & dateTime >= "2023-08-29 08:38:01"),
         !(freqCode == "149.42 89" & rkm == -0.7 & dateTime >= "2023-08-23 15:45:15"),
         !(freqCode == "149.42 90" & rkm == -0.7 & dateTime >= "2023-07-26 09:04:57"),
         !(freqCode == "149.42 91" & rkm == -0.7 & dateTime == "2023-09-02 15:32:17"),
         !(freqCode == "149.42 92" & rkm == -0.7 & dateTime >= "2023-08-18 08:50:09"),
         !(freqCode == "149.42 93" & station == "Mobile" & dateTime >= "2023-09-22 04:07:00"),
         !(freqCode == "149.42 94" & rkm == -0.7 & dateTime >= "2023-09-05 09:08:54"),
         !(freqCode == "149.42 95" & rkm == -0.7 & dateTime >= "2023-07-23 15:32:06"),
         !(freqCode == "149.42 96" & rkm == -0.7 & dateTime == "2023-08-23 21:30:49"),
         !(freqCode == "149.48 101" & station == "Mobile" & dateTime == "2023-09-22 04:49:00"),
         !(freqCode == "149.48 102" & rkm == -0.7 & dateTime >= "2023-07-29 18:15:43"),
         !(freqCode == "149.48 103" & rkm == -0.7 & dateTime >= "2023-08-10 20:32:44"),  
         !(freqCode == "149.48 104" & rkm == -0.7 & dateTime >= "2023-07-29 18:15:43"), 
         !(freqCode == "149.48 106" & rkm == 42.0 & dateTime == "2023-07-15 01:30:34"), 
         !(freqCode == "149.48 109" & rkm == -0.7 & dateTime >= "2023-09-02 17:29:06"),
         !(freqCode == "149.48 115" & rkm == -0.7 & dateTime >= "2023-08-24 10:18:47"),  
         !(freqCode == "149.48 116" & rkm == -0.7 & dateTime >= "2023-08-18 08:58:52"),  
         !(freqCode == "149.48 116" & rkm == 42 & dateTime >= "2023-08-14 14:36:50"),  
         !(freqCode == "149.48 119" & rkm == -0.7 & dateTime >= "2023-08-08 22:38:15"),
         !(freqCode == "149.48 120" & rkm == -0.7 & dateTime >= "2023-08-16 07:18:42"), 
         !(freqCode == "149.48 121" & rkm == -0.7 & dateTime >= "2023-08-17 04:58:56"),
         !(freqCode == "149.48 122" & rkm == -0.7 & dateTime >= "2023-09-02 17:24:45"),
         !(freqCode == "149.48 125" & rkm == -0.7 & dateTime >= "2023-08-05 00:31:35"),   
         !(freqCode == "149.48 126" & rkm == -0.7 & dateTime >= "2023-08-12 21:06:15"), 
         !(freqCode == "149.48 127" & rkm == -0.7 & dateTime >= "2023-08-15 17:40:36"), 
         !(freqCode == "149.48 129" & rkm == -0.7 & dateTime >= "2023-09-05 16:48:32"), 
         !(freqCode == "149.48 129" & rkm == -0.7 & dateTime >= "2023-07-27 15:51:16"), 
         !(freqCode == "149.48 130" & rkm == -0.7 & dateTime >= "2023-08-01 12:41:06"), 
         !(freqCode == "149.48 138" & dateTime >= "2023-09-05 18:42:23"),
         !(freqCode == "149.48 140" & rkm == -0.7 & dateTime >= "2023-08-13 11:00:30"),
         !(freqCode == "149.48 149" & rkm == -0.7 & dateTime >= "2023-09-05 16:44:05"),
         !(freqCode == "149.42 148" & rkm == -0.7 & dateTime >= "2023-08-14 08:32:49"),
         !(freqCode == "149.42 157" & rkm == -0.7 & dateTime >= "2023-07-31 16:11:25"),
         !(freqCode == "149.42 158" & rkm == -0.7 & dateTime == "2023-09-13 16:01:23"),
         !(freqCode == "149.42 159" & rkm == -0.7 & dateTime == "2023-08-09 16:00:27"),
         !(freqCode == "149.42 159" & rkm == 42.0 & dateTime == "2023-07-19 01:28:42")) %>%
  ungroup()


allData5 <- allData4 %>%
  arrange(dateTime) %>%
  group_by(freqCode) %>%
  ungroup() %>%
  arrange(dateTime) %>%
  group_by(freqCode) %>%
  mutate(distance = abs(rkm - lag(rkm)),
         lagDays = as.numeric(difftime(dateTime, lag(dateTime), units = "days")),
         rate = distance/lagDays) %>%
  ungroup()
      

impossible_rates <- allData5 %>%
  filter(rate > 100 & distance > 100)



# Given the high noise levels at the Morice Lake Outlet fixed station, we'll manually remove
# detections of tags that were only detected once at that station, or for code 26,
# which was detected twice but only on two different dates.
## Not done in 2023, since very few true detections 

# allData3 <- allData2 %>%
#   filter(!(freqCode == "149.46 12" & dateTime == "2022-10-14 14:20:41"),
#          !(freqCode == "149.46 14" & dateTime == "2022-10-24 13:29:34"),
#          !(freqCode == "149.46 21" & dateTime == "2022-10-24 13:39:44"),
#          !(freqCode == "149.46 25" & dateTime == "2022-10-27 04:29:27"),
#          !(freqCode == "149.46 28" & dateTime == "2022-10-27 20:14:50"),
#          !(freqCode == "149.46 40" & dateTime == "2022-10-28 15:16:42"),
#          !(freqCode == "149.46 26" & dateTime == "2022-10-27 06:41:43"),
#          !(freqCode == "149.46 26" & dateTime == "2022-10-31 18:21:51"))


# Now let's see how many detections there were per tag at the morice lake outlet
moriceLakeOutlet <- allData5 %>% 
  filter(rkm == 201) %>% count(freqCode)
# Didn't use this info for filtering, but maybe we should?
# Here are some notes:
# tag 149.48 24 detected one time - looks legit.

####this did not get used in 2024
# Now we should have a decent dataset for when fish actually made it to Morice Lake (or above),
# so we can filter out all detections downstream of Morice Lake after a fish has already made it there.
# This process removes 3 detections, mostly from tags 149.48 120 to 149.48 109 and 149.48 126
# allData4 <- allData3 %>%
#   arrange(dateTime) %>% 
#   group_by(freqCode, waterbody) %>%
#   mutate(moriceLakeSwitch = case_when(waterbody == "Morice Lake" & row_number() == 1 ~ 1,
#                                       waterbody == "Nanika River" & row_number() == 1 ~ 1,
#                                       TRUE ~ 0)) %>% # otherwise 0
#   ungroup() %>%
#   arrange(dateTime) %>%
#   group_by(freqCode) %>%
#   mutate(reachedLakeMorice = cumsum(moriceLakeSwitch)) %>%
#   filter(!(reachedLakeMorice >= 1 & rkm < 201)) %>%
#   ungroup() %>%
#   dplyr::select(-moriceLakeSwitch, -reachedLakeMorice)
# 
# impossible_rates <- allData4 %>%
#   filter(rate > 100 & distance > 100)


# Finally, only use mobile detections with the maximum
# power for each tag and for each survey.
## this removes 91 detections
allData6 <- allData5 %>%
  group_by(freqCode, date) %>%
  filter(!(method == "Mobile" & power < max(power)))



# Write Out Data --------------------------------------------------------------

# Write out cleaned data for plotting, mapping, and summarizing results
write.csv(allData6, 
          file = "Data Output/099_AllData_FinalCleaned_2023.csv")


