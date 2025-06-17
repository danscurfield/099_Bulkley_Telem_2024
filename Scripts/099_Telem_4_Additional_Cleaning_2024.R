# Read in, run additional filtering of mobile and fixed telemetry data
# Created by Pete Moniz - winter 2023
# Updated by Dan Scurfield - Feb 2024
# Updated by Dan Scurfield - Jun 2025

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
mobileDataCleaned <- read.csv("Data Output/099_MobileTrackingData_2_InitialClean_RKM_2024.csv", 
                       header = TRUE, 
                       stringsAsFactors = FALSE) %>%
  mutate(date = as_date(dateTime),
         dateTime = ymd_hms(dateTime),
         tagDateTime = as.POSIXct(tagDateTime),
         method = "Mobile") %>%
  dplyr::select(-X)

# Read in fixed station data
fixedDataCleaned <- read.csv("Data Output/099_FixedStationData_InitialClean_2024.csv", 
                      header = TRUE, 
                      stringsAsFactors = FALSE) %>%
  mutate(date = ymd(date),
         dateTime = ymd_hms(dateTime),
         tagDateTime = ymd_hms(paste(tagDateTime, "00:00:00")),
         station = as.character(station),
         latitude = case_when(station == "1" ~ 55.01835,
                         station == "2" ~ 54.78871,
                         station == "4" ~ 54.04742,
                         station == "5" ~ 54.014545,
                         station == "6" ~ 54.10783),
         longitude = case_when(station == "1" ~ -127.3189,
                          station == "2" ~ -127.1463,
                          station == "4" ~ -127.4264,
                          station == "5" ~ -127.74321,
                          station == "6" ~ -127.4250)) %>%
  dplyr::select(-X, -file)

# Read in tagging data, same as before, except we'll also make some additional 
# variables so that we can combine these with the fixed station data.
# This will allow us to more easily calculate movement rates after release and
# filter out improbable movement rates.
tagData <- read_csv("Data Input/tagData.csv") %>%
  mutate(date = ymd(tagDateTime),
         tagDateTime = as.POSIXct(tagDateTime),
         dateTime = tagDateTime,
         code = sub(".* ", "", freqCode),
         rkm = 0,
         waterbody = "Tagging",
         method = "Tagging",
         station = "Tagging",
         power = NA, 
         longitude = -127.3283,
         latitude = 55.0146) %>%
  dplyr::select(date, tagDateTime, dateTime, freqCode, code, rkm, waterbody, 
                method, station, power, sex, forkLength, latitude, longitude)
#filter out recapture fish (none)
#filter out fish with missing data (none)



#Add lost tags

new_tag <- data.frame(
  freqCode = "149.500 023", #this appears to be a lost tag from detection history plot
  earliestDate = ymd_hms("2024-08-01 05:41:06"),
  method = "Lost Tag",
  stringsAsFactors = FALSE)

lostTags <- read.csv("Data Output/099_LostTags_2024.csv", 
                     header = TRUE, 
                     stringsAsFactors = FALSE) %>%
  #remove known alive fish - observed upstream
  filter(freqCode != "149.320 127") %>%
  filter(freqCode != "149.340 001") %>%
  filter(freqCode != "149.340 002") %>%
  filter(freqCode != "149.340 003") %>%
  filter(freqCode != "149.340 004") %>%
  filter(freqCode != "149.340 006") %>%
  filter(freqCode != "149.340 008") %>%
  filter(freqCode != "149.500 001") %>%
  filter(freqCode != "149.500 002") %>%
  filter(freqCode != "149.500 018") %>%
  filter(freqCode != "149.500 034") %>%
  filter(freqCode != "149.500 044") %>%
  filter(freqCode != "149.500 071") %>%
  #note when tag is lost and change to correct date
  mutate(earliestDate = case_when(
    freqCode == "149.320 146" ~ ymd_hms("2024-08-24 10:10:14"),
    freqCode == "149.500 100" ~ ymd_hms("2024-07-31 10:50:28"))) %>%
  mutate(method = "Lost Tag") %>%
  dplyr::select(-X, -unique_freqCode) %>%
  #add new lost tags
  bind_rows(new_tag)

rm(new_tag)


#FIXED STATIONS
# These fish are alive
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

#create a fish deaths dataframe from  from mobile tracks

fishDeath <- read.csv("Data Output/099_fishDeath_2024.csv", 
                      header = TRUE, 
                      stringsAsFactors = FALSE) %>%
  mutate(method = "Fish Death") %>%
  dplyr::select(-X, -unique_freqCode)
  
#MOBILE TRACKS
  # 13s tags (fish death)
  # 149.500 008 at 2024-10-01 17:58:23
  # 149.500 025 at 2024-10-01 17:59:17 (2x)
  
  #26s tags (fish death)
  # 149.500 071 at 2024-10-01 19:29:55
  # 149.500 033 at 2024-10-01 19:38:50
  # 149.500 034
  # 149.500 038 at 2024-10-01 19:30:59
  # 149.500 014 at 2024-10-01 19:32:21


# Additional Filtering --------------------------------------------------------

# Step One: General cleaning

# First lets join are dataframes together include are lost tags and dead fish
# Then we will remove erroneous detections where the difference between the 
# detections are less that the tag pulse rate (5s)

# This removes 616 detections.

allData <- rbind(fixedDataCleaned, mobileDataCleaned, tagData) %>% #466078 detections
  #add lost tags
  left_join(lostTags, by = "freqCode", suffix = c("", "_lost")) %>%
  mutate(method = case_when(
    !is.na(earliestDate) & dateTime >= earliestDate ~ "Lost Tag",
                            TRUE ~ method)) %>%
  dplyr::select(-earliestDate, -method_lost) %>%
  #add fish deaths
  left_join(fishDeath, by = "freqCode", suffix = c("", "_death")) %>%
  mutate(method = case_when(
    !is.na(earliestDate) & dateTime >= earliestDate ~ "Death",
    TRUE ~ method)) %>%
  dplyr::select(-earliestDate, -method_death) %>%
  #arrange data nicely
  arrange(dateTime) %>%
  group_by(freqCode) %>%  
  mutate(lag = difftime(dateTime, lag(dateTime), units = "secs")) %>%
  #remove irregular tag data
  filter((lag >= 5 | is.na(lag))) %>% # is.na(lag) keeps the tagging data since those don't have a previous detection. 465462 detections
  ungroup() 

#remove unneeded df's
rm(fishDeath, lostTags, fixedDataCleaned, mobileDataCleaned, tagData)
  

# Next we'll remove detections of tags downstream of Nanika River once the fish 
# has already made it to Nanika River. The Nanika River fixed station had a 
# relatively low FP rate and high efficiency, so I have pretty high confidence that
# we can consider these real detections.
# note: Atna and some of Morive Lake is upstream of Nanika (let's not remove 
# those)
# This process removes 21,658 detections.

allData1 <- allData %>% #443804 detections
  arrange(dateTime) %>% 
  group_by(freqCode, waterbody) %>%
  mutate(nanikaSwitch = ifelse(waterbody == "Nanika River" & row_number() == 1, 1,0)) %>%
  ungroup() %>%
  arrange(dateTime) %>%
  group_by(freqCode) %>%
  mutate(reachedNanika = cumsum(nanikaSwitch)) %>%
  filter(!(reachedNanika == 1 & !waterbody %in% c("Nanika River", "Atna River", "Morice Lake") | #only remove detections at outlet fixed station and downstream of Morice Lake
             reachedNanika == 1 & waterbody == "Morice Lake" & method == "Fixed")) %>% 
  ungroup() %>%
  dplyr::select(-nanikaSwitch, -reachedNanika)

# Next we'll remove detections of tags downstream of Atna River once the fish 
# has already made it to Atna River. The Atna River fixed station had an 
# extremely low FP (false positive) rate and high efficiency as will, so I have 
#pretty high confidence that we can consider these real detections.

# This process removes 1012 detections.
allData2 <- allData1 %>% #442792 detection
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

# Next we'll remove detections of tags downstream of Morice Lake Outlet once the 
# fish has already made it to Morice Lake. In 2024, the Morice Lake Outlet 
# fixed station had relatively low FP (false positive) rate and high efficiency 
# as well
#note: Anta River and Nanika River are both upstream
#so with confidence we can consider these real detections


# This process removes 478 detections.
allData3 <- allData2 %>% # 442314 detections
  arrange(dateTime) %>% 
  group_by(freqCode, waterbody) %>%
  mutate(moriceSwitch = ifelse(waterbody == "Morice Lake" & row_number() == 1, 1,0)) %>%
  ungroup() %>%
  arrange(dateTime) %>%
  group_by(freqCode) %>%
  mutate(reachedMorice = cumsum(moriceSwitch)) %>%
  filter(!(reachedMorice == 1 & !waterbody %in% c("Nanika River", "Atna River", "Morice Lake"))) %>%
  ungroup() %>%
  dplyr::select(-moriceSwitch, -reachedMorice)

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


#Important: 
#all of the filtering for each site must be done while only calculating "lag"
# once or else might be remove true detects based on false pretenses.


# Now lets add a distance traveled, lag in days between detections, and a rate
# in rkm/day to manually filter out detections with unrealistic rates of travel
allData4 <- allData3 %>%
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

impossible_rates <- allData4 %>% #114 fish
  filter(rate > 60) # used to be  filter(rate > 100 & distance > 100) but the distance didnt make sense

# Now that we've removed a few bogus detections and calculated a rate in rkm/day,
# we can use those rates to filter out detections that suggest unrealistic movement rates.

#Note on 2): Sockeye max burst speed is 57.6 km/day (2.4 km/h). 
#We will set out rate to 60 rkm per day to filter out these detects

#Pete did this in the past:
# I did this manually since there weren't that many tags to look through, 
# but I'm sure it could be automated with a "while" loop where you:
#   1) Calculate distance, lag in days, and rates of movement between detections for each tag;
#   2) Remove the very first detection for each tag with an impossible rate (ex: movement of > 100 rkm at > 100 rkm/day);
#   3) Re-calculate distance, lag in days, and rates of movement between detections after first detection removed;
#   4) Remove next detection for each tag with an impossible rate;
#   5) Then keep doing those steps until no more detections are removed from any tags.
# I (DS) prefer to use throurough filter and manuall remove a few false using 
# the detection history plots at the end. 

#note: no need to calculate lag. This was done is allData4, and will alter the 
# how detections are removed (especially if not careful), you can easily remove
# the first true detect if you continue to lag each time.

# Remove single detections at Station 1 & 2
# Remove detections at station 2 the same day as tagging
# Remove detections at Station 1 & 2 greater than 1 min
# Remove detections at Station 1 & 2 not divisible by 5 - removing 1st detect in a sequence of detections. 
# Remove single detections at Station 2 that were missed because they are the final detection

# This removes 195,226 detections

allData5 <- allData4 %>%
  arrange(dateTime) %>%
  group_by(freqCode) %>%
  #Station 2: remove single detections and tagging same day detections
  filter(!(station == "2" & lead(station) != "2" & lag(station) != "2")) %>% #remove single detections - 435345
  filter(!(station == "2" & tagDateTime == date)) %>% # remove tags detected same day as tagging (no tagging time recorded) -  434912
  #Station 1: remove single detections
  filter(!(station == "1" & lead(station) != "1" & lag(station) != "1")) %>% #remove single detections - 426067
  # 75 fish with impossible rates
  
  #Station 2: remove > 1min lag and not divisible by 5 detects
  filter(!(station == "2" & lag > 60 & distance == 0.0)) %>% #remove tags recorded at a lag of 5 min or more - 427865
  filter(!(station == "2" & lag %% 5 != 0 & distance == 0.0)) %>% #remove tags not divisible by 5
  #Station 1: remove > 1min lag and not divisible by 5 detects
  filter(!((station == "1" & lag > 60 & distance == 0.0))) %>% #remove tags recorded at lag of 5 min or more - 418726
  filter(!(station == "1" & lag %% 5 != 0 & distance == 0.0)) %>% #remove tags not divisible by 5
  # 54 fish with impossible rates
  #remove missed false final detections 
  mutate(row_id = row_number(),
         max_row = max(row_id)) %>%
  filter(!(row_id == max_row & station == "2" & lag > 360)) %>%
  select(-row_id, -max_row) %>%
  #recalculate lag
  arrange(dateTime) %>%
  group_by(freqCode) %>%
  mutate(lag = as.numeric(difftime(dateTime, lag(dateTime), units = "secs")),
         distance = abs(rkm - lag(rkm)),
         lagDays = as.numeric(difftime(dateTime, lag(dateTime), units = "days")),
         rate = distance/lagDays) %>%
  ungroup()
  

impossible_rates <- allData5 %>% # 45 fish
  filter(rate > 60)

#Station 2 unlikely detections - to be manually removed
#	149.340 002 at 2024-08-03 20:24:41 (once) and 2024-08-04 13:31:52 (once)
# 149.340 003 at 2024-08-07 15:37:39 (once)
# 149.340 006 at 2024-08-09 11:51:46 (once)
# 149.500 018 at 2024-07-17 00:44:14 (once)
# 149.500 044 at 2024-07-22 07:21:19 (once)
# 149.500 052 at 2024-07-28 12:11:36 (once)
# 149.500 058 2024-07-24 02:21:42 (once)
# 149.500 060 2024-07-24 12:44:33 (once)
# 149.500 073 2024-07-28 13:53:23 (once)
# 149.500 016 on 2024-07-22 (4x)
# 149.500 003 on 2024-07-22 (4x)


#Station 1
# 149.500 023 weird detects from 2024-08-06 21:49:27 to 2024-08-11 14:31:59
# This is added to the lost tags (above) starting on 2024-08-01 05:41:06

# Manually filter out single detections missed in last filter
# this removes 17 detections

allData6 <- allData5 %>%
  filter(!(freqCode == "149.340 002" & station == "2" & dateTime == "2024-08-03 20:24:41" |
             freqCode == "149.340 002" & station == "2" & dateTime == "2024-08-04 13:31:52")) %>%
  filter(!(freqCode == "149.340 003" & station == "2" & dateTime == "2024-08-07 15:37:39")) %>%
  filter(!(freqCode == "149.340 006" & station == "2" & dateTime == "2024-08-09 11:51:46")) %>%
  filter(!(freqCode == "149.500 018" & station == "2" & dateTime == "2024-07-17 00:44:14")) %>%
  filter(!(freqCode == "149.500 044" & station == "2" & dateTime == "2024-07-22 07:21:19")) %>%
  filter(!(freqCode == "149.500 052" & station == "2" & dateTime == "2024-07-28 12:11:36")) %>%
  filter(!(freqCode == "149.500 058" & station == "2" & dateTime == "2024-07-24 02:21:42")) %>%
  filter(!(freqCode == "149.500 060" & station == "2" & dateTime == "2024-07-24 12:44:33")) %>%
  filter(!(freqCode == "149.500 073" & station == "2" & dateTime == "2024-07-28 13:53:23")) %>%
  filter(!(freqCode == "149.500 016" & station == "2" & date == "2024-07-22")) %>%
  filter(!(freqCode == "149.500 003" & station == "2" & date == "2024-07-22")) %>%
  #recalculate lag
  arrange(dateTime) %>%
  group_by(freqCode) %>%
  mutate(lag = as.numeric(difftime(dateTime, lag(dateTime), units = "secs")),
         distance = abs(rkm - lag(rkm)),
         lagDays = as.numeric(difftime(dateTime, lag(dateTime), units = "days")),
         rate = distance/lagDays) %>%
  ungroup()


impossible_rates <- allData6 %>% # 34 fish
  filter(rate > 60)

#All the rest of the impossible detections appear to be in fact possible 
# between the morice lake station and atna, nanika and mobile detects. 

# These tags (below) appear to get picked up speratically at station 2, since
# they are not easily filtered out as they were not observed upstream

# 149 340 001 at 2024-08-13 15:46:46 on
# 149.340 002 at 2024-08-11 09:22:28 on
# 149.340 003 at 2024-09-05 14:18:45 on (unclear)
# 149.340 004 at 2024-08-23 11:13:30 on 
# 149.340 005 at 2024-09-05 18:40:04 on
# 149.340 006 between 2024-08-13 15:56:46 & 2024-08-26 17:53:50 and 2024-09-11 20:16:30 on
# 149.340 008 at 2024-08-22 15:56:00 on
# 149.340 010 at 2024-09-07 23:39:02 on

# 149.500 019 at 2024-10-05 02:43:46 (once)
# 149.500 059 at 2024-07-25 06:09:43 (once)
# 149.500 073 at 2024-07-26 11:05:28 (once)

#Manually remove tags that are unlikely based on detection history plots
# This removes 503 detections


allData7 <- allData6 %>%
  filter(!(freqCode == "149.340 001" & station == "2" & dateTime >= "2024-08-13 15:46:46")) %>%
  filter(!(freqCode == "149.340 002" & station == "2" & dateTime >= "2024-08-11 09:22:28")) %>%
  filter(!(freqCode == "149.340 003" & station == "2" & dateTime >= "2024-09-05 14:18:45")) %>%
  filter(!(freqCode == "149.340 004" & station == "2" & dateTime >= "2024-08-23 11:13:30")) %>%
  filter(!(freqCode == "149.340 005" & station == "2" & dateTime >= "2024-09-05 18:40:04")) %>%
  filter(!(freqCode == "149.340 006" & station == "2" & dateTime >= "2024-09-05 21:07:51")) %>%
  filter(!(freqCode == "149.340 008" & station == "2" & dateTime >= "2024-08-22 15:56:00")) %>%
  filter(!(freqCode == "149.340 010" & station == "2" & dateTime >= "2024-09-07 23:39:02")) %>%
  filter(!(freqCode == "149.500 019" & station == "2" & dateTime == "2024-10-05 02:43:46")) %>%
  filter(!(freqCode == "149.500 059" & station == "2" & dateTime == "2024-07-25 06:09:43")) %>%
  filter(!(freqCode == "149.500 073" & station == "2" & dateTime == "2024-07-26 11:05:28")) %>%
  
  
  #recalculate lag
  arrange(dateTime) %>%
  group_by(freqCode) %>%
  mutate(lag = as.numeric(difftime(dateTime, lag(dateTime), units = "secs")),
         distance = abs(rkm - lag(rkm)),
         lagDays = as.numeric(difftime(dateTime, lag(dateTime), units = "days")),
         rate = distance/lagDays) %>%
  ungroup()



impossible_rates <- allData7 %>% # 34 fish
  filter(rate > 60)


# Now let's see how many detections there were per tag at the morice lake outlet
moriceLakeOutlet <- allData7 %>% 
  filter(rkm == 201) %>% count(freqCode)
# Didn't use this info for filtering, but maybe we should?
# Here are some notes:
# tag 149.320 116 detected one time - looks legit.
# tag 149.500 046 detected one time - looks legit.
# tag 149.500 058 detected one time - looks legit.


# Finally, only use mobile detections with the maximum
# power for each tag and for each survey.
# this removes 131 detections

allData8 <- allData7 %>% # 414468 detections
  group_by(freqCode, date) %>%
  filter(!(method == "Mobile" & power < max(power)))


#Now make it so that that lost tags only get 1 point for its first lost detection
# this removes 93577 detections

allData9 <- allData8 %>% # 414468 detections
  arrange(freqCode, dateTime) %>%
  group_by(freqCode) %>%
  mutate(
    lost_tag_time = if (any(method == "Lost Tag")) min(dateTime[method == "Lost Tag"], na.rm = TRUE) else as.POSIXct(NA),
    keep_row = method != "Lost Tag" | dateTime == lost_tag_time
  ) %>%
  filter(keep_row) %>%
  select(-lost_tag_time, -keep_row) %>%
  ungroup()


# Write Out Data --------------------------------------------------------------

# Write out cleaned data for plotting, mapping, and summarizing results
write.csv(allData9, 
          file = "Data Output/099_AllData_FinalCleaned_2024.csv")


