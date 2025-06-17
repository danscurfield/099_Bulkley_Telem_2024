# Make general study area map
# Read in filtered data and summarize fish movements
# Created by Pete Moniz - winter 2023
# Updated by Dan Scurfield - February 2024
# Updated by Dan Scurfield - June 2024

# Initial Setup ---------------------------------------------------------------

# Remove any objects from old R sessions
rm(list=ls(all=TRUE))

# Load packages
library(tidyverse)
library(lubridate)

# Make all times UTC and avoid auto re-display of timezone
Sys.setenv(TZ = "UTC") 

# Don't allow display of scientific notation
options(scipen = 999)

# Read In Data ----------------------------------------------------------------

detData <- read.csv("Data Output/099_AllData_FinalCleaned_2024.csv", 
                     header = TRUE, 
                     stringsAsFactors = FALSE) %>%
  # Only use variables we need.
  dplyr::select(dateTime, date, freqCode, rkm, method, waterbody, station, tagDateTime, sex, forkLength) %>%
  mutate(dateTime = ymd_hms(dateTime))

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


# Detection Efficiency --------------------------------------------------------

# There's probably a more elegant way to do this, but this first method that came
# to me to calculate det. efficiency.

# Lower Bulkley
station2dets <- detData %>% filter(rkm == 42) %>% count(freqCode)
station2USdets <- detData %>% filter(rkm >= 42) %>% count(freqCode)

nrow(station2dets)/nrow(station2USdets)*100 # 93.4%

# Morice Lake
station6dets <- detData %>% filter(rkm == 201) %>% count(freqCode)
station6USdets <- detData %>% filter(rkm >= 201) %>% count(freqCode)

nrow(station6dets)/nrow(station6USdets)*100 # 85.8%

# Nanika River
station4dets <- detData %>% filter(rkm == 214 & waterbody == "Nanika River") %>% count(freqCode)
station4USdets <- detData %>% filter(rkm >= 214 & waterbody == "Nanika River") %>% count(freqCode)

nrow(station4dets)/nrow(station4USdets)*100 # 93.4%

# Atna River
station5dets <- detData %>% filter(rkm == 226.5 & waterbody == "Atna River") %>% count(freqCode)
station5USdets <- detData %>% filter(rkm >= 226.5 & waterbody == "Atna River") %>% count(freqCode)

nrow(station5dets)/nrow(station5USdets)*100 # 93.1%

# Clean up workspace
rm(station2dets, station2USdets, 
   station6dets , station6USdets,
   station5dets, station5USdets,
   station4dets, station4USdets)

# Lower Bulkley Fallback (can't really be calculated)

# Summary Table ---------------------------------------------------------------

# First let's create a big dataframe of all tagged fish that shows the first detection
# at each station/waterbody. We'll include this in Appendix A.

fallback <- detData %>% 
  filter(station == "1") %>%
  arrange(dateTime) %>%
  group_by(freqCode) %>%
  filter(row_number() ==1) %>%
  mutate(fallbackDate = date(dateTime),
         fallbackHours = round(as.numeric(difftime(dateTime, tagDateTime, units = "hours")),1)) %>%
  dplyr::select(freqCode, sex, fallbackDate, fallbackHours, station, tagDateTime, forkLength)

bulkley <- detData %>% 
  filter(station == "2") %>% #UPDATE - should this be waterbody == "bulkley" "morice"
  arrange(dateTime) %>%
  group_by(freqCode) %>%
  filter(row_number() ==1) %>%
  mutate(bulkleyDate = date(dateTime),
        bulkleyDays = round(as.numeric(difftime(dateTime, tagDateTime, units = "days")),0)) %>%
  dplyr::select(freqCode, sex, bulkleyDate, bulkleyDays, station, tagDateTime, forkLength)

upperbulkley <- detData %>% #none in 2024
  filter(waterbody == "Upper Bulkley River") %>%
  arrange(dateTime) %>%
  group_by(freqCode) %>%
  filter(row_number() ==1) %>%
  mutate(upperBulkleyDate = date(dateTime),
         upperBulkleyDays = round(as.numeric(difftime(dateTime, tagDateTime, units = "days")),0)) %>%
  dplyr::select(freqCode, sex, upperBulkleyDate, upperBulkleyDays, station, tagDateTime, forkLength)

moriceLake <- detData %>%
  filter(waterbody == "Morice Lake") %>%
  arrange(dateTime) %>%
  group_by(freqCode) %>%
  filter(row_number() ==1) %>%
  mutate(moriceLakeDate = date(dateTime),
         moriceLakeDays = round(as.numeric(difftime(dateTime, tagDateTime, units = "days")),0)) %>%
  dplyr::select(freqCode, sex, moriceLakeDate, moriceLakeDays, station, tagDateTime, forkLength) 


nanika <- detData %>% 
  filter(waterbody == "Nanika River") %>%
  arrange(dateTime) %>%
  group_by(freqCode) %>%
  filter(row_number() ==1) %>%
  mutate(nanikaDate = date(dateTime),
         nanikaDays = round(as.numeric(difftime(dateTime, tagDateTime, units = "days")))) %>%
  dplyr::select(freqCode, sex, nanikaDate, nanikaDays, station, tagDateTime, forkLength)

atna <- detData %>% 
  filter(waterbody == "Atna River") %>%
  arrange(dateTime) %>%
  group_by(freqCode) %>%
  filter(row_number() ==1) %>%
  mutate(atnaDate = date(dateTime),
         atnaDays = round(as.numeric(difftime(dateTime, tagDateTime, units = "days")))) %>%
  dplyr::select(freqCode, sex, atnaDate, atnaDays, station, tagDateTime, forkLength)

#Read in last detections
lastDetections <- read.csv(file = "Data Output/099_AllData_FinalCleaned_2024.csv", 
                           header = TRUE, 
                           stringsAsFactors = FALSE) %>%
  arrange(desc(dateTime)) %>%
  group_by(freqCode) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  #filter out tags with missing data (none)
  #filter our tags not assigned (none)
  dplyr::select(freqCode, station, method, waterbody)

lastDetects <- as.data.frame(lastDetections) %>%
  rename(Station = station,
         Waterbody = waterbody,
         Method = method) %>%
  rename_with(~ paste0("lastDetect", .x), .cols = -freqCode)

#Create a summery table

summaryTable <- tagData %>%
  left_join(fallback, by = "freqCode") %>%
  left_join(bulkley, by = "freqCode") %>%
  left_join(upperbulkley, by = "freqCode") %>%
  left_join(moriceLake, by = "freqCode") %>%
  left_join(nanika, by = "freqCode") %>%
  left_join(atna, by = "freqCode") %>%
  left_join(lastDetects, by = "freqCode") %>%
  dplyr::select(freqCode, tagDateTime = tagDateTime.x, sex=sex.x, fallbackHours,
                bulkleyDate, bulkleyDays,
                upperBulkleyDate, upperBulkleyDays,
                 moriceLakeDate, moriceLakeDays,
                 nanikaDate, nanikaDays,
                atnaDate, atnaDays, forkLength,
                lastDetectStation, lastDetectMethod,
                lastDetectWaterbody) 
  
write.csv(summaryTable, file = "Data Output/099_Detections_SummaryTable.csv")


# Summary Stats ---------------------------------------------------------------

#number of fish with no detections

##create data frame of all detection data
allDat <- tagData %>%
  dplyr::select(freqCode) %>%
  distinct(freqCode, .keep_all = TRUE) %>%
  left_join(detData, by = "freqCode") %>%
  filter(station %in% c("1", "2", "4", "5", "6", "Mobile") & station != "Tagging") %>%
  dplyr::select(freqCode, sex, station, method, waterbody, rkm, dateTime)


## seek unique freqCodes to see the number of detected fish on array
DetectedCount <- allDat %>%
  filter(station %in% c("1", "2", "4", "5", "6", "Mobile")) %>%
  distinct(freqCode, .keep_all = TRUE)

DetectedCount #156 fish detected  fixed station or in mobile survey (92.3%)
              #13 fish not detected anywhere (7.7%)

undetectedTags <- anti_join(tagData, DetectedCount, by = "freqCode") %>%
  distinct(freqCode)

tagData %>%
  distinct(freqCode)
  

#number of fish at fixed station

fixedCount <- detData %>%
  filter(station %in% c("1", "2", "4", "5", "6")) %>%
  distinct(freqCode, .keep_all = TRUE)

fixedCount #156 fish detected at fixed stations (92.3%)

#number of fish in mobile surveys

mobileCount <- detData %>%
  filter(station == "Mobile")  %>%
  distinct(freqCode, .keep_all = TRUE)

mobileCount #78 fish detected in mobile surveys (46.7%)

#number of fish detected in multiple mobile surveys (3 surveys total)

mobileMultiDetect <- detData %>%
  filter(station == "Mobile")  %>%
  distinct(date, freqCode, .keep_all = TRUE)

mobileMultiDetect %>% #number of fish detected 3 or more times on separate mobile surveys
  count(freqCode) %>% #37  fish detected 2 or more times 
  filter(n >= 2)
  

#number of fish at fallback station
fallbackCount <- allDat %>%
  filter(station == "1")  %>%
  distinct(freqCode, .keep_all = TRUE) #80 fish at fallback station (47.3%)

sum(fallbackCount$sex == "M", na.rm = TRUE) #12 male
sum(fallbackCount$sex == "F", na.rm = TRUE) #29 female
sum(is.na(fallbackCount$sex)) #39 sex unknown


#number of fish that were last detected at Bulkley fallback station or Tagging

LowerBulkleyCount <- allDat %>%
  group_by(freqCode) %>%
  # Keep only freqCodes that were never detected at any station other than "1" or method == "tagging"
  filter(all(station == "1" | method == "tagging")) %>%
  # Now within those, keep only rows that are at station 1 (or whatever condition you want)
  filter(station == "1" | method == "tagging") %>%
  # Optional: get the most recent detection per freqCode
  filter(dateTime == max(dateTime)) %>%
  ungroup()

#19 fish were last detected at the fallback station or tagging. (11.2%)
#This includes lost tags.

sum(LowerBulkleyCount$sex == "M", na.rm = TRUE) #0 male
sum(LowerBulkleyCount$sex == "F", na.rm = TRUE) #11 female
sum(is.na(LowerBulkleyCount$sex)) #8 sex unknown


#number of fish at Bulkley fish station or Bulkley River (Morice)

bulkleyCount <- allDat %>%
  filter(station == "2" 
         | method == "Mobile"
         & waterbody == "Bulkley River"
         & rkm >= 42 & rkm <= 109 #section above bulkley station to bulkley morice confluence
         | method == "Mobile"
         & waterbody == "Morice River" #section of morice river
         | method == "Mobile"
         & waterbody == "Bulkley River"
         & rkm >= 0 & rkm <= 42 #section of bulkley above witset canyon and bulkley station
  ) %>%
  distinct(freqCode, .keep_all = TRUE) 

#128 fish at bulkley station (75.7%)
#0 fish after including mobile in bulkey station to morice(0%)
#0 fish after including mobile in morice river(0%%) 
#0 fish after including mobile in bulkley from witset canyon to bulkley station(0%) 

sum(bulkleyCount$sex == "M", na.rm = TRUE) #25 male
sum(bulkleyCount$sex == "F", na.rm = TRUE) #34 female
sum(is.na(bulkleyCount$sex)) #69 sex unknown



#number of fish in Nanika River
nanikaRCount <- allDat %>%
  filter(station == "4"
         | method == "Mobile"
         & waterbody == "Nanika River"
  ) %>%
  distinct(freqCode, .keep_all = TRUE) 

#70 fish at nanika station (41.4%)
#78 fish after including mobile (46.2%)

sum(nanikaRCount$sex == "M", na.rm = TRUE) #17 male
sum(nanikaRCount$sex == "F", na.rm = TRUE) #21 female
sum(is.na(nanikaRCount$sex)) #40 sex unknown

#number of fish in Atna River
atnaRCount <- allDat %>%
  filter(station == "5"
         | method == "Mobile"
         & waterbody == "Atna River"
  ) %>%
  distinct(freqCode, .keep_all = TRUE) 

#27 fish at at station (16.0%)
#29 fish after including mobile (17.1%)

sum(atnaRCount$sex == "M", na.rm = TRUE) #2 male
sum(atnaRCount$sex == "F", na.rm = TRUE) #9 female
sum(is.na(atnaRCount$sex)) #25 sex unknown

#number of fish in upper bulkley
#Note: no mobile tracks in upper bulkley in 2024

upperBulkleyCount <- allDat %>%
  filter(!(freqCode == "149.42 08")) %>% #tag later detected in Atna River
  filter(method == "Mobile"
         & waterbody == "Upper Bulkley River"
 #        & rkm >= 109 #section above bulkley-morice confluence
 ) %>%
  distinct(freqCode, .keep_all = TRUE) 

#0 fish in upper bulkley river (0%)

sum(upperBulkleyCount$sex == "M", na.rm = TRUE) #0 male
sum(upperBulkleyCount$sex == "F", na.rm = TRUE) #0 female
sum(is.na(upperBulkleyCount$sex)) #0 sex unknown

#number of fish at Morice Lake outlet and lake
moriceLCount <- allDat %>%
  filter(station == "6"
         | method == "Mobile"
         & waterbody == "Morice Lake"
  ) %>%
  distinct(freqCode, .keep_all = TRUE) 

#98 fish at morice lake outley station (58.0%)
#98 fish after including mobile (58.0%)

sum(moriceLCount$sex == "M", na.rm = TRUE) #17 male
sum(moriceLCount$sex == "F", na.rm = TRUE) #22 female
sum(is.na(moriceLCount$sex)) #59 sex unknown

#see if any morice lake fish are detected in atna or nanika after morice lake detection
## this is taken from the final detection on the array
## double check fish are not being counted twice

moriceLCount <- allDat %>%
  filter(station == "6" #morice outlet
         | method == "Mobile"
         & waterbody == "Morice Lake"
         | station == "4" #nanika lake
         | method == "Mobile"
         & waterbody == "Nanika River"
         | station == "5" #atna lake
         | method == "Mobile"
         & waterbody == "Atna River") %>%
  group_by(freqCode) %>%
  filter(dateTime == max(dateTime))
  
sum(moriceLCount$waterbody == "Atna River") #29 fish in atna - looks goods
sum(moriceLCount$waterbody == "Nanika River") #70 in fish nanika - looks good
# Note: 8 fish pick up at nanika station late observed US in morice or atna
sum(moriceLCount$waterbody == "Morice Lake") #14 fish in morice lake - looks goods

# spawner counts
spawnerCounts <- bind_rows(upperBulkleyCount, moriceLCount, atnaRCount, nanikaRCount)%>%
  distinct(freqCode, .keep_all = TRUE) 
# 29 in Atna
# 70 in Nanika
# 14 in Morice
# 0 in upper bulkley


# Mean date and and timing to fallback, Morice Lake, Nanika River, Atna River.
summaryTable %>%
  summarise(meanFallbackHours = round(mean(fallbackHours, na.rm = TRUE),1),
            sdFallbackHours = round(sd(fallbackHours, na.rm = TRUE),1),
            meanbulkleyDays = round(mean(bulkleyDays, na.rm = TRUE),1),
            sdbulkleyDays = round(sd(bulkleyDays, na.rm = TRUE),1),
            meanMoriceLakeDays = round(mean(moriceLakeDays, na.rm = TRUE),1),
            sdMoriceLakeDays = round(sd(moriceLakeDays, na.rm = TRUE),1),
            meanMoriceLakeDate = mean(moriceLakeDate, na.rm = TRUE),
            meanUpperBulkleyDays = round(mean(upperBulkleyDays, na.rm = TRUE),1),
            sdUpperBulkleyDays = round(sd(upperBulkleyDays, na.rm = TRUE),1),
            meanUpperBulkleyDate = mean(upperBulkleyDate, na.rm = TRUE),
            meanNanika = round(mean(nanikaDays, na.rm = TRUE),1),
            sdNanika = round(sd(nanikaDays, na.rm = TRUE),1),
            meanNanikaLakeDate = mean(nanikaDate, na.rm = TRUE),
            meanAtna = round(mean(atnaDays, na.rm = TRUE),1),
            sdAtna = round(sd(atnaDays, na.rm = TRUE),1),
            meanAtnaLakeDate = mean(atnaDate, na.rm = TRUE))



write.csv(summaryTable, file = "Data Output/099_Detections_MeanSummaryTable.csv")

