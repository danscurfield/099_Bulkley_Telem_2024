# Make general study area map
# Read in filtered data and summarize fish movements
# Created by Pete Moniz - winter 2023
# Updated by Dan Scurfield - February 2024

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

detData <- read.csv("Data Output/099_AllData_FinalCleaned_2023.csv", 
                     header = TRUE, 
                     stringsAsFactors = FALSE) %>%
  # Only use variables we need.
  dplyr::select(dateTime, date, freqCode, rkm, method, waterbody, station, tagDateTime, sex, forkLength) %>%
  mutate(dateTime = ymd_hms(dateTime))

  
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
    # #filter out tags that were not deployed
    # filter(!(is.na(tagDateTime)))



# Detection Efficiency --------------------------------------------------------

# There's probably a more elegant way to do this, but this first method that came
# to me to calculate det. efficiency.

# Lower Bulkley
station2dets <- detData %>% filter(rkm == 42) %>% count(freqCode)
station2USdets <- detData %>% filter(rkm >= 42) %>% count(freqCode)

nrow(station2dets)/nrow(station2USdets)*100 # 52.2%

# Morice Lake
station6dets <- detData %>% filter(rkm == 201) %>% count(freqCode)
station6USdets <- detData %>% filter(rkm >= 201) %>% count(freqCode)

nrow(station6dets)/nrow(station6USdets)*100 # 1.8%

# Nanika River
station4dets <- detData %>% filter(rkm == 214 & waterbody == "Nanika River") %>% count(freqCode)
station4USdets <- detData %>% filter(rkm >= 214 & waterbody == "Nanika River") %>% count(freqCode)

nrow(station4dets)/nrow(station4USdets)*100 # 94.4%

# Atna River
station5dets <- detData %>% filter(rkm == 226.5 & waterbody == "Atna River") %>% count(freqCode)
station5USdets <- detData %>% filter(rkm >= 226.5 & waterbody == "Atna River") %>% count(freqCode)

nrow(station5dets)/nrow(station5USdets)*100 # 13.3%

# Clean up workspace
rm(station2dets, station2USdets, 
   station6dets , station6USdets, 
   station4dets, station4USdets)

# Lower Bulkley

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

upperbulkley <- detData %>% 
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

summaryTable <- tagData %>%
  left_join(fallback, by = "freqCode") %>%
  left_join(bulkley, by = "freqCode") %>%
  left_join(upperbulkley, by = "freqCode") %>%
  left_join(moriceLake, by = "freqCode") %>%
  left_join(nanika, by = "freqCode") %>%
  left_join(atna, by = "freqCode") %>%
  dplyr::select(freqCode, tagDateTime = tagDateTime.x, sex=sex.x, fallbackHours,
                bulkleyDate, bulkleyDays,
                upperBulkleyDate, upperBulkleyDays,
                 moriceLakeDate, moriceLakeDays,
                 nanikaDate, nanikaDays,
                atnaDate, atnaDays, forkLength) 
  
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

DetectedCount #157 fish detected  fixed station or in mobile survey (97%)
              #5 fish not detected anywhere (3%)

undetectedTags <- anti_join(tagData, DetectedCount, by = "freqCode") %>%
  distinct(freqCode)

tagData %>%
  distinct(freqCode)
  

#number of fish at fixed station

fixedCount <- detData %>%
  filter(station %in% c("1", "2", "4", "5", "6")) %>%
  distinct(freqCode, .keep_all = TRUE)

fixedCount #151 fish detected at fixed stations (93%)

#number of fish in mobile surveys

mobileCount <- detData %>%
  filter(station == "Mobile")  %>%
  distinct(freqCode, .keep_all = TRUE)

mobileCount #82 fish detected in mobile surveys (51%)

#number of fish detected in multiple mobile surveys (10 surveys total)

mobileMultiDetect <- detData %>%
  filter(station == "Mobile")  %>%
  distinct(date, freqCode, .keep_all = TRUE)

mobileMultiDetect %>% #number of fish detected 3 or more times on separate mobile surveys
  count(freqCode) %>% #14  fish detected 3 or more times 
  filter(n >= 3)
  

#number of fish at fallback station
fallbackCount <- allDat %>%
  filter(station == "1")  %>%
  distinct(freqCode, .keep_all = TRUE) #141 fish at fallback station (87%)

sum(fallbackCount$sex == "m") #71 male
sum(fallbackCount$sex == "f") #51 female
sum(fallbackCount$sex == "") #19 sex unknown


#number of fish at Bulkley fish station or Bulkley River (Morice)
##NEEDS TO CONFIRM MORICE IS NOT UPPER BULKLEY

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

#59 fish at bulkley station (36%)
#67 fish after including mobile in bulkey station to morice(41%)
#77 fish after including mobile in morice river(47%%) 
#80 fish after including mobile in bulkley from witset canyon to bulkley station(49%) 

sum(bulkleyCount$sex == "m") #42 male
sum(bulkleyCount$sex == "f") #25 female
sum(bulkleyCount$sex == "") #13 sex unknown



#number of fish in Nanika River
nanikaRCount <- allDat %>%
  filter(station == "4"
         | method == "Mobile"
         & waterbody == "Nanika River"
  ) %>%
  distinct(freqCode, .keep_all = TRUE) 

#17 fish at nanika station (10%)
#18 fish after including mobile (11%)

sum(nanikaRCount$sex == "m") #7 male
sum(nanikaRCount$sex == "f") #7 female
sum(nanikaRCount$sex == "") #4 sex unknown

#number of fish in Atna River
atnaRCount <- allDat %>%
  filter(station == "5"
         | method == "Mobile"
         & waterbody == "Atna River"
  ) %>%
  distinct(freqCode, .keep_all = TRUE) 

#4 fish at at station (2%)
#30 fish after including mobile (19%)

sum(atnaRCount$sex == "m") #20 male
sum(atnaRCount$sex == "f") #9 female
sum(atnaRCount$sex == "") #1 sex unknown

#number of fish in upper bulkley

upperBulkleyCount <- allDat %>%
  filter(!(freqCode == "149.42 08")) %>% #tag later detected in Atna River
  filter(method == "Mobile"
         & waterbody == "Upper Bulkley River"
 #        & rkm >= 109 #section above bulkley-morice confluence
 ) %>%
  distinct(freqCode, .keep_all = TRUE) 

#10 fish in upper bulkley river (11%)

sum(upperBulkleyCount$sex == "m") #7 male
sum(upperBulkleyCount$sex == "f") #2 female
sum(upperBulkleyCount$sex == "") #1 sex unknown

#number of fish at Morice Lake outlet and lake
moriceLCount <- allDat %>%
  filter(station == "6"
         | method == "Mobile"
         & waterbody == "Morice Lake"
  ) %>%
  distinct(freqCode, .keep_all = TRUE) 

#1 fish at morice lake outley station (0.6%)
#9 fish after including mobile (6%)

sum(moriceLCount$sex == "m") #4 male
sum(moriceLCount$sex == "f") #5 female
sum(moriceLCount$sex == "") #0 sex unknown

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
  
sum(moriceLCount$waterbody == "Atna River") #30 fish in atna - looks goods
sum(moriceLCount$waterbody == "Nanika River") #18 in fish nanika - looks goods
sum(moriceLCount$waterbody == "Morice Lake") #9 fish in morice lake - looks goods

# spawner counts
spawnerCounts <- bind_rows(upperBulkleyCount, moriceLCount, atnaRCount, nanikaRCount)%>%
  distinct(freqCode, .keep_all = TRUE) 
# 30 in Atna
# 18 in Nanika
# 9 in Morice
# 10 in upper bulkley

# Mean date and and timing to fallback, Morice Lake, Nanika River, Atna River.
summaryTable %>%
  summarise(meanFallbackHours = round(mean(fallbackHours, na.rm = TRUE),1),
            sdFallbackHours = round(sd(fallbackHours, na.rm = TRUE),1),
            meanbulkley = round(mean(bulkleyDays, na.rm = TRUE),1),
            sdbulkley = round(sd(bulkleyDays, na.rm = TRUE),1),
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



