# Make general study area map
# Read in filtered mobile telemetry data and make maps for each survey date
# Created by Pete Moniz - winter 2023
# Updated by Dan Scurfield - February 2024

# Initial Setup ---------------------------------------------------------------

# Remove any objects from old R sessions
rm(list=ls(all=TRUE))

# Load packages
library(sp)
library(raster)
library(rgdal)
library(tidyverse)
library(ggsn)
library(IFRthemes)
library(cowplot) # for inset maps

# Make all times UTC and avoid auto re-display of timezone
Sys.setenv(TZ = "UTC") 

# Don't allow display of scientific notation
options(scipen = 999)

# Read In Data ----------------------------------------------------------------

# First read in coordinates of four fixed stations
fixedLocations <- read.csv(file = "Data Input/099_Fixed_Locations_2023.csv", 
                           header = TRUE, 
                           stringsAsFactors = FALSE) %>%
  mutate(station = as.character(Station)) %>%
  dplyr::select(station, lat = Latitude, long = Longitude)

# Read in all detection data, and filter for mobile detection data to plot on maps
mobileDetections <- read.csv(file = "Data Output/099_AllData_FinalCleaned_2023.csv", 
                           header = TRUE, 
                           stringsAsFactors = FALSE) %>%
  filter(method == "Mobile") %>%
  dplyr::select(date, freqCode, code, lat, long, rkm, waterbody)


# Also read in all detection data, and filter for the last detection for each tag to plot on maps
lastDetections <- read.csv(file = "Data Output/099_AllData_FinalCleaned_2023.csv", 
                    header = TRUE, 
                    stringsAsFactors = FALSE) %>%
  arrange(desc(dateTime)) %>%
  group_by(freqCode) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  filter(!(freqCode == "149.42 NA"),  #filter out tags with missing data
         !(freqCode == "149.48 3?"),
         !(freqCode == "149.48 49?"),
         !(freqCode == "149.42 14"),
         !(freqCode == "149.48 NA")) %>%
  filter(!(is.na(tagDateTime))) %>%  #filter our tags not assigned
  dplyr::select(date, freqCode, code, lat, long, rkm, waterbody)

  lastDetects <- as.data.frame(lastDetections)

# Read in stream and road shapefiles. I copied these from the 2021 data input folder.
streams <- readOGR(dsn = "Data Input/Mapping", layer = "WSA_SL_SVW_line")

roads <- readOGR(dsn = "Data Input/Mapping", layer = "DRA_MPAR_line")

lakes <- readOGR(dsn = "Data Input/Mapping", layer = "FWLKSPL_polygon")
  

# Check projection of these files to make sure they match and use to convert lat/long data in next step.
proj4string(streams)
proj4string(roads)
proj4string(lakes)


# Prep Data for Mapping -------------------------------------------------------

## Re-project lat/long data ---------------------------------------------------


# Turn the fixed station location into a shapefile, set its coordinate system, and transform it's projection to UTM
coordinates(fixedLocations) <- c("long", "lat")
proj4string(fixedLocations) <- CRS("+proj=longlat +datum=WGS84")
fixedLocations <- spTransform(fixedLocations, proj4string(streams))

# Now do the same for the mobile data
coordinates(mobileDetections) <- c("long", "lat")
proj4string(mobileDetections) <- CRS("+proj=longlat +datum=WGS84")
mobileLocations <- spTransform(mobileDetections, proj4string(streams))

# and for last detections
coordinates(lastDetections) <- c("long", "lat")
proj4string(lastDetections) <- CRS("+proj=longlat +datum=WGS84")
lastDetections <- spTransform(lastDetections, proj4string(streams))


## Subset the shapefiles to reduce their size for plotting --------------------

# We'll only want to plot streams with a stream order of 3 or higher
streams <- subset(streams, STRMRDR >= 3)

# and we'll only plot main roads
roads <- subset(roads, ROAD_CLASS == "highway" | ROAD_CLASS =="arterial")

# and finally only lakes with names
lakes <- subset(lakes, !(is.na(GNSNM1)))



## Save Shapefiles as ggplot objects ------------------------------------------

streamsPlot <- merge(fortify(streams), as.data.frame(streams), 
                              by.x = "id", 
                              by.y = 0)

roadsPlot <- merge(fortify(roads), as.data.frame(roads), 
                     by.x = "id", 
                     by.y = 0)

lakesPlot <- merge(fortify(lakes), as.data.frame(lakes), 
                     by.x = "id", 
                     by.y = 0)



# Plot Study Area -------------------------------------------------------------

mapExtent<- data.frame(ylims = c(5950000, 6120000),
                           xlims = c(580000, 660000))

mapExtentZoom<- data.frame(ylims = c(5965000, 5990000), #zoomed in on Morice Lake and tribs
                           xlims = c(575000, 590000)) #perhaps delete

# Study area
studyArea <-
  ggplot() +
  geom_path(data = streamsPlot, aes(x = long, y = lat, group = group), colour = "lightblue", size = 1.1) +
  geom_polygon(data = filter(lakesPlot), aes(x = long, y = lat, group = group), colour = "#104692", fill = "lightblue") +
  geom_path(data = roadsPlot, aes(x = long, y = lat, group = group), colour = "#747687", size = 1.1) +
  geom_point(aes(x = fixedLocations$long, y = fixedLocations$lat), colour = "#104692", fill = "#104692", size = 8, shape = 21) +
  annotate("text", x = 612000, y = 6070000, label = "Smithers", size = 12) +  annotate("text", x = 649500, y = 6025880, label = "Houston", size = 10) + 
  annotate("text", x = 590800, y = 5980000, label = "Morice Lake", size = 10) +  annotate("text", x = 624700, y = 6010000, label = "Morice River", size = 10) + 
  annotate("text", x = 613000, y = 6098000, label = "Fallback \nStation", size = 8) +   annotate("text", x = 602000, y = 6097000, label = "Witset\nCanyon", size = 8) +
  annotate("text", x = 624700, y = 6074700, label = "Lower Bulkley\nStation", size = 8) + annotate("text", x = 603000, y = 6003000, label = "Morice Lake Outlet\nStation", size = 8) + 
  annotate("text", x = 603000, y = 5983500, label = "Nanika River\nStation", size = 8) + annotate("text", x = 582351, y = 5985871, label = "Atna River\nStation", size = 8) +
  coord_cartesian(xlim = mapExtent$xlims, ylim = mapExtent$ylims) +
  ggsn::scalebar(streamsPlot, location = "bottomright", dist = 1, dist_unit = "km", st.size = 3.5, height = 0.015, transform = FALSE) + #model = 'WGS84') +
  ggsn::north(data = streamsPlot, scale = 0.1, symbol = 3) +
  theme_ifr() +
  theme(legend.position = "none", axis.title = element_blank())
#map

ggsave(studyArea, file="Figures and Tables/099_StudyArea_2023.png",
       width=15, height=18, scale=1)



# Mobile Survey Maps ----------------------------------------------------------

# 2023-08-30 Mobile Survey

##Next time to work on this - add zoom plot with labels for inset map.

#Subset data for this survey
aug30MobData <- subset(mobileLocations, date == "2023-08-30")

## adjust jitter as needed if points/lables are obstructed

jitter_amount <- 500
aug30MobData$longjittered <- aug30MobData$long + runif(length(aug30MobData$long), -jitter_amount, jitter_amount)
aug30MobData$latjittered <- aug30MobData$lat + runif(length(aug30MobData$lat), -jitter_amount, jitter_amount)

# set jitter to 500
aug30Plot <-
  ggplot() +
  geom_path(data = streamsPlot, aes(x = long, y = lat, group = group), colour = "lightblue", size = 1.1) +
  geom_polygon(data = filter(lakesPlot), aes(x = long, y = lat, group = group), colour = "#104692", fill = "lightblue") +
  geom_path(data = roadsPlot, aes(x = long, y = lat, group = group), colour = "#747687", size = 1.1) +
  #geom_point(aes(x = fixedLocations$long, y = fixedLocations$lat), colour = "#a40c01", fill = "#a40c01", size = 8, shape = 21) +
  geom_point(aes(x = aug30MobData$longjittered, y = aug30MobData$latjittered), colour = "#a40c01", fill = "#a40c01", size = 12, shape = 21) +
  geom_text(aes(label = aug30MobData$code, x = aug30MobData$longjittered, y = aug30MobData$latjittered), color = "white", size = 7) +
  annotate("text", x = 612000, y = 6070000, label = "Smithers", size = 12) +  annotate("text", x = 649500, y = 6025880, label = "Houston", size = 10) + 
  annotate("text", x = 590800, y = 5980000, label = "Morice Lake", size = 10) +  annotate("text", x = 624700, y = 6010000, label = "Morice River", size = 10) + 
  # annotate("text", x = 579900, y = 5987500, label = "Atna Lake", size = 10) + 
  # annotate("text", x = 601387, y = 5978946, label = "Nanika river", size = 10) + 
  # annotate("text", x = 580067, y = 5971031, label = "Morice Lake", size = 10) + 
  # annotate("text", x = 613000, y = 6098000, label = "Fallback \nStation", size = 8) +   
  annotate("text", x = 602000, y = 6097000, label = "Witset\nCanyon", size = 8) +
  # annotate("text", x = 624700, y = 6074700, label = "Lower Bulkley\nStation", size = 8) +
  # annotate("text", x = 603500, y = 6003000, label = "Morice Lake Outlet\nStation", size = 8) + 
  # annotate("text", x = 604500, y = 5983500, label = "Nanika River\nStation", size = 8) +
  # annotate("text", x = 582351, y = 5985871, label = "Atna River\nStation", size = 8) +
  labs(title = "August 30, 2023") +
  coord_cartesian(xlim = mapExtent$xlims, ylim = mapExtent$ylims) +
  ggsn::scalebar(streamsPlot, location = "bottomright", dist = 1, dist_unit = "km", st.size = 3.5, height = 0.015, transform = FALSE) + #model = 'WGS84') +
  ggsn::north(data = streamsPlot, scale = 0.1, symbol = 3) +
  theme_ifr() +
  theme(legend.position = "none", axis.title = element_blank())
#map

ggsave(aug30Plot, file="Figures and Tables/099_MobileDetections_Aug30_2023.png",
       width=15, height=18, scale=1)

aug30PlotZoom <-
  ggplot() +
  geom_path(data = streamsPlot, aes(x = long, y = lat, group = group), colour = "lightblue", size = 1.1) +
  geom_polygon(data = filter(lakesPlot), aes(x = long, y = lat, group = group), colour = "#104692", fill = "lightblue") +
  geom_path(data = roadsPlot, aes(x = long, y = lat, group = group), colour = "#747687", size = 1.1) +
  #geom_point(aes(x = fixedLocations$long, y = fixedLocations$lat), colour = "#a40c01", fill = "#a40c01", size = 8, shape = 21) +
  geom_point(aes(x = aug30MobData$longjittered, y = aug30MobData$latjittered), colour = "#a40c01", fill = "#a40c01", size = 12, shape = 21) +
  geom_text(aes(label = aug30MobData$code, x = aug30MobData$longjittered, y = aug30MobData$latjittered), color = "white", size = 7) +
  annotate("text", x = 612000, y = 6070000, label = "Smithers", size = 12) +  annotate("text", x = 649500, y = 6025880, label = "Houston", size = 10) + 
  annotate("text", x = 590800, y = 5980000, label = "Morice Lake", size = 10) +  annotate("text", x = 624700, y = 6010000, label = "Morice River", size = 10) + 
  annotate("text", x = 579900, y = 5987500, label = "Atna Lake", size = 10) +
  annotate("text", x = 601387, y = 5978946, label = "Nanika river", size = 10) +
  annotate("text", x = 580067, y = 5971031, label = "Morice Lake", size = 10) +
  annotate("text", x = 613000, y = 6098000, label = "Fallback \nStation", size = 8) +
  annotate("text", x = 602000, y = 6097000, label = "Witset\nCanyon", size = 8) +
  annotate("text", x = 624700, y = 6074700, label = "Lower Bulkley\nStation", size = 8) +
  annotate("text", x = 603500, y = 6003000, label = "Morice Lake Outlet\nStation", size = 8) +
  annotate("text", x = 604500, y = 5983500, label = "Nanika River\nStation", size = 8) +
  annotate("text", x = 582351, y = 5985871, label = "Atna River\nStation", size = 8) +
  labs(title = "August 30, 2023") +
  coord_cartesian(xlim = mapExtentZoom$xlims, ylim = mapExtentZoom$ylims) +
  ggsn::scalebar(streamsPlot, location = "bottomright", dist = 1, dist_unit = "km", st.size = 3.5, height = 0.015, transform = FALSE) + #model = 'WGS84') +
  ggsn::north(data = streamsPlot, scale = 0.1, symbol = 3) +
  theme_ifr() +
  theme(legend.position = "none", axis.title = element_blank())

ggsave(aug30PlotZoom, file="Figures and Tables/099_MobileDetections_Aug30_2023_Zoom.png",
       width=15, height=18, scale=1)

#inset map
aug30PlotFinal <- 
  ggdraw(aug30Plot)  +
  draw_plot(
    {
      aug30PlotZoom +
        coord_sf(
          xlim = c(576500, 583500),
          ylim = c(5984000, 5991000),
          expand = FALSE) +
        theme(legend.position = "none",
              axis.text = element_blank(),
              axis.ticks = element_blank()) +
        labs(title = "Atna Lake")
    }, 
    x = 0.675, 
    y = 0.675, 
    width = 0.3,
    height = 0.3
  ) +
  draw_plot(
    {
      aug30PlotZoom +
        coord_sf(
          xlim = c(576500, 584000), #7500 apart
          ylim = c(5966000, 5973500), #7500 apart
          expand = FALSE) +
        theme(legend.position = "none", 
              axis.text = element_blank(),
              axis.ticks = element_blank()) +
        labs(title = "SW Morice Lake")
    },
    x = 0.675,
    y = 0.025,
    width = 0.3,
    height = 0.3
  )

#Atna Lake tags: 156, 62, 86, 67, 57, 3, 140, 7, 27, 12, 121, 78, 148
#SW Morice Lake tags: 69, 71, 28, 157, 81
#Morice River by Houston tags: 84, 144


ggsave(aug30PlotFinal, file="Figures and Tables/099_MobileDetections_Aug30_2023_Final.png",
       width=15, height=18, scale=1)




# 2023-09-06 Mobile Survey

#Subset data for this survey
sept6MobData <- subset(mobileLocations, date == "2023-09-06")

## adjust jitter as needed if points/lables are obstructed

jitter_amount <- 500
sept6MobData$longjittered <- sept6MobData$long + runif(length(sept6MobData$long), -jitter_amount, jitter_amount)
sept6MobData$latjittered <- sept6MobData$lat + runif(length(sept6MobData$lat), -jitter_amount, jitter_amount)

# Study area
sept6Plot <-
  ggplot() +
  geom_path(data = streamsPlot, aes(x = long, y = lat, group = group), colour = "lightblue", size = 1.1) +
  geom_polygon(data = filter(lakesPlot), aes(x = long, y = lat, group = group), colour = "#104692", fill = "lightblue") +
  geom_path(data = roadsPlot, aes(x = long, y = lat, group = group), colour = "#747687", size = 1.1) +
  #geom_point(aes(x = fixedLocations$long, y = fixedLocations$lat), colour = "#a40c01", fill = "#a40c01", size = 8, shape = 21) +
  geom_point(aes(x = sept6MobData$longjittered, y = sept6MobData$latjittered), colour = "#a40c01", fill = "#a40c01", size = 12, shape = 21) +
  geom_text(aes(label = sept6MobData$code, x = sept6MobData$longjittered, y = sept6MobData$latjittered), color = "white", size = 7) +
  annotate("text", x = 612000, y = 6070000, label = "Smithers", size = 12) +  annotate("text", x = 649500, y = 6025880, label = "Houston", size = 10) + 
  annotate("text", x = 590800, y = 5980000, label = "Morice Lake", size = 10) +  annotate("text", x = 624700, y = 6010000, label = "Morice River", size = 10) + 
  # annotate("text", x = 579900, y = 5987500, label = "Atna Lake", size = 10) + 
  # annotate("text", x = 601387, y = 5978946, label = "Nanika river", size = 10) + 
  # annotate("text", x = 580067, y = 5971031, label = "Morice Lake", size = 10) + 
  # annotate("text", x = 613000, y = 6098000, label = "Fallback \nStation", size = 8) +   
  annotate("text", x = 602000, y = 6097000, label = "Witset\nCanyon", size = 8) +
  # annotate("text", x = 624700, y = 6074700, label = "Lower Bulkley\nStation", size = 8) +
  # annotate("text", x = 603500, y = 6003000, label = "Morice Lake Outlet\nStation", size = 8) + 
  # annotate("text", x = 604500, y = 5983500, label = "Nanika River\nStation", size = 8) +
  # annotate("text", x = 582351, y = 5985871, label = "Atna River\nStation", size = 8) +
  labs(title = "September 06, 2023") +
  coord_cartesian(xlim = mapExtent$xlims, ylim = mapExtent$ylims) +
  ggsn::scalebar(streamsPlot, location = "bottomright", dist = 1, dist_unit = "km", st.size = 3.5, height = 0.015, transform = FALSE) + #model = 'WGS84') +
  ggsn::north(data = streamsPlot, scale = 0.1, symbol = 3) +
  theme_ifr() +
  theme(legend.position = "none", axis.title = element_blank())

#map
ggsave(sept6Plot, file="Figures and Tables/099_MobileDetections_Sept06_2023.png",
       width=15, height=18, scale=1)


sept6PlotZoom <-
  ggplot() +
  geom_path(data = streamsPlot, aes(x = long, y = lat, group = group), colour = "lightblue", size = 1.1) +
  geom_polygon(data = filter(lakesPlot), aes(x = long, y = lat, group = group), colour = "#104692", fill = "lightblue") +
  geom_path(data = roadsPlot, aes(x = long, y = lat, group = group), colour = "#747687", size = 1.1) +
  #geom_point(aes(x = fixedLocations$long, y = fixedLocations$lat), colour = "#a40c01", fill = "#a40c01", size = 8, shape = 21) +
  geom_point(aes(x = sept6MobData$longjittered, y = sept6MobData$latjittered), colour = "#a40c01", fill = "#a40c01", size = 12, shape = 21) +
  geom_text(aes(label = sept6MobData$code, x = sept6MobData$longjittered, y = sept6MobData$latjittered), color = "white", size = 7) +
  annotate("text", x = 612000, y = 6070000, label = "Smithers", size = 12) +  annotate("text", x = 649500, y = 6025880, label = "Houston", size = 10) + 
  annotate("text", x = 590800, y = 5980000, label = "Morice Lake", size = 10) +  annotate("text", x = 624700, y = 6010000, label = "Morice River", size = 10) + 
  annotate("text", x = 579900, y = 5987500, label = "Atna Lake", size = 10) +
  annotate("text", x = 601387, y = 5978946, label = "Nanika river", size = 10) +
  annotate("text", x = 580067, y = 5971031, label = "Morice Lake", size = 10) +
  annotate("text", x = 613000, y = 6098000, label = "Fallback \nStation", size = 8) +
  annotate("text", x = 602000, y = 6097000, label = "Witset\nCanyon", size = 8) +
  annotate("text", x = 624700, y = 6074700, label = "Lower Bulkley\nStation", size = 8) +
  annotate("text", x = 603500, y = 6003000, label = "Morice Lake Outlet\nStation", size = 8) +
  annotate("text", x = 604500, y = 5983500, label = "Nanika River\nStation", size = 8) +
  annotate("text", x = 582351, y = 5985871, label = "Atna River\nStation", size = 8) +
  labs(title = "August 30, 2023") +
  coord_cartesian(xlim = mapExtentZoom$xlims, ylim = mapExtentZoom$ylims) +
  ggsn::scalebar(streamsPlot, location = "bottomright", dist = 1, dist_unit = "km", st.size = 3.5, height = 0.015, transform = FALSE) + #model = 'WGS84') +
  ggsn::north(data = streamsPlot, scale = 0.1, symbol = 3) +
  theme_ifr() +
  theme(legend.position = "none", axis.title = element_blank())

ggsave(sept6PlotZoom, file="Figures and Tables/099_MobileDetections_Sept06_2023_Zoom.png",
       width=15, height=18, scale=1)

#inset map
sept6PlotFinal <- 
  ggdraw(sept6Plot)  +
  draw_plot(
    {
      sept6PlotZoom +
        coord_sf(
          xlim = c(576500, 583500),
          ylim = c(5984000, 5991000),
          expand = FALSE) +
        theme(legend.position = "none",
              axis.text = element_blank(),
              axis.ticks = element_blank()) +
        labs(title = "Atna Lake")
    }, 
    x = 0.675, 
    y = 0.675, 
    width = 0.3,
    height = 0.3
  )

#Atna Lake tags: 121, 140, 12, 86, 159, 27, 29, 62, 89, 40, 66, 18, 39, 87, 1, 3, 67, 148, 104

ggsave(sept6PlotFinal, file="Figures and Tables/099_MobileDetections_Sept06_2023_Final.png",
       width=15, height=18, scale=1)



# 2022-09-13 Mobile Survey

#Subset data for this survey
sept13MobData <- subset(mobileLocations, date == "2023-09-13") 
# %>%
#   as.data.frame()

## adjust jitter as needed if points/lables are obstructed

jitter_amount <- 500
sept13MobData$longjittered <- sept13MobData$long + runif(length(sept13MobData$long), -jitter_amount, jitter_amount)
sept13MobData$latjittered <- sept13MobData$lat + runif(length(sept13MobData$lat), -jitter_amount, jitter_amount)

#set jitter to 500

# Study area
sept13Plot <-
  ggplot() +
  geom_path(data = streamsPlot, aes(x = long, y = lat, group = group), colour = "lightblue", size = 1.1) +
  geom_polygon(data = filter(lakesPlot), aes(x = long, y = lat, group = group), colour = "#104692", fill = "lightblue") +
  geom_path(data = roadsPlot, aes(x = long, y = lat, group = group), colour = "#747687", size = 1.1) +
  #geom_point(aes(x = fixedLocations$long, y = fixedLocations$lat), colour = "#a40c01", fill = "#a40c01", size = 8, shape = 21) +
  #geom_point(aes(x = sept13MobData$longjittered, y = sept13MobData$latjittered), colour = "#a40c01", fill = "#a40c01", size = 12, shape = 21) +
  #geom_text(aes(label = sept13MobData$code, x = sept13MobData$longjittered, y = sept13MobData$latjittered), color = "white", size = 7) +
  geom_point(aes(x = sept13MobData$long, y = sept13MobData$lat), colour = "#a40c01", fill = "#a40c01", size = 12, shape = 21) +
  geom_text(aes(label = sept13MobData$code, x = sept13MobData$long, y = sept13MobData$lat), color = "white", size = 7) +
  annotate("text", x = 612000, y = 6070000, label = "Smithers", size = 12) +  
  annotate("text", x = 649500, y = 6025880, label = "Houston", size = 10) + 
  annotate("text", x = 590800, y = 5980000, label = "Morice Lake", size = 10) +  
  # annotate("text", x = 579900, y = 5987500, label = "Atna Lake", size = 10) + 
  # annotate("text", x = 601387, y = 5978946, label = "Nanika river", size = 10) + 
  # annotate("text", x = 580067, y = 5971031, label = "Morice Lake", size = 10) + #for inset map of morice lake
  # annotate("text", x = 613000, y = 6098000, label = "Fallback \nStation", size = 8) +   
  annotate("text", x = 602000, y = 6097000, label = "Witset\nCanyon", size = 8) +
  # annotate("text", x = 624700, y = 6074700, label = "Lower Bulkley\nStation", size = 8) +
  # annotate("text", x = 603500, y = 6003000, label = "Morice Lake Outlet\nStation", size = 8) + 
  # annotate("text", x = 604500, y = 5983500, label = "Nanika River\nStation", size = 8) +
  # annotate("text", x = 582351, y = 5985871, label = "Atna River\nStation", size = 8) +
  labs(title = "September 13, 2023") +
  coord_cartesian(xlim = mapExtent$xlims, ylim = mapExtent$ylims) +
  ggsn::scalebar(streamsPlot, location = "bottomright", dist = 1, dist_unit = "km", st.size = 3.5, height = 0.015, transform = FALSE) + #model = 'WGS84') +
  ggsn::north(data = streamsPlot, scale = 0.1, symbol = 3) +
  theme_ifr() +
  theme(legend.position = "none", axis.title = element_blank())
#map

ggsave(sept13Plot, file="Figures and Tables/099_MobileDetections_Sept13_2023.png",
       width=15, height=18, scale=1)

#set jitter to 500

sept13PlotZoom <-
  ggplot() +
  geom_path(data = streamsPlot, aes(x = long, y = lat, group = group), colour = "lightblue", size = 1.1) +
  geom_polygon(data = filter(lakesPlot), aes(x = long, y = lat, group = group), colour = "#104692", fill = "lightblue") +
  geom_path(data = roadsPlot, aes(x = long, y = lat, group = group), colour = "#747687", size = 1.1) +
  #geom_point(aes(x = fixedLocations$long, y = fixedLocations$lat), colour = "#a40c01", fill = "#a40c01", size = 8, shape = 21) +
  geom_point(aes(x = sept13MobData$longjittered, y = sept13MobData$latjittered), colour = "#a40c01", fill = "#a40c01", size = 12, shape = 21) +
  geom_text(aes(label = sept13MobData$code, x = sept13MobData$longjittered, y = sept13MobData$latjittered), color = "white", size = 7) +
  annotate("text", x = 612000, y = 6070000, label = "Smithers", size = 12) +  annotate("text", x = 649500, y = 6025880, label = "Houston", size = 10) + 
  annotate("text", x = 590800, y = 5980000, label = "Morice Lake", size = 10) +  annotate("text", x = 624700, y = 6010000, label = "Morice River", size = 10) + 
  annotate("text", x = 579900, y = 5987500, label = "Atna Lake", size = 10) + 
  annotate("text", x = 601387, y = 5978946, label = "Nanika river", size = 10) + 
  annotate("text", x = 580067, y = 5971031, label = "Morice Lake", size = 10) + 
  # annotate("text", x = 613000, y = 6098000, label = "Fallback \nStation", size = 8) +   
  annotate("text", x = 602000, y = 6097000, label = "Witset\nCanyon", size = 8) +
  # annotate("text", x = 624700, y = 6074700, label = "Lower Bulkley\nStation", size = 8) + annotate("text", x = 603500, y = 6003000, label = "Morice Lake Outlet\nStation", size = 8) + 
  annotate("text", x = 604500, y = 5983500, label = "Nanika River\nStation", size = 8) + annotate("text", x = 582351, y = 5985871, label = "Atna River\nStation", size = 8) +
  labs(title = "September 13, 2023") +
  coord_cartesian(xlim = mapExtentZoom$xlims, ylim = mapExtentZoom$ylims) +
  ggsn::scalebar(streamsPlot, location = "bottomright", dist = 1, dist_unit = "km", st.size = 3.5, height = 0.015, transform = FALSE) + #model = 'WGS84') +
  ggsn::north(data = streamsPlot, scale = 0.1, symbol = 3) +
  theme_ifr() +
  theme(legend.position = "none", axis.title = element_blank())
#map

ggsave(sept13PlotZoom, file="Figures and Tables/099_MobileDetections_Sept13_2023_Zoom.png",
       width=15, height=18, scale=1)

##inset map

sept13PlotFinal <- 
  ggdraw(sept13Plot)  +
  draw_plot(
    {
      sept13PlotZoom +
        coord_sf(
          xlim = c(576500, 583500),
          ylim = c(5984000, 5991000),
          expand = FALSE) +
        theme(legend.position = "none",
              axis.text = element_blank(),
              axis.ticks = element_blank()) +
        labs(title = "Atna Lake")
    }, 
    x = 0.675, 
    y = 0.675, 
    width = 0.3,
    height = 0.3
  ) +
  draw_plot(
    {
      sept13PlotZoom +
        coord_sf(
          xlim = c(598500, 603500), #2500 apart
          ylim = c(5976000, 5981000), #2500 apart
          expand = FALSE) +
        theme(legend.position = "none",
              axis.text = element_blank(),
              axis.ticks = element_blank()) +
        labs(title = "Nanika River")
    },
    x = 0.375,
    y = 0.025,
    width = 0.3,
    height = 0.3
  ) +
  draw_plot(
    {
      sept13PlotZoom +
        coord_sf(
          xlim = c(576500, 584000), #7500 apart
          ylim = c(5966000, 5973500), #7500 apart
          expand = FALSE) +
        theme(legend.position = "none", 
              axis.text = element_blank(),
              axis.ticks = element_blank()) +
        labs(title = "SW Morice Lake")
    },
    x = 0.675,
    y = 0.025,
    width = 0.3,
    height = 0.3
  )

#Nanika River tags:151, 72, 125, 113, 120, 75, 118, 146, 11

ggsave(sept13PlotFinal, file="Figures and Tables/099_MobileDetections_Sept13_2023_Final.png",
       width=15, height=18, scale=1)

# 2023-09-20 Mobile Survey

#Subset data for this survey
sept20MobData <- subset(mobileLocations, date == "2023-09-20")%>%
  as.data.frame()

## adjust jitter as needed if points/lables are obstructed

jitter_amount <- 500
sept20MobData$longjittered <- sept20MobData$long + runif(length(sept20MobData$long), -jitter_amount, jitter_amount)
sept20MobData$latjittered <- sept20MobData$lat + runif(length(sept20MobData$lat), -jitter_amount, jitter_amount)


# Study area
sept20Plot <-
  ggplot() +
  geom_path(data = streamsPlot, aes(x = long, y = lat, group = group), colour = "lightblue", size = 1.1) +
  geom_polygon(data = filter(lakesPlot), aes(x = long, y = lat, group = group), colour = "#104692", fill = "lightblue") +
  geom_path(data = roadsPlot, aes(x = long, y = lat, group = group), colour = "#747687", size = 1.1) +
  #geom_point(aes(x = fixedLocations$long, y = fixedLocations$lat), colour = "#a40c01", fill = "#a40c01", size = 8, shape = 21) +
  geom_point(aes(x = sept20MobData$longjittered, y = sept20MobData$latjittered), colour = "#a40c01", fill = "#a40c01", size = 12, shape = 21) +
  geom_text(aes(label = sept20MobData$code, x = sept20MobData$longjittered, y = sept20MobData$latjittered), color = "white", size = 7) +
  annotate("text", x = 612000, y = 6070000, label = "Smithers", size = 12) +  annotate("text", x = 649500, y = 6025880, label = "Houston", size = 10) + 
  annotate("text", x = 590800, y = 5980000, label = "Morice Lake", size = 10) +  annotate("text", x = 624700, y = 6010000, label = "Morice River", size = 10) + 
  # annotate("text", x = 579900, y = 5987500, label = "Atna Lake", size = 10) + 
  # annotate("text", x = 601387, y = 5978946, label = "Nanika river", size = 10) + 
  # annotate("text", x = 580067, y = 5971031, label = "Morice Lake", size = 10) + 
  # annotate("text", x = 613000, y = 6098000, label = "Fallback \nStation", size = 8) +   
  annotate("text", x = 602000, y = 6097000, label = "Witset\nCanyon", size = 8) +
  # annotate("text", x = 624700, y = 6074700, label = "Lower Bulkley\nStation", size = 8) +
  # annotate("text", x = 603500, y = 6003000, label = "Morice Lake Outlet\nStation", size = 8) + 
  # annotate("text", x = 604500, y = 5983500, label = "Nanika River\nStation", size = 8) +
  # annotate("text", x = 582351, y = 5985871, label = "Atna River\nStation", size = 8) +
  labs(title = "September 20, 2023") +
  coord_cartesian(xlim = mapExtent$xlims, ylim = mapExtent$ylims) +
  ggsn::scalebar(streamsPlot, location = "bottomright", dist = 1, dist_unit = "km", st.size = 3.5, height = 0.015, transform = FALSE) + #model = 'WGS84') +
  ggsn::north(data = streamsPlot, scale = 0.1, symbol = 3) +
  theme_ifr() +
  theme(legend.position = "none", axis.title = element_blank())
#map

ggsave(sept20Plot, file="Figures and Tables/099_MobileDetections_Sept20_2023.png",
       width=15, height=18, scale=1)

sept20PlotZoom <-
  ggplot() +
  geom_path(data = streamsPlot, aes(x = long, y = lat, group = group), colour = "lightblue", size = 1.1) +
  geom_polygon(data = filter(lakesPlot), aes(x = long, y = lat, group = group), colour = "#104692", fill = "lightblue") +
  geom_path(data = roadsPlot, aes(x = long, y = lat, group = group), colour = "#747687", size = 1.1) +
  #geom_point(aes(x = fixedLocations$long, y = fixedLocations$lat), colour = "#a40c01", fill = "#a40c01", size = 8, shape = 21) +
  geom_point(aes(x = sept20MobData$longjittered, y = sept20MobData$latjittered), colour = "#a40c01", fill = "#a40c01", size = 12, shape = 21) +
  geom_text(aes(label = sept20MobData$code, x = sept20MobData$longjittered, y = sept20MobData$latjittered), color = "white", size = 7) +
  annotate("text", x = 612000, y = 6070000, label = "Smithers", size = 12) +  annotate("text", x = 649500, y = 6025880, label = "Houston", size = 10) + 
  annotate("text", x = 590800, y = 5980000, label = "Morice Lake", size = 10) +  annotate("text", x = 624700, y = 6010000, label = "Morice River", size = 10) + 
  annotate("text", x = 579900, y = 5987500, label = "Atna Lake", size = 10) + 
  annotate("text", x = 601387, y = 5978946, label = "Nanika river", size = 10) + 
  annotate("text", x = 580067, y = 5971031, label = "Morice Lake", size = 10) + 
  # annotate("text", x = 613000, y = 6098000, label = "Fallback \nStation", size = 8) +   
  annotate("text", x = 602000, y = 6097000, label = "Witset\nCanyon", size = 8) +
  # annotate("text", x = 624700, y = 6074700, label = "Lower Bulkley\nStation", size = 8) + annotate("text", x = 603500, y = 6003000, label = "Morice Lake Outlet\nStation", size = 8) + 
  annotate("text", x = 604500, y = 5983500, label = "Nanika River\nStation", size = 8) + annotate("text", x = 582351, y = 5985871, label = "Atna River\nStation", size = 8) +
  labs(title = "September 20, 2023") +
  coord_cartesian(xlim = mapExtentZoom$xlims, ylim = mapExtentZoom$ylims) +
  ggsn::scalebar(streamsPlot, location = "bottomright", dist = 1, dist_unit = "km", st.size = 3.5, height = 0.015, transform = FALSE) + #model = 'WGS84') +
  ggsn::north(data = streamsPlot, scale = 0.1, symbol = 3) +
  theme_ifr() +
  theme(legend.position = "none", axis.title = element_blank())
#map

ggsave(sept20PlotZoom, file="Figures and Tables/099_MobileDetections_Sept20_2023_Zoom.png",
       width=15, height=18, scale=1)

##inset map

sept20PlotFinal <- 
  ggdraw(sept20Plot)  +
  draw_plot(
    {
      sept20PlotZoom +
        coord_sf(
          xlim = c(598500, 603500), #2500 apart
          ylim = c(5976000, 5981000), #2500 apart
          expand = FALSE) +
        theme(legend.position = "none",
              axis.text = element_blank(),
              axis.ticks = element_blank()) +
        labs(title = "Nanika River")
    },
    x = 0.375,
    y = 0.025,
    width = 0.3,
    height = 0.3
  ) 


ggsave(sept20PlotFinal, file="Figures and Tables/099_MobileDetections_Sept20_2023_Final.png",
       width=15, height=18, scale=1)



# 2022-09-22 Mobile Survey

#Subset data for this survey
sept22MobData <- subset(mobileLocations, date == "2023-09-22")%>%
  as.data.frame()

## adjust jitter as needed if points/lables are obstructed

jitter_amount <- 500
sept22MobData$longjittered <- sept22MobData$long + runif(length(sept22MobData$long), -jitter_amount, jitter_amount)
sept22MobData$latjittered <- sept22MobData$lat + runif(length(sept22MobData$lat), -jitter_amount, jitter_amount)

#set jitter to 500

# Study area
sept22Plot <-
  ggplot() +
  geom_path(data = streamsPlot, aes(x = long, y = lat, group = group), colour = "lightblue", size = 1.1) +
  geom_polygon(data = filter(lakesPlot), aes(x = long, y = lat, group = group), colour = "#104692", fill = "lightblue") +
  geom_path(data = roadsPlot, aes(x = long, y = lat, group = group), colour = "#747687", size = 1.1) +
  # geom_point(aes(x = fixedLocations$long, y = fixedLocations$lat), colour = "#a40c01", fill = "#a40c01", size = 8, shape = 21) +
  geom_point(aes(x = sept22MobData$longjittered, y = sept22MobData$latjittered), colour = "#a40c01", fill = "#a40c01", size = 12, shape = 21) +
  geom_text(aes(label = sept22MobData$code, x = sept22MobData$longjittered, y = sept22MobData$latjittered), color = "white", size = 7) +
  # geom_point(aes(x = sept22MobData$long, y = sept22MobData$lat), colour = "#a40c01", fill = "#a40c01", size = 12, shape = 21) +
  # geom_text(aes(label = sept22MobData$code, x = sept22MobData$long, y = sept22MobData$lat), color = "white", size = 7) +
  annotate("text", x = 612000, y = 6070000, label = "Smithers", size = 12) +  
  annotate("text", x = 649500, y = 6025880, label = "Houston", size = 10) + 
  annotate("text", x = 590800, y = 5980000, label = "Morice Lake", size = 10) +  
  # annotate("text", x = 579900, y = 5987500, label = "Atna Lake", size = 10) + 
  # annotate("text", x = 601387, y = 5978946, label = "Nanika river", size = 10) + 
  # annotate("text", x = 580067, y = 5971031, label = "Morice Lake", size = 10) + #for inset map of morice lake
  # annotate("text", x = 613000, y = 6098000, label = "Fallback \nStation", size = 8) +   
  annotate("text", x = 602000, y = 6097000, label = "Witset\nCanyon", size = 8) +
  # annotate("text", x = 624700, y = 6074700, label = "Lower Bulkley\nStation", size = 8) +
  # annotate("text", x = 603500, y = 6003000, label = "Morice Lake Outlet\nStation", size = 8) + 
  # annotate("text", x = 604500, y = 5983500, label = "Nanika River\nStation", size = 8) +
  # annotate("text", x = 582351, y = 5985871, label = "Atna River\nStation", size = 8) +
  labs(title = "September 22, 2023") +
  coord_cartesian(xlim = mapExtent$xlims, ylim = mapExtent$ylims) +
  ggsn::scalebar(streamsPlot, location = "bottomright", dist = 1, dist_unit = "km", st.size = 3.5, height = 0.015, transform = FALSE) + #model = 'WGS84') +
  ggsn::north(data = streamsPlot, scale = 0.1, symbol = 3) +
  theme_ifr() +
  theme(legend.position = "none", axis.title = element_blank())
#map

ggsave(sept22Plot, file="Figures and Tables/099_MobileDetections_Sept22_2023.png",
       width=15, height=18, scale=1)

#set jitter to 500

sept22PlotZoom <-
  ggplot() +
  geom_path(data = streamsPlot, aes(x = long, y = lat, group = group), colour = "lightblue", size = 1.1) +
  geom_polygon(data = filter(lakesPlot), aes(x = long, y = lat, group = group), colour = "#104692", fill = "lightblue") +
  geom_path(data = roadsPlot, aes(x = long, y = lat, group = group), colour = "#747687", size = 1.1) +
  #geom_point(aes(x = fixedLocations$long, y = fixedLocations$lat), colour = "#a40c01", fill = "#a40c01", size = 8, shape = 21) +
  geom_point(aes(x = sept22MobData$longjittered, y = sept22MobData$latjittered), colour = "#a40c01", fill = "#a40c01", size = 12, shape = 21) +
  geom_text(aes(label = sept22MobData$code, x = sept22MobData$longjittered, y = sept22MobData$latjittered), color = "white", size = 7) +
  annotate("text", x = 612000, y = 6070000, label = "Smithers", size = 12) +  annotate("text", x = 649500, y = 6025880, label = "Houston", size = 10) + 
  annotate("text", x = 590800, y = 5980000, label = "Morice Lake", size = 10) +  annotate("text", x = 624700, y = 6010000, label = "Morice River", size = 10) + 
  annotate("text", x = 579900, y = 5987500, label = "Atna Lake", size = 10) + 
  annotate("text", x = 601387, y = 5978946, label = "Nanika river", size = 10) + 
  annotate("text", x = 580067, y = 5971031, label = "Morice Lake", size = 10) + 
  # annotate("text", x = 613000, y = 6098000, label = "Fallback \nStation", size = 8) +   
  annotate("text", x = 602000, y = 6097000, label = "Witset\nCanyon", size = 8) +
  # annotate("text", x = 624700, y = 6074700, label = "Lower Bulkley\nStation", size = 8) + annotate("text", x = 603500, y = 6003000, label = "Morice Lake Outlet\nStation", size = 8) + 
  annotate("text", x = 604500, y = 5983500, label = "Nanika River\nStation", size = 8) + annotate("text", x = 582351, y = 5985871, label = "Atna River\nStation", size = 8) +
  labs(title = "September 22, 2023") +
  coord_cartesian(xlim = mapExtentZoom$xlims, ylim = mapExtentZoom$ylims) +
  ggsn::scalebar(streamsPlot, location = "bottomright", dist = 1, dist_unit = "km", st.size = 3.5, height = 0.015, transform = FALSE) + #model = 'WGS84') +
  ggsn::north(data = streamsPlot, scale = 0.1, symbol = 3) +
  theme_ifr() +
  theme(legend.position = "none", axis.title = element_blank())
#map

ggsave(sept22PlotZoom, file="Figures and Tables/099_MobileDetections_Sept22_2023_Zoom.png",
       width=15, height=18, scale=1)

##inset map

sept22PlotFinal <- 
  ggdraw(sept22Plot)  +
  draw_plot(
    {
      sept22PlotZoom +
        coord_sf(
          xlim = c(638000, 653000),
          ylim = c(6023380, 6038380),
          expand = FALSE) +
        theme(legend.position = "none",
              axis.text = element_blank(),
              axis.ticks = element_blank()) +
        labs(title = "Morice-Bulkley Confluence")
    }, 
    x = 0.675, 
    y = 0.675, 
    width = 0.3,
    height = 0.3
  )

#Witset canyon tags: 76, 13, 153
#Bulkley River tags: 58, 102, 80, 106, 101, 59, 29
#Bulkley/Morice near Houston: 6, 22, 18, 9, 19, 122, 74, 88


ggsave(sept22PlotFinal, file="Figures and Tables/099_MobileDetections_Sept22_2023_Final.png",
       width=15, height=18, scale=1)


# 2022-09-25 Mobile Survey

#Subset data for this survey
sept25MobData <- subset(mobileLocations, date == "2023-09-25")
# %>%
#   as.data.frame()

## adjust jitter as needed if points/lables are obstructed

jitter_amount <- 500
sept25MobData$longjittered <- sept25MobData$long + runif(length(sept25MobData$long), -jitter_amount, jitter_amount)
sept25MobData$latjittered <- sept25MobData$lat + runif(length(sept25MobData$lat), -jitter_amount, jitter_amount)

#set jitter to 500

# Study area
sept25Plot <-
  ggplot() +
  geom_path(data = streamsPlot, aes(x = long, y = lat, group = group), colour = "lightblue", size = 1.1) +
  geom_polygon(data = filter(lakesPlot), aes(x = long, y = lat, group = group), colour = "#104692", fill = "lightblue") +
  geom_path(data = roadsPlot, aes(x = long, y = lat, group = group), colour = "#747687", size = 1.1) +
  #geom_point(aes(x = fixedLocations$long, y = fixedLocations$lat), colour = "#a40c01", fill = "#a40c01", size = 8, shape = 21) +
  geom_point(aes(x = sept25MobData$longjittered, y = sept25MobData$latjittered), colour = "#a40c01", fill = "#a40c01", size = 12, shape = 21) +
  geom_text(aes(label = sept25MobData$code, x = sept25MobData$longjittered, y = sept25MobData$latjittered), color = "white", size = 7) +
  # geom_point(aes(x = sept25MobData$long, y = sept25MobData$lat), colour = "#a40c01", fill = "#a40c01", size = 12, shape = 21) +
  # geom_text(aes(label = sept25MobData$code, x = sept25MobData$long, y = sept25MobData$lat), color = "white", size = 7) +
  annotate("text", x = 612000, y = 6070000, label = "Smithers", size = 12) +  
  annotate("text", x = 649500, y = 6025880, label = "Houston", size = 10) + 
  annotate("text", x = 590800, y = 5980000, label = "Morice Lake", size = 10) +  
  # annotate("text", x = 579900, y = 5987500, label = "Atna Lake", size = 10) + 
  # annotate("text", x = 601387, y = 5978946, label = "Nanika river", size = 10) + 
  # annotate("text", x = 580067, y = 5971031, label = "Morice Lake", size = 10) + #for inset map of morice lake
  # annotate("text", x = 613000, y = 6098000, label = "Fallback \nStation", size = 8) +   
  annotate("text", x = 602000, y = 6097000, label = "Witset\nCanyon", size = 8) +
  # annotate("text", x = 624700, y = 6074700, label = "Lower Bulkley\nStation", size = 8) +
  # annotate("text", x = 603500, y = 6003000, label = "Morice Lake Outlet\nStation", size = 8) + 
  # annotate("text", x = 604500, y = 5983500, label = "Nanika River\nStation", size = 8) +
  # annotate("text", x = 582351, y = 5985871, label = "Atna River\nStation", size = 8) +
  labs(title = "September 25, 2023") +
  coord_cartesian(xlim = mapExtent$xlims, ylim = mapExtent$ylims) +
  ggsn::scalebar(streamsPlot, location = "bottomright", dist = 1, dist_unit = "km", st.size = 3.5, height = 0.015, transform = FALSE) + #model = 'WGS84') +
  ggsn::north(data = streamsPlot, scale = 0.1, symbol = 3) +
  theme_ifr() +
  theme(legend.position = "none", axis.title = element_blank())
#map

ggsave(sept25Plot, file="Figures and Tables/099_MobileDetections_Sept25_2023.png",
       width=15, height=18, scale=1)


#Witset canyon tags: 76, 13, 153


# 2022-09-26 Mobile Survey

#Subset data for this survey
sept26MobData <- subset(mobileLocations, date == "2023-09-26") 
# %>%
#   as.data.frame()

## adjust jitter as needed if points/lables are obstructed

jitter_amount <- 2000
sept26MobData$longjittered <- sept26MobData$long + runif(length(sept26MobData$long), -jitter_amount, jitter_amount)
sept26MobData$latjittered <- sept26MobData$lat + runif(length(sept26MobData$lat), -jitter_amount, jitter_amount)

#set jitter to 500

# Study area
sept26Plot <-
  ggplot() +
  geom_path(data = streamsPlot, aes(x = long, y = lat, group = group), colour = "lightblue", size = 1.1) +
  geom_polygon(data = filter(lakesPlot), aes(x = long, y = lat, group = group), colour = "#104692", fill = "lightblue") +
  geom_path(data = roadsPlot, aes(x = long, y = lat, group = group), colour = "#747687", size = 1.1) +
  # geom_point(aes(x = fixedLocations$long, y = fixedLocations$lat), colour = "#a40c01", fill = "#a40c01", size = 8, shape = 21) +
  geom_point(aes(x = sept26MobData$longjittered, y = sept26MobData$latjittered), colour = "#a40c01", fill = "#a40c01", size = 12, shape = 21) +
  geom_text(aes(label = sept26MobData$code, x = sept26MobData$longjittered, y = sept26MobData$latjittered), color = "white", size = 7) +
  # geom_point(aes(x = sept13MobData$long, y = sept13MobData$lat), colour = "#a40c01", fill = "#a40c01", size = 12, shape = 21) +
  # geom_text(aes(label = sept13MobData$code, x = sept13MobData$long, y = sept13MobData$lat), color = "white", size = 7) +
  annotate("text", x = 612000, y = 6070000, label = "Smithers", size = 12) +  
  annotate("text", x = 649500, y = 6025880, label = "Houston", size = 10) + 
  annotate("text", x = 590800, y = 5980000, label = "Morice Lake", size = 10) +  
  # annotate("text", x = 579900, y = 5987500, label = "Atna Lake", size = 10) + 
  # annotate("text", x = 601387, y = 5978946, label = "Nanika river", size = 10) + 
  # annotate("text", x = 580067, y = 5971031, label = "Morice Lake", size = 10) + #for inset map of morice lake
  # annotate("text", x = 613000, y = 6098000, label = "Fallback \nStation", size = 8) +   
  annotate("text", x = 602000, y = 6097000, label = "Witset\nCanyon", size = 8) +
  # annotate("text", x = 624700, y = 6074700, label = "Lower Bulkley\nStation", size = 8) +
  # annotate("text", x = 603500, y = 6003000, label = "Morice Lake Outlet\nStation", size = 8) + 
  # annotate("text", x = 604500, y = 5983500, label = "Nanika River\nStation", size = 8) +
  # annotate("text", x = 582351, y = 5985871, label = "Atna River\nStation", size = 8) +
  labs(title = "September 26, 2023") +
  coord_cartesian(xlim = mapExtent$xlims, ylim = mapExtent$ylims) +
  ggsn::scalebar(streamsPlot, location = "bottomright", dist = 1, dist_unit = "km", st.size = 3.5, height = 0.015, transform = FALSE) + #model = 'WGS84') +
  ggsn::north(data = streamsPlot, scale = 0.1, symbol = 3) +
  theme_ifr() +
  theme(legend.position = "none", axis.title = element_blank())
#map

ggsave(sept26Plot, file="Figures and Tables/099_MobileDetections_Sept26_2023.png",
       width=15, height=18, scale=1)


##inset map

sept26PlotFinal <- 
  ggdraw(sept26Plot)  +
  draw_plot(
    {
      sept26Plot +
        coord_sf(
          xlim = c(645000, 660000),
          ylim = c(6023380, 6038380),
          expand = FALSE) +
        theme(legend.position = "none",
              axis.text = element_blank(),
              axis.ticks = element_blank()) +
        labs(title = "Morice-Bulkley Confluence")
    }, 
    x = 0.675, 
    y = 0.675, 
    width = 0.3,
    height = 0.3
  ) 

#Witset canyon tags: 76, 13, 153
#Bulkley/Morice near Houston: 60, 58, 19, 122


ggsave(sept26PlotFinal, file="Figures and Tables/099_MobileDetections_Sept26_2023_Final.png",
       width=15, height=18, scale=1)


# 2022-09-27 Mobile Survey

#Subset data for this survey
sept27MobData <- subset(mobileLocations, date == "2023-09-27") 
# %>%
#   as.data.frame()

## adjust jitter as needed if points/lables are obstructed

jitter_amount <- 500
sept27MobData$longjittered <- sept27MobData$long + runif(length(sept27MobData$long), -jitter_amount, jitter_amount)
sept27MobData$latjittered <- sept27MobData$lat + runif(length(sept27MobData$lat), -jitter_amount, jitter_amount)

#set jitter to 500

# Study area
sept27Plot <-
  ggplot() +
  geom_path(data = streamsPlot, aes(x = long, y = lat, group = group), colour = "lightblue", size = 1.1) +
  geom_polygon(data = filter(lakesPlot), aes(x = long, y = lat, group = group), colour = "#104692", fill = "lightblue") +
  geom_path(data = roadsPlot, aes(x = long, y = lat, group = group), colour = "#747687", size = 1.1) +
  # geom_point(aes(x = fixedLocations$long, y = fixedLocations$lat), colour = "#a40c01", fill = "#a40c01", size = 8, shape = 21) +
  geom_point(aes(x = sept27MobData$longjittered, y = sept27MobData$latjittered), colour = "#a40c01", fill = "#a40c01", size = 12, shape = 21) +
  geom_text(aes(label = sept27MobData$code, x = sept27MobData$longjittered, y = sept27MobData$latjittered), color = "white", size = 7) +
  # geom_point(aes(x = sept13MobData$long, y = sept13MobData$lat), colour = "#a40c01", fill = "#a40c01", size = 12, shape = 21) +
  # geom_text(aes(label = sept13MobData$code, x = sept13MobData$long, y = sept13MobData$lat), color = "white", size = 7) +
  annotate("text", x = 612000, y = 6070000, label = "Smithers", size = 12) +  
  annotate("text", x = 649500, y = 6025880, label = "Houston", size = 10) + 
  annotate("text", x = 590800, y = 5980000, label = "Morice Lake", size = 10) +  
  # annotate("text", x = 579900, y = 5987500, label = "Atna Lake", size = 10) + 
  # annotate("text", x = 601387, y = 5978946, label = "Nanika river", size = 10) + 
  # annotate("text", x = 580067, y = 5971031, label = "Morice Lake", size = 10) + #for inset map of morice lake
  # annotate("text", x = 613000, y = 6098000, label = "Fallback \nStation", size = 8) +   
  annotate("text", x = 602000, y = 6097000, label = "Witset\nCanyon", size = 8) +
  # annotate("text", x = 624700, y = 6074700, label = "Lower Bulkley\nStation", size = 8) +
  # annotate("text", x = 603500, y = 6003000, label = "Morice Lake Outlet\nStation", size = 8) + 
  # annotate("text", x = 604500, y = 5983500, label = "Nanika River\nStation", size = 8) +
  # annotate("text", x = 582351, y = 5985871, label = "Atna River\nStation", size = 8) +
  labs(title = "September 27, 2023") +
  coord_cartesian(xlim = mapExtent$xlims, ylim = mapExtent$ylims) +
  ggsn::scalebar(streamsPlot, location = "bottomright", dist = 1, dist_unit = "km", st.size = 3.5, height = 0.015, transform = FALSE) + #model = 'WGS84') +
  ggsn::north(data = streamsPlot, scale = 0.1, symbol = 3) +
  theme_ifr() +
  theme(legend.position = "none", axis.title = element_blank())
#map

ggsave(sept27Plot, file="Figures and Tables/099_MobileDetections_Sept27_2023.png",
       width=15, height=18, scale=1)

#set jitter to 500

sept27PlotZoom <-
  ggplot() +
  geom_path(data = streamsPlot, aes(x = long, y = lat, group = group), colour = "lightblue", size = 1.1) +
  geom_polygon(data = filter(lakesPlot), aes(x = long, y = lat, group = group), colour = "#104692", fill = "lightblue") +
  geom_path(data = roadsPlot, aes(x = long, y = lat, group = group), colour = "#747687", size = 1.1) +
  #geom_point(aes(x = fixedLocations$long, y = fixedLocations$lat), colour = "#a40c01", fill = "#a40c01", size = 8, shape = 21) +
  geom_point(aes(x = sept27MobData$longjittered, y = sept27MobData$latjittered), colour = "#a40c01", fill = "#a40c01", size = 12, shape = 21) +
  geom_text(aes(label = sept27MobData$code, x = sept27MobData$longjittered, y = sept27MobData$latjittered), color = "white", size = 7) +
  annotate("text", x = 612000, y = 6070000, label = "Smithers", size = 12) +  annotate("text", x = 649500, y = 6025880, label = "Houston", size = 10) + 
  annotate("text", x = 590800, y = 5980000, label = "Morice Lake", size = 10) +  annotate("text", x = 624700, y = 6010000, label = "Morice River", size = 10) + 
  annotate("text", x = 579900, y = 5987500, label = "Atna Lake", size = 10) + 
  annotate("text", x = 601387, y = 5978946, label = "Nanika river", size = 10) + 
  annotate("text", x = 580067, y = 5971031, label = "Morice Lake", size = 10) + 
  # annotate("text", x = 613000, y = 6098000, label = "Fallback \nStation", size = 8) +   
  annotate("text", x = 602000, y = 6097000, label = "Witset\nCanyon", size = 8) +
  # annotate("text", x = 624700, y = 6074700, label = "Lower Bulkley\nStation", size = 8) + annotate("text", x = 603500, y = 6003000, label = "Morice Lake Outlet\nStation", size = 8) + 
  annotate("text", x = 604500, y = 5983500, label = "Nanika River\nStation", size = 8) + annotate("text", x = 582351, y = 5985871, label = "Atna River\nStation", size = 8) +
  labs(title = "September 27, 2023") +
  coord_cartesian(xlim = mapExtentZoom$xlims, ylim = mapExtentZoom$ylims) +
  ggsn::scalebar(streamsPlot, location = "bottomright", dist = 1, dist_unit = "km", st.size = 3.5, height = 0.015, transform = FALSE) + #model = 'WGS84') +
  ggsn::north(data = streamsPlot, scale = 0.1, symbol = 3) +
  theme_ifr() +
  theme(legend.position = "none", axis.title = element_blank())
#map

ggsave(sept27PlotZoom, file="Figures and Tables/099_MobileDetections_Sept27_2023_Zoom.png",
       width=15, height=18, scale=1)

##inset map

sept27PlotFinal <- 
  ggdraw(sept27Plot)  +
  draw_plot(
    {
      sept27PlotZoom +
        coord_sf(
          xlim = c(598500, 603500), #2500 apart
          ylim = c(5976000, 5981000), #2500 apart
          expand = FALSE) +
        theme(legend.position = "none",
              axis.text = element_blank(),
              axis.ticks = element_blank()) +
        labs(title = "Nanika River")
    },
    x = 0.375,
    y = 0.025,
    width = 0.3,
    height = 0.3
  ) 

#Nanika River tags: 151, 126, 125, 119, 72, 146, 129, 143

ggsave(sept27PlotFinal, file="Figures and Tables/099_MobileDetections_Sept27_2023_Final.png",
       width=15, height=18, scale=1)




# 2023-09-28 Mobile Survey

#Subset data for this survey
sept28MobData <- subset(mobileLocations, date == "2023-09-28") 
# %>%
#   as.data.frame()

## adjust jitter as needed if points/lables are obstructed

jitter_amount <- 500

sept28MobData$longjittered <- sept28MobData$long + runif(length(sept28MobData$long), -jitter_amount, jitter_amount)
sept28MobData$latjittered <- sept28MobData$lat + runif(length(sept28MobData$lat), -jitter_amount, jitter_amount)

#set jitter to 500

# Study area
sept28Plot <-
  ggplot() +
  geom_path(data = streamsPlot, aes(x = long, y = lat, group = group), colour = "lightblue", size = 1.1) +
  geom_polygon(data = filter(lakesPlot), aes(x = long, y = lat, group = group), colour = "#104692", fill = "lightblue") +
  geom_path(data = roadsPlot, aes(x = long, y = lat, group = group), colour = "#747687", size = 1.1) +
  # geom_point(aes(x = fixedLocations$long, y = fixedLocations$lat), colour = "#a40c01", fill = "#a40c01", size = 8, shape = 21) +
  geom_point(aes(x = sept28MobData$longjittered, y = sept28MobData$latjittered), colour = "#a40c01", fill = "#a40c01", size = 12, shape = 21) +
  geom_text(aes(label = sept28MobData$code, x = sept28MobData$longjittered, y = sept28MobData$latjittered), color = "white", size = 7) +
  # geom_point(aes(x = sept13MobData$long, y = sept13MobData$lat), colour = "#a40c01", fill = "#a40c01", size = 12, shape = 21) +
  # geom_text(aes(label = sept13MobData$code, x = sept13MobData$long, y = sept13MobData$lat), color = "white", size = 7) +
  annotate("text", x = 612000, y = 6070000, label = "Smithers", size = 12) +  
  annotate("text", x = 649500, y = 6025880, label = "Houston", size = 10) + 
  annotate("text", x = 590800, y = 5980000, label = "Morice Lake", size = 10) +  
  # annotate("text", x = 579900, y = 5987500, label = "Atna Lake", size = 10) + 
  # annotate("text", x = 601387, y = 5978946, label = "Nanika river", size = 10) + 
  # annotate("text", x = 580067, y = 5971031, label = "Morice Lake", size = 10) + #for inset map of morice lake
  # annotate("text", x = 613000, y = 6098000, label = "Fallback \nStation", size = 8) +   
  annotate("text", x = 602000, y = 6097000, label = "Witset\nCanyon", size = 8) +
  # annotate("text", x = 624700, y = 6074700, label = "Lower Bulkley\nStation", size = 8) +
  # annotate("text", x = 603500, y = 6003000, label = "Morice Lake Outlet\nStation", size = 8) + 
  # annotate("text", x = 604500, y = 5983500, label = "Nanika River\nStation", size = 8) +
  # annotate("text", x = 582351, y = 5985871, label = "Atna River\nStation", size = 8) +
  labs(title = "September 28, 2023") +
  coord_cartesian(xlim = mapExtent$xlims, ylim = mapExtent$ylims) +
  ggsn::scalebar(streamsPlot, location = "bottomright", dist = 1, dist_unit = "km", st.size = 3.5, height = 0.015, transform = FALSE) + #model = 'WGS84') +
  ggsn::north(data = streamsPlot, scale = 0.1, symbol = 3) +
  theme_ifr() +
  theme(legend.position = "none", axis.title = element_blank())
#map

ggsave(sept28Plot, file="Figures and Tables/099_MobileDetections_Sept28_2023.png",
       width=15, height=18, scale=1)

#Witset Canyon tags: 13, 153, 76



# 2022-10-04 Mobile Survey

#Subset data for this survey
oct4MobData <- subset(mobileLocations, date == "2023-10-04") 

## adjust jitter as needed if points/lables are obstructed

jitter_amount <- 500
oct4MobData$longjittered <- oct4MobData$long + runif(length(oct4MobData$long), -jitter_amount, jitter_amount)
oct4MobData$latjittered <- oct4MobData$lat + runif(length(oct4MobData$lat), -jitter_amount, jitter_amount)

#set jitter to 500

# Study area
oct4Plot <-
  ggplot() +
  geom_path(data = streamsPlot, aes(x = long, y = lat, group = group), colour = "lightblue", size = 1.1) +
  geom_polygon(data = filter(lakesPlot), aes(x = long, y = lat, group = group), colour = "#104692", fill = "lightblue") +
  geom_path(data = roadsPlot, aes(x = long, y = lat, group = group), colour = "#747687", size = 1.1) +
  # geom_point(aes(x = fixedLocations$long, y = fixedLocations$lat), colour = "#a40c01", fill = "#a40c01", size = 8, shape = 21) +
  geom_point(aes(x = oct4MobData$longjittered, y = oct4MobData$latjittered), colour = "#a40c01", fill = "#a40c01", size = 12, shape = 21) +
  geom_text(aes(label = oct4MobData$code, x = oct4MobData$longjittered, y = oct4MobData$latjittered), color = "white", size = 7) +
  # geom_point(aes(x = sept13MobData$long, y = sept13MobData$lat), colour = "#a40c01", fill = "#a40c01", size = 12, shape = 21) +
  # geom_text(aes(label = sept13MobData$code, x = sept13MobData$long, y = sept13MobData$lat), color = "white", size = 7) +
  annotate("text", x = 612000, y = 6070000, label = "Smithers", size = 12) +  
  annotate("text", x = 649500, y = 6025880, label = "Houston", size = 10) + 
  annotate("text", x = 590800, y = 5980000, label = "Morice Lake", size = 10) +  
  # annotate("text", x = 579900, y = 5987500, label = "Atna Lake", size = 10) + 
  # annotate("text", x = 601387, y = 5978946, label = "Nanika river", size = 10) + 
  # annotate("text", x = 580067, y = 5971031, label = "Morice Lake", size = 10) + #for inset map of morice lake
  # annotate("text", x = 613000, y = 6098000, label = "Fallback \nStation", size = 8) +   
  annotate("text", x = 602000, y = 6097000, label = "Witset\nCanyon", size = 8) +
  # annotate("text", x = 624700, y = 6074700, label = "Lower Bulkley\nStation", size = 8) +
  # annotate("text", x = 603500, y = 6003000, label = "Morice Lake Outlet\nStation", size = 8) + 
  # annotate("text", x = 604500, y = 5983500, label = "Nanika River\nStation", size = 8) +
  # annotate("text", x = 582351, y = 5985871, label = "Atna River\nStation", size = 8) +
  labs(title = "October 04, 2023") +
  coord_cartesian(xlim = mapExtent$xlims, ylim = mapExtent$ylims) +
  ggsn::scalebar(streamsPlot, location = "bottomright", dist = 1, dist_unit = "km", st.size = 3.5, height = 0.015, transform = FALSE) + #model = 'WGS84') +
  ggsn::north(data = streamsPlot, scale = 0.1, symbol = 3) +
  theme_ifr() +
  theme(legend.position = "none", axis.title = element_blank())
#map

ggsave(oct4Plot, file="Figures and Tables/099_MobileDetections_Oct04_2023.png",
       width=15, height=18, scale=1)

#set jitter to 500

oct4PlotZoom <-
  ggplot() +
  geom_path(data = streamsPlot, aes(x = long, y = lat, group = group), colour = "lightblue", size = 1.1) +
  geom_polygon(data = filter(lakesPlot), aes(x = long, y = lat, group = group), colour = "#104692", fill = "lightblue") +
  geom_path(data = roadsPlot, aes(x = long, y = lat, group = group), colour = "#747687", size = 1.1) +
  #geom_point(aes(x = fixedLocations$long, y = fixedLocations$lat), colour = "#a40c01", fill = "#a40c01", size = 8, shape = 21) +
  geom_point(aes(x = oct4MobData$longjittered, y = oct4MobData$latjittered), colour = "#a40c01", fill = "#a40c01", size = 12, shape = 21) +
  geom_text(aes(label = oct4MobData$code, x = oct4MobData$longjittered, y = oct4MobData$latjittered), color = "white", size = 7) +
  annotate("text", x = 612000, y = 6070000, label = "Smithers", size = 12) +  annotate("text", x = 649500, y = 6025880, label = "Houston", size = 10) + 
  annotate("text", x = 590800, y = 5980000, label = "Morice Lake", size = 10) +  annotate("text", x = 624700, y = 6010000, label = "Morice River", size = 10) + 
  annotate("text", x = 579900, y = 5987500, label = "Atna Lake", size = 10) + 
  annotate("text", x = 601387, y = 5978946, label = "Nanika river", size = 10) + 
  annotate("text", x = 580067, y = 5971031, label = "Morice Lake", size = 10) + 
  # annotate("text", x = 613000, y = 6098000, label = "Fallback \nStation", size = 8) +   
  annotate("text", x = 602000, y = 6097000, label = "Witset\nCanyon", size = 8) +
  # annotate("text", x = 624700, y = 6074700, label = "Lower Bulkley\nStation", size = 8) + annotate("text", x = 603500, y = 6003000, label = "Morice Lake Outlet\nStation", size = 8) + 
  annotate("text", x = 604500, y = 5983500, label = "Nanika River\nStation", size = 8) + annotate("text", x = 582351, y = 5985871, label = "Atna River\nStation", size = 8) +
  labs(title = "October 4, 2023") +
  coord_cartesian(xlim = mapExtentZoom$xlims, ylim = mapExtentZoom$ylims) +
  ggsn::scalebar(streamsPlot, location = "bottomright", dist = 1, dist_unit = "km", st.size = 3.5, height = 0.015, transform = FALSE) + #model = 'WGS84') +
  ggsn::north(data = streamsPlot, scale = 0.1, symbol = 3) +
  theme_ifr() +
  theme(legend.position = "none", axis.title = element_blank())
#map

ggsave(oct4PlotZoom, file="Figures and Tables/099_MobileDetections_Oct04_2023_Zoom.png",
       width=15, height=18, scale=1)

##inset map

oct4PlotFinal <- 
  ggdraw(oct4Plot)  +
  draw_plot(
    {
      oct4PlotZoom +
        coord_sf(
          xlim = c(576500, 583500),
          ylim = c(5984000, 5991000),
          expand = FALSE) +
        theme(legend.position = "none",
              axis.text = element_blank(),
              axis.ticks = element_blank()) +
        labs(title = "Atna Lake")
    }, 
    x = 0.675, 
    y = 0.675, 
    width = 0.3,
    height = 0.3
  ) +
  draw_plot(
    {
      oct4PlotZoom +
        coord_sf(
          xlim = c(598500, 603500), #2500 apart
          ylim = c(5976000, 5981000), #2500 apart
          expand = FALSE) +
        theme(legend.position = "none",
              axis.text = element_blank(),
              axis.ticks = element_blank()) +
        labs(title = "Nanika River")
    },
    x = 0.375,
    y = 0.025,
    width = 0.3,
    height = 0.3
  ) +
  draw_plot(
    {
      oct4PlotZoom +
        coord_sf(
          xlim = c(575500, 589000), #7500 apart
          ylim = c(5963000, 5976500), #7500 apart
          expand = FALSE) +
        theme(legend.position = "none", 
              axis.text = element_blank(),
              axis.ticks = element_blank()) +
        labs(title = "SW Morice Lake")
    },
    x = 0.675,
    y = 0.025,
    width = 0.3,
    height = 0.3
  )

#Bulkley neart Smithers tags: 85
#Atna Lake tags: 78, 23, 159, 40, 104, 30, 63, 49, 61, 8, 99
#Nanika River tags: 118, 147, 120, 113, 75, 125, 146, 119, 126, 72, 143, 145

ggsave(oct4PlotFinal, file="Figures and Tables/099_MobileDetections_Oct04_2023_Final.png",
       width=15, height=18, scale=1)




# Plot Last Detection Maps ----------------------------------------------------

## Bulkley River

# # The labels get a jammed up at the fallback station, so we'll need to filter those out
# # That only leaves codes 12 and 13
# lastDetectionsBulkleyLabels <- subset(lastDetections, code %in% c(12, 13))


bulkleyExtent <- data.frame(ylims = c(6020000, 6100900),
                                 xlims = c(605700, 655000))

#to see what tags are at each station
# fallbackDetects <- lastDetects %>%
#   filter(rkm == -0.7)
# 
# bulkleyDetects <- lastDetects %>%
#   filter(rkm >= 40.0 & rkm <= 44.0)

## adjust jitter as needed if points/lables are obstructed

jitter_amount <- 3000
lastDetections$longjittered <- lastDetections$long + runif(length(lastDetections$long), -jitter_amount, jitter_amount)
lastDetections$latjittered <- lastDetections$lat + runif(length(lastDetections$lat), -jitter_amount, jitter_amount)

bulkleyPlot <-
  ggplot() +
  geom_path(data = streamsPlot, aes(x = long, y = lat, group = group), colour = "lightblue", size = 1.1) +
  geom_polygon(data = filter(lakesPlot), aes(x = long, y = lat, group = group), colour = "#104692", fill = "lightblue") +
  geom_path(data = roadsPlot, aes(x = long, y = lat, group = group), colour = "#747687", size = 1.1) +
  geom_point(aes(x = fixedLocations$long, y = fixedLocations$lat), colour = "#104692", fill = "#104692", size = 8, shape = 21) +
  geom_point(aes(x = lastDetections$long, y = lastDetections$lat), colour = "#a40c01", fill = "#a40c01", size = 12, shape = 21) +
  # geom_text(aes(label = lastDetectionsBulkleyLabels$code, x = lastDetectionsBulkleyLabels$long, y = lastDetectionsBulkleyLabels$lat), color = "white", size = 7) +
  geom_text(aes(label = lastDetections$code, x = lastDetections$long, y = lastDetections$lat), color = "white", size = 7) +
  # geom_point(aes(x = lastDetections$longjittered, y = lastDetections$latjittered), colour = "#a40c01", fill = "#a40c01", size = 12, shape = 21) +
  # geom_text(aes(label = lastDetections$code, x = lastDetections$longjittered, y = lastDetections$latjittered), color = "white", size = 7) +
  annotate("text", x = 610400, y = 6100700, label = "Fallback\nStation", size = 7) + 
  annotate("text", x = 622000, y = 6074700, label = "Lower Bulkley\nStation", size = 7) +
  annotate("text", x = 615300, y = 6071000, label = "Smithers", size = 8) +
  annotate("text", x = 649700, y = 6028000, label = "Houston", size = 8) + 
  labs(title = "Last Dectections - Bulkley River") +
  coord_cartesian(xlim = bulkleyExtent$xlims, ylim = bulkleyExtent$ylims) + 
  ggsn::scalebar(streamsPlot, 
                 location = "bottomright", 
                 dist = 1, 
                 dist_unit = "km", 
                 st.size = 3.5, 
                 height = 0.015, 
                 transform = FALSE, 
                 model = 'WGS84') +
  ggsn::north(data = streamsPlot, scale = 0.1, symbol = 3) +
  theme_ifr() +
  theme(legend.position = "none", axis.title = element_blank())

#bulkleyPlot

ggsave(bulkleyPlot, file="Figures and Tables/099_LastDetections_BulkleyRiver_2023.png",
       width=15, height=15, scale=1) 

#fallback station tags: 1, 17, 20, 25, 36, 42, 43, 44, 45, 46, 48, 50, 51, 52, 
# 55, 65, 70, 79, 82, 83, 91, 95, 96, 97, 103, 105, 107, 108, 110, 111, 112, 131, 134, 135, 136, 137, 138, 152, 154, 158, 160
#bulkley station tags: 2, 9, 53, 54, 68, 77, 90, 94, 98, 100, 127, 150, 176
#Morice_Bulkley confluence near Houston: 22, 74, 47, 60, 88, 6, 122, 64, 144, 73, 84, 92, 19, 5, 58

## Morice River

# The labels get a jammed up at the lake outlet station, so we'll need to filter those out
# # That only leaves codes 14
# lastDetectionsMoriceRiverLabels <- subset(lastDetections, code %in% c(14))

# Set Morice River extent
moriceRiverExtent <- data.frame(ylims = c(5998000, 6031000),
                                xlims = c(603000, 655000))

moriceRiverPlot <-
  ggplot() +
  geom_path(data = streamsPlot, aes(x = long, y = lat, group = group), colour = "lightblue", size = 1.1) +
  geom_polygon(data = filter(lakesPlot), aes(x = long, y = lat, group = group), colour = "#104692", fill = "lightblue") +
  geom_path(data = roadsPlot, aes(x = long, y = lat, group = group), colour = "#747687", size = 1.1) +
  geom_point(aes(x = fixedLocations$long, y = fixedLocations$lat), colour = "#104692", fill = "#104692", size = 8, shape = 21) +
  geom_point(aes(x = lastDetections$long, y = lastDetections$lat), colour = "#a40c01", fill = "#a40c01", size = 12, shape = 21) +
  geom_text(aes(label = lastDetections$code, 
                x = lastDetections$long, 
                y = lastDetections$lat), 
            color = "white", size = 7) +
  annotate("text", x = 605700, y = 5997900, label = "Morice Lake Outlet\nStation", size = 7) + 
  annotate("text", x = 649900, y = 6028500, label = "Houston", size = 8) + 
  labs(title = "Last Dectections - Morice River") +
  coord_cartesian(xlim = moriceRiverExtent$xlims, ylim = moriceRiverExtent$ylims) + 
  ggsn::scalebar(streamsPlot, 
                 location = "bottomright", 
                 dist = 1, 
                 dist_unit = "km", 
                 st.size = 3.5, 
                 height = 0.015, 
                 transform = FALSE, 
                 model = 'WGS84') +
  ggsn::north(data = streamsPlot, scale = 0.1, symbol = 3) +
  theme_ifr() +
  theme(legend.position = "none", axis.title = element_blank())

#moriceLakePlot

ggsave(moriceRiverPlot, file="Figures and Tables/099_LastDetections_MoriceRiver_2023.png",
       width=15, height=15, scale=1) 


## Morice Lake

# Filter out labels at fixed stations
# lastDetectionsMoriceLakeLabels <- subset(lastDetections, 
#                                          code %in% c(8,10,18,23,37))

#counts tags that overlap
# atnaDetects <- lastDetects %>%
#   filter(waterbody == "Atna River")
# 
# nanikaDetects <- lastDetects %>%
#   filter(waterbody == "Nanika River")


# Set Morice Lake extent
moriceLakeExtent <- data.frame(ylims = c(5962000, 5999000),
                               xlims = c(575000, 607000))


moriceLakePlot <-
  ggplot() +
  geom_path(data = streamsPlot, aes(x = long, y = lat, group = group), colour = "lightblue", size = 1.1) +
  geom_polygon(data = filter(lakesPlot), aes(x = long, y = lat, group = group), colour = "#104692", fill = "lightblue") +
  geom_path(data = roadsPlot, aes(x = long, y = lat, group = group), colour = "#747687", size = 1.1) +
  geom_point(aes(x = fixedLocations$long, y = fixedLocations$lat), colour = "#104692", fill = "#104692", size = 8, shape = 21) +
  geom_point(aes(x = lastDetections$long, y = lastDetections$lat), colour = "#a40c01", fill = "#a40c01", size = 12, shape = 21) +
  geom_text(aes(label = lastDetections$code, 
                x = lastDetections$long, 
                y = lastDetections$lat), 
            color = "white", size = 7) +
  annotate("text", x = 603000, y = 5998500, label = "Morice Lake Outlet\nStation", size = 7) + 
  annotate("text", x = 605000, y = 5990600, label = "Nanika River\nStation", size = 7) +
  annotate("text", x = 590400, y = 5985000, label = "Morice Lake", size = 8) +
  labs(title = "Last Dectections - Morice Lake, Atna Lake and Nanika River") +
  coord_cartesian(xlim = moriceLakeExtent$xlims, ylim = moriceLakeExtent$ylims) + 
  ggsn::scalebar(streamsPlot, 
                 location = "bottomright", 
                 dist = 1, 
                 dist_unit = "km", 
                 st.size = 3.5, 
                 height = 0.015, 
                 transform = FALSE, 
                 model = 'WGS84') +
  ggsn::north(data = streamsPlot, scale = 0.1, symbol = 3) +
  theme_ifr() +
  theme(legend.position = "none", axis.title = element_blank())

#moriceLakePlot

ggsave(moriceLakePlot, file="Figures and Tables/099_LastDetections_MoriceLake_2023.png",
       width=15, height=15, scale=1) 

