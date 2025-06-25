# Make general study area map
# Read in filtered mobile telemetry data and make maps for each survey date
# Created by Pete Moniz - winter 2023
# Updated by Dan Scurfield - February 2024
# Updated by Dan Scurfield - June 2025

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
fixedLocations <- read.csv(file = "Data Input/099_Fixed_Locations_2024.csv", 
                           header = TRUE, 
                           stringsAsFactors = FALSE) %>%
  mutate(station = as.character(Station)) %>%
  dplyr::select(station, lat = Latitude, long = Longitude)

# Read in all detection data, and filter for mobile detection data to plot on maps
mobileDetections <- read.csv(file = "Data Output/099_AllData_FinalCleaned_2024.csv", 
                           header = TRUE, 
                           stringsAsFactors = FALSE) %>%
  filter(method == "Mobile") %>%
  dplyr::select(date, freqCode, code, lat = latitude, long = longitude, rkm, waterbody)


# Also read in all detection data, and filter for the last detection for each tag to plot on maps
lastDetections <- read.csv(file = "Data Output/099_AllData_FinalCleaned_2024.csv", 
                    header = TRUE, 
                    stringsAsFactors = FALSE) %>%
  arrange(desc(dateTime)) %>%
  group_by(freqCode) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  #filter out tags with missing data (none)
  #filter our tags not assigned (none)
  dplyr::select(date, freqCode, code, lat = latitude, long = longitude, rkm, waterbody)

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
                           xlims = c(575000, 665000))

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

ggsave(studyArea, file="Figures and Tables/099_StudyArea_2024.png",
       width=15, height=18, scale=1)



# Mobile Survey Maps ----------------------------------------------------------

# 2024-09-06 Mobile Survey

##Next time to work on this - add zoom plot with labels for inset map.

#Subset data for this survey
sept06MobData <- subset(mobileLocations, date == "2024-09-06")

## adjust jitter as needed if points/lables are obstructed

jitter_amount <- 1000
sept06MobData$longjittered <- sept06MobData$long + runif(length(sept06MobData$long), -jitter_amount, jitter_amount)
sept06MobData$latjittered <- sept06MobData$lat + runif(length(sept06MobData$lat), -jitter_amount, jitter_amount)

# set jitter to 500
sept06Plot <-
  ggplot() +
  geom_path(data = streamsPlot, aes(x = long, y = lat, group = group), colour = "lightblue", size = 1.1) +
  geom_polygon(data = filter(lakesPlot), aes(x = long, y = lat, group = group), colour = "#104692", fill = "lightblue") +
  geom_path(data = roadsPlot, aes(x = long, y = lat, group = group), colour = "#747687", size = 1.1) +
  #geom_point(aes(x = fixedLocations$long, y = fixedLocations$lat), colour = "#a40c01", fill = "#a40c01", size = 8, shape = 21) +
  geom_point(aes(x = sept06MobData$long, y = sept06MobData$lat), colour = "#a40c01", fill = "#a40c01", size = 12, shape = 21) +
  geom_text(aes(label = sept06MobData$code, x = sept06MobData$long, y = sept06MobData$lat), color = "white", size = 7) +
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
  labs(title = "September 06, 2024") +
  coord_cartesian(xlim = mapExtent$xlims, ylim = mapExtent$ylims) +
  ggsn::scalebar(streamsPlot, location = "bottomright", dist = 1, dist_unit = "km", st.size = 3.5, height = 0.015, transform = FALSE) + #model = 'WGS84') +
  ggsn::north(data = streamsPlot, scale = 0.1, symbol = 3) +
  theme_ifr() +
  theme(legend.position = "none", axis.title = element_blank())
#map

ggsave(sept06Plot, file="Figures and Tables/099_MobileDetections_Sept06_2024.png",
       width=15, height=18, scale=1)

sept06PlotZoom <-
  ggplot() +
  geom_path(data = streamsPlot, aes(x = long, y = lat, group = group), colour = "lightblue", size = 1.1) +
  geom_polygon(data = filter(lakesPlot), aes(x = long, y = lat, group = group), colour = "#104692", fill = "lightblue") +
  geom_path(data = roadsPlot, aes(x = long, y = lat, group = group), colour = "#747687", size = 1.1) +
  geom_point(aes(x = sept06MobData$longjittered, y = sept06MobData$latjittered), colour = "#a40c01", fill = "#a40c01", size = 7, shape = 21) +
  geom_text(aes(label = sept06MobData$code, x = sept06MobData$longjittered, y = sept06MobData$latjittered), color = "white", size = 4) +
  geom_point(aes(x = fixedLocations$long, y = fixedLocations$lat), colour = "#104692", fill = "#104692", size = 5, shape = 21) +
  annotate("text", x = 603500, y = 6003000, label = "Morice Lake Outlet\nStation", size = 8) +
  annotate("text", x = 606733, y = 5987466, label = "Nanika River\nStation", size = 8) +
  annotate("text", x = 582351, y = 5983321, label = "Atna River\nStation", size = 8) +
  # annotate("text", x = 612000, y = 6070000, label = "Smithers", size = 12) +  annotate("text", x = 649500, y = 6025880, label = "Houston", size = 10) + 
  # annotate("text", x = 590800, y = 5980000, label = "Morice Lake", size = 10) +  annotate("text", x = 624700, y = 6010000, label = "Morice River", size = 10) + 
  # annotate("text", x = 579900, y = 5987500, label = "Atna Lake", size = 10) +
  # annotate("text", x = 601387, y = 5978946, label = "Nanika river", size = 10) +
  # annotate("text", x = 580067, y = 5971031, label = "Morice Lake", size = 10) +
  # annotate("text", x = 613000, y = 6098000, label = "Fallback \nStation", size = 8) +
  # annotate("text", x = 602000, y = 6097000, label = "Witset\nCanyon", size = 8) +
  # annotate("text", x = 624700, y = 6074700, label = "Lower Bulkley\nStation", size = 8) +
  # annotate("text", x = 603500, y = 6003000, label = "Morice Lake Outlet\nStation", size = 8) +
  # annotate("text", x = 604500, y = 5983500, label = "Nanika River\nStation", size = 8) +
  # annotate("text", x = 582351, y = 5985871, label = "Atna River\nStation", size = 8) +
  labs(title = "September 06, 2024") +
  # coord_cartesian(xlim = mapExtentZoom$xlims, ylim = mapExtentZoom$ylims) +
  coord_cartesian(xlim = mapExtent$xlims, ylim = mapExtent$ylims) +
  ggsn::scalebar(streamsPlot, location = "bottomright", dist = 1, dist_unit = "km", st.size = 3.5, height = 0.015, transform = FALSE) + #model = 'WGS84') +
  ggsn::north(data = streamsPlot, scale = 0.1, symbol = 3) +
  theme_ifr() +
  theme(legend.position = "none", axis.title = element_blank())

ggsave(sept06PlotZoom, file="Figures and Tables/099_MobileDetections_Sept06_2024_Zoom.png",
       width=15, height=18, scale=1)

#inset map
sept06PlotFinal <- 
  ggdraw(sept06Plot)  +
  draw_plot(
    {
      sept06PlotZoom +
        coord_sf(
          xlim = c(570000, 595000),
          ylim = c(5965500, 5990500),
          expand = FALSE) +
        theme(legend.position = "none",
              axis.text = element_blank(),
              axis.ticks = element_blank()) +
        labs(title = "Atna Lake & SW Morice Lake")
    }, 
    x = 0.675, 
    y = 0.675, 
    width = 0.3,
    height = 0.3
  ) +
  draw_plot(
    {
      sept06PlotZoom +
        coord_sf(
          xlim = c(592500, 612500), #7500 apart
          ylim = c(5975748, 5995748), #7500 apart
          expand = FALSE) +
        theme(legend.position = "none", 
              axis.text = element_blank(),
              axis.ticks = element_blank()) +
        labs(title = "Nanika River")
    },
    x = 0.675,
    y = 0.025,
    width = 0.3,
    height = 0.3
  )


ggsave(sept06PlotFinal, file="Figures and Tables/099_MobileDetections_Sept06_2024_Final.png",
       width=15, height=18, scale=1)


#Atna tags: 
#Morice Lake tags: 
#Nanika Tags

#See tags here:
sept06 <- as.data.frame(sept06MobData) %>%
  arrange(waterbody)

##### 2024-09-25 Mobile Survey ####

##Next time to work on this - add zoom plot with labels for inset map.

#Subset data for this survey
sept25MobData <- subset(mobileLocations, date == "2024-09-25")

## adjust jitter as needed if points/lables are obstructed

jitter_amount <- 1000
sept25MobData$longjittered <- sept25MobData$long + runif(length(sept25MobData$long), -jitter_amount, jitter_amount)
sept25MobData$latjittered <- sept25MobData$lat + runif(length(sept25MobData$lat), -jitter_amount, jitter_amount)

# set jitter to 500
sept25Plot <-
  ggplot() +
  geom_path(data = streamsPlot, aes(x = long, y = lat, group = group), colour = "lightblue", size = 1.1) +
  geom_polygon(data = filter(lakesPlot), aes(x = long, y = lat, group = group), colour = "#104692", fill = "lightblue") +
  geom_path(data = roadsPlot, aes(x = long, y = lat, group = group), colour = "#747687", size = 1.1) +
  #geom_point(aes(x = fixedLocations$long, y = fixedLocations$lat), colour = "#a40c01", fill = "#a40c01", size = 8, shape = 21) +
  geom_point(aes(x = sept25MobData$long, y = sept25MobData$lat), colour = "#a40c01", fill = "#a40c01", size = 12, shape = 21) +
  geom_text(aes(label = sept25MobData$code, x = sept25MobData$long, y = sept25MobData$lat), color = "white", size = 7) +
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
  labs(title = "September 25, 2024") +
  coord_cartesian(xlim = mapExtent$xlims, ylim = mapExtent$ylims) +
  ggsn::scalebar(streamsPlot, location = "bottomright", dist = 1, dist_unit = "km", st.size = 3.5, height = 0.015, transform = FALSE) + #model = 'WGS84') +
  ggsn::north(data = streamsPlot, scale = 0.1, symbol = 3) +
  theme_ifr() +
  theme(legend.position = "none", axis.title = element_blank())
#map

ggsave(sept25Plot, file="Figures and Tables/099_MobileDetections_Sept25_2024.png",
       width=15, height=18, scale=1)

sept25PlotZoom <-
  ggplot() +
  geom_path(data = streamsPlot, aes(x = long, y = lat, group = group), colour = "lightblue", size = 1.1) +
  geom_polygon(data = filter(lakesPlot), aes(x = long, y = lat, group = group), colour = "#104692", fill = "lightblue") +
  geom_path(data = roadsPlot, aes(x = long, y = lat, group = group), colour = "#747687", size = 1.1) +
  geom_point(aes(x = sept25MobData$longjittered, y = sept25MobData$latjittered), colour = "#a40c01", fill = "#a40c01", size = 7, shape = 21) +
  geom_text(aes(label = sept25MobData$code, x = sept25MobData$longjittered, y = sept25MobData$latjittered), color = "white", size = 4) +
  geom_point(aes(x = fixedLocations$long, y = fixedLocations$lat), colour = "#104692", fill = "#104692", size = 5, shape = 21) +
  annotate("text", x = 603500, y = 6003000, label = "Morice Lake Outlet\nStation", size = 8) +
  annotate("text", x = 606733, y = 5987466, label = "Nanika River\nStation", size = 8) +
  annotate("text", x = 582351, y = 5983321, label = "Atna River\nStation", size = 8) +
  # annotate("text", x = 612000, y = 6070000, label = "Smithers", size = 12) +  annotate("text", x = 649500, y = 6025880, label = "Houston", size = 10) + 
  # annotate("text", x = 590800, y = 5980000, label = "Morice Lake", size = 10) +  annotate("text", x = 624700, y = 6010000, label = "Morice River", size = 10) + 
  # annotate("text", x = 579900, y = 5987500, label = "Atna Lake", size = 10) +
  # annotate("text", x = 601387, y = 5978946, label = "Nanika river", size = 10) +
  # annotate("text", x = 580067, y = 5971031, label = "Morice Lake", size = 10) +
  # annotate("text", x = 613000, y = 6098000, label = "Fallback \nStation", size = 8) +
  # annotate("text", x = 602000, y = 6097000, label = "Witset\nCanyon", size = 8) +
  # annotate("text", x = 624700, y = 6074700, label = "Lower Bulkley\nStation", size = 8) +
  # annotate("text", x = 603500, y = 6003000, label = "Morice Lake Outlet\nStation", size = 8) +
  # annotate("text", x = 604500, y = 5983500, label = "Nanika River\nStation", size = 8) +
  # annotate("text", x = 582351, y = 5985871, label = "Atna River\nStation", size = 8) +
  labs(title = "September 25, 2024") +
  # coord_cartesian(xlim = mapExtentZoom$xlims, ylim = mapExtentZoom$ylims) +
  coord_cartesian(xlim = mapExtent$xlims, ylim = mapExtent$ylims) +
  ggsn::scalebar(streamsPlot, location = "bottomright", dist = 1, dist_unit = "km", st.size = 3.5, height = 0.015, transform = FALSE) + #model = 'WGS84') +
  ggsn::north(data = streamsPlot, scale = 0.1, symbol = 3) +
  theme_ifr() +
  theme(legend.position = "none", axis.title = element_blank())

ggsave(sept06PlotZoom, file="Figures and Tables/099_MobileDetections_Sept25_2024_Zoom.png",
       width=15, height=18, scale=1)

#inset map
sept25PlotFinal <- 
  ggdraw(sept25Plot)  +
  draw_plot(
    {
      sept25PlotZoom +
        coord_sf(
          xlim = c(570000, 595000),
          ylim = c(5965500, 5990500),
          expand = FALSE) +
        theme(legend.position = "none",
              axis.text = element_blank(),
              axis.ticks = element_blank()) +
        labs(title = "Atna Lake & SW Morice Lake")
    }, 
    x = 0.675, 
    y = 0.675, 
    width = 0.3,
    height = 0.3
  # ) +
  # draw_plot(
  #   {
  #     sept25PlotZoom +
  #       coord_sf(
  # xlim = c(592500, 612500), #7500 apart
  # ylim = c(5975748, 5995748), #7500 apart
  #         expand = FALSE) +
  #       theme(legend.position = "none", 
  #             axis.text = element_blank(),
  #             axis.ticks = element_blank()) +
  #       labs(title = "Nanika River")
  #   },
  #   x = 0.675,
  #   y = 0.025,
  #   width = 0.3,
  #   height = 0.3
  )


ggsave(sept25PlotFinal, file="Figures and Tables/099_MobileDetections_Sept25_2024_Final.png",
       width=15, height=18, scale=1)


#Atna tags: 
#Morice Lake tags: 
#Nanika Tags

#See tags here:
sept25 <- as.data.frame(sept25MobData) %>%
  arrange(waterbody)



##### 2024-10-01 Mobile Survey ####

##Next time to work on this - add zoom plot with labels for inset map.

#Subset data for this survey
oct01MobData <- subset(mobileLocations, date == "2024-10-01")

## adjust jitter as needed if points/lables are obstructed

jitter_amount <- 1000
oct01MobData$longjittered <- oct01MobData$long + runif(length(oct01MobData$long), -jitter_amount, jitter_amount)
oct01MobData$latjittered <- oct01MobData$lat + runif(length(oct01MobData$lat), -jitter_amount, jitter_amount)

# set jitter to 500
oct01Plot <-
  ggplot() +
  geom_path(data = streamsPlot, aes(x = long, y = lat, group = group), colour = "lightblue", size = 1.1) +
  geom_polygon(data = filter(lakesPlot), aes(x = long, y = lat, group = group), colour = "#104692", fill = "lightblue") +
  geom_path(data = roadsPlot, aes(x = long, y = lat, group = group), colour = "#747687", size = 1.1) +
  #geom_point(aes(x = fixedLocations$long, y = fixedLocations$lat), colour = "#a40c01", fill = "#a40c01", size = 8, shape = 21) +
  geom_point(aes(x = oct01MobData$long, y = oct01MobData$lat), colour = "#a40c01", fill = "#a40c01", size = 12, shape = 21) +
  geom_text(aes(label = oct01MobData$code, x = oct01MobData$long, y = oct01MobData$lat), color = "white", size = 7) +
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
  labs(title = "October 01, 2024") +
  coord_cartesian(xlim = mapExtent$xlims, ylim = mapExtent$ylims) +
  ggsn::scalebar(streamsPlot, location = "bottomright", dist = 1, dist_unit = "km", st.size = 3.5, height = 0.015, transform = FALSE) + #model = 'WGS84') +
  ggsn::north(data = streamsPlot, scale = 0.1, symbol = 3) +
  theme_ifr() +
  theme(legend.position = "none", axis.title = element_blank())
#map

ggsave(oct01Plot, file="Figures and Tables/099_MobileDetections_Oct01_2024.png",
       width=15, height=18, scale=1)

oct01PlotZoom <-
  ggplot() +
  geom_path(data = streamsPlot, aes(x = long, y = lat, group = group), colour = "lightblue", size = 1.1) +
  geom_polygon(data = filter(lakesPlot), aes(x = long, y = lat, group = group), colour = "#104692", fill = "lightblue") +
  geom_path(data = roadsPlot, aes(x = long, y = lat, group = group), colour = "#747687", size = 1.1) +
  geom_point(aes(x = oct01MobData$longjittered, y = oct01MobData$latjittered), colour = "#a40c01", fill = "#a40c01", size = 7, shape = 21) +
  geom_text(aes(label = oct01MobData$code, x = oct01MobData$longjittered, y = oct01MobData$latjittered), color = "white", size = 4) +
  geom_point(aes(x = fixedLocations$long, y = fixedLocations$lat), colour = "#104692", fill = "#104692", size = 5, shape = 21) +
  annotate("text", x = 603500, y = 6003000, label = "Morice Lake Outlet\nStation", size = 8) +
  annotate("text", x = 606733, y = 5987466, label = "Nanika River\nStation", size = 8) +
  annotate("text", x = 582351, y = 5983321, label = "Atna River\nStation", size = 8) +
  # annotate("text", x = 612000, y = 6070000, label = "Smithers", size = 12) +  annotate("text", x = 649500, y = 6025880, label = "Houston", size = 10) + 
  # annotate("text", x = 590800, y = 5980000, label = "Morice Lake", size = 10) +  annotate("text", x = 624700, y = 6010000, label = "Morice River", size = 10) + 
  # annotate("text", x = 579900, y = 5987500, label = "Atna Lake", size = 10) +
  # annotate("text", x = 601387, y = 5978946, label = "Nanika river", size = 10) +
  # annotate("text", x = 580067, y = 5971031, label = "Morice Lake", size = 10) +
  # annotate("text", x = 613000, y = 6098000, label = "Fallback \nStation", size = 8) +
  # annotate("text", x = 602000, y = 6097000, label = "Witset\nCanyon", size = 8) +
  # annotate("text", x = 624700, y = 6074700, label = "Lower Bulkley\nStation", size = 8) +
  # annotate("text", x = 603500, y = 6003000, label = "Morice Lake Outlet\nStation", size = 8) +
  # annotate("text", x = 604500, y = 5983500, label = "Nanika River\nStation", size = 8) +
  # annotate("text", x = 582351, y = 5985871, label = "Atna River\nStation", size = 8) +
  labs(title = "October 01, 2024") +
  # coord_cartesian(xlim = mapExtentZoom$xlims, ylim = mapExtentZoom$ylims) +
  coord_cartesian(xlim = mapExtent$xlims, ylim = mapExtent$ylims) +
  ggsn::scalebar(streamsPlot, location = "bottomright", dist = 1, dist_unit = "km", st.size = 3.5, height = 0.015, transform = FALSE) + #model = 'WGS84') +
  ggsn::north(data = streamsPlot, scale = 0.1, symbol = 3) +
  theme_ifr() +
  theme(legend.position = "none", axis.title = element_blank())

ggsave(oct01PlotZoom, file="Figures and Tables/099_MobileDetections_Oct01_2024_Zoom.png",
       width=15, height=18, scale=1)

#inset map
oct01PlotFinal <- 
  ggdraw(oct01Plot)  +
  draw_plot(
    {
      oct01PlotZoom +
        coord_sf(
          xlim = c(570000, 595000),
          ylim = c(5965500, 5990500),
          expand = FALSE) +
        theme(legend.position = "none",
              axis.text = element_blank(),
              axis.ticks = element_blank()) +
        labs(title = "Atna Lake & SW Morice Lake")
    }, 
    x = 0.675, 
    y = 0.675, 
    width = 0.3,
    height = 0.3
    ) +
    draw_plot(
      {
        oct01PlotZoom +
          coord_sf(
            xlim = c(592500, 612500), #7500 apart
            ylim = c(5975748, 5995748), #7500 apart
            expand = FALSE) +
          theme(legend.position = "none",
                axis.text = element_blank(),
                axis.ticks = element_blank()) +
          labs(title = "Nanika River")
      },
      x = 0.675,
      y = 0.025,
      width = 0.3,
      height = 0.3
  )


ggsave(oct01PlotFinal, file="Figures and Tables/099_MobileDetections_Oct01_2024_Final.png",
       width=15, height=18, scale=1)


#Atna tags: 
#Morice Lake tags: 
#Nanika Tags

#See tags here:
oct01 <- as.data.frame(oct01MobData) %>%
  arrange(waterbody)




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

jitter_amount <- 500
lastDetections$longjittered <- lastDetections$long + runif(length(lastDetections$long), -jitter_amount, jitter_amount)
lastDetections$latjittered <- lastDetections$lat + runif(length(lastDetections$lat), -jitter_amount, jitter_amount)

bulkleyPlot <-
  ggplot() +
  geom_path(data = streamsPlot, aes(x = long, y = lat, group = group), colour = "lightblue", size = 1.1) +
  geom_polygon(data = filter(lakesPlot), aes(x = long, y = lat, group = group), colour = "#104692", fill = "lightblue") +
  geom_path(data = roadsPlot, aes(x = long, y = lat, group = group), colour = "#747687", size = 1.1) +
  geom_point(aes(x = fixedLocations$long, y = fixedLocations$lat), colour = "#104692", fill = "#104692", size = 8, shape = 21) +
  geom_point(aes(x = lastDetections$longjittered, y = lastDetections$latjittered), colour = "#a40c01", fill = "#a40c01", size = 12, shape = 21) +
  # geom_text(aes(label = lastDetectionsBulkleyLabels$code, x = lastDetectionsBulkleyLabels$long, y = lastDetectionsBulkleyLabels$lat), color = "white", size = 7) +
  geom_text(aes(label = lastDetections$code, x = lastDetections$longjittered, y = lastDetections$latjittered), color = "white", size = 7) +
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

ggsave(bulkleyPlot, file="Figures and Tables/099_LastDetections_BulkleyRiver_2024.png",
       width=15, height=15, scale=1) 

#fallback station tags: 1, 17, 20, 25, 36, 42, 43, 44, 45, 46, 48, 50, 51, 52, 
# 55, 65, 70, 79, 82, 83, 91, 95, 96, 97, 103, 105, 107, 108, 110, 111, 112, 131, 134, 135, 136, 137, 138, 152, 154, 158, 160
#bulkley station tags: 2, 9, 53, 54, 68, 77, 90, 94, 98, 100, 127, 150, 176
#Morice_Bulkley confluence near Houston: 22, 74, 47, 60, 88, 6, 122, 64, 144, 73, 84, 92, 19, 5, 58

## Morice River (Not used in 2024)

# The labels get a jammed up at the lake outlet station, so we'll need to filter those out
# # That only leaves codes 14
# lastDetectionsMoriceRiverLabels <- subset(lastDetections, code %in% c(14))
# 
# # Set Morice River extent
# moriceRiverExtent <- data.frame(ylims = c(5998000, 6031000),
#                                 xlims = c(603000, 655000))
# 
# moriceRiverPlot <-
#   ggplot() +
#   geom_path(data = streamsPlot, aes(x = long, y = lat, group = group), colour = "lightblue", size = 1.1) +
#   geom_polygon(data = filter(lakesPlot), aes(x = long, y = lat, group = group), colour = "#104692", fill = "lightblue") +
#   geom_path(data = roadsPlot, aes(x = long, y = lat, group = group), colour = "#747687", size = 1.1) +
#   geom_point(aes(x = fixedLocations$long, y = fixedLocations$lat), colour = "#104692", fill = "#104692", size = 8, shape = 21) +
#   geom_point(aes(x = lastDetections$long, y = lastDetections$lat), colour = "#a40c01", fill = "#a40c01", size = 12, shape = 21) +
#   geom_text(aes(label = lastDetections$code, 
#                 x = lastDetections$long, 
#                 y = lastDetections$lat), 
#             color = "white", size = 7) +
#   annotate("text", x = 605700, y = 5997900, label = "Morice Lake Outlet\nStation", size = 7) + 
#   annotate("text", x = 649900, y = 6028500, label = "Houston", size = 8) + 
#   labs(title = "Last Dectections - Morice River") +
#   coord_cartesian(xlim = moriceRiverExtent$xlims, ylim = moriceRiverExtent$ylims) + 
#   ggsn::scalebar(streamsPlot, 
#                  location = "bottomright", 
#                  dist = 1, 
#                  dist_unit = "km", 
#                  st.size = 3.5, 
#                  height = 0.015, 
#                  transform = FALSE, 
#                  model = 'WGS84') +
#   ggsn::north(data = streamsPlot, scale = 0.1, symbol = 3) +
#   theme_ifr() +
#   theme(legend.position = "none", axis.title = element_blank())
# 
# #moriceLakePlot
# 
# ggsave(moriceRiverPlot, file="Figures and Tables/099_LastDetections_MoriceRiver_2023.png",
#        width=15, height=15, scale=1) 


## Morice Lake


# Set Morice Lake extent
moriceLakeExtent <- data.frame(ylims = c(5962000, 6007000),
                               xlims = c(575000, 607000))


moriceLakePlot <-
  ggplot() +
  geom_path(data = streamsPlot, aes(x = long, y = lat, group = group), colour = "lightblue", size = 1.1) +
  geom_polygon(data = filter(lakesPlot), aes(x = long, y = lat, group = group), colour = "#104692", fill = "lightblue") +
  geom_path(data = roadsPlot, aes(x = long, y = lat, group = group), colour = "#747687", size = 1.1) +
  geom_point(aes(x = fixedLocations$long, y = fixedLocations$lat), colour = "#104692", fill = "#104692", size = 8, shape = 21) +
  geom_point(aes(x = lastDetections$longjittered, y = lastDetections$latjittered), colour = "#a40c01", fill = "#a40c01", size = 12, shape = 21) +
  geom_text(aes(label = lastDetections$code, 
                x = lastDetections$longjittered, 
                y = lastDetections$latjittered), 
            color = "white", size = 7) +
  annotate("text", x = 603000, y = 5999500, label = "Morice Lake Outlet\nStation", size = 7) + 
  annotate("text", x = 606000, y = 5990600, label = "Nanika River\nStation", size = 7) +
  annotate("text", x = 590400, y = 5985000, label = "Morice Lake", size = 8) +
  annotate("text", x = 582351, y = 5983321, label = "Atna River\nStation", size = 8) +
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

ggsave(moriceLakePlot, file="Figures and Tables/099_LastDetections_MoriceLake_2024.png",
       width=21, height=13, scale=1) 

