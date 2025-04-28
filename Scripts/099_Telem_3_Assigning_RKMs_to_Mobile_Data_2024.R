# Read in cleaned mobile tracking data and assign RKMs to each detection
# Created by Pete Moniz - winter 2023
# Updated by Dan Scurfield - Feb 2024

# Initial Setup ---------------------------------------------------------------

# Remove any objects from old R sessions
rm(list=ls(all=TRUE))

# Load packages
library(sp)
library(raster)
library(rgdal)
library(tidyverse)

# Make all times UTC and avoid auto re-display of timezone
Sys.setenv(TZ = "UTC") 

# Read In Data ----------------------------------------------------------------

# Read in rkm shapefile. This is a point shapefile made in ArcGIS where a
# point was created every 1 km upstream of Witset Canyon using the 
# British Columbia Freshwater Atlas’ Stream Network polyline.
# Points extended from Witset Canyon upstream along the Bulkley 
# and Morice Rivers into Morice Lake, and to the upstream ends of Atna and Nanika Rivers.
# Each rkm is assigned a water body (Bulkley, Morice River, Morice Lake, Nanika, Atna).
# We should be able to use this same shapefile for future years' analysis,
# assuming we use the same start location (Witset Canyon) and continue monitoring up
# into the Atna and Nanika Rivers.
# Because it was made in ArcGIS and has an associated  projection file already, 
# we don't need to worry about setting coordinates for it.
rkmPoints <- readOGR(dsn = "Data Input/Mapping", layer = "099_rkm_pts")


# Read in initially cleaned mobile data.
# This data will still need a bit of cleaning once we add rkms and combine it
# with the fixed telemetry data.
mobileDataCleaned <- read.csv("Data Output/099_MobileTrackingData_1_InitialClean_2023.csv", 
                              header = TRUE, 
                              stringsAsFactors = FALSE) %>%
  # Make an ID to double check that rkm's and detection points match up at the end.
  rename(detID = X)


# Set Up Data -----------------------------------------------------------------

# Now we'll set our data up to be able to grab the nearest rkm point for each
# mobile tracking detection.

# First make a new simplified mobile detection dataframe
mobileDataPoints <- mobileDataCleaned %>%
  dplyr::select(detID, lat, long, dateTime, freqCode)

# Turn the data into a spatial points dataframe
coordinates(mobileDataPoints) <- c("long", "lat")

# Assign coordinate system. This is a pretty standard datum that 
# I assumed is what Lotek collects their data in. I double checked this
# in ArcGIS and everything lined up, so it should be fine.
proj4string(mobileDataPoints) <- CRS("+proj=longlat +datum=WGS84")
  
# Now we have to re-project the mobile detection coordinates to the same
# coodinates as the rkm Points, which is
# "+proj=utm +zone=9 +datum=NAD83 +units=m +no_defs"
# You can check the projection of a shapefile with the proj4string() function.
mobileDataPoints <- spTransform(mobileDataPoints, proj4string(rkmPoints))


# Assign RKMs and Waterbody's -------------------------------------------------

# I couldn't find a function that would loop through one set of spatial points 
# (in our case, the coordinates of the mobile detections) and assign each of those points
# a value from the nearest point of another set of spatial points (the rkm point shapefile).
# Instead I just used a for loop to loop through each mobile detection point and assign
# the nearest rkm point to a vector called "rkm".

# First set up blank "rkm" vector that will keep growing in the for loop with the
# nearest rkm point for each detection point.
rkm <- as.numeric()

# Same for a water body vector
waterbody <- as.numeric()

# Finally, make a blank "rkmID" vector to double check back against the detection point IDs
# to make sure we're merging everything together correctly at the end.
rkmID <- as.numeric()

# Loop through each detection point
for (i in 1:length(mobileDataPoints)) {
  # the spDistN1 function calculates the distance from the single indexed mobile detection point
  # to all rkm points
  distances <- spDistsN1(rkmPoints, mobileDataPoints[i,])
  # Isolate the index with the minimum distance (closest rkm point to the single indexed detection point)
  index <- which.min(distances)
  # Use that index to grab the associated rkm, waterbody, and rkmID from the rkmPoints shapefile 
  # and append it to the associated vectors
  rkm <- append(rkm, rkmPoints$rkm[index])
  waterbody <- append(waterbody, rkmPoints$waterbody[index])
  rkmID <- append(rkmID, mobileDataPoints$detID[i])
}


# Now merge that new rkm vector into the original cleaned mobile data dataframe
mobileDataCleanedRKM <- data.frame(rkmID, mobileDataCleaned, rkm, waterbody) %>%
  # The detection ID and rkm ID match, so we can remove those
  dplyr::select(-rkmID, -detID)
# I also double checked the results manually in ArcGIS for a few detection points 
# and those results matched these results.

# Note that there's probably a way better/cleaner way to do this, but it worked
# and should (I think) be reproducible in future years, as long as we use the
# same starting point (Witset Canyon).

# Write out a new csv to merge with the cleaned fixed station data for further
# filtering and plotting.
write.csv(mobileDataCleanedRKM, 
          file = "Data Output/099_MobileTrackingData_2_InitialClean_RKM_2023.csv")

