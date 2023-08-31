######################
#
# Purpose: Identifies sites in lu_stations that are near coordinates for sites of interest.
#          Also identifies COMID for sites of interest & compares those with COMID of site in lu_stations.
#          The end product is a table of the sites of interest with lu_stations sites that are <=100m & have the same COMID.
#
# Remember, this program looks at all sites in lu_stations, most of which have probably not been sampled before.
#     (could bring in taxonomy table to see which sites have been sampled)
#
# Why look for sites this way? Mapping all lu_stations sites in Google Earth can make that program choke.
#
# 
# Jeff Brown, April 2022
######################



library(tidyverse)

#####---  Get Data  ---#####
require('dplyr')
require('RPostgreSQL')
library ('reshape')
# connect to db
con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), host = '192.168.1.17', port = 5432, dbname = 'smc', user = 'smcread', password = '1969$Harbor')


lustations_query = "select * from sde.lu_stations"
tbl_lustations   = tbl(con, sql(lustations_query))
lustations.1     = as.data.frame(tbl_lustations)


## Sites of interest.
# OPTION 1: Add manually
SOI1 <- data.frame("SiteNumber" = c('907NP9SVC', '907TEMREF', '911GSCAPV'), "latitude" = c(32.99099, 33.049967, 32.89759), "longitude" = c(-116.85408, -116.6587, -116.52806))# Sites Of Interest
#SOI1 <- data.frame("SiteNumber" = c('BC'), "latitude" = c(33.613), "longitude" = c(-117.563))# Sites Of Interest

# OPTION 2: Read in as csv file
SOI1 <- read.csv("C:/Users/Jeffb/SCCWRP/Ephemeral Stream Assessment tools - Documents/RB4 SWAMP 2020/Identify potential reference sites/Candidate sites identified by CSUMB/RB4 sampled sites_2020_ForGE.csv", stringsAsFactors=F, strip.white=TRUE)
# format variables
SOI1 <- SOI1 %>%
  dplyr::rename(latitude=Latitude, longitude=Longitude, SiteNumber=stationcode)

#####---   Are there existing sites nearby already?   ---#####

### Are there existing sites nearby already?
NotInLuStations3 <- SOI1 %>%
  mutate(UpLat = latitude + 0.0027, # 300m in southern California
         DwnLat = latitude - 0.0027,
         UpLong = longitude + 0.0032, # 300m in southern California
         DwnLong = longitude - 0.0032)


Alist <- NotInLuStations3[!duplicated(NotInLuStations3$SiteNumber), ]
Alist <- Alist$SiteNumber

#i <- "404LLCALV" #Site will no longer work, since it has been added to lu_stations, and won't be found in 'NotInLuStations3'.
#i <- "509PS0277".  No sites nearby.
#i <- '518PS0229'.  No sites nearby.
#i <- 'R2_LOSFLORES'. 3 sites nearby.
FirstTime <- 1
for (i in Alist){
  CurrentSite <- NotInLuStations3[NotInLuStations3$SiteNumber == i, ]
  LUNearby <- lustations.1 %>%
    filter(latitude <= CurrentSite$UpLat) %>%
    filter(latitude >= CurrentSite$DwnLat) %>%
    filter(longitude >= CurrentSite$DwnLong) %>%
    filter(longitude <= CurrentSite$UpLong)
  if (length(LUNearby$stationid >0)) {
    DistDF <- data.frame(i, CurrentSite$latitude, CurrentSite$longitude, LUNearby)
    if (FirstTime == 1){
      DistDF1 <- DistDF
    } else {
      DistDF1 <- rbind(DistDF1, DistDF)
    }
    FirstTime <- FirstTime + 1 #Only advances when a nearby existing station is found (i.e., length(LUNearby$stationid >0))
  }
}

DistDF1  <- DistDF1 %>%
  dplyr::rename(SiteNumber = i,
                SOIlat = CurrentSite.latitude,
                SOIlong = CurrentSite.longitude)


#####-- Find distances between SOI site & nearby lu_stations sites  --#####
DistCalc <- function(w1, x1, y1, z1) {
  Int1 <- sin(w1*pi/180)*sin(y1*pi/180) + cos(w1*pi/180)*cos(y1*pi/180)*cos(z1*pi/180-x1*pi/180)
  Int1 <- ifelse(w1 == y1 & x1 == z1, 1, Int1)
  Int1 <- acos(Int1) * 6371000
}
Distance.Meters <- DistCalc(DistDF1$SOIlat, DistDF1$SOIlong, DistDF1$latitude, DistDF1$longitude)
DistDF2      <- data.frame(DistDF1, Distance.Meters)

DistDF3 <- DistDF2 %>%
  arrange(SiteNumber, Distance.Meters)



#####-- Find COMID for SOI, then compare with COMID from existing nearby lu_station sites  --#####
#This assumes there's only 1 nearby channel (i.e., one COMID per site, even for braided systems or sites near a confluence).
# (You could map the sites with an NHD overlay just to make sure there's only 1 stream reach associated with the site)
library(nhdplusTools)
library(sf)
# No need to convert to shapefile for NHD tool to work


# Loop to get that COMID
MySites2 <- SOI1
Alist <- MySites2[!duplicated(MySites2$SiteNumber), ]
Alist <- Alist$SiteNumber
# i <- 1 #For testing

FirstTime <- 1
for (i in Alist){
  CurrentSite <- MySites2[MySites2$SiteNumber == i, ]
  test <- st_sfc(st_point(c(CurrentSite$longitude, CurrentSite$latitude)), crs = 4326)
  test_comid <- discover_nhdplus_id(test)
  test_comid <- ifelse(is_empty(test_comid), -9999, test_comid) # this solves error when integer(empty)
  comid.df       <- data.frame(i, test_comid)
  if (FirstTime == 1){
    comid.df1 <- comid.df
  } else {
    comid.df1 <- rbind(comid.df1, comid.df)
  }
  FirstTime <- FirstTime + 1
}

comid.df1  <-dplyr::rename(comid.df1, c(SiteNumber = i, SOI.COMID = test_comid)) # new=old, using rename from dplyr (opened with tidyverse)

DistDF4 <- DistDF3 %>%
  left_join(comid.df1, by=c("SiteNumber"="SiteNumber")) %>%
  mutate(comid = as.numeric(comid))
#write.csv(DistDF4, 'C:/Users/Jeffb/SCCWRP/Ephemeral Stream Assessment tools - Documents/RB4 SWAMP 2020/Identify potential reference sites/Candidate sites identified by CSUMB/lu_stations near RB4 sampled sites.csv')


SameCOMID <- DistDF4 %>%
  filter(SOI.COMID == comid) %>%
  arrange(SiteNumber, Distance.Meters) %>%
  filter(!duplicated(SiteNumber)) %>%
  filter(Distance.Meters <= 100)
#  Sites within 100m that have the same COMID should probably be added as a new stationid entry in lu_stations, using the same masterid et al.
#  if they have a reasonable SiteNumber  Stationcodes that appear to be stationnames should be revised to match the existing
#  stationcode in lu_stations.  Sites over 100m should be added to lu_stations as a new site, along with new GIS metric calculations.
#write.csv(SameCOMID, 'L:/SMC Regional Monitoring_ES/SMC_RM/Data/Working/.../....csv')





##### USGS COMID package no longer working
# Can't find info on why or what to use as a replacement

# USGS Support Package: https://owi.usgs.gov/R/packages.html#support
# Warning message:
#   package 'nhdplusTools' was built under R version 4.1.3 


### Search results for "discover_nhdplus_id":

# https://www.rdocumentation.org/packages/nhdplusTools/versions/0.3.9/topics/discover_nhdplus_id


# https://search.r-project.org/CRAN/refmans/nhdplusTools/html/discover_nhdplus_id.html
# Examples
point <- sf::st_sfc(sf::st_point(c(-76.874, 39.482)), crs = 4326)
discover_nhdplus_id(point)

discover_nhdplus_id(point, raindrop = TRUE)

nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-08279500")
discover_nhdplus_id(nldi_feature = nldi_nwis)


# https://github.com/DOI-USGS/nhdplusTools/issues/164








