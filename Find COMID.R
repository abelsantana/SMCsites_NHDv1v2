#############
#
# Use USGS package to get COMID
# Option to read in a file of coordinates (Cara's list of sites from 2/25/2021 is used)
# Option also to manually add your site info
#
# JBrown 2021
#############




library(tidyverse)
library(sf)
library(nhdplusTools)

## Option 1: Use existing file with coordinates
MySites <- read.csv('L:/RipRAM_ES/Data/Working/SalinasWatershedStreamCrossings.csv', stringsAsFactors=F, strip.white=TRUE)
MySites2  <- MySites %>%
  rename(longitude=LON, latitude=LAT) %>%
  filter(!is.na(longitude)) %>%
  filter(!is.na(latitude)) %>%
  arrange(NAME) %>% # this should not be used if a random order is needed (e.g. random divy for train & test for random forest)
  mutate(OneNum = 1,
         SiteNum = cumsum(OneNum)) %>%
  select(-OneNum)
# test <- st_sfc(st_point(c(MySites2$longitude[1], MySites2$latitude[1])), crs = 4326) # get just the first row
# test_comid <- discover_nhdplus_id(test) # this works, using a single point

## Option 2: Create dataframe manually
MySites <- data.frame("stationcode"=c("403M01661", "408CGCS13", "408WE0654", "ME-SCR2", "WCAP99-06"),
                      "longitude"=c(-119.09618000, -119.09272000, -119.09069000, -119.09618000, -119.09069000),
                      "latitude"=c(34.30608000, 34.16781000, 34.16522000, 34.30608000, 34.16522000))
MySites2  <- MySites %>%
  mutate(OneNum = 1,
         SiteNum = cumsum(OneNum)) %>%
  select(-OneNum)



## Loop to get that COMID
MySites2 <- MySites2[!duplicated(MySites2$SiteNum), ]
Alist <- MySites2[!duplicated(MySites2$SiteNum), ]
Alist <- Alist$SiteNum
# i <- 1 #For testing

FirstTime <- 1
for (i in Alist){
  CurrentSite <- MySites2[MySites2$SiteNum == i, ]
  test <- st_sfc(st_point(c(CurrentSite$longitude, CurrentSite$latitude)), crs = 4326)
  test_comid <- discover_nhdplus_id(test)
  test_comid <- ifelse(is_empty(test_comid), -9999, test_comid) # this solves error when integer(empty)
  Score       <- data.frame(i, test_comid)
  if (FirstTime == 1){
    Score1 <- Score
  } else {
    Score1 <- rbind(Score1, Score)
  }
  FirstTime <- FirstTime + 1
}

#library (reshape)
#Score1  <-rename(Score1, c(i="SiteNum", test_comid="COMID"))       #old=new, using rename from reshape
Score1  <-dplyr::rename(Score1, c(SiteNum = i, COMID = test_comid)) # new=old, using rename from dplyr (opened with tidyverse)
#Score1 <- Score1[order(Score1$SiteNum), ]
#Bad.COMID <- Score1[Score1$COMID == -9999, ] # See how many bad sites (might be marine, with no actual stream)

MySites2_comid <- MySites2 %>%
  inner_join(Score1, by=c("SiteNum"="SiteNum")) %>%
  mutate(COMID = as.numeric(COMID)) %>%  # bind said column to sites dataframe
  select(-SiteNum)

#write.csv(MySites2_comid, 'L:/RipRAM_ES/Data/Working/SalinasWatershedStreamCrossings_COMID.csv')
