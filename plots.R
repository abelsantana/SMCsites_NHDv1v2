library(ggplot2)
library(maps)
require('dplyr')
require('RPostgreSQL')
library ('reshape')

# connect to db
con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), host = '192.168.1.17', port = 5432, dbname = 'smc', user = 'smcread', password = '1969$Harbor')
#grab all the lu stations from the SMC database
lustations_query = "select * from sde.lu_stations"
tbl_lustations   = tbl(con, sql(lustations_query))
lustations.1     = as.data.frame(tbl_lustations)

#remove stations that do not have valid lat and long
clean_stations <- lustations.1[lustations.1$latitude != -88 & lustations.1$longitude != -88,]

# Retrieve the map data for California
ca_map <- map_data("state", region = "california")

# Create a plot of the clean stations
p <- ggplot() +
  geom_polygon(data = ca_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "black") +
  geom_point(data = clean_stations, aes(x = longitude, y = latitude)) +
  theme_minimal()

print(p)
