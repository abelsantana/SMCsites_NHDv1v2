library(ggplot2)
library(maps)


# Retrieve the map data for California
ca_map <- map_data("state", region = "california")

# Create a plot of California map
p <- ggplot() +
  geom_polygon(data = ca_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "black") +
  geom_point(data = clean_stations, aes(x = longitude, y = latitude)) +
  theme_minimal()

print(p)
