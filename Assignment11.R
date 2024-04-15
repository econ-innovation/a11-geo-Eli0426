# Load required libraries
library(sf)
library(dplyr)
library(terra) # includes raster handling
library(tmap)
library(leaflet)
library(ggplot2)
library(gifski)

# Import data and convert to spatial object
hefei <- read.delim("D:/R_Project/bigdata/homework/homework11/hefei.txt")
coordinates(hefei) <- ~lng+lat

# Load development zone boundaries (update path as needed)
development_zones <- st_read("D:/R_Project/bigdata/homework/homework11/G341022合肥经济技术开发区.txt")

# Calculate number of companies within the development zone and its buffer zones
within_development_zone <- st_join(hefei, development_zones, join = st_within)
buffers_km <- st_buffer(development_zones, dist = c(1000, 3000, 5000))
count_in_buffers <- lapply(buffers_km, function(buffer) st_join(hefei, buffer, join = st_within))

# Mapping
tm_shape(development_zones) + tm_borders() + tm_shape(hefei) +
  tm_dots(col = "red", size = 0.5) + tm_layout(title = "企业分布")

# Create interactive map with leaflet
leaflet(hefei) %>% 
  addTiles() %>% 
  addMarkers(~lng, ~lat, popup = ~pripid)

# Raster data manipulation and visualization
srtm <- rast(system.file("raster/srtm.tif", package = "spDataLarge"))
resample_methods <- c("near", "bilinear", "cubic", "average", "mode")

# Display original and resampled rasters
par(mfrow=c(3,2))
plot(srtm, main="Original")
for (method in resample_methods) {
  plot(resample(srtm, res=srtm, method=method), main=method)
}
par(mfrow=c(1,1))  # Reset plot layout


library(geojsonsf)
library(rjson)
library(rlist)
hefei <- geojson_sf("D:/R_Project/bigdata/homework/homework11/hefei.json") 

ggplot() +
  geom_sf(data = hefei, fill = "lightblue") +  # 绘制开发区边界
  geom_sf(data = firm_addresses, color = "red", shape = 16, size = 0.1) +  # 绘制企业地址
  labs(title = "firm location in HEFEI") +
  theme_minimal()
