#Data + Clean
library(sf)
library(raster)
library(lubridate)
library(tidyverse)
library(dataRetrieval)

#Publication theme
source("code/Theme+Settings.R")

# AOI: Kansas
AOI <- AOI::aoi_get(state = "KS") 
Kansas <- st_read("data/spatial/Kansas/Kansas_outline.shp")

# Little Arkansas River (USGS gage 07143672), near Halstead, KS
site.info <- as_tibble(readNWISsite("07143672")) %>%
  select(2:3,7:8,12,30) %>%
  print()

pp <- tibble(
  x = 38.02853,
  y = -97.54054) %>% 
  st_as_sf(., coords = c("y","x"), crs=4326)

watershed <- st_read("data/spatial/LittleArkansasRiver_watershed/07143672.shp")
streams <- st_read("data/spatial/streams/Kansas_streams.shp") 
  #filter(., grepl("Arkansas", BASIN, ignore.case=T))

#Plot!
ggplot()+
  geom_sf(data = Kansas, fill = NA, color = "black")+
  geom_sf(data = watershed, fill = NA, color = "purple")+
  geom_sf(data = streams, color = "blue")+
  geom_sf(data = pp, color = "red")+
  scale_x_continuous(expand = c(0,0), labels = function(x) paste0(x, '\u00B0', "W"))+
  scale_y_continuous(expand = c(0,0), labels = function(x) paste0(x, '\u00B0', "N"))+
  coord_sf()+
  theme(panel.border = element_blank(),
        axis.text.x=element_text(size = 8), axis.ticks.x=element_blank(), 
        axis.text.y=element_text(size = 8), axis.ticks.y=element_blank())




# GridMET Data
AOI = AOI::aoi_get(watershed)

output = climateR::getGridMET(AOI::aoi_get(watershed),
                varname = c("pr", "tmmn", "tmmx"), #precip, tmin, tmax
                startDate = "2023-03-26",
                endDate = "2023-04-02")

# Loop over GridMET output to change CRS and crop to watershed!
output.cropped <- lapply(output, function(x){
  
  raster_reprojected <- terra::project(x, crs(watershed), method="near")
  
  raster_cropped <- terra::crop(x, watershed, mask=T)
  
  return(raster_cropped)
})

# Plot to see if things worked or not!
png("figures/Fig1.png", width=190, height=90, res=300, units = "mm")
plot(output.cropped$precipitation_amount)
dev.off()

png("figures/Fig2.png", width=190, height=90, res=300, units = "mm")
plot(output.cropped$daily_minimum_temperature)
dev.off()

png("figures/Fig3.png", width=190, height=90, res=300, units = "mm")
plot(output.cropped$daily_maximum_temperature)
dev.off()





# Little Arkansas River (USGS gage 07143672) Flow Data
Flow <- readNWISdv(siteNumber = "07143672", parameterCd = "00060", startDate = "", endDate = "") %>% 
  renameNWISColumns(.) %>%
  as_tibble(.) 

# Flow spans from 1995-05-01 to present
start = "2023-03-26"
end = "2023-04-02"
time_steps <- seq(as.Date("1995-10-01"), as.Date("2022-09-01"), by=1)

# GridMET Data
AOI = AOI::aoi_get(watershed)

output = climateR::getGridMET(AOI::aoi_get(watershed),
                              varname = c("pr", "tmmn", "tmmx"), #precip, tmin, tmax
                              startDate = "1995-10-01",
                              endDate = "2022-09-01")

# Loop over GridMET rasters to change CRS and crop to watershed!
output.cropped <- lapply(output, function(x){
  
  raster_reprojected <- terra::project(x, crs(watershed), method="near")
  
  raster_cropped <- terra::crop(x, watershed, mask=T)
  
  return(raster_cropped)
})

# Mean GridMET value per time step 
mean_df <- tibble()
for (i in 1:length(output.cropped)) {
  
  # get the variable name for each list of rasters
  variable_name <- names(output.cropped[i])
  
  # get the number of time steps in list of rasters
  n_steps <- dim(output.cropped[[i]])[3]
  
  # loop through each time step
  for (j in 1:n_steps) {
    
    # subset the raster data for the current time step
    raster_data <- output.cropped[[i]][,,j]
    
    # calculate the mean for the current time step
    mean_value <- mean(raster_data, na.rm = T)
    
    # get the corresponding date for the current time step
    date_value <- time_steps[j]
    
    # add the mean value and time step to the data frame
    mean_df <- rbind(mean_df, tibble(date = date_value, 
                                     mean_value = mean_value,
                                     variable = variable_name))
  }
}
# print results!
df <- spread(mean_df, key = variable, value = mean_value) %>% print(.)




