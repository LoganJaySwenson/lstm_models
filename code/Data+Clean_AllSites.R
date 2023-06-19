# Data + Clean @ multiple sites
library(sf)
library(raster)
library(leaflet)
library(lubridate)
library(tidyverse)
library(dataRetrieval)

# Gages in the L. Arkansas River Basin
sites <- as_tibble(whatNWISsites(huc = "11030012", 
                                 parameterCd = "00060",
                                 outputDataTypeCd = "dv")) %>% # Gages in the L. Arkansas River Basin
  rename_with(.cols = everything(), tolower) %>%
  rename(lat = dec_lat_va, 
         lon = dec_long_va) %>%
  select(site_no, station_nm, lat, lon) %>%
  print(n = nrow(.))


site_no <- sites$site_no
dir_save <- "C:/Users/Logan/OneDrive - University of Kansas/lstm_models/data/spatial/LittleArkansasRiver_basin/"

for(i in 1:length(site_no)){
  nldiURL <- paste0("https://labs.waterdata.usgs.gov/api/nldi/linked-data/nwissite/USGS-",site_no[i],"/basin")
  
  try(nldi_data <- sf::read_sf(nldiURL))
  
  no_holes <- nngeo::st_remove_holes(nldi_data) # remove internal closed basins 

  try(st_write(nldi_data, paste0(dir_save, site_no[i], ".shp"), append = F))
}

site_files <- list.files(dir_save, pattern = "\\.shp$", full.names = T)
drainage_areas <- sapply(site_files, function(file) st_area(st_read(file)))

sites <- sites %>%
  mutate(drainage_area = drainage_areas) %>%
  arrange(drainage_area)


# Flow @ Gages in the L. Arkansas River Basin
flow <- as_tibble(readNWISdv(siteNumber = sites$site_no, parameterCd = "00060")) %>%
  renameNWISColumns() %>%
  rename_with(.cols = everything(), tolower) %>%
  select(site_no, date, flow) 

flow <- inner_join(flow, select(sites, site_no, drainage_area), by = "site_no") %>% # drainage area (square meters)
  mutate(flow = flow * 0.028316847 * 86400 / drainage_area * 1000) %>% # cfs to mm/d
  select(-drainage_area) %>%
  pivot_wider(names_from = site_no, values_from = flow) %>%
  arrange(date)

# Find start and end dates for each stream gage
start_dates <- flow %>% 
  summarise(across(starts_with("0"), ~ min(as.Date(date[!is.na(.)]), na.rm = TRUE))) %>% 
  pivot_longer(cols = starts_with("0"), names_to = "site_no", values_to = "start") 

end_dates <- flow %>% 
  summarise(across(starts_with("0"), ~ max(as.Date(date[!is.na(.)]), na.rm = TRUE))) %>% 
  pivot_longer(cols = starts_with("0"), names_to = "site_no", values_to = "end") 

sites <- inner_join(sites, start_dates, by = "site_no")
sites <- inner_join(sites, end_dates, by = "site_no")

head(sites)
head(flow)

write.csv(sites, "data/Gages.csv", row.names = F)
write.csv(flow, "data/Gages_Flow.csv", row.names = F)




# Plot! 

# Read: Gages and watershed!
sites <- read_csv("data/Gages.csv")
flow <- read_csv("data/Gages_Flow.csv")
watershed <- st_read("data/spatial/LittleArkansasRiver_basin/07144200.shp")

# Leaflet + OpenStreetMap!
p <- leaflet() %>%
  addTiles() %>%
  addPolygons(data = watershed, color = "#984ea3", fill = F) %>%
  addMarkers(lat = sites$lat, lng = sites$lon, popup = paste(sites$station_nm, "<br>", sites$site_no))
p
