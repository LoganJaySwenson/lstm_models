library(sf)

sites <- c("07143672")
dir_save <- "C:/Users/Logan/OneDrive - University of Kansas/lstm_models/data/spatial/LittleArkansasRiver_watershed/"

for(i in 1:length(sites)){
  # i = 1
  nldiURL <- paste0("https://labs.waterdata.usgs.gov/api/nldi/linked-data/nwissite/USGS-",sites[i],"/basin")
  
  try( nldi_data <- sf::read_sf(nldiURL))
  try( st_write(nldi_data, paste0(dir_save, sites[i], ".shp"), append = F))
}

# if there are internal closed basins that need removed
no_holes <- nngeo::st_remove_holes(nldi_data)