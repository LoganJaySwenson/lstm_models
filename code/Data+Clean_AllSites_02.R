# Data + Clean @ multiple sites
library(sf)
library(raster)
library(lubridate)
library(tidyverse)

# Read: Gages and watershed!
sites <- read_csv("data/Gages.csv")

# L. Arkansas River Basin
basin <- st_read("data/spatial/LittleArkansasRiver_basin/07144200.shp") # L. Arkansas River @ Valley Center

# Find start and end dates 
start = as.Date("1979-01-01")
end = max(as.Date(sites$end))

site_no <- sites$site_no
dir_save <- "C:/Users/Logan/OneDrive - University of Kansas/lstm_models/data/spatial/LittleArkansasRiver_basin/"

# Get GridMET Data
time_steps <- seq(as.Date(start), as.Date(end), by=1)

output = climateR::getGridMET(AOI::aoi_get(basin),
                              varname = c("pr", "tmmn", "tmmx", "srad",
                                          "rmin", "rmax", "sph", "vs", 
                                          "pet", "etr"),
                              startDate = start,
                              endDate = end)

# Loop over GridMET rasters to change CRS and crop to L. Arkansas River Basin
basin_cropped <- lapply(output, function(x){
  
  raster_reprojected <- terra::project(x, crs(basin), method="near")
  
  raster_cropped <- terra::crop(x, basin, mask=T)
  
  return(raster_cropped)
})

# Read watersheds
watersheds <- list()

for (i in site_no) {
  
  watershed_path <- file.path(dir_save, paste0(i, ".shp"))
  
  watershed <- st_read(watershed_path)
  
  watersheds[[i]] <- watershed
}

# Mean GridMET values per time step 
results <- list()

for (i in 1:length(site_no)) {
  df <- tibble()
  site_name <- sites$station_nm[i]
  
  for (j in 1:length(basin_cropped)) {
    # get the variable name for each list of rasters
    variable_name <- names(basin_cropped[j])
    
    # get the number of time steps in the list of rasters
    n_steps <- dim(basin_cropped[[j]])[3]
    
    # crop rasters for the current site_name
    rasters_cropped <- terra::crop(basin_cropped[[j]], watersheds[[i]], mask = TRUE)
    
    # loop through each time step
    for (m in 1:n_steps) {
      # subset the cropped raster data for the current time step
      raster <- as.matrix(rasters_cropped[[m]])
      
      # calculate the mean for the current time step and site_name
      mean_value <- mean(raster, na.rm = TRUE)
      
      # get the corresponding date for the current time step
      date_value <- time_steps[m]
      
      # add the mean value, time step, site_name, and variable name to the site's tibble
      df <- rbind(df, tibble(date = date_value, 
                             mean = mean_value,
                             site_no = site_no[i],
                             variable = variable_name))
    }
  }
  results[[site_name]] <- df
}

# Re-structure for ML model
results_wide <- lapply(results, function(df) {
  df <- spread(df, key = variable, value = mean)
  df <- df %>%
    rename(precip_amount = precipitation_amount,
           min_temp = daily_minimum_temperature,
           max_temp = daily_maximum_temperature,
           min_humidity = daily_minimum_relative_humidity,
           max_humidity = daily_maximum_relative_humidity,
           specific_humidity = daily_mean_specific_humidity,
           shortwave_radiation = daily_mean_shortwave_radiation_at_surface,
           wind_speed = daily_mean_wind_speed,
           ref_etr = daily_mean_reference_evapotranspiration_alfalfa,
           ref_pet = daily_mean_reference_evapotranspiration_grass) %>%
    select(date, 
           site_no,
           precip_amount,
           min_temp,
           max_temp,
           min_humidity,
           max_humidity,
           specific_humidity,
           shortwave_radiation,
           wind_speed,
           ref_etr,
           ref_pet)
  return(df)
})

# Save to csv!
dir_save <- "C:/Users/Logan/OneDrive - University of Kansas/lstm_models/data"

for (i in 1:length(site_no)) {
  
  site_name <- sites$station_nm[i]
  
  file_name <- paste0(dir_save, "/", site_name, ".csv")
  
  write_csv(results_wide[[site_name]], file = file_name)
  
}