# Data + Clean @ multiple sites for HydroDL models
library(lubridate)
library(tidyverse)

# Set path to directories 
dir_data <- "C:/Users/Logan/OneDrive - University of Kansas/lstm_models/data/"
dir_save <- "C:/Users/Logan/OneDrive - University of Kansas/lstm_models/data_models"
dir_stats <- "C:/Users/Logan/OneDrive - University of Kansas/lstm_models/data_models/Statistics"

# Generate all folders
dir.create("data_models")
dir.create("data_models/crd")
dir.create("data_models/const")
dir.create("data_models/Statistics")


# Read: Gages
Gages <- read_csv("data/Gages.csv")

coordinates <- select(Gages, c("lat", "lon"))
drainage_area <- select(Gages, "drainage_area")

write_csv(coordinates, file = "data_models/crd/crd.csv", col_names = F)
write_csv(drainage_area, file = "data_models/const/drainage_area.csv", col_names = F)


# Read: Flow at Gages
flow <- read_csv("data/Gages_Flow.csv")

start <- as.Date("1979-01-01")
end <- max(as.Date(Gages$end))

flow <- flow %>%
  filter(date >= start & date <= end)

norm_flow <- flow

for (i in names(flow)[-1]) {
  
  # Get the corresponding drainage area for the site no
  area <- Gages$drainage_area[Gages$site_no == i]
  
  # Normalize flow to drainage area
  norm_flow[[i]] <- (flow[[i]] * 0.028316847)  # Convert flow from ft3/s to m3/s
  norm_flow[[i]] <- ((norm_flow[[i]] * 86400) / area) * 1000  # Convert flow from m3/s to mm/d
}

# Get list of years
years <- unique(year(norm_flow$date))

# Generate directories for each year
year_directories <- file.path(dir_save, years)
sapply(year_directories, dir.create)

variable_names <- c("flow")

# Iterate over each year
for (i in years){
  
  temp <- filter(norm_flow, year(date) == i)
  
  year_dir <- file.path(dir_save, i)
  
  temp_matrix <- t(as.matrix(temp[,-1]))
  colnames(temp_matrix) <- as.character(temp$date)
  
  file_path <- file.path(year_dir, paste0(variable_names, ".csv"))
  
  write.table(temp_matrix, file = file_path, sep = ",", row.names = F, col.names = F)
}


# Read: Meteorological Inputs
data <- tibble()

for (i in Gages$station_nm) {
  file_path <- paste0(dir_data, i, ".csv")
  
  data <- bind_rows(data, read_csv(file_path))
}

# Get list of years
years <- unique(year(data$date))

variable_names <- c("precip_amount", "min_temp", "max_temp", "min_humidity", "max_humidity", "specific_humidity", "shortwave_radiation", "wind_speed", "ref_etr", "ref_pet")

# Iterate over each year
for (i in years) {
  
  temp <- filter(data, year(date) == i)
  
  year_dir <- file.path(dir_save, i)
  
  # Pivot data and save each variable as a separate CSV
  for (j in variable_names) {
    
    temp_pivoted <- pivot_wider(temp, 
                                id_cols = site_no,
                                names_from = date, 
                                values_from = j)
    
    temp_variable <- select(temp_pivoted, site_no, everything())
    
    file_path <- file.path(year_dir, paste0(j, ".csv"))
    
    write_csv(temp_variable, file_path, col_names = F)
  }
}




# Stats for time-variable catchment attributes

# Flow stats
variable_names <- c("flow")

stats <- as.matrix(norm_flow[,-1])

mean_val <- mean(stats, na.rm = T)
sd_val <- sd(stats, na.rm = T)
percentile_10 <- quantile(stats, probs = 0.1, na.rm = T)
percentile_90 <- quantile(stats, probs = 0.9, na.rm = T)

stats <- matrix(c(mean_val, sd_val, percentile_10, percentile_90), nrow = 4, ncol = 1)

file_path <- file.path(dir_stats, paste0(variable_names, "_stat.csv"))

write.table(temp, file = file_path, sep = ",", row.names = F, col.names = F)


# Meteorological stats
variable_names <- c("precip_amount", "min_temp", "max_temp", "min_humidity", "max_humidity", "specific_humidity", "shortwave_radiation", "wind_speed", "ref_etr", "ref_pet")

stats <- list()

# Perform across all meteorological variables
for (i in variable_names) {
  
  temp <- data[i]
  
  mean_val <- mean(temp[[1]], na.rm = T)
  
  sd_val <- sd(temp[[1]], na.rm = T)
  
  percentile_10 <- quantile(temp[[1]], probs = 0.1, na.rm = T)
  
  percentile_90 <- quantile(temp[[1]], probs = 0.9, na.rm = T)
  
  stats[[i]] <- array(c(mean_val, sd_val, percentile_10, percentile_90), dim = c(4, 1))
}

# Save stats to csv
for (i in names(stats)) {
  temp <- stats[[i]]
  
  file_path <- file.path(dir_stats, paste0(i, "_stat.csv"))
  
  write.table(temp, file = file_path, sep = ",", row.names = F, col.names = F)
}





# Calculate stats for static catchment attributes
attributes <- list(drainage_area = drainage_area)

stats <- list()

# Iterate over each attribute
for (i in names(attributes)) {
  
  mean_val <- mean(attributes[[i]][[1]], na.rm = T)
  sd_val <- sd(attributes[[i]][[1]], na.rm = T)
  percentile_10 <- quantile(attributes[[i]][[1]], probs = 0.1, na.rm = T)
  percentile_90 <- quantile(attributes[[i]][[1]], probs = 0.9, na.rm = T)
  
  temp <- array(c(mean_val, sd_val, percentile_10, percentile_90), dim = c(4, 1))
  
  stats[[i]] <- temp
}

# Save stats to csv
for (i in names(stats)) {
  temp <- stats[[i]]
  
  file_path <- file.path(dir_stats, paste0("const_", i, "_stat.csv"))
  
  write.table(temp, file = file_path, sep = ",", row.names = F, col.names = F)
}