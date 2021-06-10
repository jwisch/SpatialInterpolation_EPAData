library(stringr)
library(data.table)
library(sp)
library(gstat)
library(dplyr)
library(ggplot2)
library(scales)
library(magrittr)
library(sf)

source("./R/params.R")

################################################################################
#Read in files
################################################################################
EPA_files <- list.files(path = Data_file_path, pattern="*.csv")
EPA_files <- str_subset(EPA_files, pollutant_measure) #keeping all files that contain ___
EPA_file_list = lapply(paste0(Data_file_path, EPA_files), read.csv)
EPA_files <- rbindlist(EPA_file_list, fill = TRUE)
rm(EPA_file_list)

EPA_files$Date <- as.Date(EPA_files$Date, format = "%m/%d/%Y")
EPA_files$Year <- substr(EPA_files$Date, start = 1, stop = 4)
EPA_files <- EPA_files[EPA_files$SITE_LATITUDE < lat_max & 
                         EPA_files$SITE_LATITUDE > lat_min ,]

address_df <- read.csv(participant_data_file_path)
address_df <- address_df[, c(participant_latitude_name, participant_longitude_name)]

################################################################################
#Summarizing by year
################################################################################
EPA_Annual <- setDT(EPA_files)[, .(pollutant_concentration = mean(get(pollutant_name)),
                                   AQI = mean(DAILY_AQI_VALUE)), by = list(Year, Site.Name,
                                                                           AQS_PARAMETER_DESC,
                                                                           SITE_LATITUDE,
                                                                           SITE_LONGITUDE)]

################################################################################
#Generating estimates
################################################################################
coordinates(address_df) <- ~ Lon + Lat
Years <- unique(EPA_Annual$Year)
EPA <- list()
interpolated_pollution_output <- list()
for(i in 1:length(Years)){
  EPA[[i]] <- EPA_Annual[EPA_Annual$Year == Years[i],]
  coordinates(EPA[[i]]) <- ~ SITE_LONGITUDE + SITE_LATITUDE
  analysis_pts <- SpatialPoints(
    coords      = address_df, 
    proj4string = CRS(proj4string(EPA[[i]]))
  )
  analysis.idw <- idw(log(pollutant_concentration) ~ 1,  EPA[[i]], 
                      newdata = analysis_pts) #inverse distance weighted interpolation
  analysis.idw$interpolated_pollutant_concentration <- exp(analysis.idw$var1.pred) #returning to original units
  
  aqi.idw <- idw(log(AQI) ~ 1,  EPA[[i]], 
                      newdata = analysis_pts) #inverse distance weighted interpolation
  aqi.idw$interpolated_AQI <- exp(aqi.idw$var1.pred) #returning to original units
  
  
  
  address_df <- data.frame(address_df)
  analysis.idw <- data.frame(analysis.idw)  
  aqi.idw <- data.frame(aqi.idw)  
  
  
  interpolated_pollution_output[[i]] <- merge(address_df[, c("Lat", "Lon")],
                                         analysis.idw[, c("Lat", "Lon", "interpolated_pollutant_concentration")],
                                         by = c("Lat", "Lon"))
  interpolated_pollution_output[[i]] <- merge(interpolated_pollution_output[[i]],
                                              aqi.idw[, c("Lat", "Lon", "interpolated_AQI")],
                                              by = c("Lat", "Lon"))
  interpolated_pollution_output[[i]]$Year <- Years[i]
  
}



interpolated_pollution_output <- rbindlist(interpolated_pollution_output)
interpolated_pollution_output <- interpolated_pollution_output[!duplicated(interpolated_pollution_output),]
colnames(interpolated_pollution_output)[3] <- paste0("interpolated_", pollutant_measure, "_concentration")


################################################################################
#Save off results
################################################################################
write.csv(interpolated_pollution_output, write_off_file_path, row.names = FALSE)


