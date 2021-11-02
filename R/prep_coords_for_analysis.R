library(stringr)
library(data.table)
library(sp)
library(gstat)
library(dplyr)
library(ggplot2)
library(scales)
library(magrittr)
library(sf)
#New comment
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
EPA_files$DAILY_AQI_VALUE <- as.numeric(EPA_files$DAILY_AQI_VALUE)
EPA_files$Days_over_Threshold <- ifelse(data.frame(EPA_files)[, pollutant_name] > min_daily_thresh, 1, 0)
EPA_files$Days_over_mod_AQI <- ifelse(data.frame(EPA_files)[, "DAILY_AQI_VALUE"] > AQI_moderate, 1, 0)
EPA_files$Days_over_sens_AQI <- ifelse(data.frame(EPA_files)[, "DAILY_AQI_VALUE"] > AQI_sens_groups, 1, 0)
EPA_files$Days_over_unhealthy_AQI <- ifelse(data.frame(EPA_files)[, "DAILY_AQI_VALUE"] > AQI_unhealthy, 1, 0)



address_df <- read.csv(participant_data_file_path)
address_df <- address_df[, c(participant_latitude_name, participant_longitude_name)]
address_df <- address_df[!duplicated(address_df),]
address_df <- address_df[!is.na(address_df$Lat),]
################################################################################
#Summarizing by year
################################################################################
EPA_Annual <- setDT(EPA_files)[, .(pollutant_concentration = mean(get(pollutant_name), na.rm = TRUE),
                                   est_days_over_legal_threshold = (sum(Days_over_Threshold, na.rm = TRUE) / .N)*365,
                                   AQI = mean(DAILY_AQI_VALUE, na.rm = TRUE),
                                   est_days_mod_AQI_thresh = (sum(Days_over_mod_AQI, na.rm = TRUE) / .N)*365,
                                   est_days_sens_AQI_thresh = (sum(Days_over_sens_AQI, na.rm = TRUE) / .N)*365,
                                   est_days_unhealthy_AQI_thresh = (sum(Days_over_unhealthy_AQI, na.rm = TRUE) / .N)*365), 
                               by = list(Year, Site.Name,
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
  
  pollutant_threshold.idw <- idw(log(est_days_over_legal_threshold) ~ 1, EPA[[i]],
                                 newdata = analysis_pts)
  pollutant_threshold.idw$interpolated_days_over_legal_threshold <- exp(pollutant_threshold.idw$var1.pred)
  
  aqi_mod.idw <- idw(log(est_days_mod_AQI_thresh) ~ 1, EPA[[i]],
                     newdata = analysis_pts)
  aqi_mod.idw$interpolated_est_days_mod_AQI_thresh <- exp(aqi_mod.idw$var1.pred)
  
  aqi_sens.idw <- idw(log(est_days_sens_AQI_thresh) ~ 1, EPA[[i]],
                     newdata = analysis_pts)
  aqi_sens.idw$interpolated_est_days_sens_AQI_thresh <- exp(aqi_sens.idw$var1.pred)
  
  aqi_unhealthy.idw <- idw(log(est_days_unhealthy_AQI_thresh) ~ 1, EPA[[i]],
                      newdata = analysis_pts)
  aqi_unhealthy.idw$interpolated_est_days_unhealthy_AQI_thresh <- exp(aqi_sens.idw$var1.pred)
  
  
  
  
  address_df <- data.frame(address_df)
  analysis.idw <- data.frame(analysis.idw)  
  pollutant_threshold.idw <- data.frame(pollutant_threshold.idw)
  aqi.idw <- data.frame(aqi.idw)  
  aqi_mod.idw <- data.frame(aqi_mod.idw)
  aqi_sens.idw <- data.frame(aqi_sens.idw)
  aqi_unhealthy.idw <- data.frame(aqi_unhealthy.idw)
  
  
  interpolated_pollution_output[[i]] <- merge(address_df[, c("Lat", "Lon")],
                                         analysis.idw[, c("Lat", "Lon", "interpolated_pollutant_concentration")],
                                         by = c("Lat", "Lon"))
  interpolated_pollution_output[[i]] <- merge(interpolated_pollution_output[[i]],
                                              pollutant_threshold.idw[, c("Lat", "Lon", "interpolated_days_over_legal_threshold")],
                                              by = c("Lat", "Lon"))
  interpolated_pollution_output[[i]] <- merge(interpolated_pollution_output[[i]],
                                              aqi.idw[, c("Lat", "Lon", "interpolated_AQI")],
                                              by = c("Lat", "Lon"))
  interpolated_pollution_output[[i]] <- merge(interpolated_pollution_output[[i]],
                                              aqi_mod.idw[, c("Lat", "Lon", "interpolated_est_days_mod_AQI_thresh")],
                                              by = c("Lat", "Lon"))
  interpolated_pollution_output[[i]] <- merge(interpolated_pollution_output[[i]],
                                              aqi_sens.idw[, c("Lat", "Lon", "interpolated_est_days_sens_AQI_thresh")],
                                              by = c("Lat", "Lon"))
  interpolated_pollution_output[[i]] <- merge(interpolated_pollution_output[[i]],
                                              aqi_unhealthy.idw[, c("Lat", "Lon", "interpolated_est_days_unhealthy_AQI_thresh")],
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


