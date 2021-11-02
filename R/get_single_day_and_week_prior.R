library(stringr)
library(data.table)
library(sp)
library(gstat)
library(dplyr)
library(ggplot2)
library(scales)
library(magrittr)
library(sf)
#https://www.epa.gov/outdoor-air-quality-data/download-daily-data
#Need to download 2011 - 2014 for PM2.5
DAILY_CONCENTRATION <- "Daily.Max.1.hour.SO2.Concentration"
POLLUTANT <- "SO2"
################################################################################
#Read in files
################################################################################
EPA_files <- list.files(path = "./Data/", pattern="*.csv")
EPA_files <- str_subset(EPA_files, POLLUTANT) #keeping all files that contain ___
EPA_file_list = lapply(paste0( "./Data/", EPA_files), read.csv)
EPA_files <- rbindlist(EPA_file_list, fill = TRUE)
rm(EPA_file_list)

EPA_files$Date <- as.Date(EPA_files$Date, format = "%m/%d/%Y")
EPA_files$Year <- substr(EPA_files$Date, start = 1, stop = 4)
EPA_files <- data.frame(EPA_files)[, c("Date", "SITE_LATITUDE", "SITE_LONGITUDE", DAILY_CONCENTRATION)]

address_df <- read.csv( "../HIV_socialDeterminants/Data/Full_Location_plus_ADI_list.csv")
address_df <- address_df[!duplicated(address_df),]
address_df <- address_df[!is.na(address_df$Lat),]
address_df$Study.date <- as.Date(address_df$Study.date, format = "%m/%d/%Y")
address_df$Study.date_lag1 <- address_df$Study.date - 1
address_df$Study.date_lag2 <- address_df$Study.date - 2
address_df$Study.date_lag3 <- address_df$Study.date - 3
address_df$Study.date_lag4 <- address_df$Study.date - 4
address_df$Study.date_lag5 <- address_df$Study.date - 5
address_df$Study.date_lag6 <- address_df$Study.date - 6
address_df$Study.date_lag7 <- address_df$Study.date - 7


################################################################################
#Generating estimates
################################################################################
address_df_to_calculate <- unique(rbind(address_df[, c("Lat", "Lon", "Study.date")],
                                        setnames(address_df[, c("Lat", "Lon", "Study.date_lag1")], c("Lat", "Lon", "Study.date")),
                                        setnames(address_df[, c("Lat", "Lon", "Study.date_lag2")], c("Lat", "Lon", "Study.date")),
                                        setnames(address_df[, c("Lat", "Lon", "Study.date_lag3")], c("Lat", "Lon", "Study.date")),
                                        setnames(address_df[, c("Lat", "Lon", "Study.date_lag4")], c("Lat", "Lon", "Study.date")),
                                        setnames(address_df[, c("Lat", "Lon", "Study.date_lag5")], c("Lat", "Lon", "Study.date")),
                                        setnames(address_df[, c("Lat", "Lon", "Study.date_lag6")], c("Lat", "Lon", "Study.date")),
                                        setnames(address_df[, c("Lat", "Lon", "Study.date_lag7")], c("Lat", "Lon", "Study.date"))))

#EPA <- list()
interpolated_pollution_output <- list()
Dates <- unique(EPA_files$Date)
counter <- 0
for(i in 1:length(Dates)){
  print(i)
  EPA <- EPA_files[EPA_files$Date == Dates[i],]
  EPA[, "Daily.Max.1.hour.SO2.Concentration"][EPA[, "Daily.Max.1.hour.SO2.Concentration"] < 0] <- 0
  EPA <- data.frame(EPA)
  EPA <- EPA[complete.cases(EPA),]
  coordinates(EPA) <- ~ SITE_LONGITUDE + SITE_LATITUDE
  address <- address_df_to_calculate[address_df_to_calculate$Study.date %in% Dates[i], c("Lat", "Lon")]
  address <- data.frame(address[!is.na(address),])
  address <- address[complete.cases(address),]
  
  if(nrow(address > 0)){
    counter <- counter + 1
  coordinates(address) <- ~ Lon + Lat
  
  analysis_pts <- SpatialPoints(
    coords      = address, 
    proj4string = CRS(proj4string(EPA))
  )
  analysis.idw <- idw(log(Daily.Max.1.hour.SO2.Concentration) ~ 1,  EPA, 
                      newdata = analysis_pts) #inverse distance weighted interpolation
  analysis.idw$interpolated_pollutant_concentration <- exp(analysis.idw$var1.pred) #returning to original units
  
  address_df_to_calculate <- data.frame(address_df_to_calculate)
  analysis.idw <- data.frame(analysis.idw)  

  interpolated_pollution_output[[counter]] <- merge(address_df_to_calculate[, c("Lat", "Lon")],
                                              analysis.idw[, c("Lat", "Lon", "interpolated_pollutant_concentration")],
                                              by = c("Lat", "Lon"))
  interpolated_pollution_output[[counter]]$Date <- Dates[i]}
  rm(analysis_pts, EPA)
}



interpolated_pollution_output <- rbindlist(interpolated_pollution_output)
interpolated_pollution_output <- interpolated_pollution_output[!duplicated(interpolated_pollution_output),]

address_df_merged <- merge(address_df, interpolated_pollution_output, 
                    by.x = c("Study.date", "Lat", "Lon"),
                    by.y = c("Date", "Lat", "Lon"), all.x = TRUE, all.y = FALSE)
colnames(address_df_merged)[length(address_df_merged)] <- "interpolated_PM25_Study.date"
address_df_merged <- merge(address_df_merged, interpolated_pollution_output, 
                           by.x = c("Study.date_lag1", "Lat", "Lon"),
                           by.y = c("Date", "Lat", "Lon"), all.x = TRUE, all.y = FALSE)
colnames(address_df_merged)[length(address_df_merged)] <- "interpolated_PM25_Study.date_lag1"
address_df_merged <- merge(address_df_merged, interpolated_pollution_output, 
                           by.x = c("Study.date_lag2", "Lat", "Lon"),
                           by.y = c("Date", "Lat", "Lon"), all.x = TRUE, all.y = FALSE)
colnames(address_df_merged)[length(address_df_merged)] <- "interpolated_PM25_Study.date_lag2"
address_df_merged <- merge(address_df_merged, interpolated_pollution_output, 
                           by.x = c("Study.date_lag3", "Lat", "Lon"),
                           by.y = c("Date", "Lat", "Lon"), all.x = TRUE, all.y = FALSE)
colnames(address_df_merged)[length(address_df_merged)] <- "interpolated_PM25_Study.date_lag3"
address_df_merged <- merge(address_df_merged, interpolated_pollution_output, 
                           by.x = c("Study.date_lag4", "Lat", "Lon"),
                           by.y = c("Date", "Lat", "Lon"), all.x = TRUE, all.y = FALSE)
colnames(address_df_merged)[length(address_df_merged)] <- "interpolated_PM25_Study.date_lag4"
address_df_merged <- merge(address_df_merged, interpolated_pollution_output, 
                           by.x = c("Study.date_lag5", "Lat", "Lon"),
                           by.y = c("Date", "Lat", "Lon"), all.x = TRUE, all.y = FALSE)
colnames(address_df_merged)[length(address_df_merged)] <- "interpolated_PM25_Study.date_lag5"
address_df_merged <- merge(address_df_merged, interpolated_pollution_output, 
                           by.x = c("Study.date_lag6", "Lat", "Lon"),
                           by.y = c("Date", "Lat", "Lon"), all.x = TRUE, all.y = FALSE)
colnames(address_df_merged)[length(address_df_merged)] <- "interpolated_PM25_Study.date_lag6"
address_df_merged <- merge(address_df_merged, interpolated_pollution_output, 
                           by.x = c("Study.date_lag7", "Lat", "Lon"),
                           by.y = c("Date", "Lat", "Lon"), all.x = TRUE, all.y = FALSE)
colnames(address_df_merged)[length(address_df_merged)] <- "interpolated_PM25_Study.date_lag7"

address_df_merged$interpolated_PM25_weekPrior <- (address_df_merged$interpolated_PM25_Study.date_lag1 +
  address_df_merged$interpolated_PM25_Study.date_lag2 + address_df_merged$interpolated_PM25_Study.date_lag3 +
  address_df_merged$interpolated_PM25_Study.date_lag4 + address_df_merged$interpolated_PM25_Study.date_lag5 +
  address_df_merged$interpolated_PM25_Study.date_lag6 + address_df_merged$interpolated_PM25_Study.date_lag7) / 7

address_df_merged <- address_df_merged[, c("related_study_id", "Study.date", "ADI_NATRANK", "interpolated_PM25_Study.date",
                                           "interpolated_PM25_Study.date_lag1", "interpolated_PM25_weekPrior")]

ggplot(address_df_merged, aes(x = ADI_NATRANK, y = interpolated_PM25_weekPrior)) +
  geom_point() + geom_smooth(method = "lm") + theme_bw()
ggplot(address_df_merged, aes(x = interpolated_PM25_Study.date, y = interpolated_PM25_Study.date_lag1)) +
  geom_point() + geom_smooth(method = "lm") + theme_bw()


colnames(address_df_merged)[4:6] <- c("interpolated_SO2_Study.date", "interpolated_SO2_Study.date_lag1",
                                      "interpolated_SO2_weekPrior")
write.csv(address_df_merged, "../HIV_socialDeterminants/Data/IDs_with_interpolatedSO2.csv", row.names = FALSE)
