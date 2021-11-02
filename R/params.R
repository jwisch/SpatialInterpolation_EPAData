
Data_file_path <- "./Data/"
pollutant_measure <- "PM25" #character string to search for in data file that contains files of interest
pollutant_name <- "Daily.Mean.PM2.5.Concentration" #EPA's name for the pollutant concentration
#EPA criteria: https://www.epa.gov/criteria-air-pollutants/naaqs-table
  # CO = 9 ppm / 8 hours
  #PB = 0.15 rolling 3 month average
  #NO2 100 ppb / hour
  #Ozone 0.07/8 hours
  #PM2.5 35 / 24 hours
  #PM10 150 / 24 hours
  #SO2 0.5 ppm / 3 hours, 75 ppb / 1 hour
min_daily_thresh <- 35

#AQI Thresholds https://www.epa.gov/sites/production/files/2016-04/documents/2012_aqi_factsheet.pdf
AQI_moderate <- 50
AQI_sens_groups <- 100
AQI_unhealthy <- 150

#file that contains the data that needs to be matched with pollutant data
#must contain lat and lon data 
#lat and lon must be named "Lat" and "Lon" for this to work
participant_data_file_path <- "./Data/Need_Interpolated_AQI.csv" 
participant_latitude_name <- "Lat"
participant_longitude_name <- "Lon"



#where to save generated file with pollution estimates for each lat lon
write_off_file_path <- paste0("./Data/supplemental_interpolated_", pollutant_measure, "_concentration.csv") 

#Constrains latitude window for EPA stations to include, so we're not pulling in chicago data 
#when using IL and MO data
lat_min <- 37.772
lat_max <-  38.8