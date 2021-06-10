
Data_file_path <- "./Data/"
pollutant_measure <- "PM25" #character string to search for in data file that contains files of interest
pollutant_name <- "Daily.Mean.PM2.5.Concentration" #EPA's name for the pollutant concentration



#file that contains the data that needs to be matched with pollutant data
#must contain lat and lon data 
#lat and lon must be named "Lat" and "Lon" for this to work
participant_data_file_path <- "./Data/masked_participant_IDs_for_example.csv" 
participant_latitude_name <- "Lat"
participant_longitude_name <- "Lon"



#where to save generated file with pollution estimates for each lat lon
write_off_file_path <- "./Data/interpolated_PM25_concentration.csv" 

#Constrains latitude window for EPA stations to include, so we're not pulling in chicago data 
#when using IL and MO data
lat_min <- 37.772
lat_max <-  38.8