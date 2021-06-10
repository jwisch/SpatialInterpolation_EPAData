library(stringr)
library(data.table)
library(sp)
library(gstat)
library(dplyr)
library(ggplot2)
library(scales)
library(magrittr)
library(tidyverse)
library(sf)
library(osmdata)
library(tidycensus)

readRenviron("~/.Renviron") #reading in my installed api key. not including api key in version pushed to git
#Good RMD on Kriging: https://rpubs.com/nabilabd/118172

################################################################################
#Read in files
################################################################################
EPA_files <- list.files(path = "./Data/", pattern="*.csv")
EPA_files <- str_subset(EPA_files, "PM25") #keeping all files that contain ___
EPA_file_list = lapply(paste0("./Data/", EPA_files), read.csv)
EPA_files <- rbindlist(EPA_file_list, fill = TRUE)
rm(EPA_file_list)

EPA_files$Date <- as.Date(EPA_files$Date, format = "%m/%d/%Y")
EPA_files$Year <- substr(EPA_files$Date, start = 1, stop = 4)
EPA_files <- EPA_files[EPA_files$SITE_LATITUDE < 38.8 & 
                         EPA_files$SITE_LATITUDE > 37.772 ,]
address_df <- read.csv("./Data/masked_participant_IDs_for_example.csv")


################################################################################
#Summarizing by year
################################################################################
EPA_Annual <- setDT(EPA_files)[, .(PM25_concentration = mean(Daily.Mean.PM2.5.Concentration),
                     AQI = mean(DAILY_AQI_VALUE)), by = list(Year, Site.Name,
                                                       AQS_PARAMETER_DESC,
                                                       SITE_LATITUDE,
                                                       SITE_LONGITUDE)]

################################################################################
#Plotting monitoring site locations
################################################################################
#using tidy census to pull census tract geometries
stl_value <- get_acs(geography = "tract"
                     , state = c("MO"),
                     county = c("St. Louis County", "St. Louis city",
                                "St. Charles County", "Jefferson County"),
                     variables = "B19013_001",
                     geometry = TRUE)
il_value <- get_acs(geography = "tract"
                    , state = c("IL"),
                    county = c("Bond County", "Calhoun County", "Clinton County",
                               "Jersey County", "Macoupin County", "Madison County",
                               "Monroe County", "St. Clair County"),
                    variables ="B19013_001",
                    geometry = TRUE)

metro_region <- rbind(stl_value, il_value)
rm(stl_value, il_value)


ggplot(metro_region, aes()) + geom_sf() + 
geom_point(data = EPA_Annual[EPA_Annual$Year == 2017,], 
           aes(y = SITE_LATITUDE, x = SITE_LONGITUDE, 
               size = PM25_concentration, alpha = 0.8, colour = "red")) + 
  #scale_colour_manual(values = c("coral2", "springgreen2")) +
  xlim(c(-90.58, -89.95)) + ylim(c(38.471, 38.83)) + theme_bw() +
  ggtitle("Particulate Matter 2.5 Monitoring Sites, Greater St. Louis Area") + xlab("") + ylab("")

################################################################################
#Spatial Interpolation for Visualization
#Note that when you visualize, you have to have an evenly spaced grid for ggplot's
#functionality to perform right. When you are just calcualted points for participants
#they do not have to be evenly spaced
################################################################################
EPA <- EPA_Annual[EPA_Annual$Year == 2017,]
coordinates(EPA) <- ~ SITE_LONGITUDE + SITE_LATITUDE
coordinates(address_df) <- ~ Lon + Lat
bbox(EPA) #gives full spatial scale that we have data for

#Plot areas of interest
# to compare, recall the bubble plot above; those points were what there were values for. this is much more sparse
EPA %>% as.data.frame %>%
  ggplot(aes(SITE_LONGITUDE, SITE_LATITUDE)) + geom_point(size=1) + #coord_equal() + 
  ggtitle("Points with measurements") + theme_bw()

metro_region_coordinates <- data.frame(unlist(metro_region$geometry))

grd <- makegrid(address_df, n = 1000)
colnames(grd) <- c("x", "y")


grd <- grd[!duplicated(grd),]
# Next, convert the grid to `SpatialPoints` and subset these points by the polygon.
grd_pts <- SpatialPoints(
  coords      = grd, 
  proj4string = CRS(proj4string(EPA))
)


lzn.idw <- idw(log(PM25_concentration) ~ 1,  EPA, 
                    newdata = grd_pts) #inverse distance weighted interpolation
lzn.idw$interpolatedPM25 <- exp(lzn.idw$var1.pred) #returning to original units

#figuring out how to plot the new pm2.5 values

  ggplot(metro_region, aes()) + geom_sf(fill = NA) +
    geom_point(data = data.frame(address_df), aes(x = Lon, y = Lat), size = 0.2, alpha = 0.8) +
    theme_bw() +
    geom_tile(data = data.frame(lzn.idw), aes(x = x, y = y, 
                                              fill = interpolatedPM25), alpha = 0.8) +
    ylim(c(min(data.frame(lzn.idw)$y), max(data.frame(lzn.idw)$y))) +
    xlim(c(min(data.frame(lzn.idw)$x), max(data.frame(lzn.idw)$x))) +
    ggtitle("Interpolated Particulate Matter 2.5 Concentration", 
    subtitle = "Greater St. Louis Area") + xlab("") + ylab("") +
    xlim(c(min(metro_region_coordinates[metro_region_coordinates$unlist.metro_region.geometry. < 0,]),
           max(metro_region_coordinates[metro_region_coordinates$unlist.metro_region.geometry. < 0,]))) +
    ylim(c(min(metro_region_coordinates[metro_region_coordinates$unlist.metro_region.geometry. > 0,]),
           max(metro_region_coordinates[metro_region_coordinates$unlist.metro_region.geometry. > 0,]))) 
    
  
  ################################################################################
  #Spatial Interpolation for analysis
  ################################################################################
  
  analysis_pts <- SpatialPoints(
    coords      = address_df, 
    proj4string = CRS(proj4string(EPA))
  )
  
  
  analysis.idw <- idw(log(PM25_concentration) ~ 1,  EPA, 
                 newdata = analysis_pts) #inverse distance weighted interpolation
  analysis.idw$interpolatedPM25 <- exp(analysis.idw$var1.pred) #returning to original units

  address_df <- data.frame(address_df)
analysis.idw <- data.frame(analysis.idw)  

address_df <- merge(address_df[, c("Lat", "Lon", "census_code", "LocationName")], 
                    analysis.idw[, c("Lat", "Lon", "interpolatedPM25")], 
                    by = c("Lat", "Lon"))


plot_df <- merge(metro_region, address_df[, c("LocationName", "interpolatedPM25", 
                                              "Lat", "Lon")], 
                 by.x = "GEOID", by.y = "LocationName", all = TRUE)

ggplot(plot_df, aes(fill = interpolatedPM25)) + geom_sf() + 
  geom_point(aes(x = Lon, y = Lat), size = 0.4) + 
  xlim(c(-90.58, -89.95)) + ylim(c(38.471, 38.83)) 
