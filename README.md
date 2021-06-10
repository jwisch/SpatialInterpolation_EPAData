# SpatialInterpolation_EPAData

Currently, I have data pulled from the EPA for Missouri and Illinois, 2015 - 2021. ( https://www.epa.gov/outdoor-air-quality-data/download-daily-data )The EPA monitors a variety of air pollutants. I have extracted CO, NO2, PB, PM2.5, and PM10 to date. These datasets are stored in the /Data/ folder. 

If you want to estimate pollutant levels for a set of lat/lon coordinates, you must supply a .csv that contains the points for which you want estimates (lat and longitude must be labeled "Lat" and "Lon" as column headers). Update the file name in the params.R file. The prep_coords_for_analysis.R script will generate point estimates of pollution for each of these lat/longs using inverse distance weighted interpolation. It will then write the result to the file location indicated in the params.R folder.
