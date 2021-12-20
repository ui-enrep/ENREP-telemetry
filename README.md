# GOES Data Viewer Shiny App

A very basic shiny app that pulls raw GOES telemetry data from an external source, cleans it, and displays two tables.  One table for SedEvent Data and one for Met Station.  The reason for this app is to try and make a single location for checking SedEvent and Met Station telmetry data compared to going to [FTS360](https://360.ftsinc.com/) and the [GOES portal](https://dcs1.noaa.gov) separately. 

The incoming raw data is originally downloaded via the NOAA provided LRGS Client and currently setup as a cron job on a linux machine in the lab.  Info and download for the client is available at [NOAA DCS Page](https://dcs1.noaa.gov) --> System Information.  The raw downloaded data is then synced to ownCloud and retrieved by the Shiny app via a shared file link.  Some sample data of what is being pulled from ownCloud is provided in the sample_data folder.  
