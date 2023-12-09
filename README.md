# GOES Telemetry

## Introduction

This repository houses scripts to download, clean, and display telemetered data from ENREP field equipment. The ENREP team operates flumes and meteorological stations in remote field locations and therefore needs easy access to the near real-time status of the equipment. This helps identify the health of the equipment and aid with field planning, logistics, and to ensure high quality, continuous data collection. The repository can be split into two components: raw data processing and data display.

## Data Processing

Due to local conditions at the various equipment sites, two telemetry platforms are used to transmit data: GOES and Iridium. Accessing the data from these two platforms is either cumbersome or requires 3rd party software. A primary goal of this project is to combine the two data streams into a single dashboard.

#### GOES Processing

The GOES data is readily available from NOAA's [Data Distribution System (DADDS)](https://dcs1.noaa.gov/) but is cumbersome and does not allow easy identification of what transmission belongs to which piece of equipment. They do, however, provide a Java based program, the [LRGS Client](https://dcs1.noaa.gov/LRGS/LRGS-Client-Getting-Started_2021.pdf), that allows you to download data via a GUI or command line. The program is operating system agnostic although it's designed to be installed in a GUI. There may be another method, but you may need more experience with Java. It appears the installation hard codes in some directory paths to the Java scripts. We were able to manually edit those when the installation was copied to a Linux VM with no GUI.

The data retrieval and cleaning scripts call the LRGS client from with R and perform all the cleaning. The LRGS client does require configuration files to identify the NESIDs to fetch data for and for what time interval. Those configuration files are located in the 'additionalConfig' folder.

#### Iridium Processing

The Iridium data is provided through the manufacturers of the FTS SedEvent systems and their online data dashboard, [FTS360](https://360.ftsinc.com/). This website pulls all the raw Iridium data and does all the processing. However, due to the mixture of telemetry data streams, it's inconvenient to not have all streams in one place. The solution here is to scrape the data from the website and ingest it into our Shiny dashboard. The web scraping uses the `selenium/standalone-chrome:111.0` Docker container as the headless browser. Versions newer than this did not work for unknown reasons.

#### Cron Jobs

Data is retrieved on \~12 hour intervals from GOES and Iridium via cron jobs running in the Linux VM. The current cron jobs are located in the 'additionalConfig' folder. That text is not referenced by cron and just copied here for record.
