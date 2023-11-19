# GOES Telemetry

-   Intro

    -   what is this project broadly and why do we need this

        -   Pulls together telemetry from different sources into single place

        -   Identify the health of equipment and aid with field trip planning logistics

    -   Two components

        -   Data Retrieval

        -   Data Display

-   Data Retrieval

    -   GOES

        -   LRGS client

            -   java client from DCS LINK HERE

            -   installer requires GUI typically. UI Linux Admins manually edited the program

        -   message configuration

    -   Iridium

        -   FTS360 web scraping

        -   Docker

    -   Cron jobs

        -   actual commands

        -   timing

This repository collects and displays telemetry data for the ENREP project.

This repository contains a simple Shiny app that displays GOES and Iridium telemetry data and an external source, cleans it, and displays two tables. One table for SedEvent Data and one for Met Station. The reason for this app is to try and make a single location for checking SedEvent and Met Station telmetry data compared to going to [FTS360](https://360.ftsinc.com/) and the [GOES portal](https://dcs1.noaa.gov) separately.

The incoming raw data is originally downloaded via the NOAA provided LRGS Client and currently setup as a cron job on a linux machine in the lab. Info and download for the client is available at [NOAA DCS Page](https://dcs1.noaa.gov) -\> System Information. The raw downloaded data is then synced to ownCloud and retrieved by the Shiny app via a shared file link. Some sample data of what is being pulled from ownCloud is provided in the sample_data folder.
