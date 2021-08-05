## code to prepare `DATASET` dataset goes here

# Libraries
library(tidyverse)
library(lubridate)
library(dbscan)
library(sf)


# READ TRAJECTORIES ==================
# Geolife trajectories https://www.microsoft.com/en-us/download/details.aspx?id=52367
records <- str_pad(sample(0:181, 5), width = 3, pad =  "0")

gps <- lapply(records, function(record){

  plt_files <- dir(file.path("data-raw/geolife", record,
                             "trajectory"), full.names = TRUE)

  lapply(plt_files, function(file){
    read_csv(file, skip = 6,
             col_names = c("lat", "lng", "x", "altitude", "dumbdate",
                           "date", "time"),
             col_types = list(
               date = col_date(),
               time = col_time()
             )) %>%
      select(-x)

  }) %>%
    set_names(plt_files) %>%
    bind_rows(.id = "file")

})  %>%
  set_names(records) %>%
  bind_rows(.id = "record")

# CLEAN TRAJECTORIES ==================
trajectories <- gps %>%
  # trajectories are in GMT; want to convert to local time
  mutate(
    gmt = as_datetime(str_c(date, time), tz = "GMT"),
    localtime = with_tz(gmt, tz = "Asia/Hong_Kong"),
    altitude = ifelse(altitude == -777, NA, altitude * 0.3048) # feet to meters
  ) %>%
  select(id = record, lat, lng, altitude, gmt, localtime) %>%

  # make spatial and project into UTM 50N (meters)
  st_as_sf(coords = c("lng", "lat"), crs = 4327) %>%
  st_transform(32650)


usethis::use_data(trajectories, overwrite = TRUE)


#' UCI DATA ============

uci <- read_csv("data-raw/ucsb/go_track_trackspoints.csv") %>%
  filter(longitude > -37.2) %>%
  filter(latitude < -10.8) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4327)

# leaflet(uci) %>%
#   addProviderTiles(providers$OpenStreetMap) %>%
#   addCircles()

usethis::use_data(uci, overwrite = TRUE)
