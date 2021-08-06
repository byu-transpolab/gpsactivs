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



# CAPS DATA (CONFIDENTIAL) ==============
#
# The file is really a folder that contains the trace information for
# a single individual. Let's read all the CSV files in that folder
files_in_folder <- dir("data-raw/caps/", full.names = TRUE)
caps <- lapply(files_in_folder, function(x){
  readr::read_csv(x, col_types = list(userId = col_character())) %>%
    dplyr::transmute(
      id = userId,
      lat, lon,
      timestamp,
      date = lubridate::date(timestamp),   # Separate Date and Time columns
      hour = lubridate::hour(timestamp),
      minute = lubridate::minute(timestamp),
      second = lubridate::second(timestamp),
      time = hms::as_hms(str_c(hour, minute, second, sep = ":")),
    ) %>% select(-hour, -minute, -second)
}) %>%
  dplyr::bind_rows()

# find record with most data points
 caps %>%
   mutate(wday = wday(timestamp)) %>%
   group_by(date(date)) %>%
   summarise(n = n(), wday = wday[1], var = sd(lon)) %>% arrange(-n) %>%
   View()

caps_tr <- caps %>%
  filter(date(date) == as_date("2021-03-05")) %>%
  arrange(timestamp) %>%
  mutate(min = str_c(str_pad(hour(timestamp), width = 2, pad = "0"),
                     str_pad(minute(timestamp), width = 2, pad = "0"))) %>%
  group_by(min) %>% slice_sample(n = 10) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4327) %>%
  st_transform(32612)


leaflet(caps_tr ) %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addCircleMarkers()
