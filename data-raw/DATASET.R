## code to prepare `DATASET` dataset goes here

# Libraries
library(tidyverse)
library(lubridate)
library(dbscan)
library(sf)


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
 # caps %>%
 #   mutate(wday = wday(timestamp)) %>%
 #   group_by(date(date)) %>%
 #   summarise(n = n(), wday = wday[1], var = sd(lon)) %>% arrange(-n) %>%
 #   View()

caps_tr <- caps %>%
  filter(date(date) == as_date("2021-03-05")) %>%
  arrange(timestamp) %>%
  mutate(min = str_c(str_pad(hour(timestamp), width = 2, pad = "0"),
                     str_pad(minute(timestamp), width = 2, pad = "0"))) %>%
  group_by(min) %>% slice_sample(n = 20) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4327) %>%
  st_transform(32612)


leaflet(caps_tr ) %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addCircleMarkers()
