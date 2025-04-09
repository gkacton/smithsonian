
# libraries ---------------------------------------------------------------

library(tidyverse)
library(rnaturalearthdata)
library(sf)
library(sfnetworks)
library(tidygraph)
library(DescTools)
library(plotly)
library(gganimate)


# data --------------------------------------------------------------------

genre_info <- read.csv("~/Desktop/smithsonian/data/genre_info.csv")
rinz <- read.csv("~/Desktop/smithsonian/data/rinzler-full-reconciled.csv")


# genre overlaps ----------------------------------------------------------

rinz_gen <- rinz %>% 
  mutate(names = str_replace_all(names, ",\\s", "_")) %>% 
  mutate(is_folk = str_detect(subjects, regex("folk", ignore_case = T))) %>% 
  mutate(is_bluegrass = str_detect(subjects, regex("bluegrass", ignore_case = T))) %>% 
  mutate(is_blues = str_detect(subjects, regex("blues", ignore_case = T))) %>% 
  mutate(is_country = str_detect(subjects, regex("country", ignore_case = T))) %>% 
  mutate(is_jazz = str_detect(subjects, regex("jazz", ignore_case = T))) %>% 
  # add important personnel 
  mutate(is_ralph = str_detect(names, "Rinzler_Ralph")) %>% 
  mutate(is_rk = str_detect(names, "Spottswood_Richard")) %>% 
  mutate(is_doc = str_detect(names, "Watson_Doc")) %>% 
  mutate(is_al = str_detect(names, "Lomax_Alan")) %>% 
  mutate(is_bill = str_detect(names, "Monroe_Bill")) %>% 
  mutate(is_molly = str_detect(names, "Jackson_Aunt")) %>% 
  mutate(is_ernest = str_detect(names, "Stoneman_Ernest")) %>% 
  mutate(is_any_seeger = str_detect(names, "Seeger"))

time <- rinz_gen %>% 
  filter(date != "" & is.na(date) == F) %>% 
  mutate(date = ifelse(date == "196x", 1960, date),
         date = ifelse(date == "195x", 1950, date),
         date = ifelse(date == "197x", 1970, date),
         date = ifelse(date == "198x", 1978, date),
         date = str_remove_all(date, "[^0-9]"),
         date = str_trunc(date, 4), 
         date = as.Date(ISOdate(date, 1, 1)))

space_time <- time %>% 
  filter(coordinates != "")

# filter to point data only
  
states <- rnaturalearthdata::states50 %>% 
filter(iso_a2 == "CA" | iso_a2 == "US") %>% 
  select(name, geometry, type_en)

space_time_points <- space_time %>% 
  left_join(states, by = c('place' = 'name')) %>% 
  filter(is.na(type_en) == T) %>% 
  left_join(countries, by = c('place' = 'name')) %>% 
  left_join(countries, by = c('place' = 'subunit')) %>% 
  filter(is.na(type.x) == T & is.na(type.y) == T) %>% 
  filter(place != "" & place != "Great Britain" & place != "Scotland" & place != "Wales" & place != "England" & place != "Northern Ireland" & place != "Appalachia") %>% 
  distinct(ID, place, .keep_all = T) %>% 
  select(-starts_with("type"), -starts_with("geometry"), -subunit, -name) %>% 
  separate_wider_delim(coordinates, delim = ",", names = c("lat", "lon")) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  filter(place != "")

# create plot

points_map <- ggplot(data = space_time_points)
points_map <- points_map + geom_sf(data = countries)
points_map <- points_map + geom_sf(aes(color = is_ralph)) + scale_color_manual(values = c("FALSE" = "brown", "TRUE" = "pink"))

points_anim <- points_map + transition_states(date, transition_length = 0, state_length = 1) + 
  shadow_mark()


# mapping the network over time -------------------------------------------


