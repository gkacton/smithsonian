
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

uk <- c("England", "Scotland", "Wales", "Northern Ireland")

biggies <- space_time %>% 
  filter(is_ralph == T |
           is_al == T |
           is_molly == T |
           is_rk == T |
           is_doc == T |
           is_bill == T |
           is_ernest == T |
           is_any_seeger == T) %>% 
  separate_wider_delim(coordinates, delim = ",", names = c("lat", "lon")) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  filter(place != "") %>% 
  filter(place %in% countries$name == F &
           place %in% countries$subunit == F &
           place %in% uk == F)


## network of people

collabs_2 <- biggies %>% 
  separate_wider_delim(cols = names, 
                       delim = ",",
                       names_sep = ".",
                       names_repair = "unique",
                       too_few = "align_start") %>% 
  select(starts_with("names"), subjects, date)

name_combos_2 <- as.data.frame(matrix(nrow=1, ncol=4))
colnames(name_combos_2) <- c("V1", "V2", "V3", "V4")

for(i in 1:nrow(collabs_2)){
  active_list <- collabs_2[i, 1:41]
  active_subjects <- collabs_2$subjects[i]
  active_date <- collabs_2$date[i]
  row_vec <- unique(discard(as.vector(active_list), is.na))
  if(length(row_vec) >= 2){
    active_df <- as.data.frame(CombSet(row_vec, 2, repl=F, ord=F))
    active_df <- active_df %>% 
      mutate(V3 = active_subjects) %>% 
      mutate(V4 = active_date) 
    name_combos_2 <- rbind(name_combos_2, active_df)
  }
}



biggies_edges <- name_combos_2 %>% 
  mutate(from_label = as.character(V1),
         to_label = as.character(V2), 
         subjects = V3) %>% 
  select(-starts_with("V")) %>% 
  mutate(is_folk = str_detect(subjects, regex("folk", ignore_case = T))) %>% 
  mutate(is_bluegrass = str_detect(subjects, regex("bluegrass", ignore_case = T))) %>% 
  mutate(is_blues = str_detect(subjects, regex("blues", ignore_case = T))) %>% 
  mutate(is_country = str_detect(subjects, regex("country", ignore_case = T))) %>% 
  mutate(is_jazz = str_detect(subjects, regex("jazz", ignore_case = T))) %>% 
  na.omit() %>% 
  left_join(all_nodes, by = c("from_label" = "text_id")) %>% 
  mutate(from = as.character(id)) %>% 
  select(-id) %>% 
  left_join(all_nodes, by = c("to_label" = "text_id")) %>% 
  mutate(to = as.character(id)) %>% 
  select(-id) %>% 
  filter(is_folk == T)

biggies_nodes <- all_nodes %>% 
  filter(id %in% biggies_edges$from | id %in% biggies_edges$to) %>% 
  mutate(is_ralph = str_detect(text_id, "Rinzler_Ralph"))

biggies_graph <- tbl_graph(nodes = biggies_nodes,
                           edges = biggies_edges,
                           directed = FALSE,
                           node_key = "id")

biggy_igraph <- as.igraph(biggies_graph) 
biggy_simple <- simplify(biggy_igraph) 

biggy_graph_simple <- as_tbl_graph(biggy_simple)


graph <- ggraph(biggy_graph_simple, layout = "stress") +
  geom_edge_density(aes(fill = is_bluegrass)) +
  geom_node_point(aes(color = is_ralph)) 



# Doing the same thing with people to place connections -------------------

biggies_2 <- space_time %>% 
  filter(is_ralph == T |
           is_al == T |
           is_molly == T |
           is_rk == T |
           is_doc == T |
           is_bill == T |
           is_ernest == T |
           is_any_seeger == T) %>% 
  separate_wider_delim(coordinates, delim = ",", names = c("lat", "lon")) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  filter(place != "") %>% 
  filter(place %in% countries$name == F &
           place %in% countries$subunit == F &
           place %in% uk == F)

collabs_3 <- biggies_2 %>% 
  separate_longer_delim(cols = names,
                        delim = ",") %>% 
  mutate(from_label = place, 
         to_label = names) %>% 
  left_join(all_nodes, by = c("from_label" = "text_id")) %>% 
  mutate(from = as.character(id)) %>% 
  select(-id) %>% 
  left_join(all_nodes, by = c("to_label" = "text_id")) %>% 
  mutate(to = as.character(id)) %>% 
  select(-id) %>% 
  select(from, from_label,
         to, to_label,
         subjects,
         starts_with("is") )

people_latlon <- rinz %>% 
  filter(coordinates != "") %>% 
  separate_wider_delim(coordinates, delim = ",", names = c("lat", "lon")) %>% 
  mutate(names = str_replace_all(names, ",\\s", "_")) %>% 
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon)) %>% 
  separate_longer_delim(cols = names, 
                        delim = ",") %>% 
  distinct(ID, lat, lon, place, names) %>% 
  filter(names != "") %>% 
  group_by(names) %>% 
  summarize(avg_lat = mean(lat), 
            avg_lon = mean(lon), 
            count = n()) %>% 
  mutate(avg_lat = as.numeric(avg_lat),
         avg_lon = as.numeric(avg_lon)) %>% 
  st_as_sf(coords = c("avg_lon", "avg_lat"), crs = 4326)

places_latlon <- rinz %>% 
  filter(coordinates != "") %>% 
  separate_wider_delim(coordinates, delim = ",", names = c("lat", "lon")) %>% 
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon)) %>% 
  distinct(place, .keep_all = T) %>% 
  select(place, lat, lon, starts_with("is")) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

biggies_place_connect <- collabs_3 

biggies_place_nodes <- all_nodes %>% 
  filter(id %in% biggies_place_connect$from | id %in% biggies_place_connect$to) %>% 
  left_join(people_latlon, by = c("text_id" = "names")) %>% 
  left_join(places_latlon, by = c("text_id" = "place"))

biggies_geo_net <- sfnetwork(nodes = biggies_place_nodes,
                             edges = biggies_place_connect,
                             directed = F,
                            node_key = "id",
                            edges_as_lines = TRUE)


# layout SF function from Lore Abad

layout_sf = function(graph){
  # Extract X and Y coordinates from the nodes
  graph = activate(graph, "nodes")
  x = sf::st_coordinates(graph)[,"X"]
  y = sf::st_coordinates(graph)[,"Y"]
  data.frame(x, y)
}

basemap <- rnaturalearthdata::map_units110

geo_net <- ggraph(biggies_geo_net, layout = layout_sf) +
  geom_sf(data = basemap) +
  geom_node_point() +
  geom_edge_link(aes(color = is_bluegrass))
