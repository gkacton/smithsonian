
# libraries ---------------------------------------------------------------

library(tidyverse)
library(rnaturalearthdata)
library(sf)
library(sfnetworks)
library(tidygraph)
library(DescTools)
library(plotly)
library(gganimate)
library(ggraph)

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
  mutate(is_any_seeger = str_detect(names, "Seeger")) %>% 
  mutate(is_black = str_detect(subjects, "African American"))

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


# the black web -----------------------------------------------------------

rinz_black <- rinz_gen %>% 
  filter(is_black == T)

black_time <- rinz_black %>% 
  filter(date != "" & is.na(date) == F) %>% 
  mutate(date = ifelse(date == "196x", 1960, date),
         date = ifelse(date == "195x", 1950, date),
         date = ifelse(date == "197x", 1970, date),
         date = ifelse(date == "198x", 1978, date),
         date = str_remove_all(date, "[^0-9]"),
         date = str_trunc(date, 4), 
         date = as.Date(ISOdate(date, 1, 1)))

black_collabs <- black_time %>% 
  separate_wider_delim(cols = names, 
                       delim = ",",
                       names_sep = ".",
                       names_repair = "unique",
                       too_few = "align_start") %>% 
  select(starts_with("names"), subjects, date)

black_combos <- as.data.frame(matrix(nrow=1, ncol=4))
colnames(black_combos) <- c("V1", "V2", "V3", "V4")

for(i in 1:nrow(black_collabs)){
  active_list <- black_collabs[i, 1:29]
  active_subjects <- black_collabs$subjects[i]
  active_date <- black_collabs$date[i]
  row_vec <- unique(discard(as.vector(active_list), is.na))
  if(length(row_vec) >= 2){
    active_df <- as.data.frame(CombSet(row_vec, 2, repl=F, ord=F))
    active_df <- active_df %>% 
      mutate(V3 = active_subjects) %>% 
      mutate(V4 = active_date) 
    black_combos <- rbind(black_combos, active_df)
  }
}



black_edges <- name_combos_2 %>% 
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
  select(-id) 

black_nodes <- all_nodes %>% 
  filter(id %in% black_edges$from | id %in% black_edges$to) %>% 
  mutate(is_ralph = str_detect(text_id, "Rinzler_Ralph")) 

black_net <- tbl_graph(nodes = black_nodes,
                       edges = black_edges,
                       directed = F,
                       node_key = "id") 

black_net_stats <- net_stats(black_net) 

black_graph <- ggraph(black_net, layout = "stress") +
  geom_node_point() +
  geom_edge_density(aes(color))

color_define <- function(df){
  for(i in 1:nrow(df)){
    if(df$is_folk[i] == T & df$is_country[i] == F & df$is_blues[i] == F & df$is_bluegrass == F){
      # FOLK ALONE
      df$color[i] <- "yellow"
    } 
    else if(df$is_folk[i] == T & df$is_country[i] == T & df$is_blues[i] == F){
      # FOLK AND COUNTRY
      df$color[i] <- "darkorange1"
    }
    else if(df$is_folk[i] == T & df$is_country[i] == F & df$is_blues[i] == T){
      # FOLK AND BLUES
      df$color[i] <- "darkslategray3"
    }
    else if(df$is_folk[i] == T & df$is_bluegrass[i] == T){
      # FOLK AND BLUEGRASS
      df$color[i] <- "darkolivegreen1"
    } 
    else if(df$is_folk[i] == T & df$is_blues[i] == T & df$is_country[i] == T){
      # FOLK AND COUNTRY AND BLUES
      df$color[i] <- "chocolate4"
    }
    else if(df$is_folk[i] == F & df$is_blues[i] == T & df$is_country[i] == T){
      # COUNTRY AND BLUES
      df$color[i] <- "blueviolet"
    }
    else if(df$is_folk[i] == F & df$is_blues[i] == T & df$is_country[i] == F){
      # BLUES ALONE
      df$color[i] <- "blue"
    }
    else if(df$is_folk[i] == F & df$is_blues[i] == F & df$is_country[i] == T){
      # COUNTRY ALONE
      df$color[i] <- "brown2"
    }
    else if(df$is_folk[i] == F & df$is_bluegrass[i] == T){
      # BLUEGRASS ALONE
      df$color[i] <- "darkgreen"
    }
    else if(df$is_jazz[i] == T){
      df$color[i] <- "deeppink"
    }
  }
} 

black_edges_colored <- color_define(black_edges)
df <- black_edges %>% 
  mutate(color = "")

df <- for(i in 1:nrow(df)){
  if(df$is_folk[i] == T & df$is_country[i] == F & df$is_blues[i] == F & df$is_bluegrass[i] == F){
    # FOLK ALONE
    df$color[i] <- "yellow"
  } 
  else if(df$is_folk[i] == T & df$is_country[i] == T & df$is_blues[i] == F){
    # FOLK AND COUNTRY
    df$color[i] <- "darkorange1"
  }
  else if(df$is_folk[i] == T & df$is_country[i] == F & df$is_blues[i] == T){
    # FOLK AND BLUES
    df$color[i] <- "darkslategray3"
  }
  else if(df$is_folk[i] == T & df$is_bluegrass[i] == T){
    # FOLK AND BLUEGRASS
    df$color[i] <- "darkolivegreen1"
  } 
  else if(df$is_folk[i] == T & df$is_blues[i] == T & df$is_country[i] == T){
    # FOLK AND COUNTRY AND BLUES
    df$color[i] <- "chocolate4"
  }
  else if(df$is_folk[i] == F & df$is_blues[i] == T & df$is_country[i] == T){
    # COUNTRY AND BLUES
    df$color[i] <- "blueviolet"
  }
  else if(df$is_folk[i] == F & df$is_blues[i] == T & df$is_country[i] == F){
    # BLUES ALONE
    df$color[i] <- "blue"
  }
  else if(df$is_folk[i] == F & df$is_blues[i] == F & df$is_country[i] == T){
    # COUNTRY ALONE
    df$color[i] <- "brown2"
  }
  else if(df$is_folk[i] == F & df$is_bluegrass[i] == T){
    # BLUEGRASS ALONE
    df$color[i] <- "darkgreen"
  }
  else if(df$is_jazz[i] == T){
    df$color[i] <- "deeppink"
  } else{
    df$color[i] <- NA
  }
}


black_edges_colored <- df %>% 
  filter(is.na(color) == F & color != "") %>% 
  select(-subjects, -starts_with("is")) %>% 
  distinct(from, to, color, .keep_all=T)

color_nodes <- all_nodes %>% 
  filter(id %in% black_edges_colored$from | id %in% black_edges_colored$to)


color_graph <- tbl_graph(nodes = color_nodes,
                         edges = black_edges_colored,
                         node_key = "id",
                         directed = F)

color_graph_vis <- ggraph(color_graph, layout = "stress") +
  geom_node_point() +
  geom_edge_link(aes(color = color)) +
  scale_edge_color_manual(
    values = c("yellow" = "yellow",
               "darkorange1"="darkorange1",
               "darkslategray3"="darkslategray3",
               "darkolivegreen1"="darkolivegreen1",
               "chocolate4"="chocolate4",
               "blueviolet"="blueviolet",
               "blue"="blue",
               "brown2"="brown2",
               "darkgreen"="darkgreen",
               "deeppink"="deeppink")
  )


# COLOR KEY
c("folk" = "yellow",
  "folk-country" = "darkorange1",
  "folk-blues" = "darkslategray3",
  "folk-bg" = "darkolivegreen1",
  "folk-country-blues" = "chocolate4",
  "country-blues" = "blueviolet",
  "blues" = "blue",
  "country" = "brown2",
  "bg" = "darkgreen",
  "jazz" = "deeppink",
  "folk-jazz" = "lightcoral",
  "country-bg" = "olivedrab",
  "country-jazz" = "coral4",
  "blues-jazz" = "darkmagenta",
  "blues-bg" = "darkcyan",
  "folk-country-blues" = "cadetblue4",
  "folk-country-jazz" = "chocolate2",
  "folk-country-bg" = "darkgoldenrod4",
  "folk-bg-blues" = "darkseagreen",
  "folk-bg-jazz" = "darkkhaki",
  "folk-blues-jazz" = "coral3",
  "country-blues-jazz" = "darkred",
  "country-bg-blues" = "darkslategray",
  "country-bg-jazz" = "brown3",
  "blues-bg-jazz" = "darkslateblue",
  "folk-country-blues-bg" = "cornsilk4")
  
