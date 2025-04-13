
# load libraries ----------------------------------------------------------

library(tidyverse)
library(rnaturalearthdata)
library(sf)
library(sfnetworks)
library(tidygraph)
library(DescTools)
library(plotly)
library(gganimate)
library(ggraph)


# load data ---------------------------------------------------------------

rinz <- read.csv("~/Desktop/smithsonian/data/rinzler-full-reconciled.csv")


# calculate avg location for each person ----------------------------------

ppl_places <- rinz %>% 
  filter(coordinates != "") %>% 
  separate_wider_delim(coordinates, delim = ",", names = c("lat", "lon")) %>% 
  mutate(lat = as.double(lat),
         lon = as.double(lon)) %>% 
  mutate(names = str_replace_all(names, ",\\s", "_")) %>% 
  separate_longer_delim(cols = names, 
                        delim = ",") %>% 
  filter(names != "") %>% 
  group_by(names) %>% 
  summarize(avg_lat = mean(lat), 
            avg_lon = mean(lon), 
            count = n()) %>% 
  mutate(avg_lat = as.numeric(avg_lat),
         avg_lon = as.numeric(avg_lon)) %>% 
  st_as_sf(coords = c("avg_lon", "avg_lat"), crs = 4326) %>% 
  mutate(id = row_number())


# Functions ---------------------------------------------------------------


# Function to generate table of network stats

net_stats <- function(net){
  net_stats <- net %>% 
    activate(nodes) %>% 
    mutate(degree = centrality_degree(),
           between = centrality_betweenness(),
           eigen = centrality_eigen()) %>% 
    arrange(desc(degree))
  
  return(as.data.frame(net_stats))
}


# function to assign genre keys based on booleans

assign_color <- function(df){
  for(i in 1:nrow(df)){
    if(df$is_folk[i] == T & df$is_country[i] == T & df$is_blues[i] == F & df$is_bluegrass[i] == F & df$is_jazz[i] == F){
      # FOLK AND COUNTRY
      df$color_key[i] <- "folk-country"
    }
    else if(df$is_folk[i] == T & df$is_country[i] == F & df$is_blues[i] == T & df$is_bluegrass[i] == F & df$is_jazz[i] == F){
      # FOLK AND BLUES
      df$color_key[i] <- "folk-blues"
    }
    else if(df$is_folk[i] == T & df$is_country[i] == F & df$is_blues[i] == F & df$is_bluegrass[i] == T & df$is_jazz[i] == F){
      # FOLK AND BLUEGRASS
      df$color_key[i] <- "folk-bg"
    } 
    else if(df$is_folk[i] == T & df$is_country[i] == F & df$is_blues[i] == F & df$is_bluegrass[i] == F & df$is_jazz[i] == T){
      # FOLK AND JAZZ
      df$color_key[i] <- "folk-jazz"
    }
    else if(df$is_folk[i] == F & df$is_country[i] == T & df$is_blues[i] == T & df$is_bluegrass[i] == F & df$is_jazz[i] == F){
      # COUNTRY AND BLUES
      df$color_key[i] <- "country-blues"
    }
    else if(df$is_folk[i] == F & df$is_country[i] == T & df$is_blues[i] == F & df$is_bluegrass[i] == T & df$is_jazz[i] == F){
      # COUNTRY AND BLUEGRASS
      df$color_key[i] <- "country-bg"
    }
    else if(df$is_folk[i] == F & df$is_country[i] == T & df$is_blues[i] == F & df$is_bluegrass[i] == F & df$is_jazz[i] == T){
      # COUNTRY AND JAZZ
      df$color_key[i] <- "country-jazz"
    }
    else if(df$is_folk[i] == F & df$is_country[i] == F & df$is_blues[i] == T & df$is_bluegrass[i] == F & df$is_jazz[i] == T){
      # BLUES AND JAZZ
      df$color_key[i] <- "blues-jazz"
    }
    else if(df$is_folk[i] == F & df$is_country[i] == F & df$is_blues[i] == T & df$is_bluegrass[i] == T & df$is_jazz[i] == F){
      # BLUES AND BLUEGRASS
      df$color_key[i] <- "blues-bg"
    }
    else if(df$is_folk[i] == F & df$is_country[i] == F & df$is_blues[i] == F & df$is_bluegrass[i] == T & df$is_jazz[i] == T){
      # BLUEGRASS AND JAZZ
      df$color_key[i] <- "bg-jazz"
    }
    else if(df$is_folk[i] == T & df$is_country[i] == T & df$is_blues[i] == T & df$is_bluegrass[i] == F & df$is_jazz[i] == F){
      # FOLK AND COUNTRY AND BLUES
      df$color_key[i] <- "folk-country-blues"
    }
    else if(df$is_folk[i] == T & df$is_country[i] == T & df$is_blues[i] == F & df$is_bluegrass[i] == F & df$is_jazz[i] == T){
      # FOLK AND COUNTRY AND JAZZ
      df$color_key[i] <- "folk-country-jazz"
    }
    else if(df$is_folk[i] == T & df$is_country[i] == T & df$is_blues[i] == F & df$is_bluegrass[i] == T & df$is_jazz[i] == F){
      # FOLK AND COUNTRY AND BLUEGRASS
      df$color_key[i] <- "folk-country-bg"
    }
    else if(df$is_folk[i] == T & df$is_country[i] == F & df$is_blues[i] == T & df$is_bluegrass[i] == T & df$is_jazz[i] == F){
      # FOLK AND BLUEGRASS AND BLUES
      df$color_key[i] <- "folk-bg-blues"
    }
    else if(df$is_folk[i] == T & df$is_country[i] == F & df$is_blues[i] == F & df$is_bluegrass[i] == T & df$is_jazz[i] == T){
      # FOLK AND BLUEGRASS AND JAZZ
      df$color_key[i] <- "folk-bg-jazz"
    }
    else if(df$is_folk[i] == T & df$is_country[i] == F & df$is_blues[i] == T & df$is_bluegrass[i] == F & df$is_jazz[i] == T){
      # FOLK AND BLUES AND JAZZ
      df$color_key[i] <- "folk-blues-jazz"
    }
    else if(df$is_folk[i] == F & df$is_country[i] == T & df$is_blues[i] == T & df$is_bluegrass[i] == F & df$is_jazz[i] == T){
      # COUNTRY AND BLUES AND JAZZ
      df$color_key[i] <- "country-blues-jazz"
    }
    else if(df$is_folk[i] == F & df$is_country[i] == T & df$is_blues[i] == T & df$is_bluegrass[i] == T & df$is_jazz[i] == F){
      # COUNTRY AND BLUEGRASS AND BLUES
      df$color_key[i] <- "country-bg-blues"
    }
    else if(df$is_folk[i] == F & df$is_country[i] == T & df$is_blues[i] == F & df$is_bluegrass[i] == T & df$is_jazz[i] == T){
      # COUNTRY AND BLUEGRASS AND JAZZ
      df$color_key[i] <- "country-bg-jazz"
    } 
    else if(df$is_folk[i] == F & df$is_country[i] == F & df$is_blues[i] == T & df$is_bluegrass[i] == T & df$is_jazz[i] == T){
      # BLUES AND BLUEGRASS AND JAZZ
      df$color_key[i] <- "blues-bg-jazz"
    } 
    else if(df$is_folk[i] == T & df$is_country[i] == T & df$is_blues[i] == T & df$is_bluegrass[i] == T & df$is_jazz[i] == F){
      #FOLK AND COUNTRY AND BLUES AND BLUEGRASS
      df$color_key[i] <- "folk-country-blues-bg"
    } 
    else if(df$is_folk[i] == F & df$is_country[i] == F & df$is_blues[i] == T & df$is_bluegrass[i] == F & df$is_jazz[i] == F){
      # BLUES ALONE
      df$color_key[i] <- "blues"
    }
    else if(df$is_folk[i] == F & df$is_country[i] == T & df$is_blues[i] == F & df$is_bluegrass[i] == F & df$is_jazz[i] == F){
      # COUNTRY ALONE
      df$color_key[i] <- "country"
    }
    else if(df$is_folk[i] == F & df$is_country[i] == F & df$is_blues[i] == F & df$is_bluegrass[i] == T & df$is_jazz[i] == F){
      # BLUEGRASS ALONE
      df$color_key[i] <- "bg"
    }
    else if(df$is_folk[i] == F & df$is_country[i] == F & df$is_blues[i] == F & df$is_bluegrass[i] == F & df$is_jazz[i] == T){
      # JAZZ ALONE
      df$color_key[i] <- "jazz"
    } 
    else if(df$is_folk[i] == T & df$is_country[i] == F & df$is_blues[i] == F & df$is_bluegrass[i] == F & df$is_jazz[i] == F){
      # FOLK ALONE
      df$color_key[i] <- "folk"
    } 
    else{
      df$color_key[i] <- NA
    }
  }
  return(df)
}

# find all name combinations ----------------------------------------------

collabs <- rinz %>% 
  mutate(names = str_replace_all(names, ",\\s", "_")) %>% 
  separate_wider_delim(cols = names, 
                       delim = ",",
                       names_sep = ".",
                       names_repair = "unique",
                       too_few = "align_start") %>% 
  select(starts_with("names"), subjects)

name_combos <- as.data.frame(matrix(nrow=1, ncol=3))
colnames(name_combos) <- c("V1", "V2", "V3")

for(i in 1:nrow(collabs)){
  active_list <- collabs[i, 1:41]
  active_subjects <- collabs$subjects[i]
  row_vec <- unique(discard(as.vector(active_list), is.na))
  if(length(row_vec) >= 2){
    active_df <- as.data.frame(CombSet(row_vec, 2, repl=F, ord=F))
    active_df <- active_df %>% 
      mutate(V3 = active_subjects)
    name_combos <- rbind(name_combos, active_df)
  }
}

name_combos <- name_combos %>% 
  mutate(from = V1,
         to = V2,
         subjects = V3) %>% 
  select(-starts_with("V")) %>% 
  mutate(is_folk = str_detect(subjects, regex("folk", ignore_case = T))) %>% 
  mutate(is_bluegrass = str_detect(subjects, regex("bluegrass", ignore_case = T))) %>% 
  mutate(is_blues = str_detect(subjects, regex("blues", ignore_case = T))) %>% 
  mutate(is_country = str_detect(subjects, regex("country", ignore_case = T))) %>% 
  mutate(is_jazz = str_detect(subjects, regex("jazz", ignore_case = T)))


# assign color keys ------------------------------------------------------


df <- name_combos %>% 
  filter(is.na(subjects) == F)

edges_colored <- assign_color(df) %>% 
  filter(is.na(color_key) == F & color_key != "") %>% 
  select(-subjects, -starts_with("is")) %>% 
  distinct(from, to, color_key, .keep_all=T)


# join edge data with node data -------------------------------------------

nodes <- ppl_places 

edges <- edges_colored %>% 
  mutate(from = as.character(from),
         to = as.character(to)) %>% 
  left_join(nodes, by = c("from" = "names")) %>% 
  mutate(from_label = from,
         from = as.character(id)) %>% 
  select(-id) %>% 
  left_join(nodes, by = c("to" = "names")) %>% 
  mutate(to_label = to,
         to = as.character(id)) %>% 
  select(from, from_label,
         to, to_label,
         color_key)

nodes <- nodes %>% 
  filter(id %in% edges$from | id %in% edges$to) %>% 
  mutate(id = as.character(id))

edges <- edges %>% 
  filter(from %in% nodes$id & to %in% nodes$id)

# get layout_sf from lore abad --------------------------------------------

layout_sf = function(graph){
  # Extract X and Y coordinates from the nodes
  graph = activate(graph, "nodes")
  x = sf::st_coordinates(graph)[,"X"]
  y = sf::st_coordinates(graph)[,"Y"]
  data.frame(x, y)
}

# get basemap data ------------------------------------------------------

basemap  <- rnaturalearthdata::map_units110
states_map <- rnaturalearthdata::states50


# do the same thing for people-place connections --------------------------

connections <- rinz %>% 
  mutate(names = str_replace_all(names, ",\\s", "_")) %>% 
  separate_longer_delim(cols = names, 
                       delim = ",") %>% 
  select(place, names, subjects) %>% 
  filter(place != "") %>% 
  mutate(is_folk = str_detect(subjects, regex("folk", ignore_case = T))) %>% 
  mutate(is_bluegrass = str_detect(subjects, regex("bluegrass", ignore_case = T))) %>% 
  mutate(is_blues = str_detect(subjects, regex("blues", ignore_case = T))) %>% 
  mutate(is_country = str_detect(subjects, regex("country", ignore_case = T))) %>% 
  mutate(is_jazz = str_detect(subjects, regex("jazz", ignore_case = T))) %>% 
  mutate(from = place,
         to = names)

df <- connections %>% 
  mutate(color_key = "")

connections_colored <- assign_color(df) %>% 
  filter(is.na(color_key) == F & color_key != "") %>% 
  select(-subjects, -starts_with("is"), -place, -names) %>% 
  distinct(from, to, color_key, .keep_all=T) 

place_nodes <- rinz %>% 
  filter(coordinates != "") %>% 
  separate_wider_delim(coordinates, delim = ",", names = c("lat", "lon")) %>% 
  mutate(lat = as.double(lat),
         lon = as.double(lon)) %>% 
  mutate(text_id = place) %>% 
  distinct(text_id, lat, lon) %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = 4326)

all_nodes <- ppl_places %>% 
  mutate(text_id = names) %>% 
  select(-names, -count, -id) %>% 
  rbind(place_nodes) %>% 
  mutate(id = row_number()) %>% 
  filter(text_id != "")

place_ppl_edges <- connections_colored %>% 
  left_join(all_nodes, by = c('from' = 'text_id')) %>% 
  mutate(from_label = from,
         from = id) %>% 
  select(-id) %>% 
  left_join(all_nodes, by = c('to' = "text_id")) %>% 
  mutate(to_label = to,
         to = id) %>% 
  select(from, from_label,
         to, to_label,
         color_key)

# full_net_edges <- rbind(edges, place_ppl_edges) %>% 
#   select(-to, -from) %>% 
#   left_join(all_nodes, by = c('from_label' = 'text_id')) %>% 
#   select(-id) %>% 
#   left_join(all_nodes, by = c('to_label' = "text_id")) %>% 
#   select(from_label, to_label, color_key) %>% 
#   filter(is.na(color_key) == F)
# 
# full_net_nodes <- all_nodes %>% 
#   filter(text_id %in% full_net_edges$from_label | text_id %in% full_net_edges$to_label) %>% 
#   select(-id) %>% 
#   mutate(id = as.character(row_number()))
# 
# full_net_edges <- full_net_edges %>% 
#   filter(from_label %in% full_net_nodes$text_id & to_label %in% full_net_nodes$text_id) %>% 
#   left_join(full_net_nodes, by = c('from_label' = 'text_id')) %>% 
#   mutate(from = id) %>% 
#   select(-id) %>% 
#   left_join(full_net_nodes, by = c('to_label' = 'text_id')) %>% 
#   mutate(to = id) %>% 
#   select(-id) %>% 
#   select(from, from_label,
#          to, to_label,
#          color_key)

# full_net_edges_multiple <- full_net_edges %>% 
#   rbind(full_net_edges) %>% 
#   rbind(full_net_edges) %>% 
#   rbind(full_net_edges) %>% 
#   rbind(full_net_edges) %>% 
#   rbind(full_net_edges) %>% 
#   mutate(to = as.character(to),
#          from = as.character(from))
# 
# full_net_nodes_multiple <- all_nodes %>% 
#   filter(id %in% full_net_edges_multiple$from | id %in% full_net_edges_multiple$to) %>% 
#   mutate(id = as.character(id))
# 
# full_net_edges_multiple <- full_net_edges_multiple %>% 
#   filter(from %in% full_net_nodes_multiple$id & to %in% full_net_nodes_multiple$id)


# do the same thing for place-place connections ---------------------------

rinz_places <- rinz %>% 
  select(ID, place, subjects) %>% 
  group_by(ID) %>% 
  summarise(places = paste0(place, collapse = ","),
            subjects = paste0(unique(subjects), collapse=",")) %>% 
  separate_wider_delim(places,
                       delim = ",",
                       names_sep = ".",
                       names_repair = "unique",
                       too_few = "align_start")


place_combos <- as.data.frame(matrix(nrow=1, ncol=3))
colnames(place_combos) <- c("V1", "V2", "V3")

for(i in 1:nrow(rinz_places)){
  active_list <- rinz_places[i, 2:93]
  active_subjects <- rinz_places$subjects[i]
  row_vec <- unique(discard(as.vector(active_list), is.na))
  if(length(row_vec) >= 2){
    active_df <- as.data.frame(CombSet(row_vec, 2, repl=F, ord=F))
    active_df <- active_df %>% 
      mutate(V3 = active_subjects)
   place_combos <- rbind(place_combos, active_df)
  }
}

place_edges <- place_combos %>% 
  filter(is.na(V3) == F) %>% 
  mutate(from = V1,
         to = V2,
         subjects = V3) %>% 
  select(-starts_with("V")) %>% 
  mutate(is_folk = str_detect(subjects, regex("folk", ignore_case = T))) %>% 
  mutate(is_bluegrass = str_detect(subjects, regex("bluegrass", ignore_case = T))) %>% 
  mutate(is_blues = str_detect(subjects, regex("blues", ignore_case = T))) %>% 
  mutate(is_country = str_detect(subjects, regex("country", ignore_case = T))) %>% 
  mutate(is_jazz = str_detect(subjects, regex("jazz", ignore_case = T))) 

place_edges_colored <- assign_color(place_edges)

place_edges_colored <- place_edges_colored %>% 
  filter(is.na(color_key) == F & color_key != "") %>% 
  select(-subjects, -starts_with("is")) %>% 
  distinct(from, to, color_key, .keep_all=T) %>% 
  mutate(from = as.character(from),
         to = as.character(to))

# join node ids

# place_edges_colored <- place_edges_colored %>% 
#   left_join(all_nodes, by = c('from' = 'text_id')) %>% 
#   mutate(from_label = from,
#          from = as.character(id)) %>% 
#   select(-id) %>% 
#   left_join(all_nodes, by = c('to' = "text_id")) %>% 
#   mutate(to_label = to,
#          to = as.character(id)) %>% 
#   select(from, from_label,
#          to, to_label,
#          color_key)
# 
# place_nodes_final <- all_nodes %>% 
#   filter(id %in% place_edges_colored$from | id %in% place_edges_colored$to) %>% 
#   mutate(id = as.character(id))
# 
# place_edges_final <- place_edges_colored %>% 
#   filter(from %in% place_nodes$id & to %in% place_nodes$id)
# 
# place_net <- sfnetwork(nodes = place_nodes_final,
#                        edges = place_edges_final,
#                        directed = F,
#                        node_key = "id")


# create graph of all connections -----------------------------------------

all_nodes <- ppl_places %>% 
  mutate(text_id = names) %>% 
  select(-names, -count, -id) %>% 
  rbind(place_nodes) %>% 
  mutate(id = row_number()) %>% 
  filter(text_id != "")

place_ppl_edges <- connections_colored %>% 
  left_join(all_nodes, by = c('from' = 'text_id')) %>% 
  mutate(from_label = from,
         from = id) %>% 
  select(-id) %>% 
  left_join(all_nodes, by = c('to' = "text_id")) %>% 
  mutate(to_label = to,
         to = id) %>% 
  select(from, from_label,
         to, to_label,
         color_key)

full_net_edges <- rbind(edges, place_ppl_edges) %>%
  select(-to, -from) %>%
  left_join(all_nodes, by = c('from_label' = 'text_id')) %>%
  select(-id) %>%
  left_join(all_nodes, by = c('to_label' = "text_id")) %>%
  select(from_label, to_label, color_key) %>%
  filter(is.na(color_key) == F)

full_net_nodes <- all_nodes %>%
  filter(text_id %in% full_net_edges$from_label | text_id %in% full_net_edges$to_label) %>%
  select(-id) %>%
  mutate(id = as.character(row_number())) # don't generate IDs until the list is filtered --> no skipped numbers!

full_net_edges <- full_net_edges %>%
  filter(from_label %in% full_net_nodes$text_id & to_label %in% full_net_nodes$text_id) %>%
  left_join(full_net_nodes, by = c('from_label' = 'text_id')) %>%
  mutate(from = id) %>%
  select(-id) %>%
  left_join(full_net_nodes, by = c('to_label' = 'text_id')) %>%
  mutate(to = id) %>%
  select(-id) %>%
  select(from, from_label,
         to, to_label,
         color_key)

# generate graph ----------------------------------------------------------

full_color_net <- sfnetwork(nodes = full_net_nodes,
                            edges = full_net_edges,
                            directed = T,
                            node_key = "id")

colors_MASTER <-  c("folk" = "yellow",
                    "folk-country" = "darkorange1",
                    "folk-blues" = "darkslategray3",
                    "folk-bg" = "darkolivegreen1",
                    "folk-country-blues" = "chocolate4",
                    "country-blues" = "blueviolet",
                    "blues" = "blue",
                    "country" = "red2",
                    "bg" = "darkgreen",
                    "jazz" = "deeppink",
                    "folk-jazz" = "lightcoral",
                    "country-bg" = "olivedrab",
                    "country-jazz" = "coral4",
                    "blues-jazz" = "darkmagenta",
                    "blues-bg" = "darkcyan",
                    "folk-country-blues" = "cadetblue4",
                    "folk-country-jazz" = "chocolate2",
                    "folk-country-bg" = "chartreuse",
                    "folk-bg-blues" = "darkseagreen",
                    "folk-bg-jazz" = "darkkhaki",
                    "folk-blues-jazz" = "coral3",
                    "country-blues-jazz" = "darkred",
                    "country-bg-blues" = "darkslategray",
                    "country-bg-jazz" = "brown3",
                    "blues-bg-jazz" = "darkslateblue",
                    "folk-country-blues-bg" = "cornsilk4")

# create places-only map --------------------------------------------------

place_net <- full_color_net %>% 
  activate(nodes) %>% 
  filter(as.numeric(id) > 2234)

# create map without "united states" as a node ----------------------------

full_color_net <- full_color_net %>% 
  activate(nodes) %>% 
  filter(text_id != "United States")
  
# plot --------------------------------------------------------------------

net <- ggraph(full_color_net, layout = layout_sf) +
  geom_sf(data = states_map, fill = "white") +
  geom_edge_density(aes(fill = color_key),
                    n = 250) +
  scale_edge_fill_manual(values =
                           c("folk" = "yellow",
                             "folk-country" = "darkorange1",
                             "folk-blues" = "darkslategray3",
                             "folk-bg" = "darkolivegreen1",
                             "folk-country-blues" = "chocolate4",
                             "country-blues" = "blueviolet",
                             "blues" = "blue",
                             "country" = "red2",
                             "bg" = "darkgreen",
                             "jazz" = "deeppink",
                             "folk-jazz" = "lightcoral",
                             "country-bg" = "olivedrab",
                             "country-jazz" = "coral4",
                             "blues-jazz" = "darkmagenta",
                             "blues-bg" = "darkcyan",
                             "folk-country-blues" = "cadetblue4",
                             "folk-country-jazz" = "chocolate2",
                             "folk-country-bg" = "chartreuse",
                             "folk-bg-blues" = "darkseagreen",
                             "folk-bg-jazz" = "darkkhaki",
                             "folk-blues-jazz" = "coral3",
                             "country-blues-jazz" = "darkred",
                             "country-bg-blues" = "darkslategray",
                             "country-bg-jazz" = "brown3",
                             "blues-bg-jazz" = "darkslateblue",
                             "folk-country-blues-bg" = "cornsilk4")
  ) +
  coord_sf(xlim = c(-130, -60), ylim = c(25,55), expand = FALSE)

bg_graph <- ggraph(bg_net, layout = layout_sf) +
  geom_sf(data = states_map, fill = "white") +
  geom_edge_density(aes(fill = color_key),
                    n = 500) +
  scale_edge_fill_manual(values =
                           c("folk-bg" = "darkolivegreen1",
                             "bg" = "darkgreen",
                             "country-bg" = "darkgoldenrod1",
                             "blues-bg" = "deepskyblue3",
                             "folk-country-bg" = "chartreuse",
                             "folk-bg-blues" = "darkseagreen",
                             "folk-bg-jazz" = "darkred",
                             "country-bg-blues" = "darkslategray",
                             "country-bg-jazz" = "brown3",
                             "blues-bg-jazz" = "darkslateblue",
                             "folk-country-blues-bg" = "black")
  ) +
  coord_sf(xlim = c(-130, -60), ylim = c(25,55), expand = FALSE)

folk_graph <- ggraph(folk_net, layout = layout_sf) +
  geom_sf(data = states_map, fill = "white") +
  geom_edge_density(aes(fill = color_key),
                    n = 100) +
  scale_edge_fill_manual(values =
                           c("folk" = "yellow",
                             "folk-country" = "darkorange1",
                             "folk-blues" = "darkslategray3",
                             "folk-bg" = "darkolivegreen1",
                             "folk-country-blues" = "chocolate4",
                             "folk-jazz" = "magenta",
                             "folk-country-blues" = "darkblue",
                             "folk-country-jazz" = "chocolate2",
                             "folk-country-bg" = "darkgreen",
                             "folk-bg-blues" = "darkseagreen1",
                             "folk-bg-jazz" = "purple",
                             "folk-country-blues-bg" = "black")
  ) +
  coord_sf(xlim = c(-130, -60), ylim = c(25,55), expand = FALSE)

country_graph <- ggraph(country_net, layout = layout_sf) +
  geom_sf(data = states_map, fill = "white") +
  geom_edge_density(aes(fill = color_key),
                    n = 500) +
  scale_edge_fill_manual(values =
                           c("folk-country" = "firebrick1",
                             "country-blues" = "blueviolet",
                             "country" = "red3",
                             "country-bg" = "olivedrab",
                             "folk-country-blues" = "cadetblue4",
                             "folk-country-bg" = "chartreuse",
                             "folk-country-blues-bg" = "cornsilk4")
  ) +
  coord_sf(xlim = c(-130, -60), ylim = c(25,55), expand = FALSE)

blues_graph <- ggraph(blues_net, layout = layout_sf) +
  geom_sf(data = states_map, fill = "white") +
  geom_edge_density(aes(fill = color_key),
                    n = 500) +
  scale_edge_fill_manual(values =
                           c("folk-blues" = "darkslategray3",
                               "folk-country-blues" = "chocolate4",
                               "country-blues" = "blueviolet",
                               "blues" = "blue",
                               "blues-jazz" = "magenta",
                               "blues-bg" = "darkgreen",
                               "folk-country-blues" = "cadetblue4",
                               "folk-bg-blues" = "chartreuse",
                               "folk-country-blues-bg" = "cornsilk4")
  ) +
  coord_sf(xlim = c(-130, -60), ylim = c(25,55), expand = FALSE)

jazz_graph <- ggraph(jazz_net, layout = layout_sf) +
  geom_sf(data = states_map, fill = "white") +
  geom_edge_density(aes(fill = color_key),
                    n=500) +
  scale_edge_fill_manual(values =
                           c(
                             "jazz" = "deeppink",
                             "folk-jazz" = "lightcoral",
                             "country-jazz" = "coral4",
                             "blues-jazz" = "darkmagenta",
                             "folk-country-jazz" = "chocolate2",
                             "folk-bg-jazz" = "darkkhaki",
                             "folk-blues-jazz" = "coral3",
                             "country-blues-jazz" = "darkred",
                             "country-bg-jazz" = "brown3",
                             "blues-bg-jazz" = "darkslateblue")
  ) +
  coord_sf(xlim = c(-130, -60), ylim = c(25,55), expand = FALSE)
# density experiment

color_net_multiple <- sfnetwork(nodes = full_net_nodes_multiple,
                              edges = full_net_edges_multiple,
                              directed = F,
                              node_key = "id")

net <- ggraph(color_net_multiple, layout = layout_sf) +
  geom_sf(data = states_map, fill = "white") +
  geom_edge_density(aes(fill = color_key)) +
  scale_edge_fill_manual(values =
                           c("folk" = "yellow",
                             "folk-country" = "darkorange1",
                             "folk-blues" = "darkslategray3",
                             "folk-bg" = "darkolivegreen2",
                             "folk-country-blues" = "chocolate4",
                             "country-blues" = "blueviolet",
                             "blues" = "blue",
                             "country" = "red2",
                             "bg" = "darkgreen",
                             "jazz" = "pink",
                             "folk-jazz" = "lightcoral",
                             "country-bg" = "olivedrab",
                             "country-jazz" = "coral4",
                             "blues-jazz" = "darkmagenta",
                             "blues-bg" = "darkcyan",
                             "folk-country-jazz" = "chocolate2",
                             "folk-country-bg" = "darkolivegreen3",
                             "folk-bg-blues" = "cadetblue4",
                             "folk-bg-jazz" = "tan",
                             "blues-bg-jazz" = "darkslateblue",
                             "folk-country-blues-bg" = "black")
  ) +
  coord_sf(xlim = c(-100, -60), ylim = c(25,55), expand = FALSE)

place_graph <- ggraph(place_net, layout = layout_sf) +
  geom_sf(data = states_map, fill = "white") +
  geom_edge_density(aes(fill = color_key),
                    n = 1000) +
  scale_edge_fill_manual(values =
                           c("folk" = "yellow",
                             "folk-country" = "darkorange1",
                             "folk-blues" = "darkslategray3",
                             "folk-bg" = "darkolivegreen1",
                             "folk-country-blues" = "chocolate4",
                             "country-blues" = "blueviolet",
                             "blues" = "blue",
                             "country" = "red2",
                             "bg" = "darkgreen",
                             "jazz" = "deeppink",
                             "folk-jazz" = "lightcoral",
                             "country-bg" = "olivedrab",
                             "country-jazz" = "coral4",
                             "blues-jazz" = "darkmagenta",
                             "blues-bg" = "darkcyan",
                             "folk-country-blues" = "cadetblue4",
                             "folk-country-jazz" = "chocolate2",
                             "folk-country-bg" = "chartreuse",
                             "folk-bg-blues" = "darkseagreen",
                             "folk-bg-jazz" = "darkkhaki",
                             "folk-blues-jazz" = "coral3",
                             "country-blues-jazz" = "darkred",
                             "country-bg-blues" = "darkslategray",
                             "country-bg-jazz" = "brown3",
                             "blues-bg-jazz" = "darkslateblue",
                             "folk-country-blues-bg" = "cornsilk4")
  ) +
  coord_sf(xlim = c(-130, -60), ylim = c(25,55), expand = FALSE)

place_graph_lines <- ggraph(place_net, layout = layout_sf) +
  geom_sf(data = states_map, fill = "white") +
  geom_edge_link(aes(color = color_key),
                 width = 2,
                 alpha = 0.01) +
  scale_edge_color_manual(values =
                           c("folk" = "yellow",
                             "folk-country" = "darkorange1",
                             "folk-blues" = "darkslategray3",
                             "folk-bg" = "darkolivegreen1",
                             "folk-country-blues" = "chocolate4",
                             "country-blues" = "blueviolet",
                             "blues" = "blue",
                             "country" = "red2",
                             "bg" = "darkgreen",
                             "jazz" = "deeppink",
                             "folk-jazz" = "lightcoral",
                             "country-bg" = "olivedrab",
                             "country-jazz" = "coral4",
                             "blues-jazz" = "darkmagenta",
                             "blues-bg" = "darkcyan",
                             "folk-country-blues" = "cadetblue4",
                             "folk-country-jazz" = "chocolate2",
                             "folk-country-bg" = "chartreuse",
                             "folk-bg-blues" = "darkseagreen",
                             "folk-bg-jazz" = "darkkhaki",
                             "folk-blues-jazz" = "coral3",
                             "country-blues-jazz" = "darkred",
                             "country-bg-blues" = "darkslategray",
                             "country-bg-jazz" = "brown3",
                             "blues-bg-jazz" = "darkslateblue",
                             "folk-country-blues-bg" = "cornsilk4")
  ) +
  coord_sf(xlim = c(-130, -60), ylim = c(25,55), expand = FALSE)

graph_noUSA <- ggraph(net_noUSA, layout = layout_sf) +
  geom_sf(data = states_map, fill = "white") +
  geom_edge_density(aes(fill = color_key)) +
  scale_edge_fill_manual(values =
                            c("folk" = "yellow",
                              "folk-country" = "darkorange1",
                              "folk-blues" = "darkslategray3",
                              "folk-bg" = "darkolivegreen1",
                              "folk-country-blues" = "chocolate4",
                              "country-blues" = "blueviolet",
                              "blues" = "blue",
                              "country" = "red2",
                              "bg" = "darkgreen",
                              "jazz" = "deeppink",
                              "folk-jazz" = "lightcoral",
                              "country-bg" = "olivedrab",
                              "country-jazz" = "coral4",
                              "blues-jazz" = "darkmagenta",
                              "blues-bg" = "darkcyan",
                              "folk-country-blues" = "cadetblue4",
                              "folk-country-jazz" = "chocolate2",
                              "folk-country-bg" = "chartreuse",
                              "folk-bg-blues" = "darkseagreen",
                              "folk-bg-jazz" = "darkkhaki",
                              "folk-blues-jazz" = "coral3",
                              "country-blues-jazz" = "darkred",
                              "country-bg-blues" = "darkslategray",
                              "country-bg-jazz" = "brown3",
                              "blues-bg-jazz" = "darkslateblue",
                              "folk-country-blues-bg" = "cornsilk4")
  ) +
  coord_sf(xlim = c(-130, -60), ylim = c(25,55), expand = FALSE)

# central figures as needed -----------------------------------------------

crossover_net <- full_color_net %>% 
  activate(edges) %>% 
  filter(color_key == "folk-country-blues-bg") %>% 
  activate(nodes) %>% 
  filter(centrality_degree() > 0)

crossover_stats <- net_stats(crossover_net)

blues_country <- full_color_net %>% 
  activate(edges) %>% 
  filter(color_key == "country-blues" | color_key == "blues" | color_key == "country") 

blues_country_stats <- net_stats(blues_country)

# separate graphs for each genre group ------------------------------------


bg_net <- full_color_net %>% 
  activate(edges) %>% 
  filter(str_detect(color_key, "bg"))

folk_net <- full_color_net %>% 
  activate(edges) %>% 
  filter(str_detect(color_key, "folk"))

country_net <- full_color_net %>% 
  activate(edges) %>% 
  filter(str_detect(color_key, "country"))

blues_net <- full_color_net %>% 
  activate(edges) %>% 
  filter(str_detect(color_key, "blues"))

jazz_net <- full_color_net %>% 
  activate(edges) %>% 
  filter(str_detect(color_key, "jazz"))

# calculate stats for each genre graph ------------------------------------

bg_stats <- net_stats(bg_net)
blues_stats <- net_stats(blues_net)
jazz_stats <- net_stats(jazz_net)
folk_stats <- net_stats(folk_net)
country_stats <- net_stats(country_net)

country_alone <- full_color_net %>% 
  activate(edges) %>% 
  filter(color_key == "country")

country_alone_stat <- net_stats(country_alone)

jazz_alone <- full_color_net %>% 
  activate(edges) %>% 
  filter(color_key == "jazz")

jazz_alone_stat <- net_stats(jazz_alone)


# see if edge densities change without the major crossover clump ----------

no_kerrville <- full_color_net %>% 
  activate(edges) %>% 
  filter(color_key != "folk-country-blues-bg")

graph_noKerrville <- ggraph(no_kerrville, layout = layout_sf) +
  geom_sf(data = states_map, fill = "white") +
  geom_edge_density(aes(fill = color_key),
                    n = 500) +
  scale_edge_fill_manual(values =
                           c("folk" = "yellow",
                             "folk-country" = "darkorange1",
                             "folk-blues" = "darkslategray3",
                             "folk-bg" = "darkolivegreen1",
                             "folk-country-blues" = "chocolate4",
                             "country-blues" = "blueviolet",
                             "blues" = "blue",
                             "country" = "red2",
                             "bg" = "darkgreen",
                             "jazz" = "lightpink",
                             "folk-jazz" = "lightcoral",
                             "country-bg" = "olivedrab",
                             "country-jazz" = "coral4",
                             "blues-jazz" = "magenta",
                             "blues-bg" = "darkcyan",
                             "folk-country-blues" = "cadetblue4",
                             "folk-country-jazz" = "chocolate2",
                             "folk-country-bg" = "chartreuse",
                             "folk-bg-blues" = "darkseagreen",
                             "folk-bg-jazz" = "darkkhaki",
                             "folk-blues-jazz" = "coral3",
                             "country-blues-jazz" = "darkred",
                             "country-bg-blues" = "darkslategray",
                             "country-bg-jazz" = "brown3",
                             "blues-bg-jazz" = "darkslateblue",
                             "folk-country-blues-bg" = "cornsilk4")
  ) +
  coord_sf(xlim = c(-130, -60), ylim = c(25,55), expand = FALSE)

noKerrville_noJazz <- no_kerrville %>% 
  activate(edges) %>% 
  filter(str_detect(color_key, "jazz") == F)

graph_noKerrville_noJazz <- ggraph(noKerrville_noJazz, layout = layout_sf) +
  geom_sf(data = states_map, fill = "white") +
  geom_edge_density(aes(fill = color_key)) +
  scale_edge_fill_manual(values =
                           c("folk" = "yellow",
                             "folk-country" = "darkorange1",
                             "folk-blues" = "darkslategray3",
                             "folk-bg" = "darkolivegreen1",
                             "folk-country-blues" = "chocolate4",
                             "country-blues" = "blueviolet",
                             "blues" = "blue",
                             "country" = "red2",
                             "bg" = "darkgreen",
                             "jazz" = "deeppink",
                             "folk-jazz" = "lightcoral",
                             "country-bg" = "olivedrab",
                             "country-jazz" = "coral4",
                             "blues-jazz" = "darkmagenta",
                             "blues-bg" = "darkcyan",
                             "folk-country-blues" = "cadetblue4",
                             "folk-country-jazz" = "chocolate2",
                             "folk-country-bg" = "chartreuse",
                             "folk-bg-blues" = "darkseagreen",
                             "folk-bg-jazz" = "darkkhaki",
                             "folk-blues-jazz" = "coral3",
                             "country-blues-jazz" = "darkred",
                             "country-bg-blues" = "darkslategray",
                             "country-bg-jazz" = "brown3",
                             "blues-bg-jazz" = "darkslateblue",
                             "folk-country-blues-bg" = "cornsilk4")
  ) +
  coord_sf(xlim = c(-130, -60), ylim = c(25,55), expand = FALSE)

folk_noKerrville <- no_kerrville %>% 
  activate(edges) %>% 
  filter(str_detect(color_key, "folk"))

folk_graph_nk <- ggraph(folk_noKerrville, layout = layout_sf) +
  geom_sf(data = states_map, fill = "white") +
  geom_edge_density(aes(fill = color_key),
                    n = 100) +
  scale_edge_fill_manual(values =
                           c("folk" = "yellow",
                             "folk-country" = "darkorange1",
                             "folk-blues" = "darkslategray3",
                             "folk-bg" = "darkolivegreen1",
                             "folk-country-blues" = "chocolate4",
                             "folk-jazz" = "magenta",
                             "folk-country-blues" = "darkblue",
                             "folk-country-jazz" = "chocolate2",
                             "folk-country-bg" = "darkgreen",
                             "folk-bg-blues" = "darkseagreen1",
                             "folk-bg-jazz" = "purple",
                             "folk-country-blues-bg" = "black")
  ) +
  coord_sf(xlim = c(-130, -60), ylim = c(25,55), expand = FALSE)


# people only, not geographical -------------------------------------------

ppl_graph <- full_color_net %>% 
  activate(edges) %>% 
  filter(str_detect(from_label, "_") & str_detect(to_label, "_")) %>% 
  activate(nodes) %>% 
  filter(str_detect(text_id, "_"))

ppl_stats <- net_stats(ppl_graph) %>% 
  filter(degree > 0)

ppl_graph_filtered <- ppl_graph %>% 
  activate(edges) %>% 
  filter(from %in% ppl_stats$id & to %in% ppl_stats$id) %>% 
  activate(nodes) %>% 
  filter(id %in% ppl_stats$id)

ppl_net <- ggraph(ppl_graph_filtered, layout = "kk") +
  geom_edge_density(aes(fill = color_key)) +
  geom_edge_link(width = 0.1, alpha = 0.5) +
  geom_node_point(aes(color = text_id)) +
  scale_edge_fill_manual(values =
                           c("folk" = "yellow",
                             "folk-country" = "darkorange1",
                             "folk-blues" = "darkslategray3",
                             "folk-bg" = "darkolivegreen1",
                             "folk-country-blues" = "chocolate4",
                             "country-blues" = "blueviolet",
                             "blues" = "blue",
                             "country" = "red2",
                             "bg" = "darkgreen",
                             "jazz" = "deeppink",
                             "folk-jazz" = "lightcoral",
                             "country-bg" = "olivedrab",
                             "country-jazz" = "coral4",
                             "blues-jazz" = "darkmagenta",
                             "blues-bg" = "darkcyan",
                             "folk-country-blues" = "cadetblue4",
                             "folk-country-jazz" = "chocolate2",
                             "folk-country-bg" = "chartreuse",
                             "folk-bg-blues" = "darkseagreen",
                             "folk-bg-jazz" = "darkkhaki",
                             "folk-blues-jazz" = "coral3",
                             "country-blues-jazz" = "darkred",
                             "country-bg-blues" = "darkslategray",
                             "country-bg-jazz" = "brown3",
                             "blues-bg-jazz" = "darkslateblue",
                             "folk-country-blues-bg" = "cornsilk4")
  ) +
  scale_color_manual(values = c("Rinzler_Ralph" = "red"), na.value = "grey10") +
  theme_void()


# blue/jazz net -----------------------------------------------------------

blues_jazz <- full_color_net %>% 
  activate(edges) %>% 
  filter(str_detect(color_key, "blues") | str_detect(color_key, "jazz"))

blues_jazz_stats <- net_stats(blues_jazz)

blues_jazz_df <- as.data.frame(blues_jazz)

blues_AND_jazz <- full_color_net %>% 
  activate(edges) %>% 
  filter(str_detect(color_key, "blues-jazz"))

bAj_df <- as.data.frame(blues_AND_jazz)

bAj_plotable <- blues_AND_jazz %>% 
  activate(nodes) %>% 
  filter(id %in% bAj_df$from | id %in% bAj_df$to)

blues_AND_jazz_stats <- net_stats(blues_AND_jazz)

bAj_viz <- ggraph(bAj_plotable, layout = "linear", circular = T) +
  geom_node_point() +
  geom_edge_arc()
  


# DAWG net ----------------------------------------------------------------

dawg_edges <- full_net_edges %>% 
  filter(from_label == "Grisman_David" | to_label == "Grisman_David") 

dawg_nodes <- full_net_nodes %>% 
  filter(id %in% dawg_edges$from | id %in% dawg_edges$to)

dawg_net <- sfnetwork(nodes = dawg_nodes,
                      edges = dawg_edges,
                      directed = T,
                      node_key = "id")
 
dawg_stats <- net_stats(dawg_net)

dawg_net <- ggraph(dawg, layout = "kk") +
  geom_node_point() +
  geom_edge_density(aes(fill = color_key)) +
  scale_edge_fill_manual(values = colors_MASTER)

## Expanded dawg net

dawg_ex_edges <- full_net_edges %>% 
  filter(from %in% dawg_edges$from |
           from %in% dawg_edges$to |
           to %in% dawg_edges$to |
           to %in% dawg_edges$to)

dawg_ex_nodes <- full_net_nodes %>% 
  filter(id %in% dawg_ex_edges$from | id %in% dawg_ex_edges$to)

dawg_ex_net <- sfnetwork(nodes = dawg_ex_nodes,
                      edges = dawg_ex_edges,
                      directed = T,
                      node_key = "id")

dawg_ex_stats <- net_stats(dawg_ex_net)

dawg_ex_viz <- ggraph(dawg_ex_net, layout = "kk") +
  geom_edge_link(width = 0.1, alpha = 0.5) +
  geom_node_point(aes(color = text_id)) +
  geom_edge_density(aes(fill = color_key)) +
  scale_edge_fill_manual(values = colors_MASTER) +
  scale_color_manual(values = c("Watson_Doc" = "green",
                                "Rinzler_Ralph" = "red",
                                "McCoury_Del_1939-" = "blue",
                                "Grisman_David" = "yellow"),
                     na.value = "grey80") +
  theme_void()

full_stats <- net_stats(full_color_net)


# bluegrass influences map ------------------------------------------------

bg_influences <- ggraph(bg_net, layout = layout_sf) + 
  geom_sf(data = states_map, fill = "white") + 
  geom_edge_link(aes(color = color_key),
                 width = 0.3,
                 alpha = 1) + 
  scale_edge_color_manual(values = colors_MASTER) +
  coord_sf(xlim = c(-130, -60), ylim = c(25,55), expand = FALSE) +
  theme_void() 

bg_infl_cloud <- ggraph(bg_net, layout = layout_sf) + 
  geom_sf(data = basemap, fill = "white") + 
  geom_edge_density(aes(fill = color_key)) + 
  scale_edge_fill_manual(values = colors_MASTER) +
  theme_void()

bg_people_key <- ppl_graph %>% 
  activate(edges) %>% 
  filter(str_detect(color_key, "bg")) %>% 
  as.data.frame()

bg_people_only <- ppl_graph %>% 
  activate(nodes) %>% 
  filter(id %in% bg_people$from | id %in% bg_people$to)

bg_ppl_stats <- net_stats(bg_people_only)

bg_ppl_viz <- ggraph(bg_people_only, layout = "kk") +
  geom_node_point() +
  geom_edge_density(aes(fill = color_key)) +
  theme_void()

folk_bg_blues <- full_color_net %>% 
  activate(edges) %>% 
  filter(color_key == "folk-bg-blues") 

fbb_df <- as.data.frame(folk_bg_blues)

folk_bg_blues_filtered <- folk_bg_blues %>% 
  activate(nodes) %>% 
  filter(id %in% fbb_df$from | id %in% fbb_df$to)

folk_bg_blues_viz <- ggraph(folk_bg_blues, layout = layout_sf) + 
  geom_sf(data = states_map, fill = "white") + 
  geom_node_point(size = 0.5) +
  geom_edge_density(fill = "darkcyan") +
  coord_sf(xlim = c(-130, -60), ylim = c(25,55), expand = FALSE) +
  theme_void()

folk_bg <- full_color_net %>% 
  activate(edges) %>% 
  filter(color_key == "folk-bg") 

fbg_df <- as.data.frame(folk_bg)

folk_bg_filtered <- folk_bg %>% 
  activate(nodes) %>% 
  filter(id %in% fbg_df$from | id %in% fbg_df$to)

folk_bg_viz <- ggraph(folk_bg, layout = layout_sf) + 
  geom_sf(data = states_map, fill = "white") + 
  geom_node_point(size = 0.5) +
  geom_edge_density(fill = "yellowgreen") +
  coord_sf(xlim = c(-130, -60), ylim = c(25,55), expand = FALSE) +
  theme_void()


# tag stats for full net --------------------------------------------------

colors_highlight_folk <- c("folk" = "black",
                 "folk-country" = "darkorange1",
                 "folk-blues" = "darkslategray3",
                 "folk-bg" = "darkolivegreen1",
                 "folk-country-blues" = "chocolate4",
                 "country-blues" = "blueviolet",
                 "blues" = "blue",
                 "country" = "red2",
                 "bg" = "darkgreen",
                 "jazz" = "deeppink",
                 "folk-jazz" = "lightcoral",
                 "country-bg" = "olivedrab",
                 "country-jazz" = "coral4",
                 "blues-jazz" = "darkmagenta",
                 "blues-bg" = "darkcyan",
                 "folk-country-blues" = "cadetblue4",
                 "folk-country-jazz" = "chocolate2",
                 "folk-country-bg" = "chartreuse",
                 "folk-bg-blues" = "darkseagreen",
                 "folk-bg-jazz" = "darkkhaki",
                 "folk-blues-jazz" = "coral3",
                 "country-blues-jazz" = "darkred",
                 "country-bg-blues" = "darkslategray",
                 "country-bg-jazz" = "brown3",
                 "blues-bg-jazz" = "darkslateblue",
                 "folk-country-blues-bg" = "cornsilk4")

folk_hl_viz <- net <- ggraph(full_color_net, layout = layout_sf) +
  geom_sf(data = states_map, fill = "white") +
  geom_edge_density(aes(fill = color_key),
                    n = 250) +
  scale_edge_fill_manual(values = colors_highlight_folk) +
  coord_sf(xlim = c(-130, -60), ylim = c(25,55), expand = FALSE)

folk_net_viz <- net <- ggraph(folk_net, layout = layout_sf) +
  geom_sf(data = states_map, fill = "white") +
  geom_edge_density(aes(fill = color_key),
                    n = 250) +
  scale_edge_fill_manual(values = colors_highlight_folk) +
  coord_sf(xlim = c(-130, -60), ylim = c(25,55), expand = FALSE)
