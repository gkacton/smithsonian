### This runs as a standalone file!! 

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


# compress to single row per item

rinz_items <- rinz %>% 
  distinct(ID, title, names, subjects)

# Various counts ----------------------------------------------------------

# number of items tagged with country

nrow(rinz_items %>% filter(str_detect(subjects, regex("country", ignore_case=T))))

# number of items tagged with bluegrass

nrow(rinz_items %>% filter(str_detect(subjects, regex("bluegrass", ignore_case=T))))

# number of items tagged with blues

nrow(rinz_items %>% filter(str_detect(subjects, regex("blues", ignore_case=T))))

# number of items tagged with old-time

nrow(rinz_items %>% filter(str_detect(subjects, regex("old-time", ignore_case=T))))

# number tagged with old-time and country

nrow(rinz_items %>% filter(str_detect(subjects, regex("country", ignore_case=T)) & 
                             str_detect(subjects, regex("old-time", ignore_case=T))))

# number tagged with old-time and blues

nrow(rinz_items %>% filter(str_detect(subjects, regex("blues", ignore_case=T)) & 
                             str_detect(subjects, regex("old-time", ignore_case=T))))

# number tagged with old-time and folk

nrow(rinz_items %>% filter(str_detect(subjects, regex("folk", ignore_case=T)) & 
                             str_detect(subjects, regex("old-time", ignore_case=T))))

# number tagged with old-time and bluegrass

nrow(rinz_items %>% filter(str_detect(subjects, regex("bluegrass", ignore_case=T)) & 
                             str_detect(subjects, regex("old-time", ignore_case=T))))

# number tagged with old-time and jazz --> 0

nrow(rinz_items %>% filter(str_detect(subjects, regex("jazz", ignore_case=T)) & 
                             str_detect(subjects, regex("old-time", ignore_case=T))))

# number tagged with old-time and country and folk

nrow(rinz_items %>% filter(str_detect(subjects, regex("country", ignore_case=T)) & 
                             str_detect(subjects, regex("old-time", ignore_case=T)) & 
                             str_detect(subjects, regex("folk", ignore_case=T))))

# number tagged with old-time and country and bluegrass

nrow(rinz_items %>% filter(str_detect(subjects, regex("country", ignore_case=T)) & 
                             str_detect(subjects, regex("old-time", ignore_case=T)) & 
                             str_detect(subjects, regex("bluegrass", ignore_case=T))))

# number tagged with old-time and country and blues

nrow(rinz_items %>% filter(str_detect(subjects, regex("country", ignore_case=T)) & 
                             str_detect(subjects, regex("old-time", ignore_case=T)) & 
                             str_detect(subjects, regex("blues", ignore_case=T))))

# number tagged with old-time and bluegrass and folk

nrow(rinz_items %>% filter(str_detect(subjects, regex("bluegrass", ignore_case=T)) & 
                             str_detect(subjects, regex("old-time", ignore_case=T)) & 
                             str_detect(subjects, regex("folk", ignore_case=T))))

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
    if(df$is_folk[i] == T & df$is_country[i] == T & df$is_blues[i] == F & df$is_bluegrass[i] == F & df$is_jazz[i] == F & df$is_ot[i] == F){
      # FOLK AND COUNTRY
      df$color_key[i] <- "folk-country"
    }
    else if(df$is_folk[i] == T & df$is_country[i] == F & df$is_blues[i] == T & df$is_bluegrass[i] == F & df$is_jazz[i] == F & df$is_ot[i] == F){
      # FOLK AND BLUES
      df$color_key[i] <- "folk-blues"
    }
    else if(df$is_folk[i] == T & df$is_country[i] == F & df$is_blues[i] == F & df$is_bluegrass[i] == T & df$is_jazz[i] == F & df$is_ot[i] == F){
      # FOLK AND BLUEGRASS
      df$color_key[i] <- "folk-bg"
    } 
    else if(df$is_folk[i] == T & df$is_country[i] == F & df$is_blues[i] == F & df$is_bluegrass[i] == F & df$is_jazz[i] == T & df$is_ot[i] == F){
      # FOLK AND JAZZ
      df$color_key[i] <- "folk-jazz"
    }
    else if(df$is_folk[i] == F & df$is_country[i] == T & df$is_blues[i] == T & df$is_bluegrass[i] == F & df$is_jazz[i] == F & df$is_ot[i] == F){
      # COUNTRY AND BLUES
      df$color_key[i] <- "country-blues"
    }
    else if(df$is_folk[i] == F & df$is_country[i] == T & df$is_blues[i] == F & df$is_bluegrass[i] == T & df$is_jazz[i] == F & df$is_ot[i] == F){
      # COUNTRY AND BLUEGRASS
      df$color_key[i] <- "country-bg"
    }
    else if(df$is_folk[i] == F & df$is_country[i] == T & df$is_blues[i] == F & df$is_bluegrass[i] == F & df$is_jazz[i] == T & df$is_ot[i] == F){
      # COUNTRY AND JAZZ
      df$color_key[i] <- "country-jazz"
    }
    else if(df$is_folk[i] == F & df$is_country[i] == F & df$is_blues[i] == T & df$is_bluegrass[i] == F & df$is_jazz[i] == T & df$is_ot[i] == F){
      # BLUES AND JAZZ
      df$color_key[i] <- "blues-jazz"
    }
    else if(df$is_folk[i] == F & df$is_country[i] == F & df$is_blues[i] == T & df$is_bluegrass[i] == T & df$is_jazz[i] == F & df$is_ot[i] == F){
      # BLUES AND BLUEGRASS
      df$color_key[i] <- "blues-bg"
    }
    else if(df$is_folk[i] == F & df$is_country[i] == F & df$is_blues[i] == F & df$is_bluegrass[i] == T & df$is_jazz[i] == T & df$is_ot[i] == F){
      # BLUEGRASS AND JAZZ
      df$color_key[i] <- "bg-jazz"
    }
    else if(df$is_folk[i] == T & df$is_country[i] == T & df$is_blues[i] == T & df$is_bluegrass[i] == F & df$is_jazz[i] == F & df$is_ot[i] == F){
      # FOLK AND COUNTRY AND BLUES
      df$color_key[i] <- "folk-country-blues"
    }
    else if(df$is_folk[i] == T & df$is_country[i] == T & df$is_blues[i] == F & df$is_bluegrass[i] == F & df$is_jazz[i] == T & df$is_ot[i] == F){
      # FOLK AND COUNTRY AND JAZZ
      df$color_key[i] <- "folk-country-jazz"
    }
    else if(df$is_folk[i] == T & df$is_country[i] == T & df$is_blues[i] == F & df$is_bluegrass[i] == T & df$is_jazz[i] == F & df$is_ot[i] == F){
      # FOLK AND COUNTRY AND BLUEGRASS
      df$color_key[i] <- "folk-country-bg"
    }
    else if(df$is_folk[i] == T & df$is_country[i] == F & df$is_blues[i] == T & df$is_bluegrass[i] == T & df$is_jazz[i] == F & df$is_ot[i] == F){
      # FOLK AND BLUEGRASS AND BLUES
      df$color_key[i] <- "folk-bg-blues"
    }
    else if(df$is_folk[i] == T & df$is_country[i] == F & df$is_blues[i] == F & df$is_bluegrass[i] == T & df$is_jazz[i] == T & df$is_ot[i] == F){
      # FOLK AND BLUEGRASS AND JAZZ
      df$color_key[i] <- "folk-bg-jazz"
    }
    else if(df$is_folk[i] == T & df$is_country[i] == F & df$is_blues[i] == T & df$is_bluegrass[i] == F & df$is_jazz[i] == T & df$is_ot[i] == F){
      # FOLK AND BLUES AND JAZZ
      df$color_key[i] <- "folk-blues-jazz"
    }
    else if(df$is_folk[i] == F & df$is_country[i] == T & df$is_blues[i] == T & df$is_bluegrass[i] == F & df$is_jazz[i] == T & df$is_ot[i] == F){
      # COUNTRY AND BLUES AND JAZZ
      df$color_key[i] <- "country-blues-jazz"
    }
    else if(df$is_folk[i] == F & df$is_country[i] == T & df$is_blues[i] == T & df$is_bluegrass[i] == T & df$is_jazz[i] == F & df$is_ot[i] == F){
      # COUNTRY AND BLUEGRASS AND BLUES
      df$color_key[i] <- "country-bg-blues"
    }
    else if(df$is_folk[i] == F & df$is_country[i] == T & df$is_blues[i] == F & df$is_bluegrass[i] == T & df$is_jazz[i] == T & df$is_ot[i] == F){
      # COUNTRY AND BLUEGRASS AND JAZZ
      df$color_key[i] <- "country-bg-jazz"
    } 
    else if(df$is_folk[i] == F & df$is_country[i] == F & df$is_blues[i] == T & df$is_bluegrass[i] == T & df$is_jazz[i] == T & df$is_ot[i] == F){
      # BLUES AND BLUEGRASS AND JAZZ
      df$color_key[i] <- "blues-bg-jazz"
    } 
    else if(df$is_folk[i] == T & df$is_country[i] == T & df$is_blues[i] == T & df$is_bluegrass[i] == T & df$is_jazz[i] == F & df$is_ot[i] == F){
      #FOLK AND COUNTRY AND BLUES AND BLUEGRASS
      df$color_key[i] <- "folk-country-blues-bg"
    } 
    else if(df$is_folk[i] == F & df$is_country[i] == F & df$is_blues[i] == T & df$is_bluegrass[i] == F & df$is_jazz[i] == F & df$is_ot[i] == F){
      # BLUES ALONE
      df$color_key[i] <- "blues"
    }
    else if(df$is_folk[i] == F & df$is_country[i] == T & df$is_blues[i] == F & df$is_bluegrass[i] == F & df$is_jazz[i] == F & df$is_ot[i] == F){
      # COUNTRY ALONE
      df$color_key[i] <- "country"
    }
    else if(df$is_folk[i] == F & df$is_country[i] == F & df$is_blues[i] == F & df$is_bluegrass[i] == T & df$is_jazz[i] == F & df$is_ot[i] == F){
      # BLUEGRASS ALONE
      df$color_key[i] <- "bg"
    }
    else if(df$is_folk[i] == F & df$is_country[i] == F & df$is_blues[i] == F & df$is_bluegrass[i] == F & df$is_jazz[i] == T & df$is_ot[i] == F){
      # JAZZ ALONE
      df$color_key[i] <- "jazz"
    } 
    else if(df$is_folk[i] == T & df$is_country[i] == F & df$is_blues[i] == F & df$is_bluegrass[i] == F & df$is_jazz[i] == F & df$is_ot[i] == F){
      # FOLK ALONE
      df$color_key[i] <- "folk"
    } 
    else if(df$is_folk[i] == F & df$is_country[i] == F & df$is_blues[i] == F & df$is_bluegrass[i] == F & df$is_jazz[i] == F & df$is_ot[i] == T){
      # OLDTIME ALONE
      df$color_key[i] <- "oldtime"
    } 
    else if(df$is_folk[i] == F & df$is_country[i] == T & df$is_blues[i] == F & df$is_bluegrass[i] == F & df$is_jazz[i] == F & df$is_ot[i] == T){
      # OLDTIME AND COUNTRY
      df$color_key[i] <- "oldtime-country"
    } 
    else if(df$is_folk[i] == T & df$is_country[i] == F & df$is_blues[i] == F & df$is_bluegrass[i] == F & df$is_jazz[i] == F & df$is_ot[i] == T){
      # OLDTIME AND FOLK
      df$color_key[i] <- "oldtime-folk"
    } 
    else if(df$is_folk[i] == F & df$is_country[i] == F & df$is_blues[i] == T & df$is_bluegrass[i] == F & df$is_jazz[i] == F & df$is_ot[i] == T){
      # OLDTIME AND BLUEGRASS
      df$color_key[i] <- "oldtime-bg"
    } 
    else if(df$is_folk[i] == F & df$is_country[i] == F & df$is_blues[i] == T & df$is_bluegrass[i] == F & df$is_jazz[i] == F & df$is_ot[i] == T){
      # OLDTIME AND BLUES
      df$color_key[i] <- "oldtime-blues"
    } 
    else if(df$is_folk[i] == T & df$is_country[i] == F & df$is_blues[i] == T & df$is_bluegrass[i] == F & df$is_jazz[i] == F & df$is_ot[i] == T){
      # OLDTIME AND FOLK AND BLUES 
      df$color_key[i] <- "oldtime-folk-blues"
    } 
    else if(df$is_folk[i] == T & df$is_country[i] == T & df$is_blues[i] == F & df$is_bluegrass[i] == F & df$is_jazz[i] == F & df$is_ot[i] == T){
      # OLDTIME AND FOLK AND COUNTRY 
      df$color_key[i] <- "oldtime-folk-blues"
    } 
    else if(df$is_folk[i] == T & df$is_country[i] == F & df$is_blues[i] == F & df$is_bluegrass[i] == T & df$is_jazz[i] == F & df$is_ot[i] == T){
      # OLDTIME AND FOLK AND BLUEGRASS
      df$color_key[i] <- "oldtime-folk-blues"
    } 
    else if(df$is_folk[i] == T & df$is_country[i] == T & df$is_blues[i] == T & df$is_bluegrass[i] == F & df$is_jazz[i] == F & df$is_ot[i] == T){
      # OLDTIME AND COUNTRY AND BLUES 
      df$color_key[i] <- "oldtime-folk-blues"
    } 
    else if(df$is_folk[i] == T & df$is_country[i] == T & df$is_blues[i] == F & df$is_bluegrass[i] == T & df$is_jazz[i] == F & df$is_ot[i] == T){
      # OLDTIME AND COUNTRY AND BLUEGRASS 
      df$color_key[i] <- "oldtime-folk-blues"
    } 
    else{
      df$color_key[i] <- NA
    }
  }
  return(df)
}

# uggghhh make another network ugggghhh -----------------------------------

country <- rinz %>% 
  filter(str_detect(subjects, regex("country", ignore_case=T)))

# find all nodes ----------------------------------

# get avg location for each person
ppl <- country %>% 
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

# get all unique places
places <- country %>% 
  filter(coordinates != "") %>% 
  separate_wider_delim(coordinates, delim = ",", names = c("lat", "lon")) %>% 
  mutate(lat = as.double(lat),
         lon = as.double(lon)) %>% 
  mutate(text_id = place) %>% 
  distinct(text_id, lat, lon) %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = 4326)

# join all nodes

all_nodes <- ppl %>% 
  mutate(text_id = names) %>% 
  select(-names, -count, -id) %>% 
  rbind(places) %>% 
  mutate(id = as.character(row_number())) %>% 
  filter(text_id != "") %>% 
  mutate(text_id = as.character(text_id))

# find all edges ----------------------------------------------

# people-people collaborations 
collabs <- country %>% 
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
  # active_list <- collabs[i, 1:41]
  active_list <- collabs[i, 1:29]
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
  mutate(is_jazz = str_detect(subjects, regex("jazz", ignore_case = T))) %>% 
  # mutate(is_ot = str_detect(subjects, regex("old-time", ignore_case = T))) %>% 
  mutate(is_ot = F) %>% 
  na.omit()

name_combos_colored <- assign_color(name_combos)

# people-place connections

connections <- country %>% 
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
  # mutate(is_ot = str_detect(subjects, regex("old-time", ignore_case = T))) %>% 
  mutate(is_ot = F) %>% 
  mutate(from = place,
         to = names)

df <- connections %>% 
  mutate(color_key = "")

connections_colored <- assign_color(df) %>% 
  filter(is.na(color_key) == F & color_key != "") %>% 
  select(-subjects, -starts_with("is"), -place, -names) %>% 
  distinct(from, to, color_key, .keep_all=T) 

country_places <- country %>% 
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

for(i in 1:nrow(country_places)){
  active_list <- country_places[i, 2:67]
  active_subjects <- country_places$subjects[i]
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
  mutate(is_jazz = str_detect(subjects, regex("jazz", ignore_case = T))) %>% 
  # mutate(is_ot = str_detect(subjects, regex("old-time", ignore_case = T)))
  mutate(is_ot = F)

place_edges_colored <- assign_color(place_edges)

place_edges_colored <- place_edges_colored %>% 
  filter(is.na(color_key) == F & color_key != "") %>% 
  select(-subjects, -starts_with("is")) %>% 
  distinct(from, to, color_key, .keep_all=T) %>% 
  mutate(from = as.character(from),
         to = as.character(to))

ppl_edges <- name_combos_colored %>% 
  select(from, to, color_key)

place_edges <- place_edges_colored

ppl_place_edges <- connections_colored

all_edges <- rbind(ppl_edges, place_edges, ppl_place_edges)


# join node ids -----------------------------------------------------------

edges <- all_edges %>% 
  mutate(from_label = as.character(from),
         to_label = as.character(to)) %>% 
  select(-from, -to) %>% 
  left_join(all_nodes, by = c("from_label" = "text_id"),
            multiple = "first") %>% 
  mutate(from = as.character(id)) %>% 
  select(-id) %>% 
  left_join(all_nodes, by = c("to_label" = "text_id"),
            multiple = "first") %>% 
  mutate(to = as.character(id))%>% 
  select(from, from_label,
         to, to_label,
         color_key) %>% 
  na.omit() %>% 
  filter(color_key != "folk-country-blues-bg")

nodes <- all_nodes %>% 
  filter(id %in% edges$from | id %in% edges$to)

edges <- edges %>% 
  filter(to %in% nodes$id & from %in% nodes$id) 

# create network ----------------------------------------------------------

country_net <- sfnetwork(nodes = nodes,
                         edges = edges, 
                         directed = T,
                         node_key = "id")


# color list --------------------------------------------------------------

# country = red, oldtime = blue, blues = gray, bluegrass = green, folk = pink 
country_colors <- c("folk-country" = "orange",
                    "country-blues" = "purple",
                    "country" = "red3",
                    "country-bg" = "green",
                    "folk-country-blues" = "darkseagreen4",
                    "folk-country-bg" = "darkolivegreen3",
                    "folk-country-blues-bg" = "cornsilk4" #,
                    # "oldtime" = "blue",
                    # "oldtime-country" = "blueviolet",
                    # "oldtime-blues" = "cadetblue",
                    # "oldtime-folk" = "magenta",
                    # "oldtime-folk-blues" = "coral2"
                    )


# basemap data ------------------------------------------------------------

states_map <- rnaturalearthdata::states50

# get layout_sf from lore abad --------------------------------------------

layout_sf = function(graph){
  # Extract X and Y coordinates from the nodes
  graph = activate(graph, "nodes")
  x = sf::st_coordinates(graph)[,"X"]
  y = sf::st_coordinates(graph)[,"Y"]
  data.frame(x, y)
}

# graph it! ---------------------------------------------------------------

country_graph <- ggraph(country_net, layout = layout_sf) +
  geom_sf(data = states_map, fill = "white") +
  geom_edge_density(aes(fill = color_key)) +
  scale_edge_fill_manual(values = country_colors, na.value = "white") +
  coord_sf(xlim = c(-130, -60), ylim = c(25,55), expand = FALSE) +
  theme_void() +
  labs(title = "Country Genre Overlaps",
       subtitle = "From Ralph Rinzler's Audio Collection",
       fill = "Color Key") 

# repeating with african-american entries ---------------------------------

# stats -------------------------------------------------------------------

rinz_black <- rinz %>% 
  filter(str_detect(subjects, "African Americans"))

rinz_black_items <- rinz_black %>% 
  distinct(ID, names, subjects)

### 292 items are tagged with "African Americans"

# number of items with "country" tag --> 5
nrow(rinz_black_items %>% filter(str_detect(subjects, regex("country", ignore_case=T))))

# number of items with "blues" tag --> 156
nrow(rinz_black_items %>% filter(str_detect(subjects, regex("blues", ignore_case=T))))

# number of items with "old-time" tag --> 2 (both "Folk Songs in America" compendia)
nrow(rinz_black_items %>% filter(str_detect(subjects, regex("old-time", ignore_case=T))))

# find all nodes ----------------------------------

# get avg location for each person
ppl <- rinz_black %>% 
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

# get all unique places
places <- rinz_black %>% 
  filter(coordinates != "") %>% 
  separate_wider_delim(coordinates, delim = ",", names = c("lat", "lon")) %>% 
  mutate(lat = as.double(lat),
         lon = as.double(lon)) %>% 
  mutate(text_id = place) %>% 
  distinct(text_id, lat, lon) %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = 4326)

# join all nodes

all_nodes <- ppl %>% 
  mutate(text_id = names) %>% 
  select(-names, -count, -id) %>% 
  rbind(places) %>% 
  mutate(id = as.character(row_number())) %>% 
  filter(text_id != "") %>% 
  mutate(text_id = as.character(text_id))

# find all edges ----------------------------------------------

# people-people collaborations 
collabs <- rinz_black %>% 
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
  active_list <- collabs[i, 1:29]
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
  mutate(is_jazz = str_detect(subjects, regex("jazz", ignore_case = T))) %>% 
  mutate(is_ot = str_detect(subjects, regex("old-time", ignore_case = T))) %>% 
  na.omit()

name_combos_colored <- assign_color(name_combos)

# people-place connections

connections <- rinz_black %>% 
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
  mutate(is_ot = str_detect(subjects, regex("old-time", ignore_case = T))) %>% 
  mutate(from = place,
         to = names)

df <- connections %>% 
  mutate(color_key = "")

connections_colored <- assign_color(df) %>% 
  filter(is.na(color_key) == F & color_key != "") %>% 
  select(-subjects, -starts_with("is"), -place, -names) %>% 
  distinct(from, to, color_key, .keep_all=T) 

rinz_black_places <- rinz_black %>% 
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

for(i in 1:nrow(rinz_black_places)){
  active_list <- rinz_black_places[i, 2:67]
  active_subjects <- rinz_black_places$subjects[i]
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
  mutate(is_jazz = str_detect(subjects, regex("jazz", ignore_case = T))) %>% 
  mutate(is_ot = str_detect(subjects, regex("old-time", ignore_case = T)))

place_edges_colored <- assign_color(place_edges)

place_edges_colored <- place_edges_colored %>% 
  filter(is.na(color_key) == F & color_key != "") %>% 
  select(-subjects, -starts_with("is")) %>% 
  distinct(from, to, color_key, .keep_all=T) %>% 
  mutate(from = as.character(from),
         to = as.character(to))

ppl_edges <- name_combos_colored %>% 
  select(from, to, color_key)

place_edges <- place_edges_colored

ppl_place_edges <- connections_colored

all_edges <- rbind(ppl_edges, place_edges, ppl_place_edges)


# join node ids -----------------------------------------------------------

edges <- all_edges %>% 
  mutate(from_label = as.character(from),
         to_label = as.character(to)) %>% 
  select(-from, -to) %>% 
  left_join(all_nodes, by = c("from_label" = "text_id"),
            multiple = "first") %>% 
  mutate(from = as.character(id)) %>% 
  select(-id) %>% 
  left_join(all_nodes, by = c("to_label" = "text_id"),
            multiple = "first") %>% 
  mutate(to = as.character(id))%>% 
  select(from, from_label,
         to, to_label,
         color_key) %>% 
  na.omit() %>% 
  filter(color_key != "folk-country-blues-bg")

nodes <- all_nodes %>% 
  filter(id %in% edges$from | id %in% edges$to)

edges <- edges %>% 
  filter(to %in% nodes$id & from %in% nodes$id) 

# create network ----------------------------------------------------------

rinz_black_net <- sfnetwork(nodes = nodes,
                         edges = edges, 
                         directed = T,
                         node_key = "id")


# color list --------------------------------------------------------------

# country = red, oldtime = blue, blues = gray, bluegrass = green, folk = pink 
colors_master <- c("folk" = "yellow",
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
                   "folk-country-blues-bg" = "cornsilk4",
                   "oldtime" = "tan"
)


# basemap data ------------------------------------------------------------

states_map <- rnaturalearthdata::states50

# get layout_sf from lore abad --------------------------------------------

layout_sf = function(graph){
  # Extract X and Y coordinates from the nodes
  graph = activate(graph, "nodes")
  x = sf::st_coordinates(graph)[,"X"]
  y = sf::st_coordinates(graph)[,"Y"]
  data.frame(x, y)
}

# graph it! ---------------------------------------------------------------

rinz_black_graph <- ggraph(rinz_black_net, layout = layout_sf) +
  geom_sf(data = states_map, fill = "white") +
  geom_edge_density(aes(fill = color_key), n = 200) +
  scale_edge_fill_manual(values = colors_master, na.value = "black") +
  coord_sf(xlim = c(-130, -60), ylim = c(25,55), expand = FALSE) +
  theme_void() +
  labs(title = "Genre Overlaps in items tagged 'African Americans'",
       subtitle = "From Ralph Rinzler's Audio Collection",
       fill = "Color Key") 

# once more (with feeling) -----------------------------------

# find all nodes ----------------------------------

# get avg location for each person
ppl <- rinz %>% 
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

# get all unique places
places <- rinz %>% 
  filter(coordinates != "") %>% 
  separate_wider_delim(coordinates, delim = ",", names = c("lat", "lon")) %>% 
  mutate(lat = as.double(lat),
         lon = as.double(lon)) %>% 
  mutate(text_id = place) %>% 
  distinct(text_id, lat, lon) %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = 4326)

# join all nodes

all_nodes <- ppl %>% 
  mutate(text_id = names) %>% 
  select(-names, -count, -id) %>% 
  rbind(places) %>% 
  mutate(id = as.character(row_number())) %>% 
  filter(text_id != "") %>% 
  mutate(text_id = as.character(text_id))

# find all edges ----------------------------------------------

# people-people collaborations 
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
  mutate(is_jazz = str_detect(subjects, regex("jazz", ignore_case = T))) %>% 
  mutate(is_ot = str_detect(subjects, regex("old-time", ignore_case = T))) %>% 
  na.omit()

name_combos_colored <- assign_color(name_combos)

# people-place connections

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
  mutate(is_ot = str_detect(subjects, regex("old-time", ignore_case = T))) %>% 
  mutate(from = place,
         to = names)

df <- connections %>% 
  mutate(color_key = "")

connections_colored <- assign_color(df) %>% 
  filter(is.na(color_key) == F & color_key != "") %>% 
  select(-subjects, -starts_with("is"), -place, -names) %>% 
  distinct(from, to, color_key, .keep_all=T) 

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
  active_list <- rinz_places[i, 2:67]
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
  mutate(is_jazz = str_detect(subjects, regex("jazz", ignore_case = T))) %>% 
  mutate(is_ot = str_detect(subjects, regex("old-time", ignore_case = T)))

place_edges_colored <- assign_color(place_edges)

place_edges_colored <- place_edges_colored %>% 
  filter(is.na(color_key) == F & color_key != "") %>% 
  select(-subjects, -starts_with("is")) %>% 
  distinct(from, to, color_key, .keep_all=T) %>% 
  mutate(from = as.character(from),
         to = as.character(to))

ppl_edges <- name_combos_colored %>% 
  select(from, to, color_key)

place_edges <- place_edges_colored

ppl_place_edges <- connections_colored

all_edges <- rbind(ppl_edges, place_edges, ppl_place_edges)


# join node ids -----------------------------------------------------------

edges <- all_edges %>% 
  mutate(from_label = as.character(from),
         to_label = as.character(to)) %>% 
  select(-from, -to) %>% 
  left_join(all_nodes, by = c("from_label" = "text_id"),
            multiple = "first") %>% 
  mutate(from = as.character(id)) %>% 
  select(-id) %>% 
  left_join(all_nodes, by = c("to_label" = "text_id"),
            multiple = "first") %>% 
  mutate(to = as.character(id))%>% 
  select(from, from_label,
         to, to_label,
         color_key) %>% 
  na.omit() %>% 
  filter(color_key != "folk-country-blues-bg")

nodes <- all_nodes %>% 
  filter(id %in% edges$from | id %in% edges$to)

edges <- edges %>% 
  filter(to %in% nodes$id & from %in% nodes$id) 

# create network ----------------------------------------------------------

rinz_net <- sfnetwork(nodes = nodes,
                      edges = edges,
                      directed = T,
                      node_key = "id")


# plot! -------------------------------------------------------------------


rinz_graph <- ggraph(rinz_net, layout = layout_sf) +
  geom_sf(data = states_map, fill = "white") +
  geom_edge_density(aes(fill = color_key)) +
  scale_edge_fill_manual(values = colors_master, na.value = "black") +
  coord_sf(xlim = c(-130, -60), ylim = c(25,55), expand = FALSE) +
  theme_void() +
  labs(title = "Genre Overlaps",
       subtitle = "From Ralph Rinzler's Audio Collection",
       fill = "Color Key") 


# stats -------------------------------------------------------------------

country_stats <- net_stats(country_net)

black_stats <- net_stats(rinz_black_net)

full_net_stats <- net_stats(rinz_net)

nrow(rinz_black_items %>% filter(str_detect(subjects, regex("prison", ignore_case=T)))) 
nrow(rinz_items %>% filter(str_detect(subjects, regex("prison", ignore_case=T)))) 


# filtered net ------------------------------------------------------------

country_only <- country_net %>% 
  activate(edges) %>% 
  filter(color_key == "country") %>% 
  net_stats()


# assign color keys to items ----------------------------------------------

rinz_items <- rinz_items %>% 
  mutate(is_folk = str_detect(subjects, regex("folk", ignore_case = T))) %>% 
  mutate(is_bluegrass = str_detect(subjects, regex("bluegrass", ignore_case = T))) %>% 
  mutate(is_blues = str_detect(subjects, regex("blues", ignore_case = T))) %>% 
  mutate(is_country = str_detect(subjects, regex("country", ignore_case = T))) %>% 
  mutate(is_jazz = str_detect(subjects, regex("jazz", ignore_case = T))) %>% 
  mutate(is_ot = F)

rinz_items_colors <- assign_color(rinz_items)

counts <- rinz_items_colors %>% 
  group_by(color_key) %>% 
  summarize(count = n())


# subject counts by item --------------------------------------------------

rinz_items_subj <- rinz_items %>% 
  separate_longer_delim(subjects,
                        delim = ",") %>% 
  group_by(subjects) %>% 
  summarize(count = n())
