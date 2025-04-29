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

source("color_net_map.R")


# filter rinz to only items tagged with 'African Americans" ---------------

rinz_black <- rinz %>% 
  filter(str_detect(subjects, "African Americans"))


# re-create nodes from just this df ---------------------------------------

places <- rinz_black %>% 
  select(place, coordinates) %>% 
  distinct(place) %>% 
  left_join(all_nodes, by = c("place" = "text_id")) %>% 
  mutate(text_id = place) %>% 
  select(id, text_id, geometry)

people <- rinz_black %>% 
  mutate(names = str_replace_all(names, ",\\s", "_")) %>% 
  separate_longer_delim(cols = names,
                        delim = ",") %>% 
  distinct(names) %>% 
  left_join(all_nodes, by = c("names" = "text_id")) %>% 
  mutate(text_id = names) %>% 
  select(id, text_id, geometry)

black_nodes <- rbind(places, people) 

# filter edges using all_nodes IDs

black_edges <- full_net_edges %>% 
  filter(from %in% black_nodes$id | to %in% black_nodes$id)

# reset IDs to get rid of skipped numbers 
black_nodes_2 <- all_nodes %>% 
  na.omit() %>% 
  filter(id %in% black_edges$from | id %in% black_edges$to) %>% 
  select(-id) %>% 
  mutate(id = as.character(row_number()))

# renumber IDs for edges using the new IDs
black_edges_2 <- black_edges %>% 
  na.omit() %>% 
  select(-from, -to) %>% 
  left_join(black_nodes_2, by = c("from_label" = "text_id")) %>% 
  mutate(from = id) %>% 
  select(- id) %>% 
  left_join(black_nodes_2, by = c("to_label" = "text_id")) %>% 
  mutate(to = id) %>% 
  select(from, from_label,
         to, to_label,
         color_key)

# now re-filter edges
black_edges_2 <- black_edges_2 %>% 
  filter(from %in% black_nodes_2$id & to %in% black_nodes_2$id)

black_net <- sfnetwork(nodes = black_nodes_2,
                       edges = black_edges_2,
                       directed = T,
                       node_key = "id")

black_net_stats <- net_stats(black_net)


# plot --------------------------------------------------------------------

black_net_viz <- ggraph(black_net, layout = layout_sf) +
  geom_sf(data = states_map, fill = "white") +
  geom_node_point(size = 0.3) +
  geom_edge_link() +
  geom_edge_density(aes(fill = color_key)) +
  # scale_edge_fill_manual(values = colors_MASTER) +
  coord_sf(xlim = c(-130, -60), ylim = c(25,55), expand = FALSE)

black_net
