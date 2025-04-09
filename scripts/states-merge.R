
# load libraries ----------------------------------------------------------

library(tidyverse)
library(rnaturalearthdata)
library(sf)
library(sfnetworks)
library(tidygraph)
library(DescTools)


# load data ---------------------------------------------------------------

genre_info <- read.csv("~/Desktop/smithsonian/data/genre_info.csv")
rinz <- read.csv("~/Desktop/smithsonian/data/rinzler-full-reconciled.csv")


# merge state shapefile ---------------------------------------------------

states <- rnaturalearthdata::states50 %>% 
  filter(iso_a2 == "CA" | iso_a2 == "US") %>% 
  select(name, geometry, type_en)

rinz_states <- rinz %>% 
  left_join(states, by = c('place' = 'name')) %>% 
  filter(is.na(type_en) == F)


# join ethnicity data -----------------------------------------------------

eth_coded <- read.csv("~/Desktop/smithsonian/data/subjects_ethnicity_coded.csv")

rinz_states_eth <-  rinz_states %>% 
  separate_longer_delim(cols = subjects, delim = ",") %>% 
  left_join(eth_coded, by = "subjects")
  
states_stats <- rinz_states_eth %>% 
  filter(place != "" & subjects != "") %>% 
  group_by(place, subjects) %>% 
  summarize(count = n()) %>% 
  pivot_wider(names_from = subjects, values_from = count)  


# maps of incidence rates of genres/ethnicities by state ------------------

folk_st <- states_stats %>% 
  select(place, starts_with("Folk songs")) %>% 
  mutate(count = sum(across(starts_with("Folk songs")), na.rm = T)) %>% 
  filter(is.na(count) == F) %>% 
  left_join(states, by = c('place' = 'name')) %>% 
  st_as_sf()

folk_st_map <- ggplot() +
  geom_sf(data = folk_st, aes(fill = count)) +
  labs(title = "Count of folk music items by state")

bg_st <- states_stats %>% 
  select(place, `Bluegrass music`) %>% 
  filter(is.na(`Bluegrass music`) == F) %>% 
  left_join(states, by = c('place' = 'name')) %>% 
  st_as_sf() 

bg_st_map <- ggplot() +
  geom_sf(data = bg_st, aes(fill = `Bluegrass music`)) +
  labs(title = "Count of bluegrass music items by state")

subj_map <- function(subject, df_title, map_title){
  df_title <- states_stats %>% 
    select(place, contains(subject)) %>% 
    mutate(count = sum(across(contains(subject)), na.rm = T)) %>% 
    filter(is.na(count) ==F) %>% 
    left_join(states, by = c('place' = 'name')) %>% 
    st_as_sf()
  
  map_title <- ggplot() +
    geom_sf(data = states, fill = "grey80") +
    geom_sf(data = df_title, aes(fill = count)) +
    labs(title = paste("Count of", subject, "items by state", sep = " "))
  
  return(map_title)
}

blues_map <- subj_map("Blues", blues_df, blues_map)
country_map <- subj_map("Country", country_df, country_map)
jazz_map <- subj_map("Jazz", jazz_df, jazz_map)
cowboy_map <- subj_map("cowboy", cowboy_df, cowboy_map)
black_map <- subj_map("African american", black_df, black_map)
cajun_map <- subj_map("cajun", cajun_df, cajun_map)


# re-construct networks from non-state data -------------------------------

countries <- rnaturalearthdata::countries50 %>% 
  select(name, type, subunit) %>% 
  st_as_sf(crs = 4326)

rinz_points <- rinz %>% 
  left_join(states, by = c('place' = 'name')) %>% 
  filter(is.na(type_en) == T) %>% 
  left_join(countries, by = c('place' = 'name')) %>% 
  left_join(countries, by = c('place' = 'subunit')) %>% 
  filter(is.na(type.x) == T & is.na(type.y) == T) %>% 
  filter(place != "" & place != "Great Britain" & place != "Scotland" & place != "Wales" & place != "England" & place != "Northern Ireland") %>% 
  distinct(ID, place, .keep_all = T) %>% 
  select(-starts_with("type"), -starts_with("geometry"), -subunit, -name)

point_coord <- rinz_points %>% 
  select(place, coordinates)

points_stats <- rinz_points %>% 
  group_by(place) %>% 
  summarize(count = n())

point_data <- left_join(points_stats, point_coord, by = c("place" = "place")) %>% 
  distinct(place, .keep_all = T) %>% 
  filter(place != "Appalachia") %>% 
  filter(coordinates != "") %>% 
  separate_wider_delim(coordinates, delim = ",", names = c("lat", "lon")) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

point_map <- ggplot() +
  geom_sf(data = countries) +
  geom_sf(data = point_data, aes(size = count/25))

# building point/person network

ppl_point <- rinz_points %>% 
  select(place, coordinates, names, subjects, date) %>% 
  mutate(names = str_replace_all(names, ",\\s", "_")) %>% 
  separate_longer_delim(cols = names, 
                       delim = ",") %>% 
  distinct(place, names, .keep_all = T) 

ppl_nodes <- ppl_point %>% 
  filter(names != "") %>% 
  mutate(text_id = names) %>% 
  mutate(type = "person") %>% 
  mutate(coordinates = "") %>% 
  group_by(text_id) %>% 
  summarise(subjects = paste0(subjects, collapse = ","),
            dates = paste0(unique(date[[1]]), collapse = ","),
            coordinates = paste0(unique(coordinates), collapse = ","),
            type = paste0(unique(type), collapse = ",")
  ) %>% 
  mutate(dates = str_remove_all(dates, "[^0-9]")) %>% 
  select(text_id, type, subjects, coordinates, dates)

place_nodes <- ppl_point %>% 
  filter(place != "") %>% 
  mutate(text_id = place) %>% 
  mutate(type = "place") %>% 
  group_by(text_id) %>% 
  summarise(subjects = paste0(subjects, collapse = ","),
            dates = paste0(unique(date[[1]]), collapse = ","),
            coordinates = paste0(unique(coordinates), collapse = ","),
            type = paste0(unique(type), collapse = ",")
  ) %>% 
  mutate(dates = str_remove_all(dates, "[^0-9]")) %>% 
  select(text_id, type, subjects, coordinates, dates)

all_ppl_place_nodes <- rbind(ppl_nodes, place_nodes) %>% 
  filter(text_id != "Appalachia") %>% 
  mutate(id = row_number()) 

### Realized this isn't really a network

# ralphs_network_places <- ppl_point %>% 
#   filter(names == "Rinzler_Ralph") %>% 
#   left_join(all_ppl_place_nodes, by = c("names" = "text_id")) %>% 
#   mutate(from = id) %>% 
#   select(-id) %>% 
#   left_join(all_ppl_place_nodes, by = c("place" = "text_id")) %>% 
#   mutate(to = id) %>% 
#   select(from, names, to, place) %>% 
#   na.omit()
# 
# sfnetwork(nodes = all_ppl_place_nodes, 
#           edges = ralphs_network_places,
#           directed = F)



# create full nodes list for entire DF ------------------------------------

ppl <- rinz %>% 
  select(place, coordinates, names, subjects, date) %>% 
  mutate(names = str_replace_all(names, ",\\s", "_")) %>% 
  separate_longer_delim(cols = names, 
                        delim = ",") %>% 
  distinct(names) %>% 
  mutate(text_id = names) %>% 
  select(-names)

places <- rinz %>% 
  distinct(place) %>% 
  mutate(text_id = place) %>% 
  select(-place)

all_nodes <- rbind(ppl, places) %>% 
  mutate(id = row_number())

# genre networks -- people only -------------------------------------------

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

name_combos_norep <- name_combos %>% 
  distinct(.keep_all = T) %>% 
  mutate(from = as.character(V1),
         to = as.character(V2), 
         subjects = as.character(V3)) %>% 
  select(-starts_with("V"))

name_combos_labeled <- name_combos %>% 
  mutate(from = as.character(V1),
         to = as.character(V2), 
         subjects = as.character(V3)) %>% 
  select(-starts_with("V"))

# define function to construct genre-specific network

genre_net <- function(genre){
  gen_edges_df <- name_combos_labeled %>% 
    filter(str_detect(subjects, regex(genre, ignore_case = T))) %>% 
    left_join(all_nodes, by = c("from" = "text_id")) %>% 
    mutate(from_label = from,
           from = as.character(id)) %>% 
    select(-id) %>% 
    left_join(all_nodes, by = c("to" = "text_id")) %>% 
    mutate(to_label = to, 
           to = as.character(id)) %>% 
    select(from, from_label,
           to, to_label) %>% 
    na.omit() # anything missing an ID has to be removed or else the network functions don't work
  
  gen_nodes_df <- all_nodes %>% 
    filter(id %in% gen_edges_df$from | id %in% gen_edges_df$to) %>% 
    mutate(id = as.character(id))
  
  gen_net <- tbl_graph(nodes = gen_nodes_df,
                       edges = gen_edges_df, 
                       node_key = "id",
                       directed = F)
  
  return(gen_net)
}

blues_net <- genre_net("blues")

## TEST

genre_test <- "blues"

gen_edges_df <- name_combos %>% 
  filter(str_detect(subjects, regex(genre_test, ignore_case = T))) %>% 
  left_join(all_nodes, by = c("from" = "text_id")) %>% 
  mutate(from_label = from,
         from = as.character(id)) %>% 
  select(-id) %>% 
  left_join(all_nodes, by = c("to" = "text_id")) %>% 
  mutate(to_label = to, 
         to = as.character(id)) %>% 
  select(from, from_label,
         to, to_label) %>% 
  na.omit()

gen_nodes_df <- all_nodes %>% 
  filter(id %in% gen_edges_df$from | id %in% gen_edges_df$to) %>% 
  mutate(id = as.character(id))

gen_net_test <- tbl_graph(nodes = gen_nodes_df,
                     edges = gen_edges_df,
                     directed = FALSE,
                     node_key = "id")


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

blues_stats <- net_stats(blues_net)

# Run network stats for big genres 

genres <- c("blues", "bluegrass", "folk", "country", "jazz")

bluegrass_stats <- net_stats(genre_net("bluegrass"))
country_stats <- net_stats(genre_net("country"))
jazz_stats <- net_stats(genre_net("jazz"))
folk_stats <- net_stats(genre_net("folk"))

african_american_stats <- net_stats(genre_net("african american"))
prison_stats <- net_stats(genre_net("prison"))


# do same thing but filterable by place -----------------------------------

collabs_place <- rinz %>% 
  group_by(ID) %>% 
  summarize(places = paste0(place, collapse = ";"),
            names = paste0(unique(names), collapse = ",")) %>% 
  mutate(names = str_replace_all(names, ",\\s", "_")) %>% 
  separate_wider_delim(cols = names, 
                       delim = ",",
                       names_sep = ".",
                       names_repair = "unique",
                       too_few = "align_start") %>% 
  select(starts_with("names"), places) 

name_combos_place <- as.data.frame(matrix(nrow=1, ncol=3))
colnames(name_combos_place) <- c("V1", "V2", "V3")

for(i in 1:nrow(collabs_place)){
  active_list <- collabs_place[i, 1:41]
  active_places <- collabs_place$places[i]
  row_vec <- unique(discard(as.vector(active_list), is.na))
  if(length(row_vec) >= 2){
    active_df <- as.data.frame(CombSet(row_vec, 2, repl=F, ord=F))
    active_df <- active_df %>% 
      mutate(V3 = active_places)
    name_combos_place <- rbind(name_combos_place, active_df)
  }
}

name_combos_place_norep <- name_combos_place %>% 
  distinct(.keep_all = T) %>% 
  mutate(from = as.character(V1),
         to = as.character(V2), 
         places = as.character(V3)) %>% 
  select(-starts_with("V"))

name_combos_place_labeled <- name_combos_place %>% 
  mutate(from = as.character(V1),
         to = as.character(V2), 
         places = as.character(V3)) %>% 
  select(-starts_with("V"))

# define function to construct place-specific network

place_net <- function(place){
  place_edges_df <- name_combos_place_labeled %>% 
    filter(str_detect(places, regex(place, ignore_case = T))) %>% 
    left_join(all_nodes, by = c("from" = "text_id")) %>% 
    mutate(from_label = from,
           from = as.character(id)) %>% 
    select(-id) %>% 
    left_join(all_nodes, by = c("to" = "text_id")) %>% 
    mutate(to_label = to, 
           to = as.character(id)) %>% 
    select(from, from_label,
           to, to_label) %>% 
    na.omit() # anything missing an ID has to be removed or else the network functions don't work
  
  place_nodes_df <- all_nodes %>% 
    filter(id %in% place_edges_df$from | id %in% place_edges_df$to) %>% 
    mutate(id = as.character(id))
  
  place_net <- tbl_graph(nodes = place_nodes_df,
                       edges = place_edges_df, 
                       node_key = "id",
                       directed = F)
  
  return(place_net)
}

carolinas_net <- place_net("carolina")

carolinas_stats <- net_stats(carolinas_net)

# Create data frame of centrality leaders by state

state_central_nodes <- as.data.frame(matrix(nrow = 1000, ncol = 5))
colnames(state_central_nodes) <- c("state", "degree_leader", "between_leader", "eigen_leader", "size")

states_names <- states$name 

for(i in 1:length(states_names)){
  state_central_nodes$state[i] <- states_names[i]
  state_stats <- net_stats(place_net(states_names[i]))
  st_deg <- state_stats %>% 
    arrange(desc(degree))
  st_btwn <- state_stats %>% 
    arrange(desc(between))
  st_eig <- state_stats %>% 
    arrange(desc(eigen))
  state_central_nodes$degree_leader[i] <- st_deg$text_id[1]
  state_central_nodes$between_leader[i] <- st_btwn$text_id[1]
  state_central_nodes$eigen_leader[i] <- st_eig$text_id[1]
  state_central_nodes$size[i] <- nrow(state_stats)
}

state_central_nodes <- state_central_nodes %>% 
  filter(is.na(state) == F)

# create df of centrality leaders by subject tag 

subject_central_nodes <- as.data.frame(matrix(nrow = 1000, ncol = 5))
colnames(subject_central_nodes) <- c("subject", "degree_leader", "between_leader", "eigen_leader", "size")

subjects <- eth_coded %>% 
  filter(subjects != "" & is.na(subjects) == F)

subjects <- subjects$subjects

for(i in 1:length(subjects)){
  subject_central_nodes$subject[i] <- subjects[i]
  subject_stats <- net_stats(genre_net(subjects[i]))
  sub_deg <- subject_stats %>% 
    arrange(desc(degree))
  sub_btwn <- subject_stats %>% 
    arrange(desc(between))
  sub_eig <- subject_stats %>% 
    arrange(desc(eigen))
  subject_central_nodes$degree_leader[i] <- sub_deg$text_id[1]
  subject_central_nodes$between_leader[i] <- sub_btwn$text_id[1]
  subject_central_nodes$eigen_leader[i] <- sub_eig$text_id[1]
  subject_central_nodes$size[i] <- nrow(subject_stats)
}

subject_central_nodes <- subject_central_nodes %>% 
  filter(is.na(subject) == F)


# visualize network of just the most frequent degree leaders --------------

sub_node_leaders <- subject_central_nodes %>% 
  mutate(name = subject) %>% 
  select(-subject)

st_node_leaders <- state_central_nodes %>% 
  mutate(name = state) %>% 
  select(-state)

central_nodes <- rbind(sub_node_leaders, st_node_leaders) 

deg <- central_nodes %>% 
  group_by(degree_leader) %>% 
  summarize(count = n())
  
btwn <- central_nodes %>% 
  group_by(between_leader) %>% 
  summarize(count = n())

eig <- central_nodes %>% 
  group_by(eigen_leader) %>% 
  summarize(count = n())

central_nodes <- deg %>% 
  mutate(deg_count = count) %>% 
  select(-count) %>% 
  full_join(btwn, by = c("degree_leader" = "between_leader")) %>% 
  mutate(btwn_count = count) %>% 
  select(-count) %>% 
  full_join(eig, by = c("degree_leader" = "eigen_leader")) %>% 
  mutate(eig_count = count) %>% 
  select(-count) %>% 
  mutate(total = select(., ends_with("count")) %>% rowSums(na.rm=T))
