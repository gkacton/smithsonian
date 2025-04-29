# Grid Format

### NOTES FROM 2025-04-07:
### - Next step is to create basically the same interface with the people/places web
### - Data should all be good to go but maybe test and make sure sfnetwork() works with the new data?

# load libraries ----------------------------------------------------------

library(shiny)
library(leaflet)
library(tidyverse)
library(ggraph)
library(sfnetworks)
library(tidygraph)
library(visNetwork)
library(fontawesome)
library(shinythemes)
library(bslib)
library(sf)
library(shinyWidgets)


# load data ---------------------------------------------------------------

nodes <- st_read("~/Desktop/smithsonian/data/all_nodes.shp")
edges <- st_read("~/Desktop/smithsonian/data/edges_combo.shp")
subjects <- read.csv("~/Desktop/smithsonian/data/subjects_latlon.csv")
people_enriched <- st_read("~/Desktop/smithsonian/data/people_enriched.shp")
genre_info <- read.csv("~/Desktop/smithsonian/data/genre_info.csv")
rinz <- read.csv("~/Desktop/smithsonian/data/rinzler-full-reconciled.csv")

# fix linestring error  -----------------------------------------------------------------

edges <- st_cast(edges, to = "LINESTRING")
# net <- sfnetworks::sfnetwork(nodes = nodes, 
#                               edges = edges, 
#                               directed = F)

edges_nongeo <- edges
st_geometry(edges_nongeo) = NULL

edges_nongeo <- edges_nongeo %>% 
  mutate(from = as.character(from),
         to = as.character(to))

nodes <- nodes %>% 
  st_make_valid() %>% 
  mutate(id = as.character(id)) %>% 
  select(-count) %>% 
  na.omit()

rinz_date <- rinz %>% 
  mutate(date = as.numeric(date))

# set lists for choices  ----------------------------------------------------

subjects <- subjects %>% 
  arrange(desc(count))
subjects_choices <- subjects$id

people <- people_enriched %>% 
  arrange(desc(count))

people_choices <- people$names

genre_choices <- c("bluegrass", "blues", "country", "folk", "jazz")


# define UI ---------------------------------------------------------------

ui <- page_fluid(
  navset_tab( 
    nav_panel("Subjects + Places", 
              fluidRow(
                       h3("Choose subject"),
                       pickerInput(
                         inputId = "subject",
                         label = NULL, 
                         choices = subjects_choices,
                         selected = "Americans"
                ),
                fluidRow(
                       # visNetworkOutput("rinz_network")
                       plotOutput("rinz_network")
                )
              )
    ),
    nav_panel("Personnel + Places", 
              fluidRow(
                h3("Choose person"),
                pickerInput(inputId = "person",
                            label = NULL, 
                            choices = people_choices,
                            selected = "Rinzler_Ralph")
              ),
              fluidRow(
                plotOutput("people_network")
              )
    ), 
    nav_panel("Genre", 
              fluidRow(
                h3("Choose a genre"),
                pickerInput(inputId = "genre",
                            label = NULL, 
                            choices = genre_choices,
                            selected = "folk")
              ),
              fluidRow(
                plotOutput("genre_network")
              )
    ),
    nav_panel("Year", 
              fluidRow(
                h3("Pick a year"),
                pickerInput(inputId = "year",
                            label = NULL, 
                            choices = c(1938:1992),
                            selected = "1961")
              ),
              fluidRow(
                plotOutput("year_places")
              )
    )
  )
)


# Define server logic 
server <- function(input, output) {
  
  # filter nodes + edges according to input
  
  edges_filtered <- reactive({
    edges_nongeo %>% 
      filter(to_label == input$subject) %>% 
      mutate(from = as.character(from),
             to = as.character(to))
  })
  
  nodes_filtered <- reactive({
    nodes %>% 
      filter(id %in% edges_filtered()$to | id %in% edges_filtered()$from) %>% 
      mutate(is_sub = ifelse(text_id == input$subject, "selected", "other")) %>% 
      mutate(id = as.character(id))
  })
  
  ### PEOPLE/PLACE
  
  edges_filtered_2 <- reactive({
    edges_nongeo %>% 
      filter(from_label == input$person) %>% 
      mutate(from = as.character(from),
             to = as.character(to))
  })
  
  nodes_filtered_2 <- reactive({
    nodes %>% 
      filter(id %in% edges_filtered_2()$to | id %in% edges_filtered_2()$from) %>% 
      mutate(is_sub = ifelse(text_id == input$person, "selected", "other")) %>% 
      mutate(id = as.character(id))
  })
  
  ### GENRE
  
  edges_filtered_3 <- reactive({
    
    people_list_col <- paste0(input$genre, "_people")
    place_list_col <- paste0(input$genre, "_places")
    people_list <- genre_info[,people_list_col] %>% na.omit()
    place_list <- genre_info[,place_list_col] %>% na.omit()
    edges_nongeo %>% 
      filter(to_type != "subject") %>% 
      filter(from_label %in% people_list |
               to_label %in% people_list |
               from_label %in% place_list |
               to_label %in% people_list) %>% 
      mutate(from = as.character(from),
             to = as.character(to))
  })
  
  nodes_filtered_3 <- reactive({
    nodes %>% 
      filter(id %in% edges_filtered_3()$to | id %in% edges_filtered_3()$from) %>% 
      mutate(id = as.character(id))
  })
  
  #### YEAR
  
  places_filtered_ralph <- reactive({
    
    rinz_date %>% 
      filter(date == as.numeric(input$year)) %>% 
      mutate(names = str_replace_all(names, ",\\s", "_")) %>% 
      filter(str_detect(names, "Rinzler_Ralph")) %>% 
      filter(coordinates != "") %>% 
      separate_wider_delim(coordinates, delim = ",", names = c("lat", "lon")) %>% 
      mutate(lat = as.numeric(lat),
             lon = as.numeric(lon)) %>% 
      st_as_sf(coords = c("lon", "lat"), crs = st_crs(4326))
    
  })
  
  places_filtered_all <- reactive({
    rinz_date %>% 
      filter(date == as.numeric(input$year)) %>% 
      filter(coordinates != "") %>% 
      separate_wider_delim(coordinates, delim = ",", names = c("lat", "lon")) %>% 
      mutate(lat = as.numeric(lat),
             lon = as.numeric(lon)) %>% 
      st_as_sf(coords = c("lon", "lat"), crs = st_crs(4326))
  })
  
  # nodes_filtered_4 <- reactive({
  #   nodes %>% 
  #     filter(id %in% edges_filtered_3()$to | id %in% edges_filtered_3()$from) %>% 
  #     mutate(id = as.character(id))
  # })
    
    # rinz_plot <- ggraph(net_sf, layout = layout_sf) +
    #   geom_sf(data = basemap) +
    #   geom_node_point() +
    #   geom_edge_density()

  # use layout_sf function from Lore Abad
  
  layout_sf = function(graph){
    # Extract X and Y coordinates from the nodes
    graph = activate(graph, "nodes")
    x = sf::st_coordinates(graph)[,"X"]
    y = sf::st_coordinates(graph)[,"Y"]
    data.frame(x, y)
  }

  # basemap
  
  basemap <- rnaturalearthdata::map_units110
  
  base_crs <- st_as_sf(basemap, crs = st_crs(4326))
  # render
  
  output$rinz_network <- renderPlot({ggraph(sfnetwork(nodes = nodes_filtered(),
                                                      edges = edges_filtered(),
                                                      node_key = "id",
                                                      edges_as_lines = TRUE,
                                                      directed = F,
                                                      force = T), 
                                            layout = layout_sf) +
                                      geom_sf(data = basemap) +
                                      geom_edge_density() +
                                      geom_edge_link(width = 0.05) +
                                      geom_node_point(aes(color = is_sub)) +
                                      coord_sf(xlim = c(-180, -30), 
                                               ylim = c(10, 90), 
                                               expand = FALSE) +
                                      scale_color_manual(values = c("selected" = "red", 
                                                                    "other" = "tan"))
  })
  
  output$people_network <- renderPlot({ggraph(sfnetwork(nodes = nodes_filtered_2(),
                                                      edges = edges_filtered_2(),
                                                      node_key = "id",
                                                      edges_as_lines = TRUE,
                                                      directed = F,
                                                      force = T), 
                                            layout = layout_sf) +
      geom_sf(data = basemap) +
      geom_edge_density() +
      geom_edge_link(width = 0.05) +
      geom_node_point(aes(color = is_sub)) +
      coord_sf(xlim = c(-180, -30), 
               ylim = c(10, 90), 
               expand = FALSE) +
      scale_color_manual(values = c("selected" = "red", 
                                    "other" = "tan"))
  })
  
  output$genre_network <- renderPlot({ggraph(sfnetwork(nodes = nodes_filtered_3(),
                                                        edges = edges_filtered_3(),
                                                        node_key = "id",
                                                        edges_as_lines = TRUE,
                                                        directed = F,
                                                        force = T), 
                                              layout = layout_sf) +
      geom_sf(data = basemap) +
      geom_edge_density() +
      # geom_edge_link(width = 0.05) +
      geom_node_point(aes(color = type)) +
      coord_sf(xlim = c(-180, -30), 
               ylim = c(10, 90), 
               expand = FALSE)
  })

  output$year_places <- renderPlot({
    ggplot() +
      geom_sf(data = base_crs) +
      geom_sf(data = places_filtered_all(), color = "brown") +
      geom_sf(data = places_filtered_ralph(), color = "green") 
  })
}



# Run the application 
shinyApp(ui = ui, server = server)