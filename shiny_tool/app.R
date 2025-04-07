# Grid Format

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
library(shinyWidgets)


# load data ---------------------------------------------------------------

nodes <- read.csv("~/Desktop/smithsonian/data/all_nodes.csv")
edges <- st_read("~/Desktop/smithsonian/data/edge_info.shp")
subjects <- read.csv("~/Desktop/smithsonian/data/subjects_latlon.csv")
people <- read.csv("")

# fix linestring error  -----------------------------------------------------------------

edges <- st_cast(edges, to = "LINESTRING")
# net <- sfnetworks::sfnetwork(nodes = nodes, 
#                               edges = edges, 
#                               directed = F)


# set list of subjects ----------------------------------------------------

subjects <- subjects %>% 
  arrange(desc(count))
subjects_choices <- subjects$id

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
                            choices = subjects_choices,
                            selected = "Rinzler_Ralph")
              ),
              fluidRow(
                # OUTPUT HERE
              )
    ), 
    nav_panel("Personnel + Subjects", "Page C content"), 
    ),
  )
)


# Define server logic 
server <- function(input, output) {
  
  # copy edge and node data without SF format
  
  edges_df <- as.data.frame(edges)
  nodes_df <- as.data.frame(nodes)
  
  # filter nodes + edges according to input
  
  edges_filtered <- reactive({
    edges_df %>% 
      filter(to_label == input$subject) 
  })
  
  nodes_filtered <- reactive({
    nodes_df %>% 
      filter(id %in% edges_filtered()$to | id %in% edges_filtered()$from) %>% 
      mutate(is_sub = ifelse(text_id == input$subject, "selected", "other"))
  })
  
  # net <- reactive({tbl_graph(nodes = nodes_filtered(),
  #                  edges = edges_filtered(),
  #                  directed = F)
  #   net
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
  
  # render
  
  output$rinz_network <- renderPlot({ggraph(sfnetwork(nodes = nodes_filtered(),
                                                      edges = edges_filtered(),
                                                      directed = F), 
                                            layout = layout_sf) +
                                      geom_sf(data = basemap) +
                                      geom_edge_density() +
                                      geom_edge_link(width = 0.05) +
                                      geom_node_point(aes(color = is_sub)) +
                                      coord_sf(xlim = c(-180, -30), 
                                               ylim = c(10, 90), 
                                               expand = FALSE) +
                                      scale_color_manual(values = c("selected" = "red", 
                                                                    "other" = "tan"),
                                                         )
  })
  
  # output$rinz_network <- renderVisNetwork({
  #   visNetwork(nodes_filtered(), edges_filtered())
  # })
  
}



# Run the application 
shinyApp(ui = ui, server = server)