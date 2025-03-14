
# libraries ---------------------------------------------------------------

library(tidyverse)
library(xml2)
library(XML)

# XML finding aids --------------------------------------------------------

page <- read_xml("AlmaMater.xml")

arch_desc <- page %>% xml_child(search = 2)
dsc <- arch_desc %>% xml_child(search = 10)
cs <- dsc %>% xml_children()

unit_titles <- page %>% xml_find_all("c[level='file']") %>% xml_text()

subseries <- page %>% xml_find_all("//c[@level='subseries']")
subseries_titles <- subseries %>% xml_find_all("./did/unittitle") %>% xml_text()

## File-level data

files <- page %>% xml_find_all("//c[@level='file']") 
titles <- files %>% xml_find_all("did/unittitle") %>% xml_text()

## List approach

list <- XML::xmlToList(xmlParse("AlmaMater.xml"))

dsc <- list[[2]][[10]]

df <- tribble(
  ~series, ~content_list,
  dsc[[1]][[1]][[1]], dsc[[1]][3:37],
  dsc[[2]][[1]][[1]], dsc[[2]][3:length(dsc[[2]])],
  dsc[[3]][[1]][[1]], dsc[[3]][3:length(dsc[[3]])],
  dsc[[4]][[1]][[1]], dsc[[4]][3:length(dsc[[4]])]
)

#df$content_list[1]

df <- unnest_longer(df, content_list)

df <- df %>% filter(content_list_id != ".attrs") %>% select(-content_list_id)

df <- unnest_longer(df, content_list)

df <- df %>% mutate(ifelse())


# rinzler -----------------------------------------------------------------

page <- read_xml("rinzler.xml")

collection <- page %>% xml_child(search = 2)

dsc <- collection %>% xml_child(search = 12)

dsc <- dsc %>% xml_ns_strip()

files <- dsc %>% xml_find_all("//c[@level='item']")

unittitle <- files %>% xml_find_all("//unittitle") %>% xml_text()
access_control <- files %>% xml_find_all("//controlaccess")

df <- xmlToDataFrame(files, colClasses = list("list"),
  nodes = files, homogeneous = F, collectNames = F) 

list <- as_list(files)

df <- as_tibble_col(list) %>% 
  unnest_wider(col = value, names_repair = "unique") 



# analysis ----------------------------------------------------------------

subjects <- df %>% 
  group_by(subject...3) %>% 
  count()

jsonlite::write_json(df, "rinzler_1.json")


