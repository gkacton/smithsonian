# libraries ---------------------------------------------------------------

library(tidyverse)
library(xml2)
library(XML)


# read XML ----------------------------------------------------------------

page <- read_xml("Rinzler_full.xml")

page_nsStrip <- page %>% xml_ns_strip()

files <- page_nsStrip %>% xml_find_all("//c[@level='series']")
audio <- files[[9]]

audio_list <- as_list(audio)

subseries_1 <- audio %>% xml_find_all("//c[@id='ref8163']")
subseries_2 <- audio %>% xml_find_all("//c[@id='ref5222']")

make_table <- function(subseries){
  items <- subseries %>% xml_find_all("//c[@level='item']")
  list <- as_list(items)
  df <- as_tibble_col(list) %>%
    unnest_wider(col = value, names_repair = "unique")
  df
}

list_2 <- subseries_2 %>% as_list()
list_2 <- list_2[[1]][2:3] 
df_2 <- as_tibble_col(list_2) %>% 
  unnest_longer(col = value, names_repair = "unique") %>% 
  filter(value_id != "did") %>% 
  unnest_wider(col = value, names_repair = "unique")

df_1_reg <- df_1 %>% select(did, controlaccess)
df_2_reg <- df_2 %>% select(did, controlaccess)

rinzler_full_df <- rbind(df_1_reg, df_2_reg)

jsonlite::write_json(rinzler_full_df, "rinzler_full.json")
