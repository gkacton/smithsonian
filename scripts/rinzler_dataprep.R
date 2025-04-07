
# libraries ---------------------------------------------------------------

library(tidyverse)
library(tidyjson)


# load data ---------------------------------------------------------------

rinzler <- tidyjson::read_json("data/rinzler_full.json",
                               format = "jsonl") %>% 
  as_tbl_json() %>% 
  gather_array()


# expand columns ----------------------------------------------------------

rinz <- rinzler %>% gather_object() 
rinz_wider <- rinz %>% unnest_wider(..JSON) 
rinz_concat <- rinz_wider %>% 
  nest(originations = contains("origination"),
       subjects = contains("subject"),
       extent = contains("extent"),
       geography = contains("geogname"),
       langmaterial = contains("langmaterial"))




