
# libraries ---------------------------------------------------------------

library(tidyverse)


# load data ---------------------------------------------------------------

rinzler <- read.csv("data/rinzler-full-reconciledcsv.csv")


# fix commas with no spaces -----------------------------------------------

rinzler_clean <- rinzler %>% 
  mutate(subjects = str_replace_all(subjects, ",", ", ")) %>% 
  mutate(names = str_replace_all(names, ", ", "--")) %>% 
  mutate(names = str_replace_all(names, ",", "; ")) %>% 
  mutate(names = str_replace_all(names, "--", ", "))


# set up booleans ---------------------------------------------------------

rinzler_bool <- rinzler_clean %>% 
  mutate(world_music = ifelse(grepl("World music", subjects), T, F)) %>% 
  mutate(blues = ifelse(grepl("Blues", subjects), T, F)) %>% 
  mutate(cajun = ifelse(grepl("Cajun", subjects), T, F))


