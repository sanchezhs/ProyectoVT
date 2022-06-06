library(tidyjson)
library(tidyverse)
library(plyr)

get_results <- function(json) {
  
  json_data <- tidyjson::read_json(json)
  df_temp <- json_data %>% 
    gather_object() %>%   
    filter(name=='scans') %>% 
    spread_all() %>% 
    gather_object() %>% 
    select(ends_with('result')) %>% 
    .[1,] %>% 
    select(-last_col())
  
  
  return(df_temp) 
}

