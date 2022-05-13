#library(jsonlite)
library(curl)
library(tidyjson)
library(dplyr)
library(purrr)
library(tidyverse)



json_data <- tidyjson::read_json("~/Documentos/LCC/ProyectoVT/Proyecto/Android2/0001.json")


tbl <- json_data %>%
  spread_all()
tbl


path <- "~/Documentos/LCC/ProyectoVT/Proyecto/Android2"
nombres_ficheros <- list.files(path)

j <- 0
df <- data.frame()
for (i in nombres_ficheros[1]) {
  filepath <- file.path(path, paste0(i))
  i <- tidyjson::read_json(filepath)
  nombre <- paste0('f_', j)
  assign(nombre, i)
  tbl <- i %>% spread_all(recursive = TRUE)
  nombre_l <- tbl %>%
    select(scan_date,first_seen,total,size,times_submitted,harmless_votes,malicious_votes,
           positives,submission.submitter_country,additional_info.exiftool.FileType)
  #name <- paste0('l_',j) 
  name <- as.data.frame(nombre_l)
  name <- name[-length(name)]
  assign(paste0('l_',j) , nombre_l)
  
  df <- rbind(df, name)
  
  
  j <- j+1
}


View(df)

plot(df$positives, df$size)

df <- df %>% 
  mutate(Year = substr(df$first_seen, 0, 4))
  