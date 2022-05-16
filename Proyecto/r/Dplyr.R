###EJERCICIOS CON DPLYR


library(dplyr)
library(tidyverse)
virusTotal <- read.csv('~/GitHub/VirusTotal/ProyectoVT/Proyecto/virusTotal.csv')
virusTotal

##Cambiamos los nombre por unos mas legibles
virusTotal <- virusTotal %>%
  rename(country = submission.submitter_country)

virusTotal <- virusTotal %>%
  rename(file_type = additional_info.exiftool.FileType)
##10 archivos mas pesados
virusTotal1 <- virusTotal %>%
  arrange(desc(size)) %>%
  slice(1:10)
virusTotal1


##Ordenar por tipo de archivo

virusTotal2 <- virusTotal %>%
  arrange(file_type)
virusTotal2

## Archivos mas pesados que la media
mediaSize <- mean(virusTotal$size)
mediaSize

virusTotal3 <- virusTotal %>%
  filter(virusTotal$size > mediaSize)%>%
  arrange(size)
virusTotal3

## Primer archivo registrado por fecha

virusTotal4 <- virusTotal %>%
  arrange(first_seen)%>%
  select(first_seen,Year,size,times_submitted,positives,file_type)%>%
  slice(1)
virusTotal4

## Contar archivos por tipo de archivo

virusTotal5 <- virusTotal%>%
  group_by(file_type)%>%
  summarise(
    n = n()
  )
virusTotal5
