library(curl)
library(tidyjson)
library(dplyr)
library(purrr)
library(tidyverse)
library(ggplot2)
library(igraph)
library(plyr)
source("~/Documentos/LCC/ProyectoVT/Proyecto/r/leer_json.R")


df <- read_csv("~/Documentos/LCC/ProyectoVT/Proyecto/virusTotal.csv")
get_ssdeep <- function(x) {
  json <- read_json(x)
  res <- json %>% gather_object() %>% filter(name == 'ssdeep') %>% as.data.frame()  
  return (res)
}
df_deep <- data.frame()
for (i in nombres_ficheros) {
  df_deep <- rbind(df_deep, get_ssdeep(paste0(path,i)))
}
df_deep <- df_deep %>% select(..JSON)
colnames(df_deep) <- ('ssdeep')

indices <- c()
for (i in 1:nrow(df_deep)) {
  for (j in (i+1):nrow(df_deep)-1) {
    if (adist(df_deep[i,], df_deep[j,])==1) {
      print(i)
      print(j)
      indices <- c(indices, i,j)
      print('-----')
    }
  }
}

## GRAFOS
get_adj_matrix <- function(distancia) {
  
  indices <- c()
  distancias <- c()
  for (i in 1:nrow(df_deep)) {
    for (j in (i+1):nrow(df_deep)) {
      if ( adist(df_deep[i,], df_deep[j,]) <= distancia) {
        indices <- c(indices, i,j)
        distancias <- c(distancias, adist(df_deep[i,], df_deep[j,]))
      }
    }
  }
  
  n <- nrow(df)
  Mat <- matrix(0, nrow = n, ncol = n)
  colnames(Mat) <- c(1:n)
  row.names(Mat) <- c(1:n)
  j <- 1
  while(j < length(indices)) {
    Mat[indices[j], indices[j+1]] <- 1
    j <- j+2
  }
  
  G <- graph_from_adjacency_matrix(Mat, mode = 'undirected')
  G <- set_edge_attr(G, 'dist', value=distancias)
  
  return(G)
  
}

# Grafo grande del .RMD
G <- get_adj_matrix(60)
c <- components(G)
c$membership
biggest <- which.max(c$csize)
vids <- V(G)[c$membership==biggest]
plot(induced_subgraph(G, vids), edge.label=E(G)$dist)

vids2 <- V(G)[c$membership==12]
plot(induced_subgraph(G, vids2), edge.label=E(G)$dist)

vids <- V(G)[c$membership==biggest]
plot(induced_subgraph(G, vids))
plot(induced_subgraph(G, vids),vertex.size = 5, vertex.color = "blue", vertex.frame.color = 'red', vertex.label.cex = .7,  vertex.label = NA, edge.curved = .5, edge.arrow.size = .3, edge.width = .7)


# Análisis de los resultados de los AV del grafo grande
source("~/Documentos/LCC/ProyectoVT/Proyecto/r/leer_resultados.R")
ficheros <- sapply(vids, function(x) paste0(sprintf("%04d", x), '.json') )
df_results <- data.frame()
for (i in ficheros) {
  df_results <- rbind.fill(df_results, get_results(paste0(path,i)))
}
df_results <- df_results %>% select(-..JSON)
colnames(df_results) <- lapply(colnames(df_results), function(x) gsub('.result', '', x))
write.csv(df_results, "~/Documentos/LCC/ProyectoVT/Proyecto/escaneres.csv")

escaneres <- read_csv("~/Documentos/LCC/ProyectoVT/Proyecto/escaneres.csv")

# Quitar columnas enteras NA
escaneres <- escaneres[, colSums(is.na(escaneres)) != nrow(escaneres)]
escaneres <- escaneres %>% select(-...1)
df_results %>% select(McAfee, Fortinet)
View(df_results)




subgrafo <- induced_subgraph(G1, vids1)
pg <- page.rank(subgrafo)

importancia_sorted <- data.frame(
  grado = sort(degree(subgrafo), decreasing = TRUE),
  page_rank = sort(pg$vector, decreasing = TRUE)
)

head(importancia_sorted, 10)
importantes <- importancia_sorted %>% row.names(.) %>% head(10)

plot(induced_subgraph(G1, importantes), edge.label=E(G1))

df_importantes <- subset(df, row.names(df) %in% importantes)
View(df_importantes)

# Comparacion del componente grande de G1
v <- as(vids, 'vector')
df_grafo <- subset(df, row.names(df) %in% v)
View(df_grafo)
library(plotly)

## GRAFICAS INTERACTIVAS DE G1
df_grafo$scan_date <- as.POSIXct(df_grafo$scan_date, format="%Y-%m-%d %H:%M:%S")
df_grafo$first_seen <- as.POSIXct(df_grafo$first_seen, format="%Y-%m-%d %H:%M:%S")

# Positivos, tiempo
p <- ggplot(df_grafo, aes(x=scan_date, y=positives)) +
  geom_line() + ylab('Positivos') + xlab('Tiempo') + theme_bw()
ggplotly(p)

# Tamaño, tiempo
ggplot(df_grafo, aes(x=first_seen, y=size)) +
  geom_line() + ylab('Tamaño') + xlab('Tiempo') + theme_bw()

# Primera vez, positivos
ggplot(df_grafo, aes(x=first_seen, y=positives)) +
  geom_line() + ylab('Positivos') + xlab('Tiempo') + theme_bw()

# Primera vez visto, veces subido
ggplot(df_grafo, aes(x=first_seen, y=times_submitted)) +
  geom_line() + ylab('Times submitted') + xlab('First seen') + theme_bw()

# Veces subido por pais
df_grafo %>% select(times_submitted, submission.submitter_country) %>% group_by(submission.submitter_country) %>% summarise(times_submitted=sum(times_submitted)) %>% 
ggplot(data=., aes(y=log(1+times_submitted))) + 
  geom_bar(mapping = aes(fill=submission.submitter_country, x=submission.submitter_country), stat = 'identity') + 
  ylab('Veces subido') + 
  xlab('País') +
  theme_bw() 


# Positivos por país
df_grafo %>% select(positives, submission.submitter_country) %>% group_by(submission.submitter_country) %>% summarise(positives=sum(positives)) %>% 
  ggplot(data=., aes(y=positives)) + 
  geom_bar(mapping = aes(fill=submission.submitter_country, x=submission.submitter_country), stat = 'identity') + 
  ylab('Positivos') + 
  xlab('País') +
  theme_bw() 
  

colnames(df_grafo)[7] <- c('Pais')






# Grafo pequeño del .RMD
G2 <- get_adj_matrix(10)
c2 <- components(G2)
biggest2 <- which.max(c2$csize)
vids2 <- V(G)[c2$membership==biggest2]
v2 <- as(vids2, 'vector')

compare <- data.frame()
for (i in v2) {
  compare <- rbind(compare, df[i,])
}
View(compare)

compare %>% select(size) %>% unique()
compare %>% select(submission.submitter_country) %>% unique()

times <- compare %>% select(first_seen, scan_date) %>% 
  mutate(first_seen = gsub('20[0-9]{2}-[0-9]+-[0-9]+', '', first_seen), scan_date = gsub('20[0-9]{2}-[0-9]+-[0-9]+', '', scan_date)) 

head(times,1)
tail(times,1)

compare %>% select(positives) %>% lapply(., mean)
compare %>% select(positives) %>% lapply(., sd)

## GRAFICAS INTERACTIVAS DEL GRAFO PEQUEÑO
# Comparacion del componente grande de G1
v2 <- as(vids2, 'vector')
df_grafo2 <- subset(df, row.names(df) %in% v2)
View(df_grafo2)
library(plotly)

df_grafo2$scan_date <- as.POSIXct(df_grafo2$scan_date, format="%Y-%m-%d %H:%M:%S")
df_grafo2$first_seen <- as.POSIXct(df_grafo2$first_seen, format="%Y-%m-%d %H:%M:%S")

p <- ggplot(df_grafo2, aes(x=scan_date, y=positives)) +
  geom_line() + ylab('Positivos') + xlab('Fecha de escáner') + theme_bw()
ggplotly(p)

ggplot(df_grafo2, aes(x=first_seen, y=size)) +
  geom_line() + ylab('Tamaño') + xlab('Primera vez visto') + theme_bw()

ggplot(df_grafo2, aes(x=first_seen, y=positives)) +
  geom_line() + ylab('Positivos') + xlab('Tiempo') + theme_bw()

ggplot(df_grafo2, aes(x=first_seen, y=times_submitted)) +
  geom_line() + ylab('Positivos') + xlab('Tiempo') + theme_bw()

ggplot(data=df_grafo2, aes(y=nrow(df_grafo2))) + 
  geom_bar(mapping = aes(fill=submission.submitter_country, x=submission.submitter_country), stat = 'identity') + 
  theme_bw() 


