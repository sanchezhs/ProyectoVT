library(curl)
library(tidyjson)
library(dplyr)
library(purrr)
library(tidyverse)
library(ggplot2)

####################################
## Hasta la linea 193 son pruebas ##
####################################
path <- "~/Documentos/LCC/ProyectoVT/Proyecto/Android2/"
nombres_ficheros <- list.files(path)

json <- read_json(paste0(path, "/0001.json"))

tbl <- json_data %>%
  spread_all()
tbl

#json2 <- fromJSON(paste0(path, "/0001.json"))
tipos <- json %>% gather_object %>% json_types %>% count(name, type)
temp <- json %>% enter_object(additional_info) %>%
        gather_object() %>% filter(name == 'androguard') %>% gather_object() 


####################################
## Indicadores de riesgo         ##
####################################
df_risk <- data.frame()
get_risk <- function(x) {
  out <- tryCatch(
    {
      json <- read_json(x)
      risk <- json %>% enter_object(additional_info) %>%
        gather_object() %>% filter(name == 'androguard') %>% gather_object() %>% 
        filter(name.2 == 'RiskIndicator') %>% spread_all() %>% select(starts_with('PERM')) %>% as.data.frame() %>% select(-..JSON)
      colnames(risk) <- colnames(risk) %>% gsub("[A-Z]+[.]", '', .)
      
      df_risk <- rbind(df_risk, risk)
      
    },
    error = function(x) {
      
    }
  )
}

dft_risk <- data.frame()
for (i in nombres_ficheros) {
  #dft_risk <- rbind(dft_risk, get_risk(paste0(path, i)))
  print(get_risk(paste0(path, i)))
}




####################################
## PERMISOS                      ##
####################################
perm <- json %>% enter_object(additional_info) %>%
  gather_object() %>% filter(name == 'androguard') %>% gather_object() %>% 
  filter(name.2 == 'Permissions') %>% gather_object() %>% gather_array()

perm_df <- as.data.frame(perm)
perm_df <- perm_df %>% select(name.3, ..JSON)
colnames(perm_df) <- c('Permiso', 'Tipo')

perm_df <- perm_df[seq(1,nrow(perm_df), 3),]
perm_df$Permiso <-  lapply(perm_df[,1], function(x) gsub("[0-9a-z.]", '', x))
perm_df$Tipo <- lapply(perm_df[,2], function(x) ifelse(x == 'dangerous', 1, 0))

ratio <- perm_df %>% select(Tipo) %>% group_by(Tipo) %>%  summarise(Permisos_peligrosos=n(), Total_permisos=nrow(perm_df)) %>% 
    filter(Tipo==1) %>% select(-Tipo)


get_permisos <- function(x) {
  json_temp <- read_json(x)
  out <- tryCatch(
    {
      perm <- json_temp %>% enter_object(additional_info) %>%
        gather_object() %>% filter(name == 'androguard') %>% gather_object() %>% 
        filter(name.2 == 'Permissions') %>% gather_object() %>% gather_array()
      
      perm_df <- as.data.frame(perm)
      perm_df <- perm_df %>% select(name.3, ..JSON)
      colnames(perm_df) <- c('Permiso', 'Tipo')
      
      perm_df <- perm_df[seq(1,nrow(perm_df), 3),]
      perm_df$Permiso <-  lapply(perm_df[,1], function(x) gsub("[0-9a-z.]", '', x))
      perm_df$Tipo <- lapply(perm_df[,2], function(x) ifelse(x == 'dangerous', 1, 0))
      
      ratio <- perm_df %>% select(Tipo) %>% group_by(Tipo) %>%  summarise(Permisos_peligrosos=n(), Total_permisos=nrow(perm_df)) %>% 
        filter(Tipo==1) %>% select(-Tipo)
      return (ratio)
    },
    error=function(x) {
      df_vacio <- data.frame(
        Permisos_peligrosos = 0,
        Total_permisos = 0
      )
      return (df_vacio)
    }
  )
  return (out)
}

dfp <- data.frame()
for (i in nombres_ficheros) {
  dfp <- rbind(dfp, get_permisos(paste0(path, i)))
}

df_vacio <- data.frame(
  Permisos_peligrosos = 0,
  Total_permisos = 0
)
for (i in 1:2) {
  dfp <- rbind(dfp, df_vacio)  
}



####################################
## SHA256                        ##
####################################

get_sha <- function(x) {
  json <- read_json(x)
  res <- json %>% gather_object() %>% filter(name == 'sha256') %>% as.data.frame()  
  return (res)
}
df_sha <- data.frame()
for (i in nombres_ficheros) {
  df_sha <- rbind(df_sha, get_sha(paste0(path,i)))
}
df_sha <- df_sha %>% select(..JSON)
colnames(df_sha) <- c('sha256')


####################################
## SSDEEP                        ##
####################################
# api de virustotal
# ssdeep is a program for computing Context Triggered Piecewise Hashes. 
# Also called fuzzy hashes, it allows identifying similar files by comparing (via Edit Distance) their hashes.
# ssdeep is a tool for recursive computing and matching of Context Triggered Piecewise Hashing (aka Fuzzy Hashing).
# Fuzzy hashing is a method for comparing similar but not identical files. This tool can be used to compare files like regular hashing does (like md5sum or sha1sum) but it will find similar files with little differences.
# For example, it can be used to identify modified versions of known files even if data has been inserted, modified, or deleted in the new files.

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
View(df)
length(indices)/2 # hay 181 muy parecidos

comparar <- df[148,]
comparar <- rbind(comparar, df[173,])


M <- matrix(0, nrow = nrow(df), ncol = nrow(df))

j <- 1
while(j < length(indices)) {
  M[indices[j], indices[j+1]] <- 1
  j <- j+2
}

G <- graph_from_adjacency_matrix(M, mode = 'undirected')
Isolated = which(degree(G)==0)
G2 = delete.vertices(G, Isolated)
plot(G2, vertex.color='green', edge.curved = .1, vertex.size=20, vertex.label=indices[V(G2)], edge.label=rep(1, length(indices)-1))

library(igraph)
G <- graph_from_adjacency_matrix(M, mode=c('undirected'))
Isolated = which(degree(G)==0)
G2 = delete.vertices(G, Isolated)
layout1 <- layout.fruchterman.reingold(g)
plot(G2, edge.label=rep(1, length(indices)), vertex.color='green', edge.curved = .1)


#E(G2)$pesos <- rep(1, length(E(G2)))
df_deep_csv <- df_deep
df_deep_csv$ssdeep <- vapply(df_deep_csv$ssdeep, paste, collapse = ", ", character(1L))
#write.csv(df_deep_csv, "~/Documentos/LCC/ProyectoVT/Proyecto/analisis_ssdeep.csv",  row.names = FALSE)




####################




####################

## Definitivo a partir de aqui 


df <- df %>% mutate(first_seen=regmatches(first_seen, regexpr('20[0-9]{2}', first_seen))) 

# Archivos que más se han subido
quince_mas_subidos <- df %>% select(first_seen, times_submitted) %>% arrange(desc(times_submitted)) %>% head(15)


# Archivos subidos por año
por_año <- df %>% select(first_seen, times_submitted) %>% group_by(first_seen) %>% summarise(total=sum(times_submitted)) 
por_año <- as.data.frame(por_año)
# En 2019 hubo muchos más archivos subidos
ggplot(por_año, aes(x=first_seen, y=total)) + geom_point() + 
  theme_bw()

# El primero se subio más de 3500 veces
quince_mas_subidos

# Tomamos log para ver los datos a escala
ggplot(data=por_año) + 
  geom_point(aes(x=first_seen, y=log(total))) + 
  theme_bw()

# Numero de positivos por año
positivos_año <- df %>% select(first_seen, positives) %>% group_by(first_seen) %>% summarise(total=sum(positives)) 

# 2021 el año con más numero de positivos sin ser el que más archivos subidos tuvo
ggplot(positivos_año, aes(x=first_seen, y=log(total))) + geom_point() + 
  theme_bw()


df %>% filter(first_seen == '2021') 

df %>% filter(size == 2669106)





