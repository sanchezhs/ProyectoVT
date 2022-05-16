###EJERCICIOS VISUALIZACION


library(ggplot2)
virusTotal <- read.csv('~/GitHub/VirusTotal/ProyectoVT/Proyecto/virusTotal.csv')
virusTotal
##Cambiamos los nombre por unos mas legibles
virusTotal <- virusTotal %>%
  rename(country = submission.submitter_country)

virusTotal <- virusTotal %>%
  rename(file_type = additional_info.exiftool.FileType)
## Aqui vemos el numero de positivos y las veces que se repiten 
ggplot(virusTotal, aes(x=positives)) + geom_histogram(binwidth = 0.5)


## Aqui vemos una grafica de los años
ggplot(virusTotal, aes(x=Year)) + geom_histogram(binwidth = 0.9)


## Ahora visualizaremos lo anterior pero con los colores dependiendo del pais
ggplot(virusTotal, aes(x=positives, fill=country)) + 
  geom_histogram(binwidth = 0.5)
              
ggplot(virusTotal, aes(x=Year, fill=country)) + 
  geom_histogram(binwidth = 0.9)      

## Y respecto al tipo del archivo

ggplot(virusTotal, aes(x=positives, fill=file_type)) + 
  geom_histogram(binwidth = 1)

ggplot(virusTotal, aes(x=Year, fill=file_type)) + 
  geom_histogram(binwidth = 1)

## Y tambien veremos el numero de positivos dependiendo del año

ggplot(virusTotal, aes(x=positives, fill=Year)) + 
  geom_histogram(binwidth = 1)


### GRAFICOS DE PUNTOS

ggplot(data=virusTotal, aes(x=positives, y=Year)) +
  geom_point(aes(colour=file_type), shape=15, size=5)

ggplot(data=virusTotal, aes(x=country, y=positives)) +
  geom_point(aes(colour=file_type), shape=15, size=5)
# De esta ultima grafica sacamos que los archivos tipo DEX solo vienen del pais CZ

ggplot(data=virusTotal, aes(x=country, y=positives)) +
  geom_point(aes(colour=Year), shape=15, size=5)


### GRAFICOS DE BARRAS

ggplot(virusTotal, aes(x=Year, y=positives, fill=file_type)) +
  geom_bar(stat="identity", position="dodge")

ggplot(virusTotal, aes(x=Year, y=positives, fill=file_type)) +
  geom_bar(stat="identity", position="dodge")

ggplot(virusTotal, aes(x=country, y=positives, fill=Year)) +
  geom_bar(stat="identity", position="dodge")
