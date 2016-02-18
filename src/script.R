library('ProjectTemplate')
library(dplyr)
library(caret)
library(kknn)
library(plyr)
library(hash)

#Leemos el .csv
df <- read.csv("C:/Users/EricBellet/Desktop/Asignacion2/Asignacion2/data/data.csv")  # read csv file 
#Modificamos la columna star_time a un formato mas entendible y guardamos la modificacion en el dataframe.
df2 <- df[,"start_time"]
df["start_time"] <- as.POSIXct(df2, origin="1970-01-01")
df <- arrange(df, start_time,source_ip)


#para facilitar que encontremos un ataque nos concentraremos en el puerto del destino, los paquetes y bytes enviados.
test <- select(obj,destination_port, num_packets, num_bytes)
#los datos útiles son: ip, tiempo, número de paquetes y puerto
test <- select(obj,source_ip, start_time, destination_port, num_packets, num_bytes)


#Now viene la parte donde está la magia, el k medias deberían de aplicarlo sobre la cantidad de paquetes
#y así If not, cuenten la cantidad de peticiones por IP y agrupen
sourceReq <- obj %>% group_by(source_ip) %>% count(source_ip,sort=TRUE)
#Y ahí van a obtener 2 grupos (deben indicarselo al kmedias)
#Pero la idea del kmedias es que lo apliquen a los paquetes/solicitudes y a partir de allí todo aquel que entre en un grupo A es porque va a atacar y el que entre en el B no

#k-means
packets <- select (obj,source_ip, num_packets)
plot(packets, pch = 19)
grupos <- kmeans(packets, 2, iter.max = 10) 
plot(packets, pch = 19)
points(grupos$centers, pch = 19, col = "blue", cex = 2)
points(packets, col = grupos$cluster + 1, pch = 19)


#Ahora la probabilidad
#Yo lo que haría es que vería qué tantas Ip están lejos de los centros como para saber cual entraría a cual grupo

