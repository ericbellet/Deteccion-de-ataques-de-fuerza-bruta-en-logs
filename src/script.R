library('ProjectTemplate')
library(dplyr)
library(caret)
library(sqldf)
library(arules)

"""
#--------------------------------------VALIDACION CRUZADA---------------------------
#APLICAMOS VALIDACION CRUZADA PERO DEBIDO A QUE EL DATASET POSEE MUCHAS FILAS Y TARDA MUCHO,
#SIN EMBARGO AQUI ESTA LA PRUEBA QUE LO HICIMOS.

 -#Calculamos la cantidad de filas
-n <- dim(df)[1]

#PROBAMOS CON DISTINTOS K E IGUAL SE TARDA BASTANTE.
-#Utilizamos 5 folds
-folds <- createFolds(1:n, 200000)
-  
-for (k in 1:200000){
-    muestra <- folds[[k]]
-    testData <- df[muestra, ]
-    trainingData <- df[-muestra, ]
    modelo <- train.kknn(source_ip ~ ., data = trainingData, kmax = 5)
   prediccion <- predict(modelo, testData[, -200000])
   
    
}

"""

#Leemos el .csv
df <- read.csv("C:/Users/EricBellet/Desktop/Asignacion2/Asignacion2/data/data.csv")  # read csv file 
df2 <- df[,"start_time"]
#Transformamos la columna tiempo
df["start_time"] <- as.POSIXct(df2, origin="1970-01-01")
#Ordenamos el dataset por tiempo
df <- arrange(df, source_ip,source_ip)
#Seleccionamos las columnas que nos ayudan a indicar si hay un ataque en el set de logs.
df <- select(df,source_ip, destination_ip, start_time, destination_port, num_packets, num_bytes)

#Obtenemos todos los IP destino.
ipsource <- unique(df$source_ip)

resul <- c()
for (i in ipsource){
  resul <- c(resul,0)
  newdata <- subset(df, source_ip == i, 
               select=c(source_ip,start_time))
  newdata <- arrange(newdata, start_time,source_ip)
  
  x<-diff(newdata$start_time)
  resul <- c(resul,x)
}
df[, "tiempototal"] <- resul

#Categorizamos la columna tiempototal
df[["tiempototal"]] <- ordered(cut(df[[ "tiempototal"]],
                                 c(-Inf,60,3600,Inf)),
                             labels = c("Bajo", "Medio", "Alto"))

#Transformamos todas las columnas a valores discretos
df$destination_port <- as.factor(df$destination_port)
df$num_packets <- as.factor(df$num_packets)
df$num_bytes <- as.factor(df$num_bytes)

#Seleccionamos las variables que utilizaremos en la matriz de transacciones.
df2 <- select(df,source_ip,destination_ip,destination_port,tiempototal, num_packets, num_bytes)
df2 <- as(df2, "transactions")

#Generamos las reglas.
#Buscamos aquellas reglas que ocurran poco pero con alta confianza.
rules <- apriori(df2,parameter = list(support = 0.0001, confidence = 1.0))
summary(rules)

#Analizamos las reglas que nos interesa.
inspect( subset( rules, subset = rhs %pin% "destination_ip=" & lhs %pin% "source_ip=" & lhs %pin% "destination_port=" & lhs %pin% "tiempototal=Bajo" & lhs %pin% "num_bytes=" & lhs %pin% "num_packets=" & lift >4.2))
probabilidad <- subset( rules, subset = rhs %pin% "destination_ip=" & lhs %pin% "source_ip=" & lhs %pin% "destination_port=" & lhs %pin% "tiempototal=Bajo" & lhs %pin% "num_bytes=" & lhs %pin% "num_packets=" & lift >4.2)

if (length(probabilidad) >= 1){
  print("La probabilidad de que en el conjunto de datos haya un ataque es: 1.0")
  
}else{
  print("La probabilidad de que en el conjunto de datos haya un ataque es: 0.0")
}

