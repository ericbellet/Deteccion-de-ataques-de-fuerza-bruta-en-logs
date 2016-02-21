library('ProjectTemplate')
library(dplyr)
library(caret)
library(sqldf)
library(arules)

"""
#--------------------------------------VALIDACION CRUZADA---------------------------
#NO APLICAMOS VALIDACION CRUZADA DEBIDO A QUE EL DATASET POSEE MUCHAS FILAS Y TARDA MUCHO,
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
df <- arrange(df, start_time,source_ip)
#Seleccionamos las columnas que nos ayudan a indicar si hay un ataque en el set de logs.
df <- select(df,source_ip, destination_ip, start_time, destination_port, num_packets, num_bytes)

#Categorizamos la columna num_packets
df[["num_packets"]] <- ordered(cut(df[[ "num_packets"]],
                                   c(0,median(df[[ "num_packets"]][df[[ "num_packets"]]<1016.098]),median(df[[ "num_packets"]][df[[ "num_packets"]]>1016.098]),Inf)),
                               labels = c("NumBajo", "NumMedio", "NumAlto"))

#Categorizamos la columna num_bytes
df[["num_bytes"]] <- ordered(cut(df[[ "num_bytes"]],
                                 c(0,median(df[[ "num_bytes"]][df[[ "num_bytes"]]<850]),median(df[[ "num_bytes"]][df[[ "num_bytes"]]>850]),Inf)),
                             labels = c("Bajo", "Medio", "Alto"))

#Seleccionamos las variables que utilizaremos en la matriz de transacciones.
df2 <- select(df,source_ip,destination_ip, num_packets, num_bytes)
df2 <- as(df2, "transactions")

#Generamos las reglas.
#Buscamos aquellas reglas que ocurran poco pero con alta confianza.
rules <- apriori(df2,parameter = list(support = 0.001, confidence = 1.0))
summary(rules)

#Analizamos las reglas que nos interesa.
inspect( subset( rules, subset = rhs %pin% "destination_ip=" & lhs %pin% "source_ip=" & lhs %pin% "num_bytes=" & lhs %pin% "num_packets=" & lift >3))
