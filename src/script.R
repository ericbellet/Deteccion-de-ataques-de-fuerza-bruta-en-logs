library('ProjectTemplate')
library(dplyr)
library(caret)
library(sqldf)
library(arules)
#Usamos la guÌa de estilo de R.

#--------------------------------------VALIDACIÛN CRUZADA---------------------------
#APLICAMOS VALIDACIÛN CRUZADA PERO DEBIDO A QUE EL DATASET POSEE MUCHAS FILAS, TARDA MUCHO
#EN EJECUTARSE SIN EMBARGO AQUI ESTA LA PRUEBA QUE LO HICIMOS.

#Calculamos la cantidad de filas
#n <- dim(df)[1]

#PROBAMOS CON DISTINTOS K E IGUAL SE TARDA BASTANTE.
#Utilizamos 5 folds (PROBAMOS CON K MUY ALTOS)
#folds <- createFolds(1:n, 5)
 
#for (k in 1:5){
#    muestra <- folds[[k]]
#    testData <- df[muestra, ]
#   trainingData <- df[-muestra, ]
#   modelo <- train.kknn(source_ip ~ ., data = trainingData, kmax = 5)
#   prediccion <- predict(modelo, testData[, -5])
#}

#Leemos el .csv.
df <- read.csv("C:/Users/EricBellet/Desktop/Asignacion2/Asignacion2/data/data.csv")  
df2 <- df[, "start_time"]
#Transformamos la columna tiempo.
df["start_time"] <- as.POSIXct(df2, origin="1970-01-01")
#Ordenamos el dataset por tiempo.
df <- arrange(df, source_ip, source_ip)
#Seleccionamos las columnas que nos ayudan a indicar si hay un ataque en el set de logs.
df <- select(df, source_ip, destination_ip, start_time, destination_port, num_packets, num_bytes)

#Obtenemos todos los IP destino.
ipsource <- unique(df$source_ip)

#Inicializamos un vector donde guardaremos los tiempos entre los request.
resul <- c()
#Recorremos todas las IP y generamos subconjuntos.
for (i in ipsource){
#Calculamos la diferencia entre los tiempos de los request.
#El primer valor de la fila le cableamos 0 ya que no hay ninguna diferencia que calcular
#ya que es el primer request.
  resul <- c(resul, 0)
#Generamos le subconjunto de source_ip.
  newdata <- subset(df, source_ip == i, 
               select=c(source_ip, start_time))
#Ordenamos el subconjunto por tiempo para calcular la diferencia entre los request.
  newdata <- arrange(newdata, start_time, source_ip)
#Calculamos la diferencia entre los request.
  x<-diff(newdata$start_time)
#Concatenamos los resultados.
  resul <- c(resul, x)
}
#Transformamos el vector resultante en una columna del dataframe.
df[, "tiempototal"] <- resul

#Categorizamos la columna tiempototal
#Si los request duran menos de 1 minuto entre ellos consideramos que es un tiempo bajo.
#Si los request duran m·s de 1 min y menos de una hora consideramos que el tiempo es medio.
#Si los request duran m·s de 1 hora entre ellos 

df[["tiempototal"]] <- ordered(cut(df[["tiempototal"]],
                                 c(-Inf, 60, 3600, Inf)),
                             labels = c("Bajo", "Medio", "Alto"))

#Transformamos todas las columnas a valores discretos
df$destination_port <- as.factor(df$destination_port)
df$num_packets <- as.factor(df$num_packets)
df$num_bytes <- as.factor(df$num_bytes)

#Seleccionamos las variables que utilizaremos en la matriz de transacciones.
df2 <- select(df, source_ip, destination_ip, destination_port, tiempototal, num_packets, 
              num_bytes)
df2 <- as(df2, "transactions")

#Generamos las reglas.
#Buscamos aquellas reglas que ocurran poco pero con alta confianza.
rules <- apriori(df2,parameter = list(support = 0.0001, confidence = 1.0))
summary(rules)


#Analizamos las reglas que nos interesa.
#Utilizamos el lift > 4.2 ya que la mediana de los lift generados es de 4.2, y nos interesa
#encontrar aquellas reglas que poseen un alto nivel


#Si el lift es 1 o muy cerca de uno significa que la relacion es producto del azar.
#Si el lift es mayor a 1 indica una relaciÛn fuerte y aparecen juntos con m·s frecuencia.
#Como la mediana del lift era de 4.2, tomamos aquellas reglas que tienen la relacion m·s fuerte.

inspect( subset( rules, subset = rhs %pin% "destination_ip=" & lhs %pin% "source_ip=" & 
                   lhs %pin% "destination_port=" & lhs %pin% "tiempototal=Bajo" & 
                   lhs %pin% "num_bytes=" & lhs %pin% "num_packets=" & lift >4.2))


#Guardamos todos los valores del inspect.
probabilidad <- subset( rules, subset = rhs %pin% "destination_ip=" & 
                          lhs %pin% "source_ip=" & lhs %pin% "destination_port=" & 
                          lhs %pin% "tiempototal=Bajo" & lhs %pin% "num_bytes=" & 
                          lhs %pin% "num_packets=" & lift >4.2)


#Si se generÛ una regla o mas con el nivel de confianza 1.
if (length(probabilidad) >= 1){
  print("La probabilidad de que en el conjunto de datos haya un ataque es: 1.0.")
  print("Las reglas fueron generadas con una confianza 1, y un nivel de lift muy alto que
        nos indica que las reglas no se crearon por simple azar.")
  
  
}else{
  print("La probabilidad de que en el conjunto de datos haya un ataque es: 0.0.")
}

