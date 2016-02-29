library('ProjectTemplate')
library(dplyr)
library(sqldf)
library(arules)


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

#-------------------------------FUNCION------------------------------------------
#Usamos la guÌa de estilo de R.
myfunction <- function(df){
  # Calcula la probabilidad de que en un dataset de logs haya un ataque.
  #
  # Args:
  #   df: Dataframe de logs.
  #
  # Returns:
  #   Returna un valor entre 0 y 1 que indica la probabilidad de que en 
  #   un dataset de logs haya un ataque.
  
  #Modificamos la columna start_time
  df2 <- df[, "start_time"]
  #Transformamos la columna tiempo.
  df["start_time"] <- as.POSIXct(df2, origin="1970-01-01")
  #Ordenamos el dataset por tiempo.
  df <- arrange(df, source_ip, source_ip)
  #Seleccionamos las columnas que nos ayudan a indicar si hay un ataque en el set de logs.
  df <- select(df, source_ip, destination_ip, start_time, destination_port, num_packets,
               num_bytes)
  
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
    if (nrow(newdata)==1){
      
    }else{
    x<-diff(newdata$start_time)
    #Concatenamos los resultados.
    resul <- c(resul, x)
    }
  }
  #Transformamos el vector resultante en una columna del dataframe.
  df[, "tiempototal"] <- resul
  
  
  #Si los request duran 1 segundo o menos entre ellos consideramos que es un tiempo bajo.
  #Si los request duran m·s de 1 seg consideramos que el tiempo es alto.
 
  
  df[["tiempototal"]] <- ordered(cut(df[["tiempototal"]],
                                     c(-Inf, 1, Inf)),
                                 labels = c("Bajo", "Alto"))
  
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
  
  
  rules <- apriori(df2,parameter = list(support = 0.0001, confidence = 0.0))
  #rules
  summary(rules)
  #rules = rules[quality(rules)$conf < 0.5]
  #Analizamos las reglas que nos interesa.

  
  

  
  
  #-----------------------------------OJO AL COMENTARIO----------------------
  #Segun el databook los ataques se pueden detectar visualizando si un source_ip hizo
  # multiples accesos a un mismo destination_port, enviando muchas veces el mismo numero
  # de paquetes de mismo tamano a un destination_ip. La siguiente regla cumple con todos
  # estos requisitos.
  
  
  probabilidad <- subset( rules, subset = rhs %pin% "destination_ip=" & 
                            lhs %pin% "source_ip=" & lhs %pin% "destination_port=" & 
                            lhs %pin% "tiempototal=Bajo" & lhs %pin% "num_bytes=" & 
                            lhs %pin% "num_packets=" & lift >1)
  
  #Si el lift es 1 o muy cerca de uno significa que la relacion es producto del azar.
  #Si el lift es mayor a 1 indica una relaciÛn fuerte y aparecen juntos con m·s frecuencia.
  
  #En general la confianza es muy alta, ya que la regla que nos interesa cumple con
  #muchos antecedentes, es decir que cuando la encuentra es confiable.
  confianzaAlta <- quality((sort(probabilidad, decreasing = TRUE, 
                                     na.last = NA,by = "confidence",
                                     order = FALSE)[1]))
  
  todosloslift <- quality((sort(probabilidad, decreasing = TRUE, 
                                 na.last = NA,by = "lift",
                                 order = FALSE)))[3]
  
  #Hacemos in inspect de las reglas con los lift mas altos.
  vallifit <- colMeans(todosloslift)
  inspect( subset( rules, subset = rhs %pin% "destination_ip=" & lhs %pin% "source_ip=" & 
                     lhs %pin% "destination_port=" & lhs %pin% "tiempototal=Bajo" & 
                     lhs %pin% "num_bytes=" & lhs %pin% "num_packets=" & lift >vallifit))
  
  soporte <- confianzaAlta[1]
  lift <- confianzaAlta[3]
  confianzaAlta <-confianzaAlta[2]
  
  confianzaBaja <- quality((sort(probabilidad, decreasing = FALSE, 
                                    na.last = NA,by = "confidence",
                                    order = FALSE)[1]))[2]
  
 
  
  OrdenadosPorconfianza <- quality((sort(probabilidad, decreasing = TRUE, 
                                         na.last = NA,by = "confidence",
                                         order = FALSE)[1]))
  soporte <- OrdenadosPorconfianza[1]
  lift <- OrdenadosPorconfianza[3]
  confianzaAlta <-OrdenadosPorconfianza[2]
  
  confianzaBaja <- quality((sort(probabilidad, decreasing = FALSE, 
                                 na.last = NA,by = "confidence",
                                 order = FALSE)[1]))[2]
  
  print(paste0("Confianza m·s alta : ", confianzaAlta))
  print(paste0("Soporte de la regla con m·s confianza: ", soporte))
  print(paste0("Lift de la regla con m·s confianza: ", lift))
  
  #Ya que las reglas generadas son de alta confianza, el soporte es un buen valor para 
  #una probabilidad aproximada.
  return(1-soporte)
}



#Leemos el .csv.
df <- read.csv("C:/Users/EricBellet/Desktop/Asignacion2/Asignacion2/data/data.csv") 

#Genera un subsetting.
#df<-df[1:200000, ]
#Genera un sample
#df <- df[sample(nrow(df), 10), ]

#Llamamos a la funcion. 
probabilidadTotal <- myfunction(df)


print(paste0("La probabilidad de que el conjunto de datos tenga un ataque es: "
             , probabilidadTotal))

