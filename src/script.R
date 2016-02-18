library('ProjectTemplate')
library(dplyr)
library(caret)
library(kknn)
library(plyr)


#Leemos el .csv
df <- read.csv("C:/Users/EricBellet/Desktop/Asignacion2/Asignacion2/data/data.csv")  # read csv file 
#Modificamos la columna star_time a un formato mas entendible y guardamos la modificacion en el dataframe.
df2 <- df[,"start_time"]
df["start_time"] <- as.POSIXct(df2, origin="1970-01-01")
df <- arrange(df, start_time,source_ip)

obj <- select(df,source_ip, start_time, destination_port, num_packets, num_bytes)


x <- ddply(df,~source_ip,summarise,NumIpDestination = length(destination_ip))
x <- arrange(x, NumIpDestination)
plot(x, type = "o")

y <- ddply(df, ~source_ip, summarise, NumPuertos = length(destination_port))
y <- arrange(y, NumPuertos)

lx<-length(x$source_ip)
ly<-length(x$source_ip)






totalipsource <- length(unique(df$source_ip))
ipsource <- unique(df$source_ip)


for (i in ipsource){
  
  newdata <- subset(df, source_ip == i, 
                  select=c(source_ip,start_time, destination_port, num_packets, num_bytes))
  
  destinationport <- unique(newdata$destination_port)
  
  for (j in destinationport){
    newdata2 <- subset(newdata, destination_port == j, 
                      select=c(source_ip,start_time, destination_port, num_packets, num_bytes))
    
    numpackages <- unique(newdata2$num_packets)
    for (k in numpackages){
      newdata3 <- subset(newdata2, num_packets == k, 
                        select=c(source_ip,start_time, destination_port, num_packets, num_bytes))
  
      numbytes <- unique(newdata3$num_bytes)
      for (h in numbytes){
        newdata4 <- subset(newdata3, num_bytes == h, 
                           select=c(source_ip,start_time, destination_port, num_packets, num_bytes))
        
        starttime <- unique(newdata4$start_time)
        for (m in starttime){
          newdata5 <- subset(newdata4, starttime == m, 
                             select=c(source_ip,start_time, destination_port, num_packets, num_bytes))
         
          p <- 0
          tiempos <- c()
          
          while (p != (length(newdata5$start_time))){
            resultado <- newdata5$start_time[p+1] - newdata5$start_time[p]
            units(resultado) <- "secs"
            tiempos <- c(tiempos,resultado)
            
            p <- p + 1
          }#endwhile
          
        }
        
      }
      
    }
  }
  
  
}

'''
#Calculamos la cantidad de filas
n <- dim(df)[1]

#Utilizamos 5 folds
folds <- createFolds(1:n, 200000)
  
for (k in 1:200000){
    muestra <- folds[[k]]
    testData <- df[muestra, ]
    trainingData <- df[-muestra, ]
    modelo <- train.kknn(source_port ~ ., data = trainingData, kmax = 5)
    prediccion <- predict(modelo, testData[, -200000])
    
    
}

########################################
filter(flights, month == 1, day == 1)
select(flights, year, month, day)
distinct(select(flights, tailnum))
########################################
'''
