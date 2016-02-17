library('ProjectTemplate')
library(dplyr)
library(caret)
library(kknn)
library(plyr)


#Leemos el .csv
df <- read.csv("C:/Users/EricBellet/Desktop/Asignacion2/Asignacion2/data/data.csv")  # read csv file 
#Modificamos la columna star_time a un formato mas entendible y guardamos la modificacion en el dataframe.
df2 <- df[,"start_time"]
valor <- as.POSIXct(df2, origin="1970-01-01")
df["start_time"] <- valorins
df <- arrange(df, start_time,source_ip)

obj <- select(df,source_ip, start_time, destination_port, num_packets, num_bytes)


x <- ddply(df,~source_ip,summarise,NumIpDestination = length(destination_ip))
x <- arrange(x, NumIpDestination)
plot(x, type = "o")

y <- ddply(df, ~source_ip, summarise, NumPuertos = length(destination_port))
y <- arrange(y, NumPuertos)

lx<-length(x$source_ip)
ly<-length(x$source_ip)




destinationport <- unique(df$destination_port)

totalipsource <- length(unique(df$source_ip))
ipsource <- unique(df$source_ip)

for (i in ipsource){
  
  newdata <- subset(df, source_ip == i, 
                  select=c(source_ip,start_time, destination_port, num_packets, num_bytes))
  
  
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
