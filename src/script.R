library('ProjectTemplate')
library(dplyr)
'''
CalculateSampleCovariance <- function(x, y, verbose = TRUE) {
  # Computes the sample covariance between two vectors.
  #
  # Args:
  #   x: One of two vectors whose sample covariance is to be calculated.
  #   y: The other vector. x and y must have the same length, greater than one,
  #      with no missing values.
  #   verbose: If TRUE, prints sample covariance; if not, not. Default is TRUE.
  #
  # Returns:
  #   The sample covariance between x and y.
  n <- length(x)
  # Error handling
  if (n <= 1 || n != length(y)) {
    stop("Arguments x and y have different lengths: ",
         length(x), " and ", length(y), ".")
  }
  if (TRUE %in% is.na(x) || TRUE %in% is.na(y)) {
    stop(" Arguments x and y must not have missing values.")
  }
  covariance <- var(x, y)
  if (verbose)
    cat("Covariance = ", round(covariance, 4), ".\n", sep = "")
  return(covariance)
}


########################################
filter(flights, month == 1, day == 1)
select(flights, year, month, day)
distinct(select(flights, tailnum))
########################################
'''
#Leemos el .csv
df <- read.csv("C:/Users/EricBellet/Desktop/Asignacion2/Asignacion2/data/data.csv")  # read csv file 
#Modificamos la columna star_time a un formato mas entendible y guardamos la modificacion en el dataframe.
df2 <- df[,"start_time"]
valor <- as.POSIXct(df2, origin="1970-01-01")
df["start_time"] <- valor

