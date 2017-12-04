MEAN <- function(x){
  sum <- 0
  n <- length(x)
  for (i in x) {
    sum <- sum + i
  }
  calculatedMean <- sum/n
  return(calculatedMean)
}

VARIANCE <- function(x){
  calculatedMean <- MEAN(x)
  n <- length(x)
  numerator <- 0
  for (i in x) {
    numerator <- (numerator + ((i - calculatedMean)^2))
  }
  calculatedVar <- (numerator / n)
  return(calculatedVar)
}

VARIANCE2 <- function(x){
  #This is sample variance with degree of freedom n-1.
  calculatedMean <- MEAN(x)
  sumOfXiSquares <- 0
  n <- length(x)
  for (i in x) {
    sumOfXiSquares <- (sumOfXiSquares + (i^2))
  }
  calculatedVar <- (sumOfXiSquares - n*(calculatedMean^2))/(n-1)
  return(calculatedVar)
}

SD <- function(x) {
  calculatedVar <- VARIANCE2(x)
  calculatedSD <- sqrt(calculatedVar)
  return (calculatedSD)
}

MEDIAN <- function(x) {
  x <- sort(x)
  n <- length(x)
  calculatedMedian <- 0
  index <- 0
  if((n %% 2) != 0) {
    index <- ((n + 1) / 2)
    calculatedMedian <- x[index] 
  }
  else {
    index <- (n / 2) 
    calculatedMedian <- ((x[index] + x[index + 1]) / 2)
  }
  return(calculatedMedian)
}

MODE <- function(x) {
  uniqueData <<- unique(x)
  count <<- array()
  
  for ( i in 1:(length(uniqueData))) {
    len <- length(grep(uniqueData[i],x))
    count[i] <<- len
  }
  index <<- which(count == max(count))
  
  if( length(index) == 1 ){
    finalMode <- uniqueData[index]
    return(finalMode)
  }
  else {
    return(-1)
  }
}

MEANABSDEV <- function(x) {
  calculatedMean <- MEAN(x)
  n <- length(x)
  calculatedAbsMeanDev <- 0
  for (i in x) {
    if (i > calculatedMean) {
      calculatedAbsMeanDev <- calculatedAbsMeanDev + (i - calculatedMean)
    }
    else {
      calculatedAbsMeanDev <- calculatedAbsMeanDev + (calculatedMean - i)
    }
  }
  return (calculatedAbsMeanDev/n)
}

MINIMUM <- function(x) {
  minValue <- x[1]
  for (i in x) {
    if (i < minValue) {
      minValue <- i
    }
  }
  return (minValue)
}

MAXIMUM <- function(x) {
  maxValue <- x[1]
  for (i in x) {
    if (i > maxValue) {
      maxValue <- i
    }
  }
  return (maxValue)
}

RANGE <- function(x) {
  return (MAXIMUM(x) - MINIMUM(x))
}

QUARTILES <- function(x) {
  x <- sort(x)
  n <- length(x)
  indexQ1 <- (n/4)+1
  indexQ3 <- (3*n/4)+1
  q1 <- x[indexQ1]
  q2 <- MEDIAN(x)
  q3 <- x[indexQ3]
  
  
  calculatedQuartiles <- cbind(q1, q2, q3)
  return (calculatedQuartiles)
}

IQR <- function(x) {
  calculatedQuartiles <- QUARTILES(x)
  q3 <- calculatedQuartiles[3]
  q1 <- calculatedQuartiles[1]
  return (q3 - q1)
}
