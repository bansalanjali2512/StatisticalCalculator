CORRELATION <- function(x,y){
  #Returns the value of Karl's Pearson of Correlation Coefficient.
  n <- length(x)
  
  sumX <- 0
  for(i in 1:n){
    sumX <- sumX + x[i]
  }
  
  sumY <- 0
  for(i in 1:n){
    sumY <- sumY + y[i]
  }
  
  prodXY <- 0
  for(i in 1:n){
    prodXY <- prodXY + x[i]*y[i]
  }
  
  sumXSq <- 0
  for(i in 1:n){
    sumXSq <- sumXSq + x[i]*x[i]
  }
  
  sumYSq <- 0
  for(i in 1:n){
    sumYSq <- sumYSq + y[i]*y[i]
  }
  
  numerator <- ((n*prodXY) - (sumX*sumY))
  denominator <- ((((n*sumXSq) - (sumX^2))*((n*sumYSq) - (sumY^2)))^(0.5))
  r <- numerator/denominator
  return(r)
  
}
