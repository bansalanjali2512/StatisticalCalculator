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


MULTREG <- function(x1,x2,y){
  #This is Multiple Linear Regression.
  #Assumptions : Works for 3 varibles only, 2 independent and one dependent.
  A <- matrix(nrow = 3,ncol = 3)
  A[1,1] <- length(y)
  A[1,2] <- sum(x1)
  A[1,3] <- sum(x2)
  A[2,1] <- sum(x1)
  A[2,2] <- sum(x1^2)
  A[2,3] <- sum(x1*x2)
  A[3,1] <- sum(x2)
  A[3,2] <- sum(x2*x1)
  A[3,3] <- sum(x2^2)
  
  B <- matrix(nrow = 3,ncol = 1)
  B[1,1] <- sum(y)
  B[2,1] <- sum(x1*y)
  B[3,1] <- sum(x2*y)
  
  X <- solve(A,B)
  return(X)
}
