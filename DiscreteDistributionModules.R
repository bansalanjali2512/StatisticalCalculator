UNIFORMDISTRIBUTION <- function(total){
  #Assumptions : total >= 1
  probability <- 1/total
  return(probability)
}


BERNOULLI <- function(x,p){
  #Assumptions  : Works well with valid inputs.
  #Valid Inputs : x can take values either 0 or 1.
  #               p can takes value in range 0-1, both inclusive.
  probability <- (p**x)*((1-p)**(1-x))
  return(probability)
}


BINOMIALDISTRIBUTION <- function(n,x,p){
  #Assumptions  : Works well with valid inputs.
  #Valid Inputs : n can take positive value greater than or equal to 1.
  #               x can takes value less than or equal to n. 
  #               p can takes value in range 0-1, both inclusive.
  probability <- COMBINATION(n,x)*(p**x)*((1-p)**(n-x))
  return(probability)
} 


GEOMETRIC <- function(k,p){
  #Assumptions  : Works well with valid inputs.
  #Valid Inputs : k is Number of trials, greater than or equal to 1.
  #               p is probability of success, ranging between 0-1, both inclusive.
  
  probability <- ((1-p)**(k-1))*p
  return(probability)
}


HYPERGEOMETRIC <- function(N,K,n,k){
  #Assumptions  : Works well with valid inputs.
  #Valid Inputs : N greater than or equal to n.
  #               K greater than or equal to k.
  #               N-K greater than or equal to n-k.
  
  #N : Population size.
  #K : Number of success states.
  #n : Number of draws.
  #k : Number of observed successes.
  probability <- (COMBINATION(K,k)*COMBINATION((N-K),(n-k)))/COMBINATION(N,n)
  return(probability)
}


NEGATIVEBIN <- function(k,r,p){
  #Assumptions  : Works well with valid inputs.
  #Valid Inputs : k+r-1 greater than or equal to k (Number of successes).
  #               p is probability ranging from 0-1, both inclusive.
  
  #k : Number of successes.
  #r : Number of failures.
  #p : Probability.
  
  probability <- COMBINATION((k+r-1),k)*(p**k)*((1-p)**r)
  return(probability)
}


POISSON <- function(lambda,k){
  #Assumptions  : Works well with valid inputs.
  #Valid Inputs : k is a whole number.

  e <- 2.718281
  probability <- ((e**(-lambda))*(lambda**k))/FACTORIAL(k)
  return(probability)
}


MULTINOMIAL <- function(x1,x2,x3,n,p1,p2,p3){
  #Assumptions  : Works well with valid inputs and for three random variables only.
  #Valid Inputs : n is a natural number, also n should be x1+x2+x3
  #               xi's should be positive.
  #               pi's should be in range 0-1, both inclsuive.
  
  
  facN <- FACTORIAL(n)
  facx1 <- FACTORIAL(x1)
  facx2 <- FACTORIAL(x2)
  facx3 <- FACTORIAL(x3)
  probability <- (facN/(facx1*facx2*facx3))*(p1**x1)*(p2**x2)*(p3**x3)
  return(probability)
}


MULTIHYPGEO <- function(x1,x2,x3,x4,M1,M2,M3,M4){
  #Assumptions  : Works well with valid inputs and for four random variables only.
  #Valid Inputs : xi's should be positive.
  #               Mi's should be positive.
  
  N <- M1+M2+M3+M4
  n <- x1+x2+x3+x4
  numerator <- COMBINATION(M1,x1)*COMBINATION(M2,x2)*COMBINATION(M3,x3)*COMBINATION(M4,x4)
  probability <- numerator/COMBINATION(N,n)
  return(probability)
}
