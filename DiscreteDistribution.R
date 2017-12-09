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
