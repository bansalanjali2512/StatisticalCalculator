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
