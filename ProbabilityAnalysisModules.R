PERMUTATION <- function(n,r){
  #Assumptions  : Works well with valid inputs.
  #Valid Inputs : n can take positive value greater than or equal to 1.
  #               r can takes value less than or equal to n.
  #Returns nPr
  numerator <- FACTORIAL(n)
  denominator <- FACTORIAL(n-r)
  nPr <- numerator/denominator
  return(nPr)
}


COMBINATION <- function(n,r){
  #Assumptions  : Works well with valid inputs.
  #Valid Inputs : n can take positive value greater than or equal to 1.
  #               r can takes value less than or equal to n.
  #Returns nCr
  numerator <- FACTORIAL(n)
  denominator <- FACTORIAL(r)*FACTORIAL(n-r)
  nCr <- numerator/denominator
  return(nCr)
}


BASICPROBABILITY <- function(favorable,total){
  #Assumptions : Works well for valid input i.e, favorable <= total and both positive.
  probability <- favorable/total
  return(probability)
}


BAYESTHEOREM <- function(pofAi, pofBAi, i) {
  len <- length(pofAi)
  numerator <- as.numeric(pofAi[i])*as.numeric(pofBAi[i])
  denominator <- 0
  
  for(j in 1:len){
    product <- as.numeric(pofAi[j])*as.numeric(pofBAi[j])
    denominator <- denominator + product
  }
  
  result <- (numerator/denominator)
  pofAiB <- formatC(result, digits = 6, format = "f")
  return(pofAiB)
}
