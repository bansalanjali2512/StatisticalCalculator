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
