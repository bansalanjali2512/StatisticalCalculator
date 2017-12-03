FACTORIAL <- function(n){
  #basic Factorial function.
  if(n == 0 || n == 1){
    return(1)
  }
  else{
    product <- 1
    for(i in 1:n){
      product <- product*i
    }
    return(product)
  }
}

FACTORIAL(5)
