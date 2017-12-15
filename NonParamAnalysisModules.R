SIGNTEST <- function(x,popMean){
  n <- length(x)
  finalN <- n
  countPlus <- 0
  for (i in 1:n){
    if(x[i] > popMean){
      countPlus <- countPlus + 1
    }
    
    if(x[i] == popMean){
      finalN <- finalN - 1
    }
  }
  retValues <- c(countPlus,finalN)
  return(retValues)
}
