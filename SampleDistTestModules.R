CHISQDIST <- function(n,samVariance,popVariance){
  return(((n-1)*samVariance)/popVariance)
}

TDIST <- function(n,samMean,popMean,samSD) {
  t <- ((samMean - popMean)/(samSD/(n^(0.5))))
  return(t)
}

FDIST <- function(samVar1,popVar1,samVar2,popVar2){
  return((samVar1/popVar1)/(samVar2/popVar2))
}

ZDIST <- function(samMean,popMean,popSD,n){
  z <- ((samMean - popMean)/(popSD/(n^(0.5))))
  return(z)
}
