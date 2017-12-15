UNICONTINOUS <- function(lowX,highX,a,b){
  if(lowX >= a && highX <= b){
    tempFun <- function(x){
      return(1/(b-a))
    }
    integralValue <- adaptIntegrate(tempFun,lowX,highX)$integral
    return(integralValue)
  }
  
  else if (lowX >= a && highX > b && lowX <= b){
    tempFun <- function(x){
      return(1/(b-a))
    }
    integralValue <- adaptIntegrate(tempFun,lowX,b)$integral
    return(integralValue)
  }
  
  else if (lowX < a && highX <= b && highX >= a){
    tempFun <- function(x){
      return(1/(b-a))
    }
    integralValue <- adaptIntegrate(tempFun,a,highX)$integral
    return(integralValue)
  }
  
  else if (lowX < a && highX > b){
    tempFun <- function(x){
      return(1/(b-a))
    }
    integralValue <- adaptIntegrate(tempFun,a,b)$integral
    return(integralValue)
  }
  
  else if ((lowX < a && highX < a)){
    return(0)
  }
  
  else if ((lowX > b && highX > b)){
    return(0)
  }
  
}

NORMALDIST <- function(lowX,highX,popMean,popVariance){
  tempFun <- function(x){
    numerator <- exp((-0.5)*(((x - popMean)/popVariance)^2))
    denominator <- ((2*pi*popVariance)^(0.5))
    return(numerator/denominator)
  }
  integrateValue <- integrate(tempFun,lowX,highX)$value
  return(integrateValue)
}

myGamma <- function(alpha){
  tempFun <- function(x){
    var <- (x^(alpha-1))*(exp(-x))
    return(var)
  }
  integrateValue <- integrate(tempFun,0,Inf)$value
  return(integrateValue)
}

GAMMADIST <- function(lowX,highX,alpha,beta){
    tempFun <- function(x){
      numerator <- (x^(alpha-1))*(exp(-x/beta))
      denominator <- (beta^alpha)*myGamma(alpha)
      return(numerator/denominator)
    }
    integralValue <- adaptIntegrate(tempFun,lowX,highX)$integral
    return(integralValue)
}
