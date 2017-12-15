CONTINUOUSDISTRIBUTION <- function(){
  
  choice <- 'y'
  while(choice == 'y'){
    cat('\f')
    cat('\t\t***** CONTINOUS DISTRIBUTION FUNCTION ******\n\n')
    cat('1. UNIFORM DISTRIBUTION\n')
    cat('2. NORMAL DISTRIBUTION\n')
    cat('3. BIVARIATE NORMAL DISTRIBUTION\n')
    cat('4. GAMMA DISTRIBUTION\n')
    cat('5. EXPONENTIAL DISTRIBUTION\n')
    
    tempSubChoice5 <- readline(prompt = "Enter the choice: ")
    subChoice5 <- as.integer(tempSubChoice5)
    
    if(subChoice5 > 0 && subChoice5 < 6){
      
      if(subChoice5 == 1){
        cat('\n\t\tUNIFORM CONTINOUS DISTRIBUTION\n\n')
        tempLowX <- readline(prompt = "Enter value of lower limit: ")
        lowX <- as.integer(tempLowX)
        
        tempHighX <- readline(prompt = "Enter value of upper limit: ")
        highX <- as.integer(tempHighX)
        
        tempA <- readline(prompt = "Enter a: ")
        a <- as.integer(tempA)
        
        tempB <- readline(prompt = "Enter b: ")
        b <- as.integer(tempB)
        
        cat('Probability is ')
        answer <- UNICONTINOUS(lowX,highX,a,b)
        finalAnswer <- formatC(answer,digits = 6,format = "f")
        cat(finalAnswer)
      }
      
      if(subChoice5 == 2){
        cat('\n\t\tNORMAL DISTRIBUTION\n\n')
        tempLowX <- readline(prompt = "Enter value of lower limit: ")
        lowX <- as.numeric(tempLowX)
        
        tempHighX <- readline(prompt = "Enter value of upper limit: ")
        highX <- as.numeric(tempHighX)
        
        tempPopMean <- readline(prompt = "Enter value of population mean: ")
        popMean <- as.numeric(tempPopMean)
        
        tempPopVariance <- readline(prompt = "Enter value of population variance: ")
        popVariance <- as.numeric(tempPopVariance)
        
        cat('Probability is ')
        answer <- NORMALDIST(lowX,highX,popMean,popVariance)
        finalAnswer <- formatC(answer,digits = 6,format = "f")
        cat(finalAnswer)
      }
      
      if(subChoice5 == 3){
        #underProcessing
      }
      
      if(subChoice5 == 4){
        cat('\n\t\tGAMMA DISTRIBUTION\n\n')
        tempLowX <- readline(prompt = "Enter value of lower limit: ")
        lowX <- as.numeric(tempLowX)
        
        tempHighX <- readline(prompt = "Enter value of upper limit: ")
        highX <- as.numeric(tempHighX)
      
        tempAlpha <- readline(prompt = "Enter alpha: ")
        alpha <- as.numeric(tempAlpha)
        
        tempBeta <- readline(prompt = "Enter beta: ")
        beta <- as.numeric(tempBeta)
        
        cat('Probability is ')
        answer <- GAMMADIST(lowX,highX,alpha,beta)
        finalAnswer <- formatC(answer,digits = 6,format = "f")
        cat(finalAnswer)
      }
      
      if(subChoice5 == 5){
        cat('\n\t\tEXPONENTIAL DISTRIBUTION\n\n')
        tempLowX <- readline(prompt = "Enter value of lower limit: ")
        lowX <- as.numeric(tempLowX)
        
        tempHighX <- readline(prompt = "Enter value of upper limit: ")
        highX <- as.numeric(tempHighX)
        
        tempLambda <- readline(prompt = "Enter lambda: ")
        lambda <- as.numeric(tempLambda)
        
        cat('Probability is ')
        answer <- GAMMADIST(lowX,highX,1,1/lambda)
        finalAnswer <- formatC(answer,digits = 6,format = "f")
        cat(finalAnswer)
      }
      
    }
    
    else{
      cat('ERROR: Please enter a valid choice.')
    }
    
    choice <- readline(prompt = "Do you want to enter again? ")
    
  }
}
