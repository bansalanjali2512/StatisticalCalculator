DISCRETEDISTRIBUTION <- function(){
  
  choice <- 'y'
  
  while(choice == 'y'){
    cat('\f')
    cat('\t\t***** DISCRETE DISTRIBUTION FUNCTION ******\n\n')
    
    cat('1. UNIFORM DISTRIBUTION\n')
    cat('2. BERNOULLI DISTRIBUTION\n')
    cat('3. BINOMIAL DISTRIBUTION\n')
    cat('4. GEOMETRIC DISTRIBUTION\n')
    cat('5. HYPER-GEOMETRIC DISTRIBUTION\n')
    cat('6. NEGATIVE BINOMIAL DISTRIBUTION\n')
    cat('7. POISSON DISTRIBUTION\n')
    cat('8. MULTINOMIAL DISTRIBUTION\n')
    cat('9. MULTIVARIATE HYPERGEOMETRIC DISTRIBUTION\n')
    
    tempSubChoice4 <- readline(prompt = "Enter the choice: ")
    subChoice4 <- as.integer(tempSubChoice4)
    
    if(subChoice4 > 0 && subChoice4 < 10){
      
      if(subChoice4 == 1){
        cat('\n\t\tUNIFORM DISTRIBUTION\n\n')
        tempTotal <- readline(prompt = "Enter the cardinality of total: ")
        total <- as.integer(tempTotal)
        cat('Probability is ')
        probability <- UNIFORMDISTRIBUTION(total)
        finalAnswer <- formatC(probability,digits = 6,format = "f")
        cat(finalAnswer)
      }
      
      if(subChoice4 == 2){
        cat('\n\t\tBERNOULLI DISTRIBUTION\n\n')
        tempX <- readline(prompt = "Enter value of x (0 or 1): ")
        x <- as.integer(tempX)
        tempProbability <- readline(prompt = "Enter value of probability (Between 0 and 1): ")
        probability <- as.numeric(tempProbability)
        cat('Probability is ')
        answer <- BERNOULLI(x,probability)
        finalAnswer <- formatC(answer,digits = 6,format = "f")
        cat(finalAnswer)
      }
      
      if(subChoice4 == 3){
        cat('\n\t\tBINOMIAL DISTRIBUTION\n\n')
        tempN <- readline(prompt = "Enter value of n: ")
        n <- as.integer(tempN)
        tempX <- readline(prompt = "Enter value of x: ")
        x <- as.integer(tempX)
        tempProbability <- readline(prompt = "Enter value of probability (Between 0 and 1): ")
        probability <- as.numeric(tempProbability)
        cat('Probability is ')
        answer <- BINOMIALDISTRIBUTION(n,x,probability)
        finalAnswer <- formatC(answer,digits = 6,format = "f")
        cat(finalAnswer)
      }
      
      if(subChoice4 == 4){
        cat('\n\t\tGEOMETRIC DISTRIBUTION\n\n')
        tempK <- readline(prompt = "Enter value of k: ")
        k <- as.integer(tempK)
        tempProbability <- readline(prompt = "Enter value of probability (Between 0 and 1): ")
        probability <- as.numeric(tempProbability)
        cat('Probability is ')
        answer <- GEOMETRIC(k,probability)
        finalAnswer <- formatC(answer,digits = 6,format = "f")
        cat(finalAnswer)
      }
      
      if(subChoice4 == 5){
        cat('\n\t\tHYPER-GEOMETRIC DISTRIBUTION\n\n')
        tempN <- readline(prompt = "Enter value of N: ")
        N <- as.integer(tempN)
        tempK <- readline(prompt = "Enter value of K: ")
        K <- as.integer(tempK)
        tempn <- readline(prompt = "Enter value of n: ")
        n <- as.integer(tempn)
        tempk <- readline(prompt = "Enter value of k: ")
        k <- as.integer(tempk)
        cat('Probability is ')
        answer <- HYPERGEOMETRIC(N,K,n,k)
        finalAnswer <- formatC(answer,digits = 6,format = "f")
        cat(finalAnswer)
      }
      
      if(subChoice4 == 6){
        cat('\n\t\tNEGATIVE BINOMIAL DISTRIBUTION\n\n')
        tempK <- readline(prompt = "Enter value of k (Number of successes): ")
        k <- as.integer(tempK)
        tempR <- readline(prompt = "Enter value of r (Number of failures): ")
        r <- as.integer(tempR)
        tempProbability <- readline(prompt = "Enter value of probability (Between 0 and 1): ")
        probability <- as.numeric(tempProbability)
        cat('Probability is ')
        answer <- NEGATIVEBIN(k,r,probability)
        finalAnswer <- formatC(answer,digits = 6,format = "f")
        cat(finalAnswer)
      }
      
      if(subChoice4 == 7){
        cat('\n\t\tPOISSON DISTRIBUTION\n\n')
        tempN <- readline(prompt = "Enter value of n: ")
        n <- as.integer(tempN)
        tempProbability <- readline(prompt = "Enter value of probability (Between 0 and 1): ")
        probability <- as.numeric(tempProbability)
        lambda <- n*probability
        cat('Lambda is ',lambda)
        tempK <- readline(prompt = "Enter value of k: ")
        k <- as.integer(tempK)
        cat('Probability is ')
        answer <- POISSON(lambda,k)
        finalAnswer <- formatC(answer,digits = 6,format = "f")
        cat(finalAnswer)
      }
      
      if(subChoice4 == 8){
        cat('\n\t\tMULTINOMIAL DISTRIBUTION\n\n')
        tempN <- readline(prompt = "Enter value of n: ")
        n <- as.integer(tempN)
        tempX1 <- readline(prompt = "Enter value of x1: ")
        x1 <- as.integer(tempX1)
        tempX2 <- readline(prompt = "Enter value of x2: ")
        x2 <- as.integer(tempX2)
        tempX3 <- readline(prompt = "Enter value of x3: ")
        x3 <- as.integer(tempX3)
        
        tempP1 <- readline(prompt = "Enter value of probability p1 (Between 0 and 1): ")
        p1 <- as.numeric(tempP1)
        tempP2 <- readline(prompt = "Enter value of probability p2 (Between 0 and 1): ")
        p2 <- as.numeric(tempP2)
        tempP3 <- readline(prompt = "Enter value of probability p3 (Between 0 and 1): ")
        p3 <- as.numeric(tempP3)
        
        cat('Probability is ')
        answer <- MULTINOMIAL(x1,x2,x3,n,p1,p2,p3)
        finalAnswer <- formatC(answer,digits = 6,format = "f")
        cat(finalAnswer)
      }
      
      if(subChoice4 == 9){
        cat('\n\t\tMULTIPLE HYPER-GEOMETRIC DISTRIBUTION\n\n')
        tempX1 <- readline(prompt = "Enter value of x1: ")
        x1 <- as.integer(tempX1)
        tempX2 <- readline(prompt = "Enter value of x2: ")
        x2 <- as.integer(tempX2)
        tempX3 <- readline(prompt = "Enter value of x3: ")
        x3 <- as.integer(tempX3)
        tempX4 <- readline(prompt = "Enter value of x4: ")
        x4 <- as.integer(tempX4)
        
        tempM1 <- readline(prompt = "Enter value of M1: ")
        M1 <- as.integer(tempM1)
        tempM2 <- readline(prompt = "Enter value of M2: ")
        M2 <- as.integer(tempM2)
        tempM3 <- readline(prompt = "Enter value of M3: ")
        M3 <- as.integer(tempM3)
        tempM4 <- readline(prompt = "Enter value of M4: ")
        M4 <- as.integer(tempM4)
        
        cat('Probability is ')
        answer <- MULTIHYPGEO(x1,x2,x3,x4,M1,M2,M3,M4)
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
