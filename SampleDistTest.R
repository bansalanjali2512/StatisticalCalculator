SAMPLEDISTTEST <- function(){
  
  choice <- 'y'
  
  while(choice == 'y') {
    cat('\f')
    cat('\t\t***** SAMPLE DISTRIBUTION TEST STATISTIC ******\n\n')
    cat('1. CHI-SQUARE TEST\n')
    cat('2. STUDENT t-TEST\n')
    cat('3. F-TEST\n')
    cat('4. Z-TEST\n')
    
    tempSubChoice6 <- readline(prompt = "Enter the choice: ")
    subChoice6 <- as.integer(tempSubChoice6)
    
    if(subChoice6 > 0 && subChoice6 < 5){
      if(subChoice6 == 1){
        cat('\n\t\tCHI-SQUARE TEST\n\n')
        tempN <- readline(prompt = "Enter value of n: ")
        n <- as.numeric(tempN)
        
        tempSamVariance <- readline(prompt = "Enter value of sample variance: ")
        samVariance <- as.numeric(tempSamVariance)
        
        tempPopVariance <- readline(prompt = "Enter value of population variance: ")
        popVariance <- as.numeric(tempPopVariance)
        
        tempAlpha <- readline(prompt = "Enter significance level alpha: ")
        alpha <- as.numeric(tempAlpha)
        
        calcChiSq <- CHISQDIST(n,samVariance,popVariance)
        obsChiSq <- qchisq((1-alpha),(n-1))
        
        if(calcChiSq >= obsChiSq){
          cat("Calculated value is",calcChiSq, "and observed is", obsChiSq,'\n')
          cat("Because Calculated is greater than observed, therefore REJECT")
        }
        
        if(calcChiSq < obsChiSq){
          cat("Calculated value is",calcChiSq, "and observed is", obsChiSq,'\n')
          cat("Because Calculated is less than observed, therefore ACCEPT")
        }
        
      }
      
      if(subChoice6 == 2){
        cat('\n\t\tSTUDENT t-TEST\n\n')
        tempN <- readline(prompt = "Enter value of n: ")
        n <- as.numeric(tempN)
        
        tempSamMean <- readline(prompt = "Enter value of sample mean: ")
        samMean <- as.numeric(tempSamMean)
        
        tempPopMean <- readline(prompt = "Enter value of population mean: ")
        popMean <- as.numeric(tempPopMean)
        
        tempSamSD <- readline(prompt = "Enter value of sample standard deviation: ")
        samSD <- as.numeric(tempSamSD)
        
        tempAlpha <- readline(prompt = "Enter significance level alpha: ")
        alpha <- as.numeric(tempAlpha)
        
        calcT <- TDIST(n,samMean,popMean,samSD)
        if(calcT < 0){
          calcT <- -calcT
        }
        
        cat("\nTAIL-TEST\n1. ONE-TAILED\n2. TWO-TAILED\n")
        tailInput <- readline(prompt = "Enter the input: ")
        
        if(tailInput == 1){
          obsT <- qt((1-alpha),(n-1))
        }
        
        if(tailInput == 2){
          obsT <- qt((1-(alpha/2)),(n-1))
        }
        
        if(calcT >= obsT){
          cat("Calculated value is",calcT, "and observed is", obsT,'\n')
          cat("Because Calculated is greater than observed, therefore REJECT")
        }
        
        if(calcT < obsT){
          cat("Calculated value is",calcT, "and observed is", obsT,'\n')
          cat("Because Calculated is less than observed, therefore ACCEPT")
        }
        
      }
      
      if(subChoice6 == 3){
        cat('\n\t\tF TEST\n\n')
        tempN1 <- readline(prompt = "Enter value of n1: ")
        n1 <- as.numeric(tempN1)
        
        tempN2 <- readline(prompt = "Enter value of n2: ")
        n2 <- as.numeric(tempN2)
        
        tempSamVariance1 <- readline(prompt = "Enter value of sample variance1: ")
        samVariance1 <- as.numeric(tempSamVariance1)
        
        tempPopVariance1 <- readline(prompt = "Enter value of population variance1: ")
        popVariance1 <- as.numeric(tempPopVariance1)
        
        tempSamVariance2 <- readline(prompt = "Enter value of sample variance2: ")
        samVariance2 <- as.numeric(tempSamVariance2)
        
        tempPopVariance2 <- readline(prompt = "Enter value of population variance2: ")
        popVariance2 <- as.numeric(tempPopVariance2)
        
        tempAlpha <- readline(prompt = "Enter significance level alpha: ")
        alpha <- as.numeric(tempAlpha)
        
        calcF <- FDIST(samVariance1,popVariance1,samVariance2,popVariance2)
        
        cat("\nTAIL-TEST\n1. ONE-TAILED\n2. TWO-TAILED\n")
        tailInput <- readline(prompt = "Enter the input: ")
        
        if(tailInput == 1){
          obsF <- qf((1-alpha),(n1-1),(n2-1))
        }
        
        if(tailInput == 2){
          obsF <- qf((1-(alpha/2)),(n1-1),(n2-1))
        }
        
        if(calcF >= obsF){
          cat("Calculated value is",calcF, "and observed is", obsF,'\n')
          cat("Because Calculated is greater than observed, therefore REJECT")
        }
        
        if(calcF < obsF){
          cat("Calculated value is",calcF, "and observed is", obsF,'\n')
          cat("Because Calculated is less than observed, therefore ACCEPT")
        }
        
      }
      
      if(subChoice6 == 4){
        cat('\n\t\tZ-TEST\n\n')
        tempN <- readline(prompt = "Enter value of n: ")
        n <- as.numeric(tempN)
        
        tempSamMean <- readline(prompt = "Enter value of sample mean: ")
        samMean <- as.numeric(tempSamMean)
        
        tempPopMean <- readline(prompt = "Enter value of population mean: ")
        popMean <- as.numeric(tempPopMean)
        
        tempPopSD <- readline(prompt = "Enter value of population standard deviation: ")
        PopSD <- as.numeric(tempPopSD)
        
        tempAlpha <- readline(prompt = "Enter significance level alpha: ")
        alpha <- as.numeric(tempAlpha)
        
        calcZ <- ZDIST(samMean,popMean,PopSD,n)
        if(calcZ < 0){
          calcZ <- -calcZ
        }
        
        cat("\nTAIL-TEST\n1. ONE-TAILED\n2. TWO-TAILED\n")
        tailInput <- readline(prompt = "Enter the input: ")
        
        if(tailInput == 1){
          obsZ <- qnorm((1-alpha))
        }
        
        if(tailInput == 2){
          obsZ <- qnorm((1-(alpha/2)))
        }
        
        if(calcZ >= obsZ){
          cat("Calculated value is",calcZ, "and observed is", obsZ,'\n')
          cat("Because Calculated is greater than observed, therefore REJECT")
          pValue <- 1 - pnorm(calcZ)
          cat("\nP-value is",pValue)
        }
        
        if(calcZ < obsZ){
          cat("Calculated value is",calcZ, "and observed is", obsZ,'\n')
          cat("Because Calculated is less than observed, therefore ACCEPT")
          pValue <- 1 - pnorm(calcZ)
          cat("\nP-value is",pValue)
        }
        
      }
    }
    
    else{
      cat('ERROR: Please enter a valid choice.')
    }
    
    choice <- readline(prompt = "Do you want to enter again? ")
    
  }
}
