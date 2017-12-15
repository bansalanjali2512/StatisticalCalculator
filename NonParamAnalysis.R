NONPARAMANALYSIS <- function() {
  
  choice <- 'y'
  
  while(choice == 'y'){
    cat('\f')
    cat('\t\t***** NON-PARAMETRIC ANALYSIS ******\n\n')
    cat('1. SIGN TEST\n')
    cat('2. WILCOXON SIGNED-RANK TEST\n')
    cat('3. MANN-WHITNEY TEST\n')
    cat('4. KRUSKAL-WALLIS TEST\n')
    
    
    tempSubChoice8 <- readline(prompt = "Enter the choice: ")
    subChoice8 <- as.integer(tempSubChoice8)
    
    if(subChoice8 > 0 && subChoice8 < 5){
      if(subChoice8 == 1){
        cat('\n\t\tSIGN TEST\n\n')
        USERINPUT()
        x <<- as.numeric(y)
        
        tempPopMean <- readline(prompt = "Enter value of population mean: ")
        popMean <- as.numeric(tempPopMean)
        
        retVector <- SIGNTEST(x,popMean)
        countPlus <- retVector[1]
        finalN <- retVector[2]
        
        samMean <- finalN*0.5
        samSD <- (finalN*0.5*0.5)^(0.5)
        
        tempAlpha <- readline(prompt = "Enter significance level alpha: ")
        alpha <- as.numeric(tempAlpha)
        
        calValue <- qnorm(1-alpha,samMean,samSD)
        
        if(calValue < 0){
          calValue <- -calValue
        }
        
        cat("\nTAIL-TEST\n1. ONE-TAILED\n2. TWO-TAILED\n")
        tailInput <- readline(prompt = "Enter the input: ")
        
        if(tailInput == 1){
          obsZ <- qnorm((1-alpha))
        }
        
        if(tailInput == 2){
          obsZ <- qnorm((1-(alpha/2)))
        }
        
        if(calValue >= obsZ){
          cat("Calculated value is",calValue, "and observed is", obsZ,'\n')
          cat("Because Calculated is greater than observed, therefore REJECT")
          pValue <- 1 - pnorm(calValue)
          cat("\nP-value is",pValue)
        }
        
        if(calValue < obsZ){
          cat("Calculated value is",calValue, "and observed is", obsZ,'\n')
          cat("Because Calculated is less than observed, therefore ACCEPT")
          pValue <- 1 - pnorm(calValue)
          cat("\nP-value is",pValue)
        }
      } 
      
      if(subChoice8 == 2){
        
      } 
      
      if(subChoice8 == 3){
        
      } 
      
      if(subChoice8 == 4){
        
      } 
      
    }
    
    else{
      cat('ERROR: Please enter a valid choice.')
    }
    
    choice <- readline(prompt = "Do you want to enter again? ")
    
  }
}
