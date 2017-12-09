PROBABILITYANALYSIS <- function(){
  
  choice <- 'y'
  
  while(choice == 'y'){
    
    cat('\f')
    cat('\t\t***** PROBABILITY ANALYSIS ******\n\n')
    
    cat('1. PERMUTATION\n')
    cat('2. COMBINATION\n')
    cat('3. BASIC PROBABILITY\n')
    cat('4. CONDITIONAL PROBABILITY\n')
    cat('5. BAYES THEOREM\n')
    
    tempSubChoice3 <- readline(prompt = "Enter the choice: ")
    subChoice3 <- as.integer(tempSubChoice3)
    
    if(subChoice3 > 0 && subChoice3 < 6){
      
      if(subChoice3 == 1){
        cat('\n\t\tPERMUTATION\n\n')
        n <- readline(prompt = "Enter 'n': ")
        nInt <- as.integer(n)
        r <- readline(prompt = "Enter 'r': ")
        rInt <- as.integer(r)
        cat('nPr is ')
        nPr <- PERMUTATION(nInt,rInt)
        cat(nPr)
      }
      
      if(subChoice3 == 2){
        cat('\n\t\tCOMBINATION\n\n')
        n <- readline(prompt = "Enter 'n': ")
        nInt <- as.integer(n)
        r <- readline(prompt = "Enter 'r': ")
        rInt <- as.integer(r)
        cat('nCr is ')
        nCr <- COMBINATION(nInt,rInt)
        cat(nCr)
      }
      
      if(subChoice3 == 3){
        cat('\n\t\tBASIC PROBABILITY\n\n')
        tempFavorable <- readline(prompt = "Enter the favorable outcomes: ")
        favorable <- as.integer(tempFavorable)
        tempTotal <- readline(prompt = "Enter the total possible outcomes: ")
        total <- as.integer(tempTotal)
        cat('Probability is ')
        probability <- BASICPROBABILITY(favorable,total)
        finalAnswer <- formatC(probability,digits = 6,format = "f")
        cat(finalAnswer)
      }
      
      if(subChoice3 == 4){
        cat('\n\t\tCONDITIONAL PROBABILITY\n\n')
        
        tempSampleSpace <- readline(prompt = "Enter the cardinality of sample space: ")
        sampleSpace <- as.integer(tempSampleSpace)
        tempIntersection <- readline(prompt = "Enter the cardinality of A intersection B: ")
        intersection <- as.integer(tempIntersection)
        tempCardinalityA <- readline(prompt = "Enter the cardinality of A: ")
        cardinalityA <- as.integer(tempCardinalityA)
        tempCardinalityB <- readline(prompt = "Enter the cardinality of B: ")
        cardinalityB <- as.integer(tempCardinalityB)
        
        cat('Probability of A given B is ')
        probabiltyAgB <- BASICPROBABILITY(intersection,sampleSpace)/BASICPROBABILITY(cardinalityB,sampleSpace)
        finalAnswerAgB <- formatC(probabiltyAgB,digits = 6,format = "f")
        cat(finalAnswerAgB)
        cat('\n')
        
        cat('Probability of B given A is ')
        probabiltyBgA <- BASICPROBABILITY(intersection,sampleSpace)/BASICPROBABILITY(cardinalityA,sampleSpace)
        finalAnswerBgA <- formatC(probabiltyBgA,digits = 6,format = "f")
        cat(finalAnswerBgA)
        cat('\n')
      }
      
      if(subChoice3 == 5){
        cat('\n\t\tBAYES THEOREM\n\n')
        
        cat("Enter Ai: ")
        USERINPUT()
        Ai <- as.numeric(x)
        
        cat("Enter BAi: ")
        USERINPUT()
        BAi <- as.numeric(x)
        
        tempI <- readline(prompt = "Enter i: ")
        i <- as.numeric(tempI)
        
        cat("Probability is ")
        finalAnswer <- BAYESTHEOREM(Ai,BAi,i)
        final <- formatC(finalAnswer,digits = 6,format = "f")
        cat(final)
        cat('\n')
      }
      
    }
    
    else{
      cat('ERROR: Please enter a valid choice.')
    }
    
    choice <- readline(prompt = "Do you want to enter again? ")
    
  }
}
