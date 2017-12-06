PREDANALYSIS <- function(){
  
  choice <- 'y'
  
  while(choice == 'y'){
    
    cat('\f')
    cat('\t\t***** PREDICTIVE ANALYSIS ******\n\n')
    
    cat('1. CORRELATION\n')
    cat('2. MULTIPLE LINEAR REGRESSION\n')
    
    tempSubChoice2 <- readline(prompt = "Enter the choice: ")
    subChoice2 <- as.integer(tempSubChoice2)
    
    if(subChoice2 > 0 && subChoice2 < 3){
      if(subChoice2 == 1){
        cat('\n\t\tCORRELATION\n\n')
        cat("Choose a CSV File from Local Storage\n")
        READFROMCSV()
        r <- CORRELATION(x1,y)
        cat("Correlation Coefficient is ",r)
      } 
      
      if(subChoice2 == 2){
        cat('\n\t\tMULTIPLE LINEAR REGRESSION\n\n')
        cat("Choose a CSV File from Local Storage\n")
        READFROMCSV()
        X <- MULTREG(x1,x2,y)
        b0 <- X[1,1]
        b1 <- X[2,1]
        b2 <- X[3,1]
        cat("Intercept coefficient is",b0,'\n')
        cat("First slope coefficient is",b1,'\n')
        cat("Second slope coefficient is",b2,'\n')
        
      } 
    }
    
    else{
      cat('ERROR: Please enter a valid choice.')
    }
    
    choice <- readline(prompt = "Do you want to enter again? ")
    
  }
}
