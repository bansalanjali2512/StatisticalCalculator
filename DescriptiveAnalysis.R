DESCRIPTIVEANALYSIS <- function(){
  
  choice <- 'y'
  
  while(choice == 'y'){
    cat('\f')
    cat('\t\t***** DESCRIPTIVE ANALYSIS ******\n\n')
    
    cat('1. MEAN\n')
    cat('2. MEDIAN\n')
    cat('3. MODE\n')
    cat('4. VARIANCE\n')
    cat('5. STANDARD DEVIATION\n')
    cat('6. MEAN ABSOLUTE DEVIATION\n')
    cat('7. RANGE\n')
    cat('8. QUARTILES\n')
    cat('9. INTER QUARTILE RANGE (IQR)\n')
    cat('10. MINIMUM\n')
    cat('11. MAXIMUM\n')
    cat('12. SKEWNESS\n')
    cat('13. KURTOSIS\n')
    cat('14. MOMENTS\n')
    
    tempSubChoice1 <- readline(prompt = "Enter the choice: ")
    subChoice1 <- as.integer(tempSubChoice1)
    
    if(subChoice1 > 0 && subChoice1 < 15){
      
      if(subChoice1 == 1){
        cat('\n\t\tMEAN\n\n')
        INPUT()
        answer <- MEAN(x1)
        cat("Mean is",answer)
      }
      
      if(subChoice1 == 2){
        cat('\n\t\tMEDIAN\n\n')
        INPUT()
        answer <- MEDIAN(x1)
        cat("Median is",answer)
      }
      
      if(subChoice1 == 3){
        cat('\n\t\tMODE\n\n')
        INPUT()
        answer <- MODE(x1)
        cat("MODE is",answer)
      }
      
      if(subChoice1 == 4){
        cat('\n\t\tVARIANCE\n\n')
        INPUT()
        answer <- VARIANCE2(x1)
        cat("Variance is",answer)
      }
      
      if(subChoice1 == 5){
        cat('\n\t\tSTANDARD DEVIATION\n\n')
        INPUT()
        answer <- SD(x1)
        cat("Standard Deviation is",answer)
      }
      
      if(subChoice1 == 6){
        cat('\n\t\tMEAN ABSOLUTE DEVIATION\n\n')
        INPUT()
        answer <- MEANABSDEV(x1)
        cat("Mean Absolute Deviation is",answer)
      }
      
      if(subChoice1 == 7){
        cat('\n\t\tRANGE\n\n')
        INPUT()
        answer <- RANGE(x1)
        cat("Range is",answer)
      }
      
      if(subChoice1 == 8){
        cat('\n\t\tQUARTILES\n\n')
        INPUT()
        answer <- QUARTILES(x1)
        cat("Quartiles are",answer)
      }
      
      if(subChoice1 == 9){
        cat('\n\t\tINTER QUARTILE RANGE\n\n')
        INPUT()
        answer <- IQR(x1)
        cat("Inter Quartile Range is",answer)
      }
      
      if(subChoice1 == 10){
        cat('\n\t\tMINIMUM\n\n')
        INPUT()
        answer <- MINIMUM(x1)
        cat("Minimum is",answer)
      }
      
      if(subChoice1 == 11){
        cat('\n\t\tMAXIMUM\n\n')
        INPUT()
        answer <- MAXIMUM(x1)
        cat("Maximum is",answer)
      }
      
      if(subChoice1 == 12){
        cat('\n\t\tSKEWNESS\n\n')
        INPUT()
        answer <- SKEWNESS(x1)
        cat("Skewness is",answer)
      }
      
      if(subChoice1 == 13){
        cat('\n\t\tKURTOSIS\n\n')
        INPUT()
        answer <- KURTOSIS(x1)
        cat("Kurtosis is",answer)
      }
      
      if(subChoice1 == 14){
        cat('\n\t\tMOMENTS\n\n')
        INPUT()
        answer <- MOMENTS(x1)
        cat("Moments are",answer)
      }
      
    }
    
    else{
      cat('ERROR: Please enter a valid choice.')
    }
    
    choice <- readline(prompt = "Do you want to enter again? ")
    
  }
}
