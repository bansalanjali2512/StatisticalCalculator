VISUALIZATIONS <- function() {
  choice <- 'y'
  
  while(choice == 'y') {
    cat('\f')
    cat('\t\t***** VISUALIZATIONS ******\n\n')
    cat('1. HISTOGRAM\n')
    cat('2. LINE GRAPH\n')
    cat('3. BAR GRAPH\n')
    cat('4. PIE CHART\n')
    cat('5. SCATTER PLOT\n')
    cat('6. BOX PLOT\n')
    cat('7. Q-Q PLOT\n')
    cat('8. STEM LEAF PLOT\n')
    cat('9. PARETO CHART\n')
    
    tempSubChoice9 <- readline(prompt = "Enter the choice: ")
    subChoice9 <- as.integer(tempSubChoice9)
    
    if(subChoice9 > 0 && subChoice9 < 10){
      if(subChoice9 == 1){
        cat('\n\t\tHISTOGRAM\n\n')
        INPUT()
        x1 <- sort(x1)
        HISTOGRAM(x1)
      }  
      
      if(subChoice9 == 2){
        cat('\n\t\tLINE GRAPH\n\n')
        INPUT()
        LINEGRAPH(x1)
      }  
      
      if(subChoice9 == 3){
        cat('\n\t\tBAR GRAPH\n\n')
        INPUT()
        BARGRAPH(x1)
      }  
      
      if(subChoice9 == 4){
        cat('\n\t\tPIE CHART\n\n')
        INPUT()
        PIECHART(x1)
      }  
      
      if(subChoice9 == 5){
        cat('\n\t\tSCATTER PLOT\n\n')
        cat("Choose a CSV File from Local Storage\n")
        READFROMCSV()
        
        SCATTERPLOT(x1,y)
      }  
      
      if(subChoice9 == 6){
        cat('\n\t\tBOX PLOT\n\n')
        INPUT()
        BOXPLOT(x1)
      }  
      
      if(subChoice9 == 7){
        cat('\n\t\tQQ PLOT\n\n')
        cat("Choose a CSV File from Local Storage\n")
        READFROMCSV()
        
        QQPLOT(x1,y)
      }  
      
      if(subChoice9 == 8){
        cat('\n\t\tSTEM LEAF PLOT\n\n')
        INPUT()
        STEMLEAFPLOT(x1)
      }  
      
      if(subChoice9 == 9){
        cat('\n\t\tPARETO CHART\n\n')
        INPUT()
        PARETOCHART(x1)
      }  
      
    }
    
    else{
      cat('ERROR: Please enter a valid choice.')
    }
    
    choice <- readline(prompt = "Do you want to enter again? ")
    
  }
}
