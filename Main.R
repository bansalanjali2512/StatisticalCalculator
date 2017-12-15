#MAIN_MENU
MAIN <- function(){
  choice <- 'y'
  while(choice == 'y'){
    cat('\f')
    cat('\t\t***** STATISTICS CALCULATOR ******\n\n')
    cat('1. DESCRIPTIVE ANALYSIS\n')
    cat('2. PREDICTIVE ANALYSIS\n')
    cat('3. PROBABILITY ANALYSIS\n')
    cat('4. DISCRETE DISTRIBUTION FUNCTION\n')
    cat('5. CONTINOUS DISTRIBUTION FUNCTION\n')
    cat('6. SAMPLE DISTRIBUTION TEST STATISTIC\n')
    cat('7. INTERVAL ESTIMATION\n')
    cat('8. NON-PARAMETRIC ANALYSIS\n')
    cat('9. VISUALIZATIONS\n')
    
    tempMainMenuChoice <- readline(prompt = "Enter the choice: ")
    mainMenuChoice <- as.integer(tempMainMenuChoice)
    
    if(mainMenuChoice > 0 && mainMenuChoice < 10){
      
      if(mainMenuChoice == 1){
        DESCRIPTIVEANALYSIS()
      }
      
      if(mainMenuChoice == 2){
        PREDANALYSIS()
      }
      
      if(mainMenuChoice == 3){
        PROBABILITYANALYSIS()
      }
      
      if(mainMenuChoice == 4){
        DISCRETEDISTRIBUTION()
      }
      
      if(mainMenuChoice == 5){
        CONTINOUSDISTRIBUTION()
      }
      
      if(mainMenuChoice == 6){
        SAMPLEDISTTEST()
      }
      
      if(mainMenuChoice == 7){
        INTERVALESTIMATION()
      }
      
      if(mainMenuChoice == 8){
        NONPARAMANALYSIS()
      }
      
      if(mainMenuChoice == 9){
        VISUALIZATIONS()
      }
    }
    
    else{
      cat('ERROR: Please enter a valid choice.')
    }
      
    choice <- readline(prompt = "Do you want to go to main menu? ")
  }
}

cat('\f')
MAIN()
