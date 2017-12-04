USERINPUT <- function(){
  #This function takes the input from command line seperated by enter.
  #x will be a list, which is to be converted into numreric for computation purpose.
  cat("Enter data seperated by enter: ")
  x <<- list()
  index <- 1
  y <- readline("")
  while(y != ''){
    x[index] <<- y
    index <- index + 1
    y <- readline("")    
  }
}

READFROMCSV <- function() {
  #This function reads the CSV file from Local Storage.
  library(data.table)
  fileData <<- fread(file.choose())
  x1 <<- fileData$x1
  x2 <<- fileData$x2
  y <<- fileData$y
}

INPUT <- function() {
  #This function lets the user choose from the option of READFROMCSV or USERINPUT. 
  cat('\f')
  cat('\t\t***** CHOOSE DATA ENTRY METHOD ******\n\n')
  cat('1. READ FROM CSV\n')
  cat('2. COMMAND LINE INPUT\n')
  inputchoice <- readline(prompt = "Enter the choice: ")
  inputchoice <- as.integer(inputchoice)
  
  if(inputchoice == 1){
    READFROMCSV()
  }
  if(inputchoice == 2){
    USERINPUT()
    x1 <<- as.numeric(x)
  }
}
