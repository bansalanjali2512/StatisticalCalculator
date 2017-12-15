INTERVALESTIMATION <- function(){
  choice <- 'y'
  
  while(choice == 'y') {
    cat('\f')
    cat('\t\t***** INTERVAL ESTIMATIONS ******\n\n')
    cat('1. ESTIMATION OF MEANS\n')
    cat('2. ESTIMATION OF DIFFERENCE IN MEANS\n')
    cat('3. ESTIMATION OF PROPORTIONS\n')
    cat('4. ESTIMATION OF DIFFERENCE IN PROPORTIONS\n')
    cat('5. ESTIMATION OF VARIANCES\n')
    cat('6. ESTIMATION OF RATIO OF TWO VARIANCES\n')
    
    tempSubChoice7 <- readline(prompt = "Enter the choice: ")
    subChoice7 <- as.integer(tempSubChoice7)
    
    if(subChoice7 > 0 && subChoice7 < 7){
      if(subChoice7 == 1){
        cat('\n\t\tESTIMATION OF MEANS\n\n')
        tempN <- readline(prompt = "Enter value of n: ")
        n <- as.numeric(tempN)
        
        tempSamMean <- readline(prompt = "Enter value of sample mean: ")
        samMean <- as.numeric(tempSamMean)
        
        if(n >= 30){
          tempPopSD <- readline(prompt = "Enter value of population standard deviation: ")
          popSD <- as.numeric(tempPopSD)
        }
        
        if(n < 30){
          tempSamSD <- readline(prompt = "Enter value of sample standard deviation: ")
          samSD <- as.numeric(tempSamSD)
        }
        
        tempAlpha <- readline(prompt = "Enter significance level alpha: ")
        alpha <- as.numeric(tempAlpha)
        
        if(n >=30 ){
          zValue <- qnorm(1-(alpha/2))
        
          lowLimit <- (samMean - (zValue*((popSD*popSD/n)^(0.5))))
          upperLimit <- (samMean + (zValue*((popSD*popSD/n)^(0.5))))
        
          cat((1-alpha)*100,"% confidence limits are",lowLimit,"and",upperLimit)
        }
        
        if(n < 30){
          tValue <- qt((1-(alpha/2)),(n-1))
          
          lowLimit <- (samMean - (tValue*((samSD*samSD/n)^(0.5))))
          upperLimit <- (samMean + (tValue*((samSD*samSD/n)^(0.5))))
          
          cat((1-alpha)*100,"% confidence limits are",lowLimit,"and",upperLimit)
        }
      }  
      
      if(subChoice7 == 2){
        cat('\n\t\tESTIMATION OF DIFFERENCE MEANS\n\n')
        tempN1 <- readline(prompt = "Enter value of n1: ")
        n1 <- as.numeric(tempN1)
        
        tempN2 <- readline(prompt = "Enter value of n2: ")
        n2 <- as.numeric(tempN2)
        
        tempSamMean1 <- readline(prompt = "Enter value of sample mean1: ")
        samMean1 <- as.numeric(tempSamMean1)
        
        tempSamMean2 <- readline(prompt = "Enter value of sample mean2: ")
        samMean2 <- as.numeric(tempSamMean2)
        
        if(n1 >= 30){
          tempPopVar1 <- readline(prompt = "Enter value of population variance1: ")
          popVar1 <- as.numeric(tempPopVar1)
        }
        
        if(n2 >= 30){
          tempPopVar2 <- readline(prompt = "Enter value of population variance2: ")
          popVar2 <- as.numeric(tempPopVar2)
        }
        
        if(n1 < 30 && n2 < 30){
          tempSamVar1 <- readline(prompt = "Enter value of sample variance1: ")
          samVar1 <- as.numeric(tempSamVar1)
          
          tempSamVar2 <- readline(prompt = "Enter value of sample variance2: ")
          samVar2 <- as.numeric(tempSamVar2)
          
          poolSamVar <- (((n1-1)*samVar1)+((n2-1)*samVar2))/(n1+n2-2)
          
        }
        
        tempAlpha <- readline(prompt = "Enter significance level alpha: ")
        alpha <- as.numeric(tempAlpha)
        
        if(n1 >= 30 && n2 >= 30){
          zValue <- qnorm(1-(alpha/2))
          
          meanDifference <- samMean1 - samMean2
          varianceFactor <- (popVar1/n1)+(popVar2/n2)
          lowLimit <- (meanDifference - (zValue*((varianceFactor)^(0.5))))
          upperLimit <- (meanDifference + (zValue*((varianceFactor)^(0.5))))
          
          cat((1-alpha)*100,"% confidence limits are",lowLimit,"and",upperLimit)
        }
        
        if(n1 < 30 && n2 < 30){
          tValue <- qt((1-(alpha/2)),(n1+n2-2))
          
          meanDifference <- samMean1 - samMean2
          varianceFactor <- (poolSamVar/n1)+(poolSamVar/n2)
          lowLimit <- (meanDifference - (tValue*(varianceFactor^(0.5))))
          upperLimit <- (meanDifference + (tValue*(varianceFactor^(0.5))))
          
          cat((1-alpha)*100,"% confidence limits are",lowLimit,"and",upperLimit)
        }
      }  
      
      if(subChoice7 == 3){
        cat('\n\t\tESTIMATION OF PROPORTIONS\n\n')
        tempX <- readline(prompt = "Enter value of x: ")
        x <- as.numeric(tempX)
        
        tempN <- readline(prompt = "Enter value of n: ")
        n <- as.numeric(tempN)
        
        tempAlpha <- readline(prompt = "Enter significance level alpha: ")
        alpha <- as.numeric(tempAlpha)
        
        zValue <- qnorm(1-(alpha/2))
        p <- x/n
        
        lowLimit <- (p - zValue*(((p*(1-p))/n)^(0.5)))
        upperLimit <- (p + zValue*(((p*(1-p))/n)^(0.5)))
        cat((1-alpha)*100,"% confidence limits are",lowLimit,"and",upperLimit) 
        
      }  
      
      if(subChoice7 == 4){
        cat('\n\t\tESTIMATION IN DIFFERENCE OF PROPORTIONS\n\n')
        tempX1 <- readline(prompt = "Enter value of x1: ")
        x1 <- as.numeric(tempX1)
        
        tempN1 <- readline(prompt = "Enter value of n1: ")
        n1 <- as.numeric(tempN1)
        
        tempX2 <- readline(prompt = "Enter value of x2: ")
        x2 <- as.numeric(tempX2)
        
        tempN2 <- readline(prompt = "Enter value of n2: ")
        n2 <- as.numeric(tempN2)
        
        tempAlpha <- readline(prompt = "Enter significance level alpha: ")
        alpha <- as.numeric(tempAlpha)
        
        zValue <- qnorm(1-(alpha/2))
        p1 <- x1/n1
        p2 <- x2/n2
        
        pDifference <- p1-p2
        varianceFactor <- ((p1*(1-p1))/n1)+((p2*(1-p2))/n2)
        
        lowLimit <- (pDifference - (zValue*(((varianceFactor)^(0.5)))))
        upperLimit <- (pDifference + (zValue*(((varianceFactor)^(0.5)))))
        cat((1-alpha)*100,"% confidence limits are",lowLimit,"and",upperLimit) 
      }  
      
      if(subChoice7 == 5){
        cat('\n\t\tESTIMATION OF VARIANCES\n\n')
        
        tempN <- readline(prompt = "Enter value of n: ")
        n <- as.numeric(tempN)
        
        tempSamVar <- readline(prompt = "Enter value of sample variance: ")
        samVar <- as.numeric(tempSamVar)
        
        tempAlpha <- readline(prompt = "Enter significance level alpha: ")
        alpha <- as.numeric(tempAlpha)
        
        lowDen <- qchisq((1-(alpha/2)),(n-1))
        upperDen <- qchisq((alpha/2),(n-1))
        
        lowLimit <- ((n-1)*samVar)/lowDen
        upperLimit <- ((n-1)*samVar)/upperDen
        cat((1-alpha)*100,"% confidence limits are",lowLimit,"and",upperLimit) 
      }  
      
      if(subChoice7 == 6){
        cat('\n\t\tESTIMATION OF RATIO OF VARIANCES\n\n')
        
        tempN1 <- readline(prompt = "Enter value of n1: ")
        n1 <- as.numeric(tempN1)
        
        tempSamVar1 <- readline(prompt = "Enter value of sample variance1: ")
        samVar1 <- as.numeric(tempSamVar1)
        
        tempN2 <- readline(prompt = "Enter value of n2: ")
        n2 <- as.numeric(tempN2)
        
        tempSamVar2 <- readline(prompt = "Enter value of sample variance2: ")
        samVar2 <- as.numeric(tempSamVar2)
        
        tempAlpha <- readline(prompt = "Enter significance level alpha: ")
        alpha <- as.numeric(tempAlpha)
        
        samVarFactor <- samVar1/samVar2
        
        lowDenFac <- qf((1-(alpha/2)),(n1-1),(n2-1))
        upperFac <- qf((1-(alpha/2)),(n2-1),(n1-1))
        
        lowLimit <- samVarFactor/lowDenFac
        upperLimit <- samVarFactor*upperFac
        cat((1-alpha)*100,"% confidence limits are",lowLimit,"and",upperLimit)
      }  
      
    }
    
    else{
      cat('ERROR: Please enter a valid choice.')
    }
    
    choice <- readline(prompt = "Do you want to enter again? ")
    
  }
}
