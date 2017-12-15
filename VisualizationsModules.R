HISTOGRAM <- function(x){
  x <<- as.numeric(x)
  hist(x, col="CYAN4", main="HISTOGRAM", xlab="x-axis", ylab="y-axis")
}

LINEGRAPH <- function(x){
  x <<- as.numeric(x)
  plot(x, type="l", col="GREEN", main="LINEGRAPH", xlab="x-axis", ylab="y-axis")
}

BARGRAPH <- function(x){
  x <<- as.numeric(x)
  barplot(x, col="RED", main="BAR GRAPH", xlab="x-axis", ylab="y-axis")
}

PIECHART <- function(x){
  x <<- as.numeric(x)
  pie(x, col=rainbow(length(x)), main="PIE CHART")
}

SCATTERPLOT <- function(x,y){
  x <<- as.numeric(x)
  y <<- as.numeric(y)
  plot(x, y, col="BLUE", main="SCATTER PLOT", xlab="x-axis", ylab="y-axis")
}

BOXPLOT <- function(x){
  x <<- as.numeric(x)
  boxplot(x, y, col="BLUE", main="BOX PLOT", xlab="x-axis", ylab="y-axis")
}

QQPLOT <- function(x,y){
  x <<- as.numeric(x)
  y <<- as.numeric(y)
  qqplot(x, y, col="BLUE", main="Q-Q PLOT")
  qqnorm(y)
  qqline(y, col="GREEN")
}

STEMLEAFPLOT <- function(x){
  x <<- as.numeric(x)
  stem(x)
}

PARETOCHART <- function(x){
  x <<- as.numeric(x)
  x <<- sort(x, decreasing = TRUE)
  library(qcc)
  pareto.chart(x, main="PARETO CHART", ylab="Frequency", ylab2="Cumulative Percentage", xlab="x-axis")
}
