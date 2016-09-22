corr <- function(directorio,horizonte=0){
  id <- 1:332
  mama <-vector("numeric")
  papa <- vector("numeric")
  colll <- vector("numeric")
  for (i in id) {
    if (0<i && i<10){
      tabla <- read.csv(paste("00",i,".csv",sep=""))
    }else if (i>9 && i<100){
      tabla <- read.csv(paste("0",i,".csv",sep=""))
    }else {
      tabla <- read.csv(paste(i,".csv",sep=""))
    }
    completosbien <- na.omit(tabla)
    numfilas <- nrow(completosbien)
    if (numfilas>=horizonte){
      
    mama <- c(mama,i)
    papa <- c(papa,numfilas)
    vec1 <- completosbien[,2]
    vec2 <- completosbien[,3]
    correlacion <- cor(vec1,vec2)
    colll <- c(colll,correlacion)
    }
  }
  ompletos <- data.frame(id = mama, nobs = papa)
  print(colll)
  
}