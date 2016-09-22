setwd("C:/Users/Toshiba/Documents/GitHub/Programacion_Actuarial_III_OT2016/specdata")
columna <- 0
tabla <- 0
mediocontaminante <- function(directorio,contaminante,id = 1:332){

  if (contaminante=="sulfate"){
    x = 2
  }else if (contaminante == "nitrate" ) {
    x = 3
  }
  suma<-0
  promedio1<-0
  suma2 <-2
  n<-0
  for (i in id) {
    if (0<i && i<10){
      tabla <- read.csv(paste("00",i,".csv",sep=""))
    }else if (i>9 && i<100){
      tabla <- read.csv(paste("0",i,".csv",sep=""))
    }else {
      tabla <- read.csv(paste(i,".csv",sep=""))
    }
  
    columnas <- tabla[x]
    columnabien <- na.omit(columnas)
    numfilas <- nrow(columnabien)


    for (j in 1:numfilas){
      suma <- suma + columnabien[j,]
      
    }
    suma2 <- suma/numfilas
        n <- n+1
    promedio1<- promedio1+suma2
    
    suma2 <- 0
    suma <- 0 
  }
  promedio2 <- promedio1/n
  "este es tu promedio"
  print(promedio2)

}


