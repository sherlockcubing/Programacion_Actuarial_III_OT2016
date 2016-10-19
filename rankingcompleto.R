setwd("~/GitHub/Programacion_Actuarial_III_OT2016")
rankingcompleto <- function(resultado,num){
  
  base <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  base <- data.frame(base[2],base[7],base[11],base[17],base[23])
  
  if (resultado == "ataque"){
    base1 <- data.frame(base[1],base[2],base[3])
    base1 <- base1[base1[3]!= "Not Available",]
  }
  else if (resultado == "falla") {
    base1 <- data.frame(base[1],base[2],base[4])
    base1 <- base1[base1[3]!= "Not Available",]
  }
  else if (resultado == "neumonia") {
    base1 <- data.frame(base[1],base[2],base[5])
    base1 <- base1[base1[3]!= "Not Available",]
  }
  else{
    stop("valor incorrecto")
  }
  estad <- vector("numeric")
  estados <- base1[,2]
  estados <- unique(estados)
  nestados <- length(estados)
  base5 <- vector("numeric")
  rankin <- vector("numeric")
  for (i in 1:nestados){
    base2 <- base1[base1[2]== estados[i],]
    if (num>nrow(base1)){
      estad <- c(estad,"NA")
    }

  for (i in 1:num){
    minimo <- which.min(base2[,3])
    base3 <- base2[,3]
    minimo2 <- base3[minimo]
    base4 <- base2[base2[3]== minimo2,]
    base6 <- base4[,1]
    base6 <- sort(base6)
    base5 <- c(base5,base6)
    base2 <- base2[base2[3] != minimo2,]
    

  }
    rankin <- c(rankin,base5)
    base5 <- vector("numeric")
  }
print(num)
}