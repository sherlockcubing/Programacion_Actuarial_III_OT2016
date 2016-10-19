setwd("~/GitHub/Programacion_Actuarial_III_OT2016")
mejor <- function(estado,resultado){
  base <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  base <- data.frame(base[2],base[7],base[13],base[19],base[25])

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
  XX <- base[,2]
  if (!estado %in% XX){
    stop("estado incorrecto")
  }
  base1 <- base1[base1[2]== estado,]
  minimo <- which.min(base1[,3])
  base2 <- base1[,3]
  minimo2 <- base2[minimo]
  base1 <- base1[base1[3]==minimo2]
  base3 <- base1[1]
  base3
}