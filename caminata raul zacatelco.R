n <- 0
y <- 0
z <- 5
salida <- vector("numeric")
caminata <- vector("numeric")
for (i in 1:100) {
  caminata <- vector("numeric")
  z <- 5
while(z>=3 && z<=10){
  caminata <- c(caminata,z)
  print(z)
  moneda <- rbinom(1,1,0.5)
  if(moneda==1){
    z <- z + 0.5
  } else {
    z <- z - 0.5
  }
}
  salida <- c(salida,z)
    if (z<3){
    n <- n+1
  }else {
    y <- y+1
  }
}
#Numero de veces que se sale por abajo 
n
#Numero de veces que se sale por arriba 
y