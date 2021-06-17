# Să se calculeze covarianța și coeficientul de corelație pentru două variabile
# aleatoare continue. Trebuie să folosiți densitatea comună a celor două variabile aleatoare.

support_verif <- function(sX, sY){
  if(sX[1] == -Inf || sX[2] == Inf){
    return (FALSE)
  }
  if(sY[1] == -Inf || sY[2] == Inf){
    return (FALSE)
  }
  return (TRUE)
}

densitatea_marginala_X <- function(f, sY){
  return (Vectorize(function(y){
    integrate(f = function(x){
      f(x, y)
    }, sY[1], sY[2])$value
  }))
}

densitatea_marginala_Y <- function(f, sX){
  return (Vectorize(function(x){
    integrate(f = function(y){
      f(x, y)
    }, sX[1], sX[2])$value
  }))
}

medie_X <- function(x, sX){
  return (integrate(f = function(x, sX){x*Vectorize(densitatea_marginala_X(x, sX))}, sX[1], sX[2])$value)
}

medie_Y <- function(y, sY){
  return (integrate(f = function(y, sY){y*Vectorize(densitatea_marginala_Y(y, sY))}, sY[1], sY[2])$value)
}

cov <- function(f, x, y, sX, sY){
  return((x - medie_X(x, sX))*(y-medie_Y(y, sY)*f(x*y)))
}

covarianta <- function(f, sX, sY){
 if(support_verif(sX, sY) == FALSE){
   cat("Un capat al suportului este -Infinit sau Infinit")
   return (FALSE)
 }
  return (Vectorize(integrate(function(y){
    integrate(function(x){
      cov(f, x, y, sX, sY)
    }, sX[1], sX[2])$value
  }, sY[1], sY[2])$value))
}

f <- function(x, y){
  return (x^2 + y^2)
}




