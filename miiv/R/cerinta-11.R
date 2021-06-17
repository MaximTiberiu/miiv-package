# Pornind de la densitatea comună a două variabile aleatoare continue, să se construiască
# densitățile marginale continue și densitățile condiționate.

#PARTIAL REZOLVATA

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
