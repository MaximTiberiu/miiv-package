# Pornind de la densitatea comună a două variabile aleatoare continue, să se construiască
# densitățile marginale continue și densitățile condiționate.

# PARTIAL REZOLVATA

densitate_marginala_X <- function(f, sY){
  return (Vectorize(function(y) {
    integrate(f = function(x) { f(x, y) }, lower = sY[1], upper = sY[2]) $ value
  }))
}

densitate_marginala_Y <- function(f, sX){
  return (Vectorize(function(x) {
    integrate(f = function(y) { f(x, y) }, lower = sX[1], upper = sX[2]) $ value
  }))
}
