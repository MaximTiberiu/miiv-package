# Să se calculeze covarianța și coeficientul de corelație pentru două variabile
# aleatoare continue. Trebuie să folosiți densitatea comună a celor două variabile aleatoare.

# Funcție care verifică dacă suportul funcției este potrivit pentru a calcula covarianța
support_verif <- function(sX, sY) {
  if(sX[1] == -Inf || sX[2] == Inf){
    return (FALSE)
  }

  if(sY[1] == -Inf || sY[2] == Inf){
    return (FALSE)
  }

  return (TRUE)
}

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

medie_X <- function(x, sX, sY){
  int <- integrate(f = function(x) {
    return (x * densitate_marginala_Y(f, sX)(x))
  }, lower = sY[1], upper = sY[2])
  return (int$value)
}

medie_Y <- function(y, sX, sY){
  int <- integrate(f = function(y) {
    return (y * densitate_marginala_X(f, sY)(y))
  }, lower = sX[1], upper = sX[2])
  return (int$value)
}

cov <- function(f, x, y, sX, sY) {
  return((x - medie_X(x, sX, sY)) * (y - medie_Y(y, sX, sY) * f(x, y)))
}

covarianta <- function(f, sX, sY) {
  if(support_verif(sX, sY) == FALSE){
    message(print("Un capat al suportului este -Infinit sau Infinit!\n"))
    return (NA)
  }
  int <- integrate(f = Vectorize(function(y) {
    integrate(f = function(x) {cov(f, x, y, sX, sY) }, lower = sX[1], upper = sX[2]) $ value
  }), lower = sY[1], upper = sY[2])
  return (int$value)
}

# varianta_X <- integrate(f = function(f, x, sX, sY) {
#   return ((x - medie_X(s, sX, sY)) ^ 2 * densitate_marginala_X(f, sY))
#   }, lower = sX[1], upper = sX[2]
# ) $ value
#
# varianta_Y <- integrate(f = function(f, y, sX, sY) {
#   return ((y - medie_Y(s, sX, sY)) ^ 2 * densitate_marginala_Y(f, sX))
#   }, lower = sY[1], upper = sY[2]
# ) $ value

f <- function(x, y){
  return (3/2*(x^2+y^2))
}




