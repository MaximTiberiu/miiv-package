# Să se calculeze covarianța și coeficientul de corelație pentru două variabile
# aleatoare continue. Trebuie să folosiți densitatea comună a celor două variabile aleatoare.

# Funcție care verifică dacă suportul funcției este potrivit pentru a calcula covarianța
suport_verif <- function(sX, sY) {
  if(sX[1] == -Inf || sX[2] == Inf){
    return (FALSE)
  }

  if(sY[1] == -Inf || sY[2] == Inf){
    return (FALSE)
  }

  return (TRUE)
}


covarianta_corelatie <- function(f, sX, sY){
  if(suport_verif(sX, sY) == FALSE){
    message(print("Un capăt al suportului este -Inf sau Inf!\n"))
    return(NA)
  }

  # caclulează cele două densități marginale
  densitate_marginala_X <- Vectorize(function(x) {
    integrate(f = function(y){ f(x,y) },
              lower = sY[1],
              upper = sY[2]) $ value
  })

  densitate_marginala_Y <- Vectorize(function(y) {
    integrate(f = function(x){ f(x,y) },
              lower = sX[1],
              upper = sX[2]) $ value
  })

  # calculează media lui X, repesctiv Y
  medie_X <- integrate(f = function(x) { return (x * densitate_marginala_X(x)) },
                       lower = sX[1],
                       upper = sX[2]) $ value

  medie_Y <- integrate(f = function(y) { return (y * densitate_marginala_Y(y)) },
                       lower = sY[1],
                       upper = sY[2]) $ value

  cov <- function(x, y) { return ((x - medie_X) * (y - medie_Y) * f(x, y))}

  covarianta <- integrate(f = Vectorize(function (y) {
                            integrate(f = function (x) { cov(x, y) },
                                      lower = sX[1],
                                      upper = sX[2]) $ value
                            }),
                          lower = sY[1],
                          upper = sY[2]) $ value

  # calculăm varianțele celor două variabile aleatoare
  varianta_X <- integrate(f = function(x) { return ((x - medie_X) ^ 2 * densitate_marginala_X(x)) },
                         lower = sX[1],
                         upper = sX[2]) $ value

  varianta_Y <- integrate(f = function(y) { return ((y - medie_Y) ^ 2 * densitate_marginala_Y(y)) },
                          lower = sY[1],
                          upper = sY[2]) $ value

  coeficient_corelatie <- covarianta / sqrt(varianta_X * varianta_Y)

  res <- list("covarianta" = covarianta, "coeficient_corelatie" = coeficient_corelatie)
  return (res)
}

f <- function(x, y){
  return (3/2*(x^2+y^2))
}

sX = c(1, 2)
sY = c(5, 7)

covarianta_corelatie(f, sX, sY)



