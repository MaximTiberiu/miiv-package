# Pornind de la densitatea comună a două variabile aleatoare continue, să se construiască
# densitățile marginale continue și densitățile condiționate.

# PARTIAL REZOLVATA

densitati_marginale_conditionate <- function(f, sX, sY) {
  densitate_marginala_X <- Vectorize(function(x) {
    integrate(f = function(y){ f(x, y) },
              lower = sY[1],
              upper = sY[2]) $ value
  })

  densitate_marginala_Y <- Vectorize(function(y) {
    integrate(f = function(x){ f(x, y) },
              lower = sX[1],
              upper = sX[2]) $ value
  })

  # densitate_conditionata_X <- Vectorize(function(y) {
  #   g <- function(x) {
  #     f(x, y)
  #   }
  #   return (g / densitate_marginala_Y(y))
  # })
  #
  # densitate_conditionata_Y <- Vectorize(function(x) {
  #   g <- function(y) {
  #     f(x, y)
  #   }
  #   return (g / densitate_marginala_Y(x))
  # })
}


