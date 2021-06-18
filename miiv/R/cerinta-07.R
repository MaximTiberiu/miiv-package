# Să se creeze o funcție P care permite calculul diferitelor tipuri de probabilități
# asociate unei variabile aleatoare continue, similar funcției P din pachetul discreteRV.
P <- function(f) {
  cat("1. <\n")
  cat("2. >\n")
  enter <- as.numeric(readline(prompt = "Optiunea aleasa este: "))
  limita <- as.numeric(readline(prompt = "Capatul integralei este: "))

  switch(EXPR = enter,
         {
           res <- integrate(f = Vectorize(f), lower = -Inf, upper = limita) $ value
           return (res)
         },
         {
           res <- integrate(f = Vectorize(f), lower = limita, upper = Inf) $ value
           return (res)
         })
}

g <- function(x) {
  if((x >= 0) && (x <= pi)) {sin(x) / 2} else {0}
}
