# Să se calculeze media și dispersia unei variabile aleatoare g(X), unde X are o
# repartiție continuă cunoscută, iar g este o funcție continuă precizată de utilizator.

medie_dispersie <- function(g, f) {
  #media variabilei aleatoare g(X)
  prod_func <- function(x) {
    return (g(x) * f(x))
  }
  medie <- integrate(f = Vectorize(prod_func), lower = -Inf, upper = Inf) $ value

  #dispersia variabilei aleatoare g(X)
  prod_func_patrat <- function(x) {
    return ((g(x) - medie) ^ 2 * f(x))
  }
  dispersie <- integrate(f = Vectorize(prod_func_patrat), lower = -Inf, upper = Inf) $ value

  result <- list("medie" = medie, "dispersie" = dispersie)
  return(result)
}

f <- function(x) dnorm(x, 0.5, 1)
g <- function(x) dlnorm(x, 0.7, 1)
medie_dispersie(f, g)
