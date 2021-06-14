# Să se calculeze media și dispersia unei variabile aleatoare g(X), unde X are o
# repartiție continuă cunoscută, iar g este o funcție continuă precizată de utilizator.

medie_dipsersie <- function(g) {
  #media variabilei aleatoare g(X)
  prod_func <- function(x) {
    x * g(x)
  }
  medie <- integrate(f = Vectorize(prod_func), lower = -Inf, upper = Inf)$value

  #dispersia variabilei aleatoare g(X)
  prod_func_patrat <- function(x) {
    x*x*g(x)
  }
  medie_patrat <- integrate(f = Vectorize(prod_func_patrat), lower = -Inf, upper = Inf)$value
  dispersie <- medie_patrat - medie^2

  result <- list("medie" = medie, "dispersie" = dispersie)
  return(result)
}

f <- function(x) {
     if(x >= 0 && x <= 1)
       (exp(1)*(exp(-x)+exp(x)))/(exp(2)-1)
     else 0
}

g <- function(x) {
  if((x >= 0) && (x <= pi)) {sin(x)/2} else {0}
}
