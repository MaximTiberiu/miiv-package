# Să se calculeze media, dispersia și momentele inițiale și centrate până
# la ordinul 4, dacă există. Atunci când unul dintre momente nu există, se
# va afișa un mesaj corespunzător către utilizator.

#Calculeaza media functiei f
medie <- function(f) {
  tryCatch({
    prod <- function(x) {
      x * f(x)
    }
    return (integrate(
      f = Vectorize(prod),
      lower = -Inf,
      upper = Inf
    )$value)
  },
  error = function(err) {
    cat("Nu se poate calcula media!")
  })
}

#Calculeaza dispersia functiei f
dispersie <- function(f) {
  tryCatch({
    prod <- function(x) {
      (x - medie(f)) ^ 2 * f(x)
    }
    return (integrate(
      f = Vectorize(prod),
      lower = -Inf,
      upper = Inf
    )$value)
  },
  error = function(err) {
    cat("Nu se poate calcula dispersia!")
  })
}

#Calculeaza momentele initiale ale functiei f

momente_initiale <- function(f) {
  mom_ini <- list()
  for (i in 1:4) {
   # tryCatch({
      prod <- function(x) {
        x ^ i * f(x)
      }
      int <-
        integrate(f = Vectorize(prod),
                  lower = -Inf,
                  upper = Inf)$value
      mom_ini <- append(mom_ini, int)
    } #,
 #   error <- function(err) {
  #    cat("Nu se poate calcula momentul initial de ordin ")
  #  }
  #  )
#  }
  return (mom_ini)
}

momente_centrate <- function(f) {
  mom_cen <- list()
  for (i in 1:4) {
  #  tryCatch({
      prod <- function(x) {
        (x - medie(f)) ^ i * f(x)
      }
      int <-
        integrate(f = Vectorize(prod),
                  lower = -Inf,
                  upper = Inf)$value
      mom_cen <- append(mom_cen, int)
    }#,
  #  error <- function(err) {
   #   cat("Nu se poate calcula momentul centrat de ordin")
#    }
 #   )
#  }
  return (mom_cen)
}


m_d_mi_mc <- function(f) {
  result_1 <- medie(f)
  result_2 <- dispersie(f)
  result_3 <-  momente_initiale(f)
  result_4 <- momente_centrate(f)
  result <-
    list(
      "Medie" = result_1,
      "Dispersie" = result_2,
      "Mom_init" = result_3,
      "Mom_cen" = result_4
    )
}

f <- function(x) {
  if (x >= 0 && x <= 1)
    (exp(1) * (exp(-x) + exp(x))) / (exp(2) - 1)
  else
    0
}

g <- function(x) {
  if ((x >= 0) && (x <= pi)) {
    sin(x) / 2
  } else {
    0
  }
}

exemplu <- m_d_mi_mc(g)
exemplu
