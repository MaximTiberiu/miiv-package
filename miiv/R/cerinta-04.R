# Să se reprezinte grafic densitatea și funcția de repartiție pentru diferite 
# valori ale parametrilor repartiției. În cazul în care funcția de repartiție nu 
# este dată într-o formă explicită (ex. repartiția normală), se acceptă 
# reprezentarea grafică a unei aproximări a acesteia.

#1.UNIFORMA
grafic_densitate_uniforma <- function(a, b){
  curve(expr = dunif(x,a,b),
        from = -3*b,
        to = 3*b,
        main = "Densitatea repartitiei uniforme",
        col = "magenta",
        lwd = 3,
        ylab = "Functia de densitate"
        )
}

grafic_repartitie_uniforma <- function(a, b){
  curve(expr = punif(x,a,b),
        from = -3*b,
        to = 3*b,
        main = "Functia de repartitie uniforma",
        col = "magenta",
        lwd = 3,
        ylab = "Functia de repartitie"
  )
}


#2.NORMALA
grafic_densitate_normala <- function(mean, sd){
  curve(expr = dnorm(x, mean, sd),
        from = mean - 3*sd,
        to = mean + 3*sd,
        main = "Densitatea repartitiei normale",
        col = "magenta",
        lwd = 3,
        ylab = "Functia de densitate"
  )
}

grafic_repartitie_normala <- function(mean, sd){
  curve(expr = pnorm(x, mean, sd),
        from = mean - 3*sd,
        to = mean + 3*sd,
        main = "Functia de repartitie normala",
        col = "magenta",
        lwd = 3,
        ylab = "Functia de repartitie"
  )
}

#3.EXPONENTIALA
grafic_densitate_exponentiala <- function(rate){
  curve(expr = dexp(x, rate),
        from = 0,
        to = 20,
        main = "Densitatea repartitiei exponentiala",
        col = "magenta",
        lwd = 3,
        ylab = "Functia de densitate"
  )
}


#4.BETA
grafic_densitate_beta <- function(shape1, shape2){
  curve(expr = dbeta(x, shape1, shape2),
        from = 0,
        to = 1,
        main = "Densitatea repartitiei beta",
        col = "magenta",
        lwd = 3,
        ylab = "Functia de densitate"
  )
}

grafic_repartitie_beta <- function(shape1, shape2){
  curve(expr = pbeta(x, shape1, shape2),
        from = 0,
        to = 1,
        main = "Functia de repartitie beta",
        col = "magenta",
        lwd = 3,
        ylab = "Functia de repartitie"
  )
}

#5.CAUCHY
grafic_densitate_cauchy <- function(location, scale){
  curve(expr = dcauchy(x, location, scale),
        from = 0,
        to = 5,
        main = "Densitatea repartitiei Cauchy",
        col = "magenta",
        lwd = 3,
        ylab = "Functia de densitate"
  )
}

grafic_repartitie_cauchy <- function(location, scale){
  curve(expr = pcauchy(x, location, scale),
        from = 0,
        to = 5,
        main = "Functia de repartitie Cauchy",
        col = "magenta",
        lwd = 3,
        ylab = "Functia de repartitie"
  )
}

#6.GAMMA
grafic_densitate_gamma <- function(shape, rate){
  curve(expr = dgamma(x, shape, rate),
        from = 0,
        to = 20,
        main = "Densitatea repartitiei gamma",
        col = "magenta",
        lwd = 3,
        ylab = "Functia de densitate"
  )
}

grafic_repartitie_gamma <- function(shape, rate){
  curve(expr = pgamma(x, shape, rate),
        from = 0,
        to = 20,
        main = "Functia de repartitie gamma",
        col = "magenta",
        lwd = 3,
        ylab = "Functia de repartitie"
  )
}

#7.CHI-SQUARE
grafic_densitate_chi_square <- function(ncp){
  curve(expr = dchisq(x, ncp),
        from = 0,
        to = 60,
        main = "Densitatea repartitiei chi-square",
        col = "magenta",
        lwd = 3,
        ylab = "Functia de densitate"
  )
}

grafic_repartitie_chi_square <- function(ncp){
  curve(expr = pchisq(x, ncp),
        from = 0,
        to = 60,
        main = "Functia de repartitie chi-square",
        col = "magenta",
        lwd = 3,
        ylab = "Functia de repartitie"
  )
}

#8.LOG-NORMAL
grafic_densitate_log <- function(meanlog, sdlog){
  curve(expr = dlnorm(x, meanlog, sdlog),
        from = 0,
        to = 5,
        main = "Densitatea repartitiei log-normal",
        col = "magenta",
        lwd = 3,
        ylab = "Functia de densitate"
  )
}

grafic_repartitie_log <- function(meanlog, sdlog){
  curve(expr = plnorm(x, meanlog, sdlog),
        from = 0,
        to = 5,
        main = "Functia de repartitie log-normal",
        col = "magenta",
        lwd = 3,
        ylab = "Functia de repartitie"
  )
}