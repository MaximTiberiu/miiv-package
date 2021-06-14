# Să se reprezinte grafic densitatea și funcția de repartiție pentru diferite
# valori ale parametrilor repartiției. În cazul în care funcția de repartiție nu
# este dată într-o formă explicită (ex. repartiția normală), se acceptă
# reprezentarea grafică a unei aproximări a acesteia.

library("RColorBrewer")

# 1. Repartiție uniformă
grafic_densitate_uniforma <- function(a, b){
  curve(expr = dunif(x = x, min = a, max = b),
        from = -3 * b,
        to = 3 * b,
        main = "Densitatea repartiției Uniforme",
        col = brewer.pal(n = 1, name = "Set2"),
        lwd = 3,
        ylab = "f(x)"
        )
}

grafic_repartitie_uniforma <- function(a, b){
  curve(expr = punif(x = x, min = a, max = b),
        from = -3 * b,
        to = 3 * b,
        main = "Funcția de repartiție Uniformă",
        col = brewer.pal(n = 1, name = "Set2"),
        lwd = 3,
        ylab = "F(x)"
  )
}


# 2. Repartiție normală
grafic_densitate_normala <- function(mean, sd){
  curve(expr = dnorm(x = x, mean = mean, sd = sd),
        from = mean - 3 * sd,
        to = mean + 3 * sd,
        main = "Densitatea repartiției Normale",
        col = brewer.pal(n = 1, name = "Set2"),
        lwd = 3,
        ylab = "f(x)"
  )
}

grafic_repartitie_normala <- function(mean, sd){
  curve(expr = pnorm(x = x, mean = mean, sd = sd),
        from = mean - 3 * sd,
        to = mean + 3 * sd,
        main = "Funcția de repartiție Normală",
        col = brewer.pal(n = 1, name = "Set2"),
        lwd = 3,
        ylab = "F(x)"
  )
}

# 3. Repartiție exponențială
grafic_densitate_exponentiala <- function(rate){
  curve(expr = dexp(x = x, rate = rate),
        from = 0,
        to = 20,
        main = "Densitatea repartiției Exponențiale",
        col = brewer.pal(n = 1, name = "Set2"),
        lwd = 3,
        ylab = "f(x)"
  )
}


grafic_densitate_exponentiala <- function(rate){
  curve(expr = pexp(x = x, rate = rate),
        from = 0,
        to = 20,
        main = "Funcția de repartiție Exponențială",
        col = brewer.pal(n = 1, name = "Set2"),
        lwd = 3,
        ylab = "F(x)"
  )
}

# 4. Repartiția Cauchy
grafic_densitate_cauchy <- function(location, scale){
  curve(expr = dcauchy(x = x, location = location, scale = scale),
        from = 0,
        to = 5,
        main = "Densitatea repartiției Cauchy",
        col = brewer.pal(n = 1, name = "Set2"),
        lwd = 3,
        ylab = "f(x)"
  )
}

grafic_repartitie_cauchy <- function(location, scale){
  curve(expr = pcauchy(x = x, location = location, scale = scale),
        from = 0,
        to = 5,
        main = "Funcția de repartiție Cauchy",
        col = brewer.pal(n = 1, name = "Set2"),
        lwd = 3,
        ylab = "F(x)"
  )
}

# 5. Repartiția Beta
grafic_densitate_beta <- function(shape1, shape2){
  curve(expr = dbeta(x = x, shape1 = shape1, shape2 = shape2),
        from = 0,
        to = 1,
        main = "Densitatea repartiției Beta",
        col = brewer.pal(n = 1, name = "Set2"),
        lwd = 3,
        ylab = "f(x)"
  )
}

grafic_repartitie_beta <- function(shape1, shape2){
  curve(expr = pbeta(x = x, shape1 = shape1, shape2 = shape2),
        from = 0,
        to = 1,
        main = "Funcția de repartiție Beta",
        col = brewer.pal(n = 1, name = "Set2"),
        lwd = 3,
        ylab = "F(x)"
  )
}

# 6. Repartiția Gamma
grafic_densitate_gamma <- function(shape, rate){
  curve(expr = dgamma(x = x, shape = shape, rate = rate),
        from = 0,
        to = 20,
        main = "Densitatea repartiției Gamma",
        col = brewer.pal(n = 1, name = "Set2"),
        lwd = 3,
        ylab = "f(x)"
  )
}

grafic_repartitie_gamma <- function(shape, rate){
  curve(expr = pgamma(x = x, shape = shape, rate = rate),
        from = 0,
        to = 20,
        main = "Funcția de repartiție Gamma",
        col = brewer.pal(n = 1, name = "Set2"),
        lwd = 3,
        ylab = "F(x)"
  )
}

# 7. Repartiția Chi-Square
grafic_densitate_chi_square <- function(ncp){
  curve(expr = dchisq(x = x, ncp = ncp),
        from = 0,
        to = 60,
        main = "Densitatea repartiției Chi-Square",
        col = brewer.pal(n = 1, name = "Set2"),
        lwd = 3,
        ylab = "f(x)"
  )
}

grafic_repartitie_chi_square <- function(ncp){
  curve(expr = pchisq(x = x, ncp = ncp),
        from = 0,
        to = 60,
        main = "Funcția de repartiție Chi-Square",
        col = brewer.pal(n = 1, name = "Set2"),
        lwd = 3,
        ylab = "F(x)"
  )
}

# 8. Repartiția log-normală
grafic_densitate_log <- function(meanlog, sdlog){
  curve(expr = dlnorm(x = x, meanlog = meanlog, sdlog = sdlog),
        from = 0,
        to = 5,
        main = "Densitatea repartiției Log-normală",
        col = brewer.pal(n = 1, name = "Set2"),
        lwd = 3,
        ylab = "f(x)"
  )
}

grafic_repartitie_log <- function(meanlog, sdlog){
  curve(expr = plnorm(x = x, meanlog = meanlog, sdlog = sdlog),
        from = 0,
        to = 5,
        main = "Funcția de repartiție Log-normală",
        col = brewer.pal(n = 1, name = "Set2"),
        lwd = 3,
        ylab = "F(x)"
  )
}

# TO-DO

meniu_afisare_densitati <- function() {
  print("Introduceți numele repartiției.")
  enter <- readline(prompt = "Repartiția aleasă este: ")

  if (enter == "unif") {
    grafic_densitate_uniforma(min = 1, max = 2)
  }
  else if (enter == "norm") {
    grafic_densitate_normala(mean = 0, sd = 1)
  }
}

meniu_afisare_grafice <- function() {
  print("Introduceți un număr din lista de mai jos:")
  print("1. Afișare Densitate Repartiție")
  print("2. Afișare Funcție de Repartiție")

  enter <- as.integer(readline(prompt = "Numărul ales este: "))

  if (enter == 1) {
    meniu_afisare_densitati()
  }
  else if (enter == 2) {

  }
}

meniu_afisare_grafice()


