# Să se reprezinte grafic densitatea și funcția de repartiție pentru diferite
# valori ale parametrilor repartiției. În cazul în care funcția de repartiție nu
# este dată într-o formă explicită (ex. repartiția normală), se acceptă
# reprezentarea grafică a unei aproximări a acesteia.

library("RColorBrewer")

# 1. Repartiție uniformă
grafic_densitate_uniforma <- function(min, max){
  curve(expr = dunif(x = x, min = min, max = max),
        from = -3 * max,
        to = 3 * max,
        main = "Densitatea repartiției Uniforme",
        col = brewer.pal(n = 3, name = "Set2"),
        lwd = 3,
        ylab = "f(x)"
  )
}

grafic_repartitie_uniforma <- function(min, max){
  curve(expr = punif(q = x, min = min, max = max),
        from = -3 * max,
        to = 3 * max,
        main = "Funcția de repartiție Uniformă",
        col = brewer.pal(n = 3, name = "Set2"),
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
        col = brewer.pal(n = 3, name = "Set2"),
        lwd = 3,
        ylab = "f(x)"
  )
}

grafic_repartitie_normala <- function(mean, sd){
  curve(expr = pnorm(q = x, mean = mean, sd = sd),
        from = mean - 3 * sd,
        to = mean + 3 * sd,
        main = "Funcția de repartiție Normală",
        col = brewer.pal(n = 3, name = "Set2"),
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
        col = brewer.pal(n = 3, name = "Set2"),
        lwd = 3,
        ylab = "f(x)"
  )
}


grafic_densitate_exponentiala <- function(rate){
  curve(expr = pexp(q = x, rate = rate),
        from = 0,
        to = 20,
        main = "Funcția de repartiție Exponențială",
        col = brewer.pal(n = 3, name = "Set2"),
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
        col = brewer.pal(n = 3, name = "Set2"),
        lwd = 3,
        ylab = "f(x)"
  )
}

grafic_repartitie_cauchy <- function(location, scale){
  curve(expr = pcauchy(q = x, location = location, scale = scale),
        from = 0,
        to = 5,
        main = "Funcția de repartiție Cauchy",
        col = brewer.pal(n = 3, name = "Set2"),
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
        col = brewer.pal(n = 3, name = "Set2"),
        lwd = 3,
        ylab = "f(x)"
  )
}

grafic_repartitie_beta <- function(shape1, shape2){
  curve(expr = pbeta(q = x, shape1 = shape1, shape2 = shape2),
        from = 0,
        to = 1,
        main = "Funcția de repartiție Beta",
        col = brewer.pal(n = 3, name = "Set2"),
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
        col = brewer.pal(n = 3, name = "Set2"),
        lwd = 3,
        ylab = "f(x)"
  )
}

grafic_repartitie_gamma <- function(shape, rate){
  curve(expr = pgamma(q = x, shape = shape, rate = rate),
        from = 0,
        to = 20,
        main = "Funcția de repartiție Gamma",
        col = brewer.pal(n = 3, name = "Set2"),
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
        col = brewer.pal(n = 3, name = "Set2"),
        lwd = 3,
        ylab = "f(x)"
  )
}

grafic_repartitie_chi_square <- function(ncp){
  curve(expr = pchisq(q = x, ncp = ncp),
        from = 0,
        to = 60,
        main = "Funcția de repartiție Chi-Square",
        col = brewer.pal(n = 3, name = "Set2"),
        lwd = 3,
        ylab = "F(x)"
  )
}

# 8. Repartiția Log-normală
grafic_densitate_log <- function(meanlog, sdlog){
  curve(expr = dlnorm(x = x, meanlog = meanlog, sdlog = sdlog),
        from = 0,
        to = 5,
        main = "Densitatea repartiției Log-normală",
        col = brewer.pal(n = 3, name = "Set2"),
        lwd = 3,
        ylab = "f(x)"
  )
}

grafic_repartitie_log <- function(meanlog, sdlog){
  curve(expr = plnorm(q = x, meanlog = meanlog, sdlog = sdlog),
        from = 0,
        to = 5,
        main = "Funcția de repartiție Log-normală",
        col = brewer.pal(n = 3, name = "Set2"),
        lwd = 3,
        ylab = "F(x)"
  )
}

# grafic_denistate_oarecare <- function(f, min, max) {
#   x <- seq(min, max, 0.01)
#   plot(x, f(x),
#        main = "Densitatea",
#        xlab = "x",
#        ylab = "f(X)",
#        col = brewer.pal(n = 3, name = "Set2"))
# }

meniu_afisare_densitati <- function() {
  cat("Introduceți numărul corespunzător repartiției dorite.\n")
  cat("1. Repartiție uniformă\n")
  cat("2. Repartiție normală\n")
  cat("3. Repartiție exponențială\n")
  cat("4. Repartiția Cauchy\n")
  cat("5. Repartiția Beta\n")
  cat("6. Repartiția Gamma\n")
  cat("7. Repartiția Chi-Square\n")
  cat("8. Repartiția Log-normală\n")
  enter <- as.numeric(readline(prompt = "Repartitia aleasă este: "))

  switch(  EXPR = enter,
           {
             min = as.numeric(readline(prompt = "Min: "))
             max = as.numeric(readline(prompt = "Max: "))
             grafic_densitate_uniforma(min = min, max = max)
           },
           {
             mean = as.numeric(readline(prompt = "Mean: "))
             sd = as.numeric(readline(prompt = "Sd: "))
             grafic_densitate_normala(mean = mean, sd = sd)
           },
           {
             rate = as.numeric(readline(prompt = "Rate: "))
             grafic_densitate_exponentiala(rate = rate)
           },
           {
             location = as.numeric(readline(prompt = "Location: "))
             scale = as.numeric(readline(prompt = "Scale: "))
             grafic_densitate_cauchy(location = location, scale = scale)
           },
           {
             shape1 = as.numeric(readline(prompt = "Shape1: "))
             shape2 = as.numeric(readline(prompt = "Shape2: "))
             grafic_densitate_beta(shape1 = shape1, shape2 = shape2)
           },
           {
             shape = as.numeric(readline(prompt = "Shape: "))
             rate = as.numeric(readline(prompt = "Rate: "))
             grafic_densitate_gamma(shape = shape, rate = rate)
           },
           {
             ncp = as.numeric(readline(prompt = "Ncp: "))
             grafic_densitate_chi_square(ncp = ncp)
           },
           {
             meanlog = as.numeric(readline(prompt = "Meanlog: "))
             sdlog = as.numeric(readline(prompt = "Sdlog: "))
             grafic_densitate_log(meanlog = meanlog, sdlog = sdlog)
           }
  )
}

meniu_afisare_functie_repartitie <- function() {
  cat("Introduceți numărul corespunzător repartiției dorite.\n")
  cat("1. Repartiție uniformă\n")
  cat("2. Repartiție normală\n")
  cat("3. Repartiție exponențială\n")
  cat("4. Repartiția Cauchy\n")
  cat("5. Repartiția Beta\n")
  cat("6. Repartiția Gamma\n")
  cat("7. Repartiția Chi-Square\n")
  cat("8. Repartiția Log-normală\n")
  enter <- as.numeric(readline(prompt = "Repartitia aleasă este: "))

  switch(  EXPR = enter,
           {
             min = as.numeric(readline(prompt = "Min: "))
             max = as.numeric(readline(prompt = "Max: "))
             grafic_repartitie_uniforma(min = min, max = max)
           },
           {
             mean = as.numeric(readline(prompt = "Mean: "))
             sd = as.numeric(readline(prompt = "Sd: "))
             grafic_repartitie_normala(mean = mean, sd = sd)
           },
           {
             rate = as.numeric(readline(prompt = "Rate: "))
             grafic_repartitie_exponentiala(rate = rate)
           },
           {
             location = as.numeric(readline(prompt = "Location: "))
             scale = as.numeric(readline(prompt = "Scale: "))
             grafic_repartitie_cauchy(location = location, scale = scale)
           },
           {
             shape1 = as.numeric(readline(prompt = "Shape1: "))
             shape2 = as.numeric(readline(prompt = "Shape2: "))
             grafic_repartitie_beta(shape1 = shape1, shape2 = shape2)
           },
           {
             shape = as.numeric(readline(prompt = "Shape: "))
             rate = as.numeric(readline(prompt = "Rate: "))
             grafic_repartitie_gamma(shape = shape, rate = rate)
           },
           {
             ncp = as.numeric(readline(prompt = "Ncp: "))
             grafic_repartitie_chi_square(ncp = ncp)
           },
           {
             meanlog = as.numeric(readline(prompt = "Meanlog: "))
             sdlog = as.numeric(readline(prompt = "Sdlog: "))
             grafic_repartitie_log(meanlog = meanlog, sdlog = sdlog)
           }
  )
}

meniu_afisare_grafice <- function() {
  cat("Introduceți un număr din lista de mai jos:\n")
  cat("1. Afișare Densitate Repartiție\n")
  cat("2. Afișare Funcție de Repartiție\n")

  enter <- as.numeric(readline(prompt = "Numărul ales este: "))
  switch(EXPR = enter,
         meniu_afisare_densitati(),
         meniu_afisare_functie_repartitie()
  )
}



