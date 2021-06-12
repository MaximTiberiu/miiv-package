# Să se verifice dacă o funcție introdusă de utilizator este densitate de probabilitate.


verifica_densitate <- function(f) {
  tryCatch({
    interval <- seq(-99999, 99999, 0.01) #Interval simulat care poate fi comparat cu (-Inf,Inf)
    valori <- sapply(interval, f) #Prin intermediul comenzii sapply se generează valorile funcției f pe interval
    if (sum(valori < 0) > 0) #Verificăm condiția nr 1) f(x) >= 0
    {
      error
    }
    #Calculăm integrală din funcția introdusă pe -Inf, Inf
    integrala <-
      integrate(f = Vectorize(f),
                lower = -Inf,
                upper = Inf)$value
    if (integrala < 0.99 && integrala > 1.001) #Verificăm dacă valoarea integralei este egală cu 1 ținând cont de posibilele erori
    {
      error
    }
    print("Funcția este densitate de probabilitate.")
  },
  error = function(err) {
    print("Funcția NU este densitate de probabilitate.")
  })
}
