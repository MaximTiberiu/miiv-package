# Să se verifice dacă o funcție introdusă de utilizator este densitate de probabilitate.

verifica_densitate <- function(f) {
  tryCatch(
    {
      # Interval simulat care poate fi comparat cu (-Inf,Inf)
      interval <- seq(-99999, 99999, 0.01)

      # Prin intermediul comenzii sapply(), se generează valorile funcției f pe interval
      valori <- sapply(interval, f)

      # Verificăm prima condiție: f(x) >= 0
      if (sum(valori < 0) > 0) {
        error
      }

      # Calculăm integrală din funcția introdusă pe -Inf, Inf
      integrala <- integrate(f = Vectorize(f), lower = -Inf, upper = Inf) $ value

      # Verificăm dacă valoarea integralei este egală cu 1 (a doua condiție), ținând cont de posibilele erori
      if (integrala < 0.99 && integrala > 1.001) {
        error
      }

      # Dacă funcția îndeplinește ambele condiții, rezultă că este densitate de probabilitate.
      cat("Funcția este densitate de probabilitate.\n")
      return (TRUE)
    },

    # Afișare mesaj, pentru cazul contrar.
    error = function(err) {
      cat("Funcția NU este densitate de probabilitate.\n")
      return (FALSE)
    }
  )
}
