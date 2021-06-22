# Fiind dată o funcție f, introdusă de utilizator, să se determine constata de
# normalizare k. În cazul în care o asemenea constantă nu există, se va afișa un
# mesaj corespunzător către utilizator.

constanta_normalizare <- function(f) {
  # folosim blocul tryCatch pentru a verifica dacă integrala este convergentă
  tryCatch(

    # aplicăm formula pentru constanta de normalizare
    return(1 / integrate(f = Vectorize(f), lower = -Inf, upper = Inf) $ value),

    error = function(err) {
      # dacă funcția nu are constantă de normalizare, se afișează următorul mesaj
      message(paste("Nu se poate determina constanta de normalizare!\n"))
      message(paste(err, "\n"))
      return (NA)
    }
  )
}

# TEST COD
f <- function(x) exp(-x ^ 2)
constanta_normalizare(f)

g <- function(x) exp(x)
constanta_normalizare(g)
