# Fiind dată o funcție f, introdusă de utilizator, să se determine constata de
# normalizare k. În cazul în care o asemenea constantă nu există, se va afișa un
# mesaj corespunzător către utilizator.

constanta_normalizare <- function(f) {
  tryCatch(
    return(1/integrate(f = f, lower = -Inf, upper = Inf)$value),
      error = function(err) {
        print("Nu se poate determina constanta de normalizare!")
      }
  )
}
