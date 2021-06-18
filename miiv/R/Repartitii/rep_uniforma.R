# Repartiția uniformă
uniforma <- function(){
  cat("Repartiția uniformă\nDEFINIȚIE: Spunem că X este o variabilă aleatoare uniformă continuă pe intervalul [a, b]
           dacă X are funcția de distribuție dată de: \n
       |1/(b-a), x ∈ [a,b]
f(x) = |
       |   0,   în rest \n\n")

  cat("NOTAȚIE: X ~ U [a,b] \n")
  cat("MEDIA: E(X) = (a+b)/2 \n")
  cat("DISPERSIA: Var(X) = [(a-b)^2]/12 \n")
  cat("COMANDĂ ÎN R: punif(q, min, max)
              -> q = vector de cuantile
              -> min = a (limita inferioară a intervalului)
              -> max = b (limita superioară a intervalului) \n")
  hist(runif(10000, min = -2, max = 0.8), freq = FALSE, xlab = 'x', main = "Repartiția uniformă", col = "lightblue")
  cat("SURSA: http://cs.unitbv.ro/~pascu/stat/Distributii%20continue%20clasice.pdf")
}
