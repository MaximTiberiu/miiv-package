# Repartiția exponențială
exponential <- function(){
  cat("Repartiția exponențială\nDEFINIȚIE: Spunem că variabila aleatoare X are o distribuție exponențială cu parametrul λ > 0
           dacă X are densitatea dată de: \n
       |λe^(-λx), x > 0
f(x) = |
       |   0,   în rest \n\n")

  cat("NOTAȚIE: X ∈ Exp(λ)\n")
  cat("MEDIA: E(X) = 1/λ \n")
  cat("DISPERSIA: Var(X) = 1/λ^2 \n")
  cat("COMANDĂ ÎN R: pexp(q, rate)
              -> q = vector de cuantile
              -> rate = λ \n")
  hist(rexp(10000), freq = FALSE, xlab = 'x', main = "Repartiția exponențială", col = "lightblue")
  cat("SURSA: http://cs.unitbv.ro/~pascu/stat/Distributii%20continue%20clasice.pdf")
}
