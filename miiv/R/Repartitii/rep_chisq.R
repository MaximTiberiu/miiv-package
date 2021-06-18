# Repartiția chi-squared
chisquare <- function(){
  cat("Repartiția chi-squared\nDEFINIȚIE: Spunem că variabila aleatoare X are o distribuție χ^2 (Chi-squared) cu n ∈ N grade de
           libertate dacă are densitatea: \n
       |cx^(n/2 - 1)e(-x/2), x >= 0
f(x) = |
       |   0,   în rest \n
unde c = 1/(2^(n/2)Γ(n/2))\n\n")

  cat("NOTAȚIE: X ∈ χ^2(n) \n")
  cat("MEDIA: E(X) = n \n")
  cat("DISPERSIA: Var(X) = 2n \n")
  cat("COMANDĂ ÎN R: pchisq(q, df, ncp)
              -> q = vector de cuantile
              -> df = n (grade de libertate)
              -> ncp = parametrul non-central \n")
  hist(rchisq(10000, 16), freq = FALSE, xlab = 'x', main = "Repartiția chi-squared", col = "lightblue")
  cat("SURSA: http://cs.unitbv.ro/~pascu/stat/Distributii%20continue%20clasice.pdf")
}
