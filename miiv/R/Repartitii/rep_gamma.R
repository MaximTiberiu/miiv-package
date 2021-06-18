# Repartiția Gamma
gamma <- function(){
  cat("Repartiția Gamma\nDEFINIȚIE: Spunem că variabila aleatoare X are o repartiție Gamma cu parametrii
          η și λ dacă are densitatea: \n
f(x) = (λ^η/Γ(η))x^(n-1)e(-λx), x >= 0, λ > 0, η > 0

unde Γ(η) = integrate( f = x^(η-1)e(-x), lower = 0, upper = Inf) $ value\n\n")
  cat("NOTAȚIE: X ∈ Γ(x; η, λ) \n")
  cat("MEDIA: E(X) = η/λ \n")
  cat("DISPERSIA: Var(X) = η/λ^2 \n")
  cat("COMANDĂ ÎN R: pgamma(q, shape, rate)
              -> q = vector de cuantile
              -> shape = η (parametru de formă)
              -> rate = λ \n")
  hist(rgamma(10000, 10), freq = FALSE, xlab = 'x', main = "Repartiția Gamma", col = "lightblue")
  cat("SURSA: https://www.afahc.ro/ro/facultate/cursuri/luculescu/4.%20Functii%20de%20repartitie.pdf")
}
