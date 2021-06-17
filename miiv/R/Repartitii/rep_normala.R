#Repartiția normală
normala <- function(){
  cat("Repartiția normală\nDEFINIȚIE: Spunem că variabila aleatoare X are o distribuție normală cu parametrii μ și σ^2
           dacă X are densitatea: \n
f(x) = (1/√2πσ^2)e^(-(x-μ)^2/2σ^2), X ∈ R\n\n")

  cat("NOTAȚIE: X ∈ N(μ, σ^2)\n")
  cat("MEDIA: E(X) = μ\n")
  cat("DISPERSIA: Var(X) = σ^2\n")
  cat("COMANDĂ ÎN R: pnorm(q, mean, sd)
              -> q = vector de cuantile
              -> mean = μ (media)
              -> sd = σ (deviația standard) \n")
  hist(rnorm(10000), freq = FALSE, xlab = 'x', main = "Repartiția normală", col = "lightblue")
  cat("SURSA: http://cs.unitbv.ro/~pascu/stat/Distributii%20continue%20clasice.pdf")
}
