# Repartiția beta

beta <- function(){
  cat("Repartiția beta\nDEFINIȚIE: : Spunem ca o variabila aleatoare X este repartizata Beta
            de parametrii (α, β), cu α, β > 0, daca densitatea ei are forma: \n
      | f(x) = (1/B(α, β))* (x ^ (α-1))*(1 - x)^(β-1), 0 ≤ x ≤ 1,
f(x)= |
      | 0, în rest \n\n

unde B(α, β) este funcția (Beta, numită s, i integrala Euler de primul tip) definita prin B(α, β) = integrala de la 0 la infinit din ((x ^ (α-1)) *(1 - x)^(β-1) dx, α, β > 0. \n\n\n")
  cat("NOTAȚIE: X ∈ Beta(α, β)\n")
  cat("MEDIA: E(X) = α/(α+β) \n")
  cat("DISPERSIA: Var(X) =(αβ)/((α+β)^2*(α+β+1)) \n")
  cat("COMANDĂ ÎN R: pbeta(x, shape1, shape2)
              -> x = vector de cuantile
              -> shape1 = α
              -> shape2= β \n")
  hist(rbeta(10000,5,2), freq = FALSE, xlab = 'x', main = "Repartiția beta", col = "lightblue")
  cat("SURSA: http://cs.unitbv.ro/~pascu/stat/Distributii%20continue%20clasice.pdf")
}
