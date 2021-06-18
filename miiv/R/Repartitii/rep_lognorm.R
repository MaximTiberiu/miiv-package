# Repartiția log-normală
log_normal <- function(){
  cat("Repartiția log-normală\nDEFINIȚIE: Spunem că X este o variabilă aleatoare log-normala de parametrii(σ, σ^2)
          dacă X are funcția de distribuție dată de: \n

f(x) =(1/(σ*x*√2π))*e^(-(ln x-m)^2/(2*σ^2))\n\n")
  cat("NOTAȚIE: X ∈ LN(σ,σ^2)\n")
  cat("MEDIA: E(X) = e^(m + (σ^2)/2\n")
  cat("DISPERSIA: Var(X) = [e^(σ^2)-1]e^(2*m+σ^2)\n")
  cat("COMANDĂ ÎN R: plnorm(q, meanlog, sdlog)
              -> q = vector de cuantile
              -> meanlog = media
              -> sdlog = deviatia standard \n")
  hist(rlnorm(20000, 0, 0.50), freq = FALSE, xlab = 'x', main = "Repartiția log-normală", col = "lightblue")
  cat("SURSA: https://www.afahc.ro/ro/facultate/cursuri/luculescu/4.%20Functii%20de%20repartitie.pdf")
}
