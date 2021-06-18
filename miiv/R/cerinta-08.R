# Afișați o „fișă de sinteză” care să conțină informații de bază despre respectiva
# repartiție (cu precizarea sursei informației). Relevant aici ar fi să precizați
# pentru ce e folosită în mod uzual acea repartiție, semnificația parametrilor, media, dispersia etc.

afisare_fisa <- function() {
  cat("Introduceți numărul corespunzător repartiției dorite.\n")
  cat("1. Repartiție uniformă\n")
  cat("2. Repartiție normală\n")
  cat("3. Repartiție exponențială\n")
  cat("4. Repartiția Chi-Square\n")
  cat("5. Repartiția Beta\n")
  cat("6. Repartiția Gamma\n")
  cat("7. Repartiția Log-normală\n")
  enter <- as.numeric(readline(prompt = "Repartitia aleasă este: "))

  switch(  EXPR = enter,
           {
             uniforma()
           },
           {
            normala()
           },
           {
             exponential()
           },
           {
             chisquare()
           },
           {
             beta()
           },
           {
             gamma()
           },
           {
             log_normal()
           }
  )
}

afisare_fisa()
