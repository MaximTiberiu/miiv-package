# Afișați o „fișă de sinteză” care să conțină informații de bază despre respectiva
# repartiție (cu precizarea sursei informației). Relevant aici ar fi să precizați
# pentru ce e folosită în mod uzual acea repartiție, semnificația parametrilor, media, dispersia etc.

afisare_fisa <- function() {
  cat("Introduceți numărul corespunzător repartiției dorite.\n")
  cat("1. Repartiție uniformă\n")
  cat("2. Repartiție normală\n")
  cat("3. Repartiție exponențială\n")
  cat("4. Repartiția Chi-Square\n")
  enter <- as.numeric(readline(prompt = "Repartitia aleasă este: "))

  #setwd('./Repartitii/')
  switch(  EXPR = enter,
           {
             uniforma()
             # cat("Dorești salvarea fișei de sinteză?\n")
             # cat("1. Da\n")
             # cat("2. Nu\n")
             # opt <- as.numeric(readline(prompt = "Optiunea aleasa este: "))
             # if(opt == 1) {
             #   knitr::stitch('rep_uniforma.R')
             # }
           },
           {
            normala()
             # cat("Dorești salvarea fișei de sinteză?\n")
             # cat("1. Da\n")
             # cat("2. Nu\n")
             # opt <- as.numeric(readline(prompt = "Optiunea aleasa este: "))
             # if(opt == 1) {
             #   #knitr::stitch('cerinta-08.R')
             #   rmarkdown::render('cerinta-08.R', 'pdf_document')
             # }
           },
           {
             exponential()
           },

           {
             chisquare()
           }
  )
}

afisare_fisa()
