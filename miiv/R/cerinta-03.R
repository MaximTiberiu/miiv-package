# Să se creeze un obiect de tip variabilă aleatoare continuă pornind de la o
# densitate de probabilitate introdusă de utilizator. Funcția trebuie să aibă
# opțiunea pentru variabile aleatoare unidimensionale, respectiv bidimensionale.

# Definirea clasei
setClass(class = "VAC",
         slots = list(denistate = "function", bidimensionala = "logical", suport = "list"))

# Constructor clasa
VAC <- function(densitate, bidimensionala = FALSE, suport = list(c(-Inf, Inf))) {
  res <- new("VAC", densitate = densitate, bidimensionala = bidimensionala, suport = suport)
  return (res)
}

# EROARE
# setGeneric(name = "afisare_VA", function(object) standardGeneric(f = "afisare_VA"))
# setMethod(f = "afisare_VA", signature = "VAC",
#           definition = function(object) {
#             cat("Densitatea de probabilitate: ")
#           #  print(body(fun = object@densitate))
#           })
