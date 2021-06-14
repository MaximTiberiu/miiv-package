# Să se construiască suma și diferența a două variabile aleatoare continue independente,
# folosind formula de convoluție.

# suma a doua variabile aleatoare, folosind formula de convolutie
sumaVA_convolutie <- function(f, g) {
  function(z) (integrate(f = function(x) (f(x) * g(z - x)), lower = -Inf, upper = Inf) $ value)
}

# diferenta a doua variabile aleatoare, folosind formula de convolutie
difVA_convolutie <- function(f, g) {
  function(z) (integrate(f = function(x) (f(x) * g(x - z)), lower = -Inf, upper = Inf) $ value)
}


f.X <- function(X) dnorm(X, 1, 0.5)
f.Y <- function(Y) dlnorm(Y, 1.5, 0.75)

f.S <- sumaVA_convolutie(f.X, f.Y)
f.S <- Vectorize(f.S)
f.D <- difVA_convolutie(f.X, f.Y)
f.D <- Vectorize(f.D)

X <- rnorm(1000, 1, 0.5)
Y <- rlnorm(1000, 1.5, 0.75)
S <- X + Y
D <- X - Y

hist(S, freq=F, breaks=50, xlim=c(0, 30))
s <- seq(0, 50, 0.01)
lines(s, f.S(s), lty=2, col="red")

hist(D, freq=F, breaks=50, xlim=c(0, 30))
d <- seq(0, 50, 0.01)
lines(d, f.D(d), lty=2, col="blue")
