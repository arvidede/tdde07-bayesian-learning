
# A
y <- c(-2.44, 2.14, 2.54, 1.83, 2.01, 2.33, -2.79, 2.23, 2.07, 2.02)
k <- seq(0,10, by=0.01)
mu <- 2.39
lambda <- 1
kappa <- dexp(x,lambda)

plot(k, kappa, type = 'l')

# B -> compute the posterior mode

Mises <- function(kappa, y, mu) {
  I <- besselI(kappa,0)
  return (exp(kappa* cos (y-mu)))/(2*pi*I)
}

likelihood = Mises(kappa[1], y, mu)
