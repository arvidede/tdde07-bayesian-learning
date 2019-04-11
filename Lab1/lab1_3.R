
# A: Plot the posterior distribution of k
# posterior(k | y, mu) = likelihood * prior(k) = prod_prob * prior

y <- c(-2.44, 2.14, 2.54, 1.83, 2.01, 2.33, -2.79, 2.23, 2.07, 2.02)
kSeq <- seq(0,10, by=0.01)
mu <- 2.39
lambda <- 1
# kappa <- dexp(kSeq, lambda)

mises <- function(k, y, mu) {
  I <- besselI(k,0)
  return ((exp(k * cos(y - mu))) / (2 * pi * I))
}

kPos <- function(k, mu,y) {
  return ( prod( mises(k, y, mu) ) * dexp(k))
}

posterior = c()
for (k in kSeq){
  posterior = c(posterior, c(kPos(k, mu, y)))
}

plot(kSeq, posterior)


# B: Compute the posterior mode of k

kPosMode <- kSeq[which.max(posterior)]