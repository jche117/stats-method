### Insight via R code  #####
### ppois is Pr(X<=x) when X ~ Poisson(lambda=5)
lam = 5; n = 1
hist(rpois(n*lam+sqrt(n*lam), lambda=n*lam))
mean(rpois(n*lam+sqrt(n*lam), lambda=n*lam))
var(rpois(n*lam+sqrt(n*lam), lambda=n*lam))

n=25
hist(rpois(n*lam+sqrt(n*lam), lambda=n*lam))
mean(rpois(n*lam+sqrt(n*lam), lambda=n*lam))
var(rpois(n*lam+sqrt(n*lam), lambda=n*lam))

n=50
hist(rpois(n*lam+sqrt(n*lam), lambda=n*lam))
mean(rpois(n*lam+sqrt(n*lam), lambda=n*lam))
var(rpois(n*lam+sqrt(n*lam), lambda=n*lam))

n=100
hist(rpois(n*lam+sqrt(n*lam), lambda=n*lam))
mean(rpois(n*lam+sqrt(n*lam), lambda=n*lam))
var(rpois(n*lam+sqrt(n*lam), lambda=n*lam))

# Using CLT approx (Xbar-lamda)/sd(xbar) ~ Normal(0, 1) when n is large
# the default setting in R assumes mean=0, sd=1
pnorm(1)

### Insight via R code  #####
par(mfrow = c(2, 2)) # 2x2 plots/page
ns = c(1, 25, 50, 100) ## various sample sizes
Nsim = 10000 # simulate these results 10,000 at a time.
Xbar = rep(0, Nsim)
for (j in 1:4) {
  # The 4 sample sizes we wish to choose from
  n = ns[j] # obtaining sample size of interest
  for (i in 1:Nsim) {  # Computing sample mean Nsim times
    Xbar[i] = mean(runif(n, lambda = lam))
  }
  # density histogram
  hist(Xbar, main = paste(" sample mean for n = ", n),
       prob = TRUE, xlab = expression(bar(X)))
  #overlay Normal(lam, sqrt(lam/n)) density on histogram
  x = seq(min(Xbar), max(Xbar), length = 1e5)
  curve(dnorm(x, mean = lam, sd = sqrt(lam/n)),
        col = "darkblue", lwd = 1, add = TRUE, yaxt = "n")
}