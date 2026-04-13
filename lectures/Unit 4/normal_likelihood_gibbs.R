### Normal/IG Gibbs Sampler


n <- nrow(penguin_data)
y <- penguin_data$bill_length_mm
y_bar <- mean(y)

sigma2 <- 9   # Assume variance of data is known

# Prior parameters
s2_mu <- 10^2   # Prior variance
m_mu <- 50    # Prior mean

alpha_0 <- 2
beta_0 <- 2

# Posterior parameters
m_star <- (sigma2 / (n * s2_mu + sigma2)) * m_mu + (n * s2_mu / (n * s2_mu + sigma2)) * y_bar
s2_star <- (sigma2 * s2_mu) / (n * s2_mu + sigma2)

#Number of samples
N.iter <- 1000
#set vectors to save samples for mu and sigma2
mu <- rep(20, N.iter)
sigma2 <- rep(10, N.iter)
mu_current <- 20
sigma2_current <- 10
for(i in 2:N.iter){
  # sample a value for mu
  m_star <- (sigma2_current / (n * s2_mu + sigma2_current)) * m_mu + (n * s2_mu / (n * s2_mu + sigma2_current)) * y_bar
  s2_star <- (sigma2_current * s2_mu) / (n * s2_mu + sigma2_current)
  mu_current <- rnorm(1, m_star, sqrt(s2_star))
  
  #sample a value for sigma2
  alpha_star <- alpha_0 + n/2
  beta_star <- beta_0 + 0.5*sum((y - mu_current)^2)
  sigma2_current <- 1 / rgamma(1, alpha_star, rate = beta_star)
  
  mu[i] <- mu_current
  sigma2[i] <- sigma2_current
  
  cat(i, "\r")
}
#joint draws
plot(mu, sigma2, type = 'l')
plot(mu, sigma2, type = 'p', col = 'blue')

#throw away "burn-in" or "warmup"
mu <- mu[-c(1:5)]
sigma2 <- sigma2[-c(1:5)]

#marginal
plot(density(sigma2))
plot(density(mu))
plot(mu, sigma2, type = 'p', col = 'blue', pch = 19)


### posterior predictive
n_samples <- length(mu)
posterior_pred <- rnorm(n_samples, mean = mu, sd = sqrt(sigma2))
plot(density(posterior_pred))
lines(density(y), col = 'red')


