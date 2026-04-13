

# Posterior Intervals -----------------------------------------------------
library(HDInterval)
data(mtcars)

# Data setup
y <- mtcars$mpg
n <- length(y)
y_bar <- mean(y)
hist(y)
# Known variance
sigma2 <- 9  # Variance of data
s2_mu <- 25  # Prior variance
m_mu <- 22   # Prior mean

# Posterior parameters
m_star <- (sigma2 / (n * s2_mu + sigma2)) * m_mu + (n * s2_mu / (n * s2_mu + sigma2)) * y_bar
s2_star <- (sigma2 * s2_mu) / (n * s2_mu + sigma2)

# Generate posterior samples
posterior <- rnorm(1000, mean = m_star, sd = sqrt(s2_star))
plot(density(posterior))
# Central credible interval
central_ci <- quantile(posterior, probs = c(0.025, 0.975))
central_ci
## OR since we have a conjugate prior we can calculate the posterior in closed form
qnorm(c(0.025, 0.975), m_star, sqrt(s2_star))

#Another credible interval (not central) from samples
non_central_ci <- quantile(posterior, probs = c(0.01, 0.96))
non_central_ci
#same non-central credible interval in closed form
qnorm(c(0.01, 0.96), m_star, sqrt(s2_star))

# HPDI using HDInterval
hpdi <- hdi(posterior, credMass = 0.95)
hpdi
cat("Data Sample Mean:", y_bar, "\n")
cat("Prior Mean:", m_mu, "\n")
cat("Posterior Mean:", m_star, "\n")
cat("Central CI:", central_ci, "\n")
cat("Another CI:", non_central_ci, "\n")
cat("HPDI:", hpdi, "\n")

# R Code for Posterior Hypothesis Testing ---------------------------------

  
data(mtcars)

# Data setup
y <- mtcars$mpg
n <- length(y)
y_bar <- mean(y)

# Prior parameters
sigma2 <- 9   # Variance of data
s2_mu <- 25   # Prior variance
m_mu <- 22    # Prior mean

# Posterior parameters
m_star <- (sigma2 / (n * s2_mu + sigma2)) * m_mu + (n * s2_mu / (n * s2_mu + sigma2)) * y_bar
s2_star <- (sigma2 * s2_mu) / (n * s2_mu + sigma2)



# Generate posterior samples
posterior <- rnorm(10000, mean = m_star, sd = sqrt(s2_star))
hist(posterior)
# Hypothesis testing

# P(mu > 20)
p_h1 <- mean(posterior > 20)  
1 - pnorm(20, m_star, sqrt(s2_star))

cat("Posterior Mean:", m_star, "\n")
cat("P(mu > 20):", p_h1, "\n")


# P(mu > 25 & mu < 30)
mean(posterior > 25 & posterior < 30)

# Posterior Predictive ----------------------------------------------------

library(palmerpenguins)
data(penguins)
penguin_data <- penguins |> 
  dplyr::filter(!is.na(bill_length_mm))

n <- nrow(penguin_data)
y <- penguin_data$bill_length_mm
y_bar <- mean(y)

sigma2 <- 9   # Assume variance of data is known

# Prior parameters
s2_mu <- 10^2   # Prior variance
m_mu <- 50    # Prior mean

# Posterior parameters
m_star <- (sigma2 / (n * s2_mu + sigma2)) * m_mu + (n * s2_mu / (n * s2_mu + sigma2)) * y_bar
s2_star <- (sigma2 * s2_mu) / (n * s2_mu + sigma2)


# Samples 
posterior_mu <- rnorm(1000, mean = m_star, sd = sqrt(s2_star) )
#what is this a plot of?
hist(posterior_mu, freq = FALSE)
lines(density(posterior_mu), col = 'red')

#once again we could just plot this density since we know it in closed form
# 
curve(dnorm(x, mean = m_star, sd = sqrt(s2_star)),
      from = 42, to = 46)

#the sample size here (1000) and the number of posterior draws (1000) must match here
posterior_pred <- rnorm(1000, mean = posterior_mu, sd = sqrt(sigma2))
#what are these plots of? 
hist(posterior_pred)

hist(posterior_pred, freq = FALSE)

plot(density(posterior_pred))
lines(density(y), col = 'red')

# Visualize
hist(posterior_pred, main = "Posterior Predictive for Bill Length", xlab = "Bill Length (mm)")


#can we plot this posterior predictive in closed form?