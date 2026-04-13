#stan_intro.R
library(rstan)
#this allows you to run multiple chains simultaneously on different cores
options(mc.cores = parallel::detectCores())


# Simulated Data ----------------------------------------------------------



#simulate basic regression data
set.seed(123)
n <- 100
x <- rnorm(n)
y <- 2*x + rnorm(n)

#Write your Stan model (as shown earlier).

data <- list(N = n, x = x, y = y)

#Fit the model (compiles first then fits):
fit <- stan(file = 'regression.stan', 
            data = data, 
            chains = 4, 
            iter = 2000,
            warmup = 1000#if you ignore this it defaults to half of "iter"
)

#extract and interpret results:
print(fit, digits = 2)
plot(fit)

#traceplots
traceplot(fit, pars = c('beta_0', 'beta_1', 'sigma'))



# Bodyfat Data ------------------------------------------------------------


# Load required libraries
library(parallel)
library(rstanarm)# allows you to use r model syntax: stan_lm(y ~ x, data = dataset)
library(tidybayes)
library(broom.mixed)
library(tidyverse)

data <- read.csv("bodyfat.csv")

# want to predict body fat using multiple regression of height & weight

# Define the STAN model
stan_code <- "
data {
  int<lower=0> N;
  vector[N] x1;
  vector[N] x2;
  vector[N] y;
}

parameters {
  real alpha;
  real beta1;
  real beta2;
  real<lower=0> sigma;
}

model {
  //prior distributions (if omitted flat prior is used)
  beta1 ~ normal(0, 10); // equivalent to RIDGE REGRESSION on beta 1
  beta2 ~ double_exponential(0, 10);//don't need to use a normal! - LASSO on 1 parameter
  sigma ~ lognormal(2, 3);//or gamma/inverse gamma!

  y ~ normal(alpha + x1*beta1 + x2*beta2, sigma);//you feed it std. deviation not variance
}

generated quantities {//posterior predictive
  vector[N] y_pred;
  for (i in 1:N)
    y_pred[i] = normal_rng(alpha + x1[i]*beta1 + x2[i]*beta2, sigma);
}
"

# Put data in a list
stan_data <- list(N = nrow(data), 
                  x1 = data$weight, 
                  x2 = data$height, 
                  y = data$bodyfat)

# Compile the STAN model
stan_model <- stan_model(model_code = stan_code)
#or 
# stan_model <- stan_model(file = "body_fat.stan")

# Sample Using Hamiltonian Monte Carlo (HMC)
hmc_samples <- sampling(stan_model, 
                        data = stan_data, 
                        algorithm = "HMC", 
                        chains = 4, 
                        warmup = 500,
                        #thin = 3,
                        iter = 1000)
summary(hmc_samples)$summary
print(hmc_samples, digits = 2)
# Sample Using No U-Turn Sampling (NUTS)
nuts_samples <- sampling(stan_model, 
                         data = stan_data, 
                         algorithm = "NUTS", 
                         chains = 4,
                         #thin = 3,
                         iter = 1000)

print(nuts_samples, digits = 2)
# Extract and print results
# library(broom.mixed)
broom.mixed::tidy(hmc_samples)
broom.mixed::tidy(nuts_samples)


# Compare Samples ---------------------------------------------------------


#compare
model_summary_compare <- broom.mixed::tidy(nuts_samples) |> 
  rename(nuts_est = estimate, 
         nuts_stderr = std.error) |> 
  left_join(
    broom.mixed::tidy(hmc_samples) |> 
      rename(hmc_est = estimate, 
             hmc_stderr = std.error),
    by = "term"
  )

model_summary_compare


# actual samples
library(tidybayes)
get_variables(nuts_samples)
post_draws <- spread_draws(nuts_samples, c(beta1, beta2, alpha, sigma))

#plot traceplots
post_draws |> 
  ggplot(aes(x = .iteration,
             col = factor(.chain),
             y = alpha)
  ) + 
  geom_line() + 
  theme_bw() 

#post predictive samples (one method)
spread_draws(hmc_samples, y_pred[i])

spread_draws(hmc_samples, y_pred[i]) |> 
  filter(i == 1) |> 
  ggplot(aes(x = y_pred)) +
  geom_density() + 
  geom_vline(aes(xintercept = data$bodyfat[1]), col = 'red') + 
  theme_bw()
  
#what is the probability for subject 1 that with their given
### height and weight, their body fat is less than observed?
spread_draws(hmc_samples, y_pred[i]) |> 
  filter(i == 1) |> 
  summarize(mean(y_pred < data$bodyfat[1]))

# #plot posterior predictive ----------------------------------------------

# Extract posterior predictive samples
posterior_samples <- rstan::extract(nuts_samples, pars = "y_pred")$y_pred
head(posterior_samples)
dim(posterior_samples)

# Summary statistics
posterior_mean <- apply(posterior_samples, 2, mean)
posterior_lower <- apply(posterior_samples, 2, quantile, probs = 0.05)
posterior_upper <- apply(posterior_samples, 2, quantile, probs = 0.95)

# Visualizing posterior predictive samples
#  `y_obs` exists comparison
y_obs <- tibble(observed = data$bodyfat, index = 1:nrow(data))

# Prepare data for plotting
posterior_df <- data.frame(
  index = 1:ncol(posterior_samples),
  mean = posterior_mean,
  lower = posterior_lower,
  upper = posterior_upper
)

### Posterior Coverage

posterior_df |> 
  left_join(y_obs, by = "index") |>
  summarize(mean(observed < upper & observed > lower) * 100)

# Plot posterior predictive intervals with observed data
ggplot(posterior_df, aes(x = index)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.2) +
  geom_point(aes(y = mean), color = "blue", size = 1) +
  geom_point(data = y_obs, aes(x = index, y = observed), color = "red") +
  labs(
    title = "Posterior Predictive Distribution",
    x = "Index",
    y = "Predicted / Observed Value"
  ) +
  theme_minimal()





# Using BRMS --------------------------------------------------------------

library(brms)
# Fit the model
fit <- brm(
  formula = bodyfat ~ weight + height,
  data = data,
  family = gaussian(),
  # family = cumulative(link = "probit"),# if we wanted logistic/probit
  prior = c(
    prior(normal(0, 100), class = Intercept),#omit for flat prior
    prior(normal(0, 10), class = b, coef = "weight"),
    prior(double_exponential(0, 10), class = b, coef = 'height'),
    prior(lognormal(2,3), class = sigma)
    ),
  control = list(max_treedepth = 12),
  chains = 4,
  iter = 1000
)

#look at underlying stan code
stancode(fit)

#
broom.mixed::tidy(fit)
# actual samples
get_variables(fit)
spread_draws(fit, c(sigma, b_Intercept, b_weight, b_height))



# rstanarm ----------------------------------------------------------------
library(rstanarm)
#not specifying priors here, it will assume defaults (noninformative for coefficients)
bodyfat_fit <- stan_glm(
  formula = bodyfat ~ weight + height,
  data = data,
  family = gaussian(),
  # family = binomial(),# if we wanted logistic/probit
  control = list(max_treedepth = 12),
  chains = 4,
  iter = 1000
)

summary(bodyfat_fit)
#posterior predictive check
pp_check(bodyfat_fit)

# can use the same tidybayes functions as above
get_variables(bodyfat_fit)
spread_draws(bodyfat_fit, c(weight, height, sigma, `(Intercept)`))

# can use the same broom.mixed functions as above
library(broom.mixed)
tidy(bodyfat_fit)

### different prior choices using rstanarm
bodyfat_fit2 <- stan_glm(
  formula = bodyfat ~ weight + height,
  data = data,
  family = gaussian(),
  ## autoscale makes it weakly informative, if we want 2.5 to mean std. dev. of 2.5 then autoscale = FALSE
  prior_intercept = normal(50, 2.5, autoscale = TRUE),#\beta_0 prior
  prior = normal(0, 2.5, autoscale = TRUE), #\beta_p for p != 0 prior (independent)
  prior_aux = rstanarm::exponential(1, autoscale = TRUE),#\sigma prior
  chains = 4, 
  iter = 5000*2, 
  seed = 84735)
tidy(bodyfat_fit2)


### different prior choices using rstanarm
bodyfat_fit3 <- stan_glm(
  formula = bodyfat ~ weight + height,
  data = data,
  family = gaussian(),
  ## autoscale makes it weakly informative, if we want 2.5 to mean std. dev. of 2.5 then autoscale = FALSE
  prior_intercept = normal(50, 2.5, autoscale = FALSE),#\beta_0 prior
  prior = normal(0, 2.5, autoscale = FALSE), #\beta_p for p != 0 prior (independent)
  prior_aux = rstanarm::exponential(1, autoscale = FALSE),#\sigma prior
  chains = 4, 
  iter = 5000*2, 
  seed = 84735)
tidy(bodyfat_fit3)
