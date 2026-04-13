#lecture_6.R

# Load packages
library(bayesrules)
library(tidyverse)
library(rstan)
library(rstanarm)
library(bayesplot)
library(tidybayes)
library(janitor)
library(broom.mixed)
rstan_options(auto_write = TRUE)##automatically save compiled model
options(mc.cores = parallel::detectCores())##use multiple cores
plot_normal(mean = 5000, sd = 1000) + 
  labs(x = "beta_0c", y = "pdf")
plot_normal(mean = 100, sd = 40) + 
  labs(x = "beta_1", y = "pdf")
plot_gamma(shape = 1, rate = 0.0008) + 
  labs(x = "sigma", y = "pdf")


# Load and plot data
data(bikes)
ggplot(bikes, aes(x = temp_feel, y = rides)) + 
  geom_point(size = 0.5) + 
  geom_smooth(method = "lm", se = FALSE)


bike_model <- stan_glm(rides ~ temp_feel, data = bikes,
                       family = gaussian,
                       prior_intercept = normal(5000, 1000),
                       prior = normal(100, 40), 
                       prior_aux = exponential(rate = 0.0008),
                       chains = 4, iter = 5000*2, seed = 84735)

#prior_summary(bike_model)

# Effective sample size ratio and Rhat
neff_ratio(bike_model)
# (Intercept)   temp_feel       sigma 
# 1.042       1.037       1.004 
rhat(bike_model)
# (Intercept)   temp_feel       sigma 
# 0.9999      0.9999      1.0000 

# Trace plots of parallel chains
mcmc_trace(bike_model, size = 0.1)

# Density plots of parallel chains
mcmc_dens_overlay(bike_model)

# stan code directly
# STEP 1: DEFINE the model
stan_bike_model <- "
  data {
    int<lower = 0> n;
    vector[n] Y;
    vector[n] X;
  }
  parameters {
    real beta0;
    real beta1;
    real<lower = 0> sigma;
  }
  model {
    Y ~ normal(beta0 + beta1 * X, sigma);
    beta0 ~ normal(-2000, 1000);
    beta1 ~ normal(100, 40);
    sigma ~ exponential(0.0008);
  }
"

# STEP 2: SIMULATE the posterior
stan_bike_sim <- 
  stan(model_code = stan_bike_model, 
       data = list(n = nrow(bikes), Y = bikes$rides, X = bikes$temp_feel), 
       chains = 4, iter = 5000*2, seed = 84735)

# Posterior summary statistics
tidy(bike_model, effects = c("fixed", "aux"),
     conf.int = TRUE, conf.level = 0.80)
# A tibble: 4 x 5
# term        estimate std.error conf.low conf.high
# <chr>          <dbl>     <dbl>    <dbl>     <dbl>
# 1 (Intercept)  -2195.     354.    -2646.    -1736. 
# 2 temp_feel       82.2      5.07     75.7      88.7
# 3 sigma         1282.      40.6    1232.     1336. 
# 4 mean_PPD      3487.      81.5    3382.     3593. 

# Store the 4 chains for each parameter in 1 data frame
bike_model_df <- as.data.frame(bike_model)

# Check it out
nrow(bike_model_df)
# [1] 20000
head(bike_model_df, 3)
# (Intercept) temp_feel sigma
# 1   -2125.091  80.92653 1230.511
# 2   -2087.977  80.60855 1200.141
# 3   -2265.712  83.64980 1365.039


# 50 simulated model lines
bikes %>%
  add_linpred_draws(bike_model, ndraws = 50) %>%
  ggplot(aes(x = temp_feel, y = rides)) +
  geom_line(aes(y = .linpred, group = .draw), alpha = 0.15) + 
  geom_point(data = bikes, size = 0.05)



# Tabulate the beta_1 values that exceed 0
bike_model_df %>% 
  mutate(exceeds_0 = temp_feel > 0) %>% 
  tabyl(exceeds_0)
# exceeds_0     n percent
# TRUE 20000       1


# Simulate four sets of data
bikes %>%
  add_predicted_draws(bike_model, ndraws = 4) %>%
  ggplot(aes(x = temp_feel, y = rides)) +
  geom_point(aes(y = .prediction, group = .draw), size = 0.2) + 
  facet_wrap(~ .draw)


first_set <- head(bike_model_df, 1)
first_set
# (Intercept) temp_feel sigma
# 1       -2657     88.16  1323


mu <- first_set$`(Intercept)` + first_set$temp_feel * 75
mu
# [1] 3944.398


set.seed(84735)
y_new <- rnorm(1, mean = mu, sd = first_set$sigma)
y_new
# [1] 4838

# Predict rides for each parameter set in the chain
set.seed(84735)
predict_75 <- bike_model_df %>% 
  mutate(mu = `(Intercept)` + temp_feel*75,
         y_new = rnorm(20000, mean = mu, sd = sigma))


head(predict_75, 3)
# (Intercept) temp_feel sigma   mu y_new
# 1       -2657     88.16  1323 3955  4838
# 2       -2188     83.01  1323 4038  3874
# 3       -1984     81.54  1363 4132  5196



# Construct 80% posterior credible intervals
predict_75 %>% 
  summarize(lower_mu = quantile(mu, 0.025),
            upper_mu = quantile(mu, 0.975),
            lower_new = quantile(y_new, 0.025),
            upper_new = quantile(y_new, 0.975))
# lower_mu upper_mu lower_new upper_new
# 1     3843     4095      1500      6482


# Plot the posterior model of the typical ridership on 75 degree days
ggplot(predict_75, aes(x = mu)) + 
  geom_density()

# Plot the posterior predictive model of tomorrow's ridership
ggplot(predict_75, aes(x = y_new)) + 
  geom_density()


# Simulate a set of predictions
set.seed(84735)
shortcut_prediction <- 
  posterior_predict(bike_model, newdata = data.frame(temp_feel = 75))


# Construct a 95% posterior credible interval
posterior_interval(shortcut_prediction, prob = 0.95)
# 2.5% 97.5%
# 1492.331 6502.669

# Plot the approximate predictive model
mcmc_dens(shortcut_prediction) + 
  xlab("predicted ridership on a 75 degree day")


bikes %>% 
  select(date, temp_feel, rides) %>% 
  head(3)
# date temp_feel rides
# 1 2011-01-01     64.73   654
# 2 2011-01-03     49.05  1229
# 3 2011-01-04     51.09  1454


### Using default rstanarm priors ####
bike_model_default <- stan_glm(
  rides ~ temp_feel, data = bikes, 
  family = gaussian,
  prior_intercept = normal(5000, 2.5, autoscale = TRUE),
  prior = normal(0, 2.5, autoscale = TRUE), 
  prior_aux = exponential(1, autoscale = TRUE),
  chains = 4, iter = 5000*2, seed = 84735)

prior_summary(bike_model_default)
# Priors for model 'bike_model_default' 
# ------
#   Intercept (after predictors centered)
# Specified prior:
#   ~ normal(location = 5000, scale = 2.5)
# Adjusted prior:
#   ~ normal(location = 5000, scale = 3937)
# 
# Coefficients
# Specified prior:
#   ~ normal(location = 0, scale = 2.5)
# Adjusted prior:
#   ~ normal(location = 0, scale = 351)
# 
# Auxiliary (sigma)
# Specified prior:
#   ~ exponential(rate = 1)
# Adjusted prior:
#   ~ exponential(rate = 0.00064)
# ------
# See help('prior_summary.stanreg') for more details

# Perform a prior simulation 
bike_default_priors <- update(bike_model_default, prior_PD = TRUE)



# 200 prior model lines
bikes %>%
  add_linpred_draws(bike_default_priors, ndraws = 200) %>%
  ggplot(aes(x = temp_feel, y = rides)) +
  geom_line(aes(y = .linpred, group = .draw), alpha = 0.15)

# 4 prior simulated datasets
set.seed(3)
bikes %>%
  add_predicted_draws(bike_default_priors, ndraws = 4) %>%
  ggplot(aes(x = temp_feel, y = rides)) +
  geom_point(aes(y = .prediction, group = .draw)) + 
  facet_wrap(~ .draw)

