### Poisson Gamma Conjugate
#simulate dataset y ~ Poisson(lambda) (33 data points)
n = 33
lambda_true = 2
set.seed(5)#set seed for reproducibility
y <- rpois(n, lambda_true)

hist(y, freq = FALSE)

#prior
# I will use a gamma prior for lambda
#lambda ~ Gamma(alpha, beta)

#suppose I want to use moment matching to set the prior
#E[lambda] = alpha/beta
#Var[lambda] = alpha/beta^2
#I want E[lambda] = 10 and Var[lambda] = 15
prior_mean <- 10
prior_var <- 15
#solving for alpha and beta
alpha <- prior_mean^2/prior_var
beta <- prior_mean/prior_var

### posterior is conjugate  ###
# lambda|y ~ Gamma(alpha_post, beta_post)
alpha_post <- alpha + sum(y)
beta_post <- beta + n

#posterior mean
lambda_post_mean <- alpha_post/beta_post

#posterior variance
lambda_post_var <- alpha_post/beta_post^2

#plot posterior distribution of lambda|y
curve(dgamma(x, alpha_post, beta_post),
      from = 0, to = 5, 
      xlab = "lambda",
      ylab = "Density", 
      main = "Posterior distribution of lambda|y")


#posterior predictive - take 100 samples from posterior predictive
n_post_pred <- 100 
set.seed(5)
#sample lambda from posterior
lambda_post <- rgamma(n_post_pred, alpha_post, beta_post)
#sample new y using samples of lambda from posterior
y_post <- rpois(n_post_pred, lambda_post)

hist(y_post, freq = FALSE)
