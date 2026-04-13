//regression.stan
data {
  int<lower=0> N;
  vector[N] x;
  vector[N] y;
}
parameters {
  real beta_0;
  real beta_1;
  real<lower=0> sigma;
}
model {
//prior
  beta_0 ~ normal(0, 100);//weakly informative priors
  beta_1 ~ normal(0, 100);
  sigma ~ cauchy(0, 5);// a "half-cauchy" prior because sigma is constrained

// likelihood
  y ~ normal(beta_0 + beta_1 * x, sigma);
}

