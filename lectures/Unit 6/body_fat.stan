data {
  int<lower=0> N;
  vector[N] x1;
  vector[N] x2;
  vector[N] y;
  //matrix[N,p] X; //must define p for this to work
}

transformed data{
  // vector[N] x1_transformed ;
  // 
  // 
  // x1_tranformed = x1 / 3.0;
}

parameters {
  real alpha;
  real beta1;
  real beta2;
  real<lower=0> sigma;
}

transformed parameters{
  // real sumbeta;
  // sumbeta = beta1 + beta2;
}

model {
  
  // sumbeta ~ normal(0, 0.0000001);//soft-constraint to sum 0 of parameters
  
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

