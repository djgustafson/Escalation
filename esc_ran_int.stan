data{
  int<lower=0> n; // Number of obs.
  int<lower=0> b; // Number effect predictors
  matrix[n,b] X; // Predictor matrix
  int y[n]; // Outcome vector
  int j; // Number of countries
  int k; // Number of years
  int country[n]; // Country id
  int year[n]; // Year id
}

parameters{
  vector[b] beta; // Beta coefficients
  vector[j] u_raw; // Country intercepts (raw)
  vector[k] w_raw; // Year intercepts (raw)
  real<lower=0> sigma_u; // Country sd
  real<lower=0> sigma_w; // Year sd
}

transformed parameters{
  vector[j] u;
  vector[k] w;
  u = sigma_u * u_raw; // Implies u ~ normal(0, sigma_u)
  w = sigma_w * w_raw; // Implies w ~ normal(0, sigma_w)
}

model{
  sigma_u ~ normal(0,100);
  sigma_w ~ normal(0,100);
  
  u_raw ~ normal(0,1);
  w_raw ~ normal(0,1);
  
  beta[1] ~ cauchy(0,10); // Intercept
  beta[2:b] ~ cauchy(0, 2.5); // Predictors
  
  for (i in 1:n){
    y[i] ~ bernoulli_logit(X[i,] * beta + u[country[i]] + w[year[i]]);
  }
}

generated quantities{
  real p[n];
  
  for(i in 1:n){
    p[i] = inv_logit(X[i,] * beta + u[country[i]] + w[year[i]]);
  }
}
