functions {
  // GEV log pdf 
  real gev_lpdf(vector y, vector mu, vector sigma, real xi){ // 
    int N = rows(y);
    real inv_xi = inv(xi); // inv(x): 1/x

    vector[N] z = rep_vector(0,N);
    vector[N] v = rep_vector(0,N);
    for (i in 1:N) z[i] = (y[i]-mu[i])/sigma[i];
    for (i in 1:N) v[i] = (1+xi*z[i])^(-inv_xi);
    
    if (xi > 0 && min(z) < -inv_xi) // support of PDF 
      reject("shape>0 and min(y-mu)/sigma > -1/xi; found xi, sigma =", xi, sigma);
    if (xi < 0 && max(z) > -inv_xi) // support of PDF 
      reject("shape<0 and max(y-mu)/sigma > -1/xi; found xi, sigma =", xi, sigma);
    if (min(sigma) <= 0) // scale parameter should be non-negative 
      reject("sigma<=0; found sigma =", sigma);
      
    if (fabs(xi) > 1e-15) // shape != 0 
      return -(inv_xi+1)*sum(log1p((xi*z))) -sum(v) -sum(log(sigma)); // log1p(x): ln(1+x)
    else                  // shape  = 0 i.e. limit xi -> 0
      return  -sum((xi+1)*z + exp(-z)) -sum(log(sigma));
  }
  
  // GEV random number generator
  real gev_rng(real mu, real sigma, real xi){
    real inv_xi = inv(xi);
    
    if (sigma<=0)
      reject("sigma<=0; found sigma =", sigma);
    
    if (fabs(xi) > 1e-15)
      return mu + sigma*inv_xi * ((log(inv(uniform_rng(0,1))))^-xi - 1);
    else
      return mu - sigma* log(-log(uniform_rng(0,1)));
      }
} 

data {
  int<lower=0> N;
  vector[N] y;
  }

parameters {
  vector[N] mu;
  real xi;
  
  real<lower=0> mu_0;
  real<lower=0> gamma;
  
  real beta1;
  real beta2;
}

transformed parameters{
  vector<lower=0>[N] sigma; 
  for (t in 1:N)
  sigma[t] = beta1*exp(-beta2*t);
}

model {
  mu[1] ~ normal(mu_0,gamma);  
  mu[2:N] ~ normal(mu[1:(N-1)],gamma);
  
  y ~ gev(mu,sigma,xi);
}

generated quantities {
  vector[N] yrep;
  for (n in 1:N)
  yrep[n] = gev_rng(mu[n],sigma[n],xi);
}
