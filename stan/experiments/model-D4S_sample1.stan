functions {
  // GEV log pdf 
  real gev_lpdf(vector y, vector mu, real sigma, real xi){ // 
    int N = rows(y);
    real inv_xi = inv(xi); // inv(x): 1/x
    vector[N] z = (y-mu)/sigma;
    vector[N] v = rep_vector(0,N);
    for (i in 1:N) v[i] = (1+xi*z[i])^(-inv_xi);
    
    if (xi > 0 && min(z) < -inv_xi) // support of PDF 
      reject("shape>0 and min(y-mu)/sigma > -1/xi; found xi, sigma =", xi, sigma);
    if (xi < 0 && max(z) > -inv_xi) // support of PDF 
      reject("shape<0 and max(y-mu)/sigma > -1/xi; found xi, sigma =", xi, sigma);
    if (sigma <= 0) // scale parameter should be non-negative 
      reject("sigma<=0; found sigma =", sigma);
      
    if (fabs(xi) > 1e-15) // shape != 0 
      return -(inv_xi+1)*sum(log1p((xi*z))) -sum(v) -N*log(sigma); // log1p(x): ln(1+x)
    else                  // shape  = 0 i.e. limit xi -> 0
      return  -sum((xi+1)*z + exp(-z)) -N*log(sigma);
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
  vector[N] season;
  real<lower=0> sigma;
  real<lower=-0.75,upper=0.75> xi;
  
  real<lower=0> mean_0;
  real<lower=0> gamma;
  real<lower=0> gamma_s;
}

transformed parameters{
  real g1;
  vector<lower=0>[N] Mean;
  vector[N] mu_s;
  vector[N-11] season_sum;
  
  mu_s = mu + season;
  for(t in 12:N)
  season_sum[t-11] = sum(season[(t-11):t]); // season_sum[1] =  sum(season[1:12]) ~ N(0,s)
  
  g1 = tgamma(1-xi);
  if (fabs(xi) > 1e-15)   // shape != 0
    Mean = mu_s + sigma*(g1-1)/xi;
  else                    // shape == 0
    Mean = mu_s + sigma*.57721;
}

model {
  gamma ~ normal(0,1);
  gamma_s ~ cauchy(0,1);
  
  Mean[1] ~ normal(mean_0, gamma);
  Mean[2:N] ~ normal(Mean[1:(N-1)], gamma);
  
  season_sum ~ normal(0,gamma_s);

  y ~ gev(mu_s,sigma,xi);
}

generated quantities {
  vector[N] yrep;
  for (n in 1:N)
  yrep[n] = gev_rng(mu_s[n],sigma,xi);
}
