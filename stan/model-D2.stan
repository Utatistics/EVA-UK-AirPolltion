functions { // FUNCTION DEFINITON BLOCK!!
  
  // generalised Pareto log pdf 
  real gpareto_lpdf(vector y, vector u, real sigma, real xi){ // ymin: thr, k: shape, sigma: scale, 
    int N = rows(y);
    real inv_xi = inv(xi); // inv(x): 1/x
    
    if (xi < 0 && max(y-u)/sigma > -inv_xi) // support of PDF 
      reject("k<0 and max(y-ymin)/sigma > -1/k; found k, sigma =", xi, sigma);
    if (sigma <= 0) // scale parameter should be non-negative 
      reject("sigma<=0; found sigma =", sigma);
      
    if (fabs(xi) > 1e-15) // shape not 0 *fabs(x): absolute value of x
      return -(1+inv_xi)*sum(log1p((y-u) * (xi/sigma))) -N*log(sigma); // log1p(x): ln(1+x)
    else // shape = 0 i.e. limit k -> 0
      return -sum(y-u)/sigma -N*log(sigma); 
  }
  
  // generalised Pareto cdf
  real gpareto_cdf(vector y, vector u, real sigma, real xi){ 
    real inv_xi = inv(xi);
    
    if (xi < 0 && max(y-u)/sigma > -inv_xi)
      reject("k<0 and max(y-ymin)/sigma > -1/k; found k, sigma =", xi, sigma);
    if (sigma <= 0)
      reject("sigma<=0; found sigma =", sigma);
      
    if (fabs(xi) > 1e-15)
      return exp(sum(log1m_exp((-inv_xi)*(log1p((y-u) * (xi/sigma)))))); //log1m_exp(x): ln{1-exp(x)}
    else
      return exp(sum(log1m_exp(-(y-u)/sigma))); // limit k->0
  }
  
  // generalised Pareto log cdf
  real gpareto_lcdf(vector y, vector u, real sigma, real xi){
    real inv_xi = inv(xi);
    
    if (xi<0 && max(y-u)/sigma > -inv_xi)
      reject("k<0 and max(y-ymin)/sigma > -1/k; found k, sigma =", xi, sigma);
    if (sigma<=0)
      reject("sigma<=0; found sigma =", sigma);
      
    if (fabs(xi) > 1e-15)
      return sum(log1m_exp((-inv_xi)*(log1p((y-u) * (xi/sigma)))));
    else
      return sum(log1m_exp(-(y-u)/sigma)); // limit k->0
  }
  
  // generalised Pareto log ccdf
  real gpareto_lccdf(vector y, vector u, real sigma, real xi){
    real inv_xi = inv(xi);
    
    if (xi<0 && max(y-u)/sigma > -inv_xi)
      reject("k<0 and max(y-ymin)/sigma > -1/k; found k, sigma =", xi, sigma);
    if (sigma<=0)
      reject("sigma<=0; found sigma =", sigma);
      
    if (fabs(xi) > 1e-15)
      return (-inv_xi)*sum(log1p((y-u) * (xi/sigma)));
    else
      return -sum(y-u)/sigma; // limit k->0
  }
  
  // generalised Pareto rng
  real gpareto_rng(vector u, real sigma, real xi){
    if (sigma<=0)
      reject("sigma<=0; found sigma =", sigma);
    
    if (fabs(xi) > 1e-15)
      return u + (uniform_rng(0,1)^-xi -1) * sigma / xi;
    else
      return u - sigma*log(uniform_rng(0,1)); // limit k->0
  }
}

data {
  int<lower=0> N;
  vector[N] y;
}

transformed data {
  real ymax = max(y);
}

parameters {
  vector[N] u;
  real<lower=0> sigma; 
  real xi; // support  of PDF
}

model {
  y ~ gpareto(u,sigma,xi);
}
