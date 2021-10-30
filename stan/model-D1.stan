functions { // FUNCTION DEFINITON BLOCK!!
  
  // generalised Pareto log pdf 
  real gpareto_lpdf(vector y, real ymin, real k, real sigma){ // ymin: thr, k: shape, sigma: scale, 
    int N = rows(y);
    real inv_k = inv(k); // inv(x): 1/x
    
    if (k < 0 && max(y-ymin)/sigma > -inv_k) // support of PDF 
      reject("k<0 and max(y-ymin)/sigma > -1/k; found k, sigma =", k, sigma);
    if (sigma <= 0) // scale parameter should be non-negative 
      reject("sigma<=0; found sigma =", sigma);
      
    if (fabs(k) > 1e-15) // shape not 0 *fabs(x): absolute value of x
      return -(1+inv_k)*sum(log1p((y-ymin) * (k/sigma))) -N*log(sigma); // log1p(x): ln(1+x)
    else // shape = 0 i.e. limit k -> 0
      return -sum(y-ymin)/sigma -N*log(sigma); 
  }
  
  // generalised Pareto cdf
  real gpareto_cdf(vector y, real ymin, real k, real sigma){ 
    real inv_k = inv(k);
    
    if (k < 0 && max(y-ymin)/sigma > -inv_k)
      reject("k<0 and max(y-ymin)/sigma > -1/k; found k, sigma =", k, sigma);
    if (sigma <= 0)
      reject("sigma<=0; found sigma =", sigma);
      
    if (fabs(k) > 1e-15)
      return exp(sum(log1m_exp((-inv_k)*(log1p((y-ymin) * (k/sigma)))))); //log1m_exp(x): ln{1-exp(x)}
    else
      return exp(sum(log1m_exp(-(y-ymin)/sigma))); // limit k->0
  }
  
  // generalised Pareto log cdf
  real gpareto_lcdf(vector y, real ymin, real k, real sigma){
    real inv_k = inv(k);
    
    if (k<0 && max(y-ymin)/sigma > -inv_k)
      reject("k<0 and max(y-ymin)/sigma > -1/k; found k, sigma =", k, sigma);
    if (sigma<=0)
      reject("sigma<=0; found sigma =", sigma);
      
    if (fabs(k) > 1e-15)
      return sum(log1m_exp((-inv_k)*(log1p((y-ymin) * (k/sigma)))));
    else
      return sum(log1m_exp(-(y-ymin)/sigma)); // limit k->0
  }
  
  // generalised Pareto log ccdf
  real gpareto_lccdf(vector y, real ymin, real k, real sigma){
    real inv_k = inv(k);
    
    if (k<0 && max(y-ymin)/sigma > -inv_k)
      reject("k<0 and max(y-ymin)/sigma > -1/k; found k, sigma =", k, sigma);
    if (sigma<=0)
      reject("sigma<=0; found sigma =", sigma);
      
    if (fabs(k) > 1e-15)
      return (-inv_k)*sum(log1p((y-ymin) * (k/sigma)));
    else
      return -sum(y-ymin)/sigma; // limit k->0
  }
  
  // generalised Pareto rng
  real gpareto_rng(real ymin, real k, real sigma){
    if (sigma<=0)
      reject("sigma<=0; found sigma =", sigma);
    
    if (fabs(k) > 1e-15)
      return ymin + (uniform_rng(0,1)^-k -1) * sigma / k;
    else
      return ymin - sigma*log(uniform_rng(0,1)); // limit k->0
  }
}

data {
  real ymin;
  int<lower=0> N;
  vector<lower=ymin>[N] y;
}

transformed data {
  real ymax = max(y);
}

parameters {
  real<lower=0> sigma; 
  real<lower=-sigma/(ymax-ymin)> k; // support  of PDF
}

model {
  y ~ gpareto(ymin,k,sigma);
}
