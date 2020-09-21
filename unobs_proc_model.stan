data {
  int<lower=0> N;              // num obs
  int<lower=1> D;              // num domains
  int<lower=1> J;              // num groups
  int<lower=1> L;              // num group predictors
  int<lower=1,upper=J> jj[N];  // group for obs
  matrix[N, D] x;              // observation-varying domain dummy codes
  row_vector[L] u[J];          // group predictors
  vector[N] y;                 // outcomes (fo)
  vector[N] z;                 // outcomes (observed arousal)
}
transformed data{
  int<lower=2> K = D*2 + 1;    // effect of presence or absence and account for intercept
}
parameters {
  corr_matrix[K] Omega;        // prior correlation
  vector<lower=0>[K] tau;      // prior scale
  matrix[L, K] gamma;          // group coeffs
  vector[K] beta[J];           // indiv coeffs by group
  real<lower=0> sigma;         // prediction error scale
  real<lower=0> epsilon;       // latent measurement error
  matrix[N, D] X_;             // unobserved arousal by domain
}
transformed parameters {
  matrix[N, D] X = X_ .* x; // latent arousal is always 0 when domain is not present.
  matrix[N, K] X_i = append_col(rep_vector(1, N), append_col(x, X));
}
model {
  tau ~ cauchy(0, 2.5);
  Omega ~ lkj_corr(2);
  epsilon ~ weibull(1, .1);    // assume very little error averaging over latent arousal
  to_vector(gamma) ~ normal(0, 5);
  to_vector(X_) ~ normal(0,5);
  {
    row_vector[D] u_gamma[J];
    for (j in 1:J)
      u_gamma[j] = u[j] * gamma;
    beta ~ multi_normal(u_gamma, quad_form_diag(Omega, tau));
  }
  for (n in 1:N){
    z[n] ~ normal(mean(X[n]), epsilon); //observed arousal should be just the mean of latent arousal when that domain is present.
    y[n] ~ normal(X_i[n] * beta[jj[n]], sigma); //fo is conditional on latent arousal
  }
}