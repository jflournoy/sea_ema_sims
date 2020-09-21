library(rstan)
#This simulation and the stan file will, I hope, clarify the data a little bit.
source('simulate_data.R')
set.seed(6793)
rstan_options(auto_write = TRUE)
options(mc.cores = 4)

d <- simulate_data()
names(d)
## Presence or absence (0, 1) of Parent, ..., Other stress:
#[1] "parent"   "peer"     "academic" "other"    
#
## stress level, id index, negative affect
#"stress"   "ids"      "na"

#(Compare the simulated data to real data)
d_real <- readr::read_csv('SEA_EMA_long.csv')
names(d_real)
names(d_real)[c(3, 26:31)]
## id index, stress level, raw source of stress 
#[1] "ID" "stress_level" "stress_domain"
#
## Presence or absence of peer, family, other stress (and a missing response
## dummy) 
#"peer_stress"   "family_stress" "other_stress"  "NoData_stress"
#
## Note: like the simulated data, if a child reports "1 - no stress" for the 
## stress_level variable, we might code that as not having a domain, by
## definition.
## How often does this come up?
domain_but_no_stress_idx <- apply(d_real, 1, function(x){
  domain <- 1 %in% x[c('peer_stress', 'family_stress', 'other_stress')]
  no_stress <- x['stress_level'] == '1 - Not at all'
  return(domain & no_stress)
})
#A lot of responses have a domain selected but stress level of "Not at all"
sum(domain_but_no_stress_idx, na.rm = TRUE)
mean(domain_but_no_stress_idx, na.rm = TRUE)

###
# Fit stan model to simulated data:
##

domain_cols <- c("parent", "peer", "academic", "other")
N <- dim(d)[1]
D <- length(domain_cols)
K <- D*2 + 1
stan_data <- list(
  N = N,
  D = D,
  J = length(unique(d$ids)),
  L = 1,
  jj = d$ids,
  x = d[, domain_cols],
  u = matrix(rep(1, length(unique(d$ids))), ncol = 1),
  y = d$na,
  z = d$stress
)

#Exclude some parameters from being returned from the unobs data matrix:
#matrix[N, D] X
#matrix[N, K] X_i
ND <- expand.grid(1:N, 1:D)
NK <- expand.grid(1:N, 1:K)
exclude_pars <- c(paste0('X[', apply(ND, 1, paste, collapse = ','), ']'),
                  paste0('X_i[', apply(NK, 1, paste, collapse = ','), ']'))

fit_fn <- 'unobs_proc_model_fit.rds'
if(!file.exists(fit_fn)){
  fit <- rstan::stan('unobs_proc_model.stan',
                     model_name = 'unobs_proc',
                     data = stan_data,
                     iter = 4000,
                     warmup = 1000,
                     chains = 4,
                     cores = 4,
                     pars = exclude_pars, include = FALSE,
                     control = list(adapt_delta = .999, max_treedepth = 20))
  saveRDS(fit, fit_fn)
} else {
  fit <- readRDS(fit_fn)
}