simulate_data <- function(N = 100, obs = 30, nn = N*obs, ids = rep(1:N, each = obs)){
  
  #Simulate whether or not certain stress experiences happened across several
  #days for several participants (we assume they are independent):
  stressors <- data.frame(parent = rbinom(nn, size = 1, prob = .2),
                          peer = rbinom(nn, size = 1, prob = .2),
                          academic = rbinom(nn, size = 1, prob = .2),
                          other = rbinom(nn, size = 1, prob = .5))
  
  #Simulate possible level of stress experienced for each stressor. Could
  #specify a more complicated dgp here than just rnorm.
  stress_scores <- function(n){
    rp <- rnorm(n)
    return(rp)
  }
  possible_stress <- data.frame(parent = stress_scores(nn),
                                peer = stress_scores(nn),
                                academic = stress_scores(nn),
                                other = stress_scores(nn))
  #Cell-by-cell multiplication. If they experienced a stressor, the level of
  #stress is determined by the corresponding possible_stress score.
  experienced_stress <- stressors * possible_stress 
  
  ###
  #Create the observed scores for reported arousal:
  ##
  
  #I'm going to assume that a person will report an average level of stress
  #across all experiences, with some error.
  avging_error <- 0 # setting to 0 for now
  stress_reported <- apply(experienced_stress, 1, function(arow){
    n_strsrs <- sum(arow != 0)
    if(n_strsrs == 0){
      reported_stress <- 0
    } else {
      total_stress <- sum(arow)
      average_stress <- total_stress / n_strsrs + rnorm(1, 0, avging_error)
      #there may be other things that intervene on this process, but keep it simple for now.
      reported_stress <- average_stress
    }
    return(reported_stress)
  })
  #translate the reported stress to a likert type measurement scale
  mn <- min(stress_reported)
  mx <- max(stress_reported)
  stress_reported <- round((stress_reported - mn) / (mx - mn) * 6)
  stress_reported <- ifelse(stress_reported > 6, 5, stress_reported)
  #ensure that if folks report no stressful experiences, they report no stress
  stress_reported <- stress_reported * as.numeric(rowSums(stressors)>0) + 1
  
  #create the matrix for the data generating process of the effect of stress on
  #negative arousal the first columns are dummy codes for experiencing the emo
  #and the second set of columns are the corresponding stress scores.
  latent_X <- cbind(stressors, experienced_stress, ids)
  names(latent_X) <- c(names(latent_X)[1:4], paste0(names(latent_X)[5:8], '_s'), names(latent_X)[9])
  
  #id varying intercept
  fo_intercept <- rnorm(N)
  
  #assuming no effect of experiencing a particular stressor, per se.
  pop_coefs <- c(0, 0, 0, 0, 5, -2, -6, 1)
  id_coefs <- lapply(1:N, function(i, coefs){
    c <- rnorm(length(coefs), mean = coefs, sd = .5)
    return(c)
  }, coefs = pop_coefs)
  
  true_na <- unlist(lapply(X = 1:N, FUN = function(id, Xmat, intercept, idcol){
    X <- Xmat[Xmat[,idcol] == id, -idcol] #select rows from X for id in idcol
    y <- intercept[[id]] + as.matrix(X) %*% id_coefs[[id]] + rnorm(dim(X)[1], 0, 1)
    return(y)
  }, Xmat = latent_X, intercept = fo_intercept, idcol = 9))
  
  #Put the observed NA score on the instrument scale
  obs_na <- round((true_na - min(true_na)) / (max(true_na) - min(true_na)) * 6 + 1)
  
  #observed data frame:
  observed_data <- cbind(stressors, data.frame(stress = stress_reported), ids, na = obs_na)
  return(observed_data)
}