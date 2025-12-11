
helper_priors_vec <- function(varname, nag, priors.dist) {
  dist = paste0('r', priors.dist[[varname]][[1]])
  param = priors.dist[[varname]][2:length(priors.dist[['R']])] |> 
    as.numeric() |> 
    as.list()
  
  # Using array for computing speed
  vals = do.call(what = dist, args = c(n = priors.dist$n.priors * nag, param))
  res = array(data = vals, dim = c(nag, priors.dist$n.priors)) 
  return(res)
}



helper_priors_mat <- function(varname, nag, priors.dist) {
  dist = paste0('r', priors.dist[[varname]][[1]])
  param = priors.dist[[varname]][2:length(priors.dist[['R']])] |> 
    as.numeric() |> 
    as.list()
  
  # Using array for computing speed
  vals = do.call(what = dist, args = c(n = priors.dist$n.priors * nag^2, param))
  res = array(data = vals, dim = c(nag, nag, priors.dist$n.priors))  # 3D array: 2×2×n
  return(res)
}


generate_priors <- function(priors.dist, nag) {
  
  if(0){
    nag = 2
    priors.dist = list(
      n.priors = 100,
      R = c('unif', 0.1, 3),
      odds.testpos = c('unif', 0.9, 100),
      h.prop = c('unif', 0.0, 0.8)
    )
  }
  
  nmp = names(priors.dist)
  
  res = list()
  
  if('R' %in% nmp){
    res[['R']] = helper_priors_mat(
      varname = 'R', nag = nag, priors.dist = priors.dist)
  }
  # priors.R[,,2]
  
  if('odds.testpos' %in% nmp){
    res[['odds.testpos']] = helper_priors_vec(
      varname = 'odds.testpos', 
      nag = nag, priors.dist = priors.dist)
  }
  # priors.odds.testpos[,33]
  if('h.prop' %in% nmp){
    res[['h.prop']] = helper_priors_vec(
      varname = 'h.prop', 
      nag = nag, priors.dist = priors.dist)
  }
  return(res)
  
}



#' Fit a model object to data.
#'
#' @param obj List Model object as returned by \code{amrem::create()}.
#' @param prms.fit List Parameters related to the fitting algorithm. 
#' @param data List Data to fit the model to. 
#'
#' @returns TODO
#' @export
#'
#' @examples foo = fit()
#' 
#' 
fit <- function(obj, prms.fit, data) {
  
  # Data that can be observed:
  # testpos -> tau_a --> odds.testpos
  # hospadm -> h_a --> h.prop
  # ww -> w_a
  
  # --- DEBUG
  if(1){
    
    devtools::load_all()
    library(ggplot2)
    library(dplyr)
    library(tidyr)
    
    # set up simulation to generate "observations"
    prms0 = example_model_prms()
    nag = length(prms0$N)
    obj0 = amrem::create(prms0)
    
    simobs = simulate(obj0)
    
    simobs |> ggplot(aes(x=time))+
      geom_line(aes(y = tau_1)) + 
      geom_line(aes(y = tau_2), color = 'steelblue1') + 
      labs(title = 'simulated data')
    
    t.obs = 12*c(1:4)
    
    data.testpos1 = simobs |> 
      filter(time %in% t.obs )    |> 
      select(time, value = tau_1)
    data.testpos2 = simobs |> 
      filter(time %in% t.obs )    |> 
      select(time, value = tau_2)
    
    data = list(
      testpos_1 = data.testpos1,
      testpos_2 = data.testpos2
    )
    
    prms.fit = list(
      prms.to.fit = c('R', 'odds.testpos', 'h.prop'),
      data.used.fit = c('testpos', 'hospadm'),
      priors.dist = list(
        n.priors     = 1e3,
        R            = c('unif', 0.1, 3),
        odds.testpos = c('unif', 0.9, 100),
        h.prop       = c('unif', 0.0, 0.8)
      )
    )
  

  }  # --- end debug
  
  
  priors = generate_priors(
    priors.dist = prms.fit$priors.dist,
    nag = nag)
  
  simulate_unit <- function(i, obj, priors) {
    obj$prms$R            = priors[['R']][,,i]
    obj$prms$odds.testpos = priors[['odds.testpos']][,i]
    obj$prms$h.prop       = priors[['h.prop']][,i]
    
    sim = amrem::simulate(obj)
    
    df1 = data$testpos_1 |> left_join(select(sim, time,tau_1), by ='time')
    df2 = data$testpos_2 |> left_join(select(sim, time,tau_2), by ='time')
    
    err = sum(df1$value - df1$tau_1)^2 + sum(df2$value - df2$tau_2)^2
    res = data.frame(idx = i, err = err)
    return(res)
  }
  
  
  obj = obj0
  
  system.time({
    z = lapply(1:prms.fit$priors.dist$n.priors, 
               simulate_unit, 
               obj = obj, 
               priors = priors)
  })
  df = bind_rows(z) |> 
    arrange(err)
  
  n.post = 10
  idx.post = df$idx[1:n.post]
  
    # i = 33  
    # x = priors$odds.testpos
    
  extract_post <- function(i, x) {
    d = dim(x)
    if(length(d)==2) res = x[,i]
    if(length(d)==3) res = x[,,i]
    return(res)
  }
  
  post = list()
  for(a in names(priors)){
    post[[a]] = lapply(idx.post, extract_post, x = priors[[a]])
  }
  
  prm.name = 'odds.testpos'
  prm.name = 'h.prop'
  q = matrix(unlist(post[[prm.name]]), ncol = nag, byrow = TRUE)
  k = 2
  plot(density(q[,k]))
  abline(v=obj0$prms[[prm.name]][k])
  
  # Thu Dec 11 15:23:12 2025 ------------------------------
  # STOPPED HERE . GTG
  # continue checking the posterior is close to the
  #  prm value used to simulate the observations...
  
  ggplot(data$testpos_1, aes(x=time, y = value)) + 
    geom_line()+ 
    geom_point()+
    geom_point(data = data$testpos_2, color = 'blue')+
    geom_line(data = sim, aes(x=time, y = tau_1))+
    geom_line(data = sim, aes(x=time, y = tau_2), color = 'blue')+
    labs(title = 'foo')
  
}