
#' Helper function for prior vectors
#'
#' @param varname String. Variable name.
#' @param nag Integer. Number of age groups.
#' @param priors.dist List defining prior distributions.
#'
#' @keywords internal
#' 
helper_priors_vec <- function(varname, nag, priors.dist) {
  dist = paste0('r', priors.dist[[varname]][[1]])
  param = priors.dist[[varname]][2:length(priors.dist[[varname]])] |> 
    as.numeric() |> 
    as.list()
  
  # Using array for computing speed
  vals = do.call(what = dist, args = c(n = priors.dist$n.priors * nag, param))
  res = array(data = vals, dim = c(nag, priors.dist$n.priors)) 
  return(res)
}

#' Helper function for prior matrices.
#'
#' @param varname String. Variable name.
#' @param nag Integer. Number of age groups.
#' @param priors.dist List defining prior distributions.
#'
#' @keywords internal
#' 
helper_priors_mat <- function(varname, nag, priors.dist) {
  dist = paste0('r', priors.dist[[varname]][[1]])
  param = priors.dist[[varname]][2:length(priors.dist[[varname]])] |> 
    as.numeric() |> 
    as.list()
  
  # Using array for computing speed
  vals = do.call(what = dist, args = c(n = priors.dist$n.priors * nag^2, param))
  res = array(data = vals, dim = c(nag, nag, priors.dist$n.priors))  # 3D array: 2×2×n
  return(res)
}


#' Generate priors
#'
#' @param priors.dist List defining the priors.
#' @param nag Integer. Number of age groups.
#'
#' @returns List of priors arrays.
#' @keywords internal
#' 
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
  
  if('odds.testpos' %in% nmp){
    res[['odds.testpos']] = helper_priors_vec(
      varname = 'odds.testpos', 
      nag = nag, priors.dist = priors.dist)
  }
  
  if('h.prop' %in% nmp){
    res[['h.prop']] = helper_priors_vec(
      varname = 'h.prop', 
      nag = nag, priors.dist = priors.dist)
  }
  
  return(res)
}


#' Helper function to extract posterior distribution
#'
#' @keywords internal
#' 
extract_post <- function(i, x) {
  d = dim(x)
  res = NULL
  if(length(d)==2) res = x[,i]
  if(length(d)==3) res = x[,,i]
  return(res)
}


#' Simulation used in fit algorithm 
#' to calculate error from data
#'
#' @param i Integer. Index
#' @param obj List. Model object.
#' @param priors Dataframe of priors.
#'
#' @returns Dataframe of prior indices and errors.
#' @keywords internal
#'
simulate_fit_unit <- function(i, obj, priors, data, fit.data.type) {
  
  # Retrieve priors values
  priors.names = names(priors)
  for(nam in priors.names){
    if(!is.matrix(obj$prms[[nam]])) obj$prms[[nam]] = priors[[nam]][,i]
    if( is.matrix(obj$prms[[nam]])) obj$prms[[nam]] = priors[[nam]][,,i]
  }
  
  # obj$prms$R            = priors[['R']][,,i]
  # obj$prms$odds.testpos = priors[['odds.testpos']][,i]
  # obj$prms$h.prop       = priors[['h.prop']][,i]
  
  # Simulate with ith prior value
  sim = amrem::simulate(obj)
  sim$idx = i 
  return(sim)
  # # Match simulation and observed data
  # if(fit.data.type$testpos){
  #   df1 = data$testpos_1 |> left_join(select(sim, time,tau_1), by ='time')
  #   df2 = data$testpos_2 |> left_join(select(sim, time,tau_2), by ='time')
  # }
  # if(fit.data.type$hosp){
  #   
  # }
  # 
  # err = sum(df1$value - df1$tau_1)^2 + sum(df2$value - df2$tau_2)^2
  # res = data.frame(idx = i, err = err)
  # return(res)
}

#' Helper function for type of data fit
#'
#' @param v String. Name of variable.
#' @param nam String vector. Names of all variables.
#' @param x List.
#'
#' @keywords internal
#' 
helper_fit_data_type <- function(v,nam,x) {
  x[[v]] = FALSE
  if(any(grepl(v,nam))) x[[v]] = TRUE
  return(x)
}

#' Type of data fitted.
#'
#' @param data Dataframe of observed data.
#'
#' @returns Named logical vector. Element is TRUE if data type is fitted.
#' @keywords internal
#'
fit_data_type <- function(data) {
  x = list()
  nam = names(data)
  u = c('testpos', 'hosp')
  x = sapply(u, helper_fit_data_type, nam, x, USE.NAMES = FALSE)
  x
  return(x)
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
fit <- function(obj, prms.fit, data) {
  
  priors = generate_priors(
    priors.dist = prms.fit$priors.dist,
    nag = nag)
  
  npriors = prms.fit$priors.dist$n.priors
  
  fit.data.type = fit_data_type(data)
  
  # Simulate for each prior
  system.time({
    z = lapply(1:npriors, 
               simulate_fit_unit, 
               obj    = obj, 
               priors = priors,
               data   = data,
               fit.data.type = fit.data.type)
  })
  df = dplyr::bind_rows(z) 
  
  
  # Calculate errors
  
  # Thu Dec 18 09:02:50 2025 ------------------------------
  # STOPPED HERE: find a smart way to calculate errors with data
  
  if(fit.data.type$testpos){
    foo = df |>
      dplyr::select(idx, time, tidyr::starts_with('tau')) |>
      tidyr::pivot_longer(-c(idx,time)) |> 
      dplyr::left_join()
  }
  
  # OLD CODE BELOW (assumed error calculated in simulate_fit_unit)
  # Order priors by goodness of fit
  df = bind_rows(z) |> 
    arrange(err)
  
  # Select the best priors as posteriors
  n.post = round(prms.fit$p.accept * prms.fit$priors.dist$n.priors)
  idx.post = df$idx[1:n.post]
  
  if(0){
    plot(df$err, log='xy', las=1, typ='s', main = 'ABC errors') 
    grid()
    abline(v=n.post, col = 'blue', lwd=3)
  }
  
  post = list()
  for(a in names(priors)){
    post[[a]] = lapply(idx.post, extract_post, x = priors[[a]])
  }
  
  if(0){
    prm.name = 'odds.testpos'
    # prm.name = 'h.prop'
    q = matrix(unlist(post[[prm.name]]), 
               ncol = nag, 
               byrow = TRUE)
    k = 1
    # plot(density(q[,k]), main = prm.name)
    hist(q[,k], breaks = 30, main = paste(prm.name,k,sep='_'))
    abline(v = obj0$prms[[prm.name]][k], col = 'red', lwd=2)
    
    # Thu Dec 11 15:23:12 2025 ------------------------------
    # GOAL: check the posterior is close to the
    #  prm value used to simulate the observations...
  }
}


if(0){ # --- Application example ----
  
  library(amrem)
  devtools::load_all()
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  
  # set up simulation to generate "observations"
  prms0 = example_model_prms()
  nag   = length(prms0$N)
  
  obj0  = amrem::create(prms0)
  
  # Model that generates data
  
  # Data that can be observed:
  # testpos -> tau_a --> odds.testpos
  # hospadm -> h_a --> h.prop
  # ww -> w_a
  
  simobs = simulate(obj0)
  g.simobs = plot_timeseries(simobs)
  plot(g.simobs + labs(title = 'simulated data'))
  
  
  # Observations
  
  t.obs = 12*c(1:4)    # observation times
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
  
  # Parameters for the fitting algorithm
  prms.fit = list(
    prms.to.fit   = c('R', 'odds.testpos', 'h.prop'),
    data.used.fit = c('testpos', 'hospadm'),
    p.accept      = 0.03,
    priors.dist = list(
      n.priors     = 1e3,
      # R            = c('unif', 0.1, 3),
      odds.testpos = c('unif', 0.9, 100)#,
      # h.prop       = c('unif', 0.0, 0.8)
    )
  )
  
  # Starting point model to feed fit
  obj = obj0

  thefit = fit(obj, 
               prms.fit = prms.fit, 
               data = data)  
  
  
}


