
#' Helper function for prior vectors
#'
#' @param varname String. Variable name.
#' @param nag Integer. Number of age groups.
#' @param priors.dist List defining prior distributions.
#' @param n.priors Integer. Number of prior samples.
#'
#' @keywords internal
#' 
helper_priors_vec <- function(varname, nag, priors.dist, n.priors) {
  p = priors.dist[[varname]][[1]]
  dist = paste0('r', p[[1]])
  param = p[2:length(p)] |> 
    as.numeric() |> 
    as.list()
  
  # Using array for computing speed
  vals = do.call(what = dist, args = c(n = n.priors * nag, param))
  res = array(data = vals, dim = c(nag, n.priors)) 
  return(res)
}


#' Helper function for prior vectors when prior defined as vector. 
#'
#' @param varname String. Variable name.
#' @param nag Integer. Number of age groups.
#' @param priors.dist List defining prior distributions.
#' @param n.priors Integer. Number of prior samples.
#'
#' @keywords internal
#' 
helper_priors_vec2 <- function(varname, nag, priors.dist, n.priors) {
  
  p = priors.dist[[varname]]
  
  if(length(p) != nag){
    stop( 'The priors specification for `',varname,
          '` must have as many distributions as number of age groups.',
          ' (',nag, ' instead of ',length(p),')')
  }
  vals = list()
  for(i in 1:nag){
    dist = paste0('r', p[[i]][[1]])
    param = p[[i]][2:length(p[[i]])] |> 
      as.numeric() |> 
      as.list()
    vals[[i]] = do.call(what = dist, 
                        args = c(n = n.priors * nag, param))
  }
  
  # Using array for computing speed
  res = do.call(rbind, vals)
  return(res)
}



#' Helper function for prior matrices.
#'
#' @param varname String. Variable name.
#' @param nag Integer. Number of age groups.
#' @param priors.dist List defining prior distributions.
#' @param n.priors Integer. Number of prior samples.
#'
#' @keywords internal
#' 
helper_priors_mat <- function(varname, nag, priors.dist, n.priors) {
  p = priors.dist[[varname]][[1]]
  dist = paste0('r', p[1])
  param = p[2:length(p)] |> 
    as.numeric() |> 
    as.list()
  
  # Using array for computing speed
  vals = do.call(what = dist, args = c(n = n.priors * nag^2, param))
  res = array(data = vals, dim = c(nag, nag, n.priors))  # 3D array: 2×2×n
  return(res)
}


#' Helper function to generate priors
#' for a parameter defined a matrix format
#'
#' @param varname String. Name of the variable.
#' @param nag Integer. Number of age groups.
#' @param priors.dist List defining the prior 
#' distribution for each matrix element.
#' @param n.priors Integer. Number of prior samples.
#'
#' @returns Array of prior values.
#' @keywords internal
#'
helper_priors_mat2 <- function(varname, nag, priors.dist, n.priors) {
  # varname = 'R'  
  
  vals = list()
  
  # Build the priors of the matrix, 
  # element by element
  for(i in 1:nag){
    vals[[i]] = list()
    for(j in 1:nag){
      z = paste0('r',i,'c',j)
      prior_ij = priors.dist[[varname]][[z]]
      prior_ij
      
      dist = paste0('r', prior_ij[[1]])
      param = prior_ij[2:length(prior_ij)] |> 
        as.numeric() |> 
        as.list()
      
      vals[[i]][[j]] = do.call(what = dist, 
                               args = c(n = n.priors, param))
      
    }
  }
  # vals
  
  # Using array for computing speed
  # 3D array: nag × nag × npriors
  res = helper_list_to_array(vals)
  return(res)
}


#' Helper to convert a list into an array
#'
#' @param x List
#'
#' @returns Array
#' @keywords internal
#'
helper_list_to_array <- function(x) {
  M <- length(x)
  N <- length(x[[1]][[1]])
  
  # Convert each row list (over j) to an N x M matrix, then stack over i
  row_mats <- lapply(x, function(row) simplify2array(row))  # each is N x M
  A_tmp <- simplify2array(row_mats)                         # N x M x M (dims: elements, j, i)
  A <- aperm(A_tmp, c(3, 2, 1))                             # M x M x N
  return(A)
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


is_param_matrix <- function(x) {
  res = FALSE
  if(x == 'R') return(TRUE)
  return(res)
}



is_param_vect <- function(x) {
  res = FALSE
  if(x == 'h.prop')       return(TRUE)
  if(x == 'odds.testpos') return(TRUE)
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
generate_priors <- function(priors.dist, n.priors, nag) {
  
  if(0){ # DEBUG
    nag = 2
    n.priors = 100
    priors.dist = list(
      R = list(
        r1c1 = c('unif', 0.1, 0.3),
        r2c1 = c('unif', 0.5, 0.9),
        r1c2 = c('unif', 1.1, 1.5),
        r2c2 = c('unif', 1.6, 2.1)
      ),
      odds.testpos = list(c('unif', 0.9, 100)),
      h.prop = list(
        c('unif', 0.2, 0.3),
        c('unif', 0.0, 0.1)
      )
    ) 
  }
  
  nmp = names(priors.dist)
  
  res = list()
  
  for(x in nmp){
    
    message('generating priors for ',x)
    
    npx = length(priors.dist[[x]])
    
    if(is_param_matrix(x)){
      
      if(npx == 1){
        res[[x]] = helper_priors_mat(
          varname     = x, 
          nag         = nag, 
          priors.dist = priors.dist,
          n.priors    = n.priors)
      }
      if(npx == nag^2){
        res[[x]] = helper_priors_mat2(
          varname     = x, 
          nag         = nag, 
          priors.dist = priors.dist,
          n.priors    = n.priors)
      }
    }
    
    if(is_param_vect(x)){
        
        if(npx == 1){
          res[[x]] = helper_priors_vec(
            varname     = x, 
            nag         = nag, 
            priors.dist = priors.dist,
            n.priors    = n.priors)
        }
        if(npx == nag){
          res[[x]] = helper_priors_vec2(
            varname     = x, 
            nag         = nag, 
            priors.dist = priors.dist,
            n.priors    = n.priors)
      }
    }
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
  
  # Simulate with ith prior value
  sim = amrem::simulate(obj)
  sim$idx = i 
  return(sim)
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



#' Normalize element values of a vector to [0,1] using its min and max values.
#'
#' @param x Numeric vector.
#' @returns Normalized vector.
#' @export
#'
normalize_minmax <- function(x) {
  minx = min(x)
  maxx = max(x)
  #TODO: deal with case min = max
  return( (x-minx)/(maxx-minx) )
}

#' Error function. 
#'
#' @param target 
#' @param value 
#'
#' @keywords internal
#' 
error_fct <- function(target, value) {
  err = (target - value)^2
  # Errors are normalized in order to be compared 
  # across different fitted variables that may be 
  # different orders of magnitude apart (eg positivity and hosp count)
  err.n = normalize_minmax(err)
  return(err.n)
}

#' Calculate the fitting error
#'
#' @param data.type 
#' @param data 
#' @param dfsim 
#' @param nag 
#'
#' @returns Dataframe of errors, corresponding prior indices and variable name fitted.
#' @keywords internal
#'
calc_fit_errors <- function(data.type, data, dfsim, nag) {
  if(0) {
    data.type = 'hospadm'
    data.type = 'testpos'
  }
  
  nam.data = get_data_names(data.type, nag)
  nam.sim  = get_sim_varnames(data.type, nag)
  
  
  
  tmp = list()
  for(i in 1:nag){
    data.i = data[[nam.data[i] ]]
    
    # Make the left_join faster by trimming the data
    data.i$date = as.Date(data.i$date)
    dfsim.i = dfsim[dfsim$date %in% data.i$date,]
    
    thejoin = dplyr::left_join(data.i, dfsim.i, by = 'date') 
    
    tmp[[i]] = data.frame(
      time      = thejoin$date,
      idx       = thejoin$idx,
      error     = error_fct(target = thejoin$value,
                            value  = thejoin[[nam.sim[i] ]]),
      data.name = nam.data[i]
    )
  }
  res = do.call(rbind, tmp)
  return(res)
}

#' Fit a model object to data.
#'
#' @param obj List Model object as returned by \code{amrem::create()}.
#' @param prms.fit List Parameters related to the fitting algorithm. 
#' @param data List Data to fit the model to. 
#'
#' @returns A list containing multiple fit outputs...
#' @export
#'
#' @examples foo = fit()
#' 
fit <- function(obj, prms.fit, data) {
  
  # Extract number of age groups
  nag = length(obj$prms$N)
  
  # Check consistency of fit parameters
  check_prms_fit(prms.fit, nag)
  check_data_fit_date(data, obj)
  
  # Generate prior samples
  n.priors = prms.fit$n.priors
  priors = generate_priors(
    priors.dist = prms.fit$priors.dist,
    n.priors    = n.priors,
    nag         = nag)
  
  
  fit.data.type = fit_data_type(data)
  
  # Simulate epidemic for each prior
  n.cores = prms.fit$n.cores
  
  snowfall::sfInit(parallel = n.cores > 1, cpus = n.cores)
  snowfall::sfExportAll()
  
  z = snowfall::sfLapply(
    x      = 1:n.priors, 
    fun    = simulate_fit_unit, 
    obj    = obj, 
    priors = priors,
    data   = data,
    fit.data.type = fit.data.type)
  
  snowfall::sfStop()
  
  dfsim = dplyr::bind_rows(z) 
  
  # Calculate errors
  errs = lapply(
    X     = prms.fit$data.used.fit,
    FUN   = calc_fit_errors, 
    data  = data, 
    dfsim = dfsim, 
    nag   = nag) |> 
    dplyr::bind_rows()
  
  errs.sorted = errs |> 
    dplyr::group_by(idx) |>
    dplyr::summarise(err.total = sum(error), .groups = 'drop') |> 
    dplyr::arrange(err.total)
  
  # Select the best priors as posteriors
  n.post = round(prms.fit$p.accept * n.priors)
  idx.post = errs.sorted$idx[1:n.post]
  
  if(0){ # DEBUG
    plot(errs.sorted$err.total, log='xy', las=1, typ='s', main = 'ABC errors') 
    grid()
    abline(v=n.post, col = 'blue', lwd=3)
  }
  
  # Extract posterior distribution and trajectories
  post = list()
  for(a in names(priors)){
    post[[a]] = lapply(idx.post, extract_post, x = priors[[a]])
  }
  
  simpost = dfsim |> 
    dplyr::filter(idx %in% idx.post)
  
  if(0){ # DEBUG
    simpost |> 
      ggplot(aes(x=date)) +
      geom_point(data = data$hospadm_2, aes(x=date, y=value))+
      geom_line(aes(y=hospadm_2, group = idx)) 
  }
  res = list(
    obj      = obj,
    post     = post,
    priors   = priors,
    prms.fit = prms.fit, 
    data     = data,
    simtraj  = dfsim,   # all simulated trajectories (:priors)
    simpost  = simpost, # posterior trajectories only
    errors   = errs,
    errorsTotal = errs.sorted
  )
  return(res)
}


if(0){ # --- Application example ----
  
  devtools::load_all()
  
  model.prms = example_model_prms()
  date.obs = model.prms$date.start + 12*c(1:10)
  data = example_simulated_data(model.prms = model.prms, 
                                date.obs = date.obs)
  
  
  model.prms$R
  model.prms$h.prop
  model.prms$odds.testpos
  
  # Parameters for the fitting algorithm
  prms.fit = list(
    data.used.fit = c('testpos', 'hospadm'),
    n.priors      = 1e3,
    p.accept      = 1e-2,
    priors.dist = list(
      R = list(
        r1c1 = c('unif', 0.90, 2.0),
        r2c1 = c('unif', 0.05, 0.7),
        r1c2 = c('unif', 0.05, 0.7),
        r2c2 = c('unif', 0.80, 1.6)
      ), 
      odds.testpos = list(
        c('unif', 0.7, 2),
        c('unif', 15, 30)),
      h.prop       = list(
        c('unif', 0.0, 0.05)
      )),
    n.cores = 1
  )
  
  
  # Starting point model to feed fit
  obj = amrem::create(model.prms)
  
  system.time({
    fitobj = fit(obj, 
                 prms.fit = prms.fit, 
                 data = data)  
  })  
  
  ci = 0.90
  g.fit.traj = plot_fit_traj(fitobj = fitobj, ci = ci)
  # g.fit.traj
  
  gfp = plot_fit_post(fitobj = fitobj, ci = ci,
                      true.values = model.prms)
  
  g.fit.post = patchwork::wrap_plots(gfp)
  
  tmp2d = plot_fit_post_2d(fitobj)
  g.fit.2d = patchwork::wrap_plots(tmp2d)
  
  gerr = plot_fit_errors(fitobj)
  g.fit.err = patchwork::wrap_plots(gerr)
  
  fname = paste0('tmp-',reem::timestamp_short(), '.pdf')
  pdf(fname, width=24, height = 15)
  plot(g.fit.traj)
  plot(g.fit.post)
  plot(g.fit.2d)
  plot(g.fit.err)
  dev.off()
  
}


if(0){ # --- Application 5 age groups ----
  
  devtools::load_all()
  
  model.prms = example_model_prms_ag()
  date.obs = model.prms$date.start + 12*c(1:10)
  data = example_simulated_data2(model.prms = model.prms, 
                                date.obs = date.obs)
  
  
  model.prms$R
  model.prms$h.prop
  model.prms$odds.testpos
  
  # Parameters for the fitting algorithm
  prms.fit = list(
    data.used.fit = c('testpos', 'hospadm'),
    n.priors      = 5e3,
    p.accept      = 1e-2,
    priors.dist = list(
      R = list(c('unif', 0.10, 2.0)),
      odds.testpos = list(c('unif', 0.1, 15)),
      h.prop       = list(c('unif', 0.0, 0.05))),
    n.cores = 1
  )
  
  
  # Starting point model to feed fit
  obj = amrem::create(model.prms)
  
  system.time({
    fitobj = fit(obj, 
                 prms.fit = prms.fit, 
                 data = data)  
  })  
  
  ci = 0.90
  g.fit.traj = plot_fit_traj(fitobj = fitobj, ci = ci)
  # g.fit.traj
  
  gfp = plot_fit_post(fitobj = fitobj, ci = ci,
                      true.values = model.prms)
  
  g.fit.post = patchwork::wrap_plots(gfp)
  
  # tmp2d = plot_fit_post_2d(fitobj)
  # g.fit.2d = patchwork::wrap_plots(tmp2d)
  
  gerr = plot_fit_errors(fitobj)
  g.fit.err = patchwork::wrap_plots(gerr)
  
  fname = paste0('tmp-',reem::timestamp_short(), '.pdf')
  pdf(fname, width=24, height = 15)
  plot(g.fit.traj)
  plot(g.fit.post)
  # plot(g.fit.2d)
  plot(g.fit.err)
  dev.off()
  
}

if(0){
  # Mon Apr 13 13:05:35 2026 ------------------------------
  data = readRDS('C:/Users/DCHAMPRE/Downloads/on-covid.rds')
  devtools::load_all()
  
  ontario_population <- c(
    `0–4`   = 683515,
    `5–14`  = 1568280,
    `15–64` = 9334445,
    `65+`   = 2637715
  )
  nag = 4
  model.prms = example_model_prms_ag(
    r0 = 1.57, 
    N = ontario_population,
    S0.prop = rep(0.6, nag), 
    h.prop = c(0.1, 0.1, 0.1, 1.1)/100, 
    odds.testpos = c(3, 1, 2, 6))
  
  model.prms$date.start <- lubridate::ymd('2024-04-01')
  model.prms$horizon <- 300
  
  model.prms$i0
  model.prms$R
  model.prms$odds.testpos
  model.prms$h.prop
  
  # Starting point model to feed fit
  obj = amrem::create(model.prms)
  sim = amrem::simulate(obj)
  
  amrem::plot_timeseries(sim)
  
  
  # Parameters for the fitting algorithm
  prms.fit = list(
    data.used.fit = c('testpos', 'hospadm'),
    p.accept      = 3e-3,
    n.priors     = 4e3,
    priors.dist = list(
      # R = list(
      #   r1c1 = c('unif', 1.00, 1.50),
      #   r1c2 = c('unif', 0.01, 0.70),
      #   r2c1 = c('unif', 0.05, 0.50),
      #   r2c2 = c('unif', 0.40, 1.20)
      # ),
      # odds.testpos = list(
      #   c('unif', 0.60, 1.50),
      #   c('unif', 10, 32)
      # ),
      R = list(c('unif', 0.5, 2.00)), 
      odds.testpos = list(c('unif', 0.5, 7)),
      h.prop = list( c('beta', 1 , 50))
    ),
    n.cores = 1
  )
  
  system.time({
    fitobj = fit(obj, 
                 prms.fit = prms.fit, 
                 data = data)  
  })
  
    ci = 0.90
    g.fit.traj = plot_fit_traj(fitobj = fitobj, ci = ci)
    g.fit.traj
    
    gfp = plot_fit_post(fitobj = fitobj, ci = ci)
    
    g.fit.post = patchwork::wrap_plots(gfp, nrow = 2)
    
    # tmp2d = plot_fit_post_2d(fitobj)
    # g.fit.2d = patchwork::wrap_plots(tmp2d)
    
    gerr = plot_fit_errors(fitobj)
    g.fit.err = patchwork::wrap_plots(gerr)
    gerr$total
  
}
