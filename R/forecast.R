#' Forecast epidemic.
#'
#' @param fitobj List. Object as returned by the function \code{fit()}.
#' @param varnames String vector. Names of the variables to forecast. 
#' (e.g. "hospadm" or "h").
#' Do not specify the age group, all age groups will be returned.
#' @param simulate Logical. If \code{FALSE}, will simply use the posterior
#' simulations of the fit object. This implicitly constrains the time horizon
#' and the number of forecast trajectories.
#' @param prm.fcst List. Forecast parameters.
#'
#' @returns SOMETHING
#' @export
#'
#' @examples
#' 
#' 
forecast <- function(
    fitobj, 
    simulate,
    prm.fcst) {
  
  if(0) {
    
    prm.fcst = list(
      varnames = c('hospadm', 'testpos'),
      asof = as.Date.character('2026-05-15')
    )
  }
  
  varnames = prm.fcst[['varnames']]
  
  # Converts to simulation variable names
  nag = get_nag(fitobj[['obj']]) 
  v = sapply(varnames, get_sim_varnames, nag = nag ) |> as.vector()
  # v
  
  d = get_last_date_data(fitobj)
  date.last.data = max(unlist(d)) |> as.Date.numeric(origin = "1970-01-01") 
  
  if(!simulate){
    tmp = fitobj$simpost
    # Filter dates after last data
    traj.fcst = tmp[tmp$date > date.last.data, ]
  }
  res = list(
    asof = prm.fcst[['asof']],
    traj = traj.fcst,
    fitobj = fitobj
  )
}



### Application example ####

if(0){
  
  devtools::load_all()
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  
  model.prms = example_model_prms()
  date.obs = model.prms$date.start + 12*c(1:10)
  data = example_simulated_data(model.prms = model.prms, 
                                date.obs = date.obs)
  
  # Parameters for the fitting algorithm
  prms.fit = list(
    prms.to.fit   = c('R', 'odds.testpos', 'h.prop'),
    data.used.fit = c('testpos', 'hospadm'),
    p.accept      = 5e-3,
    priors.dist = list(
      n.priors     = 5e3,
      R            = c('unif', 0.1, 1.3),
      odds.testpos = c('unif', 0.9, 10),
      h.prop       = c('unif', 0.0, 0.08)
    ),
    n.cores = 3
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
  g.fit.traj
  
  # Forecasts
  
  prm.fcst = list(
    varnames = c('hospadm', 'testpos'),
    asof = as.Date.character('2026-05-15')
  )
  
  fcstobj = forecast(fitobj, 
                     prm.fcst = prm.fcst,
                     simulate = FALSE)
  
  # Thu Mar  5 10:32:10 2026 ------------------------------
  # STOPPED HERE
  g = fcstobj[['traj']] |> 
    pivot_longer(cols = -c(time, date,idx)) |> 
    filter(stringr::str_detect(name, 
                               stringr::str_c(prm.fcst$varnames, 
                                              collapse = "|"))) |>
    ggplot(aes(x=date, y = value, group = idx))+ 
    facet_wrap(~name, scales = 'free_y') + 
    geom_line()
  g  
}

