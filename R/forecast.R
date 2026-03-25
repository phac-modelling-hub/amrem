#' Forecast epidemic.
#'
#' @param fitobj List. Object as returned by the function \code{\link{fit}()}.
#' @param simulate Logical. If \code{FALSE}, will simply use the posterior
#' simulations of the fit object. This implicitly constrains the time horizon
#' and the number of forecast trajectories.
#' @param prm.fcst List. Forecast parameters.
#' @param silent Logical. Display informational messages if \code{TRUE}. 
#' Default is \code{FALSE}. 
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
    prm.fcst,
    silent = TRUE) {
  
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
  
  # Do not run _new_ simulations to 
  # determine forecast trajectories,
  # simply use pre-calculated posteriors
  if(!simulate){
    if(!silent) message('Use pre-calculated posteriors to generate forecasts.')
    tmp = fitobj$simpost
    if(!silent) message('Number of forecast (posterior) samples: ',nrow(tmp))
    # Filter dates after last data
    traj.fcst = tmp[tmp$date > date.last.data, ]
  }
  
  # Draw parameters from posterior distributions
  # to generate new simulated trajectories.
  if(simulate){
    # TODO
    stop('`simulate = TRUE` is not implemented yet in `forecast()`.')
  }
  
  # -- Final output
  res = list(
    asof   = prm.fcst[['asof']],
    traj   = traj.fcst,
    fitobj = fitobj
  )
}



#' Summary statistics if a forecast object.
#'
#' @param fcstobj List. Object as returned by the function \code{forecast(...)}.
#' @param ci Numerical vector of credible intervals.
#'
#' @returns List of dataframes. Element \code{mean} returns the mean values for 
#' each variable at every date. Element \code{quantile} returns the 
#' corresponding quantiles based on input \code{ci}.
#' 
#' @export
#'
#' @examples
#' 
summarize_forecast <- function(fcstobj, ci) {
  
  # ci = seq(0.1, 0.9, by = 0.2)
 
  qvec = sort(0.5 + c(-ci,ci) / 2)
   
  df = fcstobj$traj
  
  dflong = df |> 
    dplyr::select(-time) |>
    tidyr::pivot_longer(cols = -c(date,idx))
  
  # -- Quantiles
  
  dq = dflong |> 
    dplyr::group_by(date, name) |> 
    dplyr::reframe(
      q = qvec,
      value = as.numeric(quantile(value, probs = qvec, na.rm = TRUE))
    )
  dq$q <- as.factor(dq$q)
  
  # -- Means
  
  dm = dflong |> 
    dplyr::group_by(date, name) |> 
    dplyr::summarize(value = mean(value), .groups = 'drop')
  
  # Final result
  res = list(
    mean     = dm,
    quantile = dq
  )
  return(res)
  
  # Debug
  g = dq |> ggplot(aes(x = date))+
    facet_wrap(~name, scales = 'free_y')+
    scale_color_brewer(palette = 'RdYlBu')+
    theme(panel.grid.minor = element_blank()) + 
    geom_line(aes(y = value, color = q), linewidth = 1)+
    geom_line(data = dm, aes(y=mean), linewidth = 1.5, alpha = 0.4)+
    labs(y='value')
  g
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
    data.used.fit = c('testpos', 'hospadm'),
    p.accept      = 5e-3,
    priors.dist = list(
      n.priors     = 5e3,
      R            = c('unif', 0.1, 1.3),
      odds.testpos = c('unif', 0.9, 10),
      h.prop       = c('unif', 0.0, 0.08)
    ),
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
  g.fit.traj
  
  # Forecasts
  
  prm.fcst = list(
    varnames = c('hospadm', 'testpos'),
    asof = as.Date.character('2026-05-15')
  )
  
  fcstobj = forecast(fitobj, 
                     prm.fcst = prm.fcst,
                     simulate = FALSE)
  
  fcst.summ = summarize_forecast(fcstobj, ci = c(0.5, 0.9))
  
  # Tue Mar 10 08:54:53 2026 ------------------------------
  # STOPPED HERE
  
  dat = fcstobj$fitobj$data |> 
    flatten_data() |> 
    rename(name = source)
  
  g = dat |> 
    ggplot(aes(x = date, y = value)) + 
    facet_wrap(~name, scales = 'free_y') + 
    theme_bw()+
    geom_point() + 
    geom_line(data = fcst.summ$mean) +
    geom_line(data = fcst.summ$quantile, 
              aes(color = q))+
    scale_color_brewer(palette = 'RdYlBu')+
    theme(panel.grid.minor = element_blank())
  g
  
  
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

