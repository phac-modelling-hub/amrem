#' Simulate an epidemic.
#'
#' @param check.prms Logical. Check input parameters? Default = TRUE.
#' @param obj List representing the `amrem` object as returned by \code{amrem::create()}.
#' @param ww A boolean to switch on/off fecal shedding into wastewater.
#' @param hosp A boolean to switch on/off hospital admissions.
#' @param testpos A boolean to switch on/off clinical test positivity.
#'
#' @returns Dataframe of simulated variables.
#' @export
#'
#' @examples
#' prms = example_model_prms()
#' obj = amrem::create(prms = prms, name = 'foo')
#' s = simulate(obj)
#' head(s)
#' 
simulate <- function(obj, 
                     check.prms = TRUE,
                     ww         = TRUE, 
                     hosp       = TRUE,
                     testpos    = TRUE) {
  if(check.prms) check_prms_simulate(obj[['prms']])
  s = simulate_c(obj[['prms']], ww, hosp, testpos)
  # Append dates
  d = obj[['prms']][['date.start']] + s$time
  res = cbind(date = d, s)
  return(res)
}







if(0){
  
  # m = create(prms, name)
  # m.sim = simulate(m)
  # m.fit = fit(m, prms.fit, data, priors) --> posterior
  # m.fcst = fcst(m.fit, prms.fcst) --> traj ensemble
  
  
  library(amrem)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(microbenchmark)
  

  prms = example_model_prms()
  obj = amrem::create(prms = prms, name = 'foo')
  
  s = simulate(obj,
               check.prms = TRUE,  
               ww = TRUE)
  g = plot_timeseries(sim = s)
  g  
  
  
  prms2 = prms
  prms2$g = dist_create(mean = 3, var = 1, max = 5)
  obj2 = create(prms = prms2, name = 'foo2', check.prms = F)
  
  microbenchmark(
    rnochk = simulate(obj, check.prms = F), 
    rchk   = simulate(obj, check.prms = T), 
    c_byprod   = simulate_c(obj$prms, 1,1,1), 
    c_nobyprod = simulate_c(obj$prms, 0,0,0),
    c_noww_gi1 = simulate_c_backup(obj2$prms, 0),
    times = 1e3, unit = 'ms')
  
  # horizon = 200 ; ng = 7 => ~1500 microsec
}

