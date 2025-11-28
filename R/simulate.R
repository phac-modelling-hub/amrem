#' Simulate an epidemic.
#'
#' @param check.prms Logical. Check input parameters? Default = TRUE.
#' @param obj List representing the `amrem` object as returned by \code{amrem::create()}.
#' @param ww A boolean to switch on/off fecal shedding into wastewater.
#'
#' @returns Dataframe of simulated variables.
#' @export
#'
#' @examples 1 + 2 #TODO
#' 
#' 
simulate <- function(obj, 
                     check.prms = TRUE,
                     ww = TRUE) {
  if(check.prms) check_prms_simulate(obj[['prms']])
  s = simulate_c(obj[['prms']], ww)
  return(s)
}

if(0){
  
  # m = create(prms, name)
  # m.sim = simulate(m)
  # m.fit = fit(m, prms.fit, data, priors) --> posterior
  # m.fcst = fcst(m.fit, prms.fcst) --> traj ensemble
  
  # TODO: age-dependent generation interval
  # in order to have delayed waves bw age groups (as observed) 
  
  
  library(amrem)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(microbenchmark)
  
  N = c(1e6, 2e6)
  r0 = 1.2
  
  prms = list(
    N = N,
    S0 = round(N * c(0.99, 0.99)),
    horizon = 300,
    alpha = 0,
    # Contact matrix R0
    R = r0 * rbind(
      c(1.9, 0.9),
      c(1.0, 1.0)),
    g = c(0.1, 0.2, 0.6, 0.3, 0.1),
    fec = c(0, 1, 1, 2, 5, 9, 7, 5, 3, 1),
    # fec = c(0, 2, 1),
    i0 = cbind(1:5, N[2]/N[1]*(1:5))  # length(g)
  )

  obj = create(prms = prms, name = 'foo')
  
  s = simulate(obj,
               check.prms = TRUE,  
               ww = TRUE)
  
  g = s |>
    pivot_longer(-c(time)) |>
    select(-starts_with('S')) |>
    separate(name, into = c('name2', 'agegroup'), sep = '_') |>
    ggplot(aes(x = time, y = value, color = agegroup))+
    facet_wrap(~name2, scales = 'free', ncol = 1)+
    geom_line(linewidth = 1, alpha = 1)

  g
  
  microbenchmark(
    rnochk = simulate(obj, check.prms = F), 
    rchk = simulate(obj, check.prms = T), 
    c_ww = simulate_c(obj$prms, 1), 
    c_noww = simulate_c(obj$prms, 0),
    times = 300)
}

