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
  
  # Mon Dec  1 15:59:34 2025 ------------------------------
  # TODO: fix problem  GTG
  
  
  library(amrem)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(microbenchmark)
  
  N = c(1e6, 2e6)
  r0 = 1.2
  A = length(N)
  
  gi.means = matrix(c(5,5,5,5), ncol=A)
  gi.vars  = matrix(c(1,1,1,1), ncol=A)
  gi.maxs  = matrix(rep(9, times = A*A), ncol=A)
  
  g = amrem::dist_create_matrix(means = gi.means, vars = gi.vars, maxs = gi.maxs)
  
  prms = list(
    N = N,
    S0 = round(N * c(0.99, 0.99)),
    horizon = 300,
    alpha = 0,
    # Contact matrix R0
    R = r0 * rbind(
      c(1.0, 1.0),
      c(1.0, 1.0)),
    g = g, # c(0.1, 0.2, 0.6, 0.3, 0.1),
    fec = c(0, 1, 1, 2, 5, 9, 7, 5, 3, 1),
    # fec = c(0, 2, 1),
    i0 = cbind(1:5, N[2]/N[1]*(1:5))  # length(g)
  )

  print(prms$R)
  print(prms$g)
  
  obj = create(prms = prms, name = 'foo')
  
  prms2 = prms
  prms2$g = c(0.1, 0.2, 0.6, 0.3, 0.1)
  obj2 = create(prms = prms2, name = 'foo2')
  
  s = simulate(obj,
               check.prms = TRUE,  
               ww = TRUE)
    
    
  g = s |>
    # filter(time < 200) |>
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
    c_noww_gi1 = simulate_c_backup(obj2$prms, 0),
    times = 200)
}

