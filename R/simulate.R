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
#' @examples 1 + 2 #TODO
#' 
#' 
simulate <- function(obj, 
                     check.prms = TRUE,
                     ww = TRUE, 
                     hosp = TRUE,
                     testpos = TRUE) {
  if(check.prms) check_prms_simulate(obj[['prms']])
  s = simulate_c(obj[['prms']], ww, hosp, testpos)
  return(s)
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
  
  N = c(1e5, 2e5)
  r0 = 1.3
  A = length(N)  # number of age groups
  ng = 7
  
  gi.means = matrix(c(2.5,3,
                      4,4), 
                    ncol=A, byrow = TRUE)
  gi.vars  = matrix(c(1,1,1,1), ncol=A)
  gi.maxs  = matrix(rep(ng, times = A*A), ncol=A)
  
  gi = amrem::dist_create_matrix(means = gi.means, 
                                vars = gi.vars, 
                                maxs = gi.maxs)
  if(0) plot_dist_matrix(gi)
  
  L = length(gi[[1]][[1]])
  
  prms = list(
    N = N,
    S0 = round(N * c(0.99, 0.99)),
    horizon = 200,
    alpha = 0,
    # Contact matrix R0
    R = r0 * rbind(
      c(1.0, 0.0),
      c(0.1, 0.7)),
    g = gi, 
    h.prop = c(0.01, 0.02),
    h.delay = amrem::dist_create(mean = 5, var = 2, max = 10),
    fec =  amrem::dist_create(mean = 4, var = 2, max = 10),
    odds.testpos = c(1, 20),
    i0 = cbind(1:L, N[2]/N[1]*c(1:L))
    )

  # print(prms$R)
  # print(gi.means)
  # plot(prms$fec, typ= 'b')
  
  obj = amrem::create(prms = prms, name = 'foo')
  
  s = simulate(obj,
               check.prms = TRUE,  
               ww = TRUE)
    
  g = s |>
    # filter(time < 50) |>
    mutate(inc_all = inc_1 + inc_2) |> 
    pivot_longer(-c(time)) |>
    select(-starts_with('S')) |>
    separate(name, into = c('name2', 'agegroup'), sep = '_') |>
    ggplot(aes(x = time, y = value, color = agegroup))+
    facet_wrap(~name2, scales = 'free', ncol = 2)+
    geom_line(linewidth = 1, alpha = 0.8)

  g
  
  # g+facet_grid(agegroup~name2)
  
  
  prms2 = prms
  prms2$g = dist_create(3,1,ng)
  obj2 = create(prms = prms2, name = 'foo2', check.prms = F)
  
  microbenchmark(
    rnochk = simulate(obj, check.prms = F), 
    rchk   = simulate(obj, check.prms = T), 
    c_byprod   = simulate_c(obj$prms, 1,1,1), 
    c_nobyprod = simulate_c(obj$prms, 0,0,0),
    c_noww_gi1 = simulate_c_backup(obj2$prms, 0),
    times = 200, unit = 'ms')
  
  # horizon = 200 ; ng = 7 => ~1500 microsec
}

