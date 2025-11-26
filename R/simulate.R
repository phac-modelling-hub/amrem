#' Simulate an epidemic.
#'
#' @param prms List of model parameters
#' @param check.prms Logical. Check input parameters? Default = TRUE.
#'
#' @returns Dataframe of simulated variables.
#' @export
#'
#' @examples 1 + 2
#' 
#' 
simulate <- function(prms, check.prms = TRUE) {
  if(check.prms) check_prms_simulate(prms)
  s = simulate_c(prms)
  return(s)
}

if(0){

  library(amrem)
  library(ggplot2)
  library(dplyr)
  library(microbenchmark)
  
  N = c(1e5, 3e5)
  
  prms = list(
    N = N,
    S0 = round(N * c(0.98, 0.70)),
    horizon = 100,
    alpha = 0,
    # Contact matrix R0
    R = 1.3 * rbind(
      c(1.5, 1.0),
      c(1.0, 1.0)),
    g = c(0.1, 0.2, 0.6, 0.3, 0.1),
    i0 = cbind(1:5, 2*(1:5))  # length(g)
  )

  s = simulate(prms)


  s |>
    tidyr::pivot_longer(-c(time)) |>
    select(-starts_with('S')) |>
    ggplot(aes(x = time, y = value))+
    facet_wrap(~name, scales = 'free')+
    geom_line(linewidth = 1, alpha = 1)

  
  microbenchmark(
    rnochk = simulate(prms, F), 
    rchk = simulate(prms, T), 
    c = simulate_c(prms), 
    times = 100)
}

