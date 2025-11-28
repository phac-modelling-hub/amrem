#' Simulate an epidemic.
#'
#' @param check.prms Logical. Check input parameters? Default = TRUE.
#' @param obj List representing the `amrem` object as returned by \code{amrem::create()}.
#'
#' @returns Dataframe of simulated variables.
#' @export
#'
#' @examples 1 + 2 #TODO
#' 
#' 
simulate <- function(obj, check.prms = TRUE) {
  if(check.prms) check_prms_simulate(obj[['prms']])
  s = simulate_c(obj[['prms']])
  return(s)
}

if(0){

  library(amrem)
  library(ggplot2)
  library(dplyr)
  library(microbenchmark)
  
  N = c(1e6, 1e6)
  r0 = 1.2
  
  prms = list(
    N = N,
    S0 = round(N * c(0.99, 0.99)),
    horizon = 300,
    alpha = 0,
    # Contact matrix R0
    R = r0 * rbind(
      c(1.5, 1.0),
      c(1.0, 1.0)),
    g = c(0.1, 0.2, 0.6, 0.3, 0.1),
    i0 = cbind(1:5, N[2]/N[1]*(1:5))  # length(g)
  )

  obj = create(prms = prms, name = 'foo')
  
  s = simulate(obj) |> 
    mutate(Sall = S_1 + S_2)
  
  g = s |>
    tidyr::pivot_longer(-c(time)) |>
    select(-starts_with('S')) |>
    mutate(name2 = substr(name, 1,1)) |>
    ggplot(aes(x = time, y = value, color = name))+
    facet_wrap(~name2, scales = 'free', ncol = 1)+
    geom_line(linewidth = 1, alpha = 1)

  g
  
  microbenchmark(
    rnochk = simulate(obj, F), 
    rchk = simulate(obj, T), 
    c = simulate_c(obj$prms), 
    times = 100)
}

