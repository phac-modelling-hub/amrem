#
#  REMOVE THIS FILE AS THE C++ CODE SEEMS TO WORK
#  BUT SAVE TEST CODE AT THE BOTTOM
#

 xprod <- function(i, g, rj) {
      crossprod(g, rj[i,])
    }


#' Simulate epidemic.
#'
#' @param prms List of model parameters.
#'
#' @returns Dataframe of simulated epidemic.
#'
#' @export
#'
#' @examples simulate(list(foo = 1))
#'
simulate_r <- function(prms) {

  N       = prms[['N']]
  S0      = prms[['S0']]
  horizon = prms[['horizon']]
  alpha   = prms[['alpha']]
  g       = prms[['g']] 
  i0      = prms[['i0']]
  R       = prms[['R']]
  
  g.norm = sum(g)

  L = length(g)
  # 
  A = ncol(R) # number of age groups

  inc = matrix(data = NA, nrow = horizon, ncol = A)
  S   = matrix(data = NA, nrow = horizon, ncol = A)
  K   = matrix(data = NA, nrow = horizon, ncol = A)
  Nmat = matrix (N, nrow = horizon, ncol = A, byrow = TRUE)

  inc[1:L, ] = i0
  S[1:L, ] = matrix( rep(S0,L), ncol = 2, byrow = TRUE)

  for(t in (L+1):horizon){

    K = ( ( S / Nmat ) ^ exp(alpha) ) / g.norm
    # head(K)

    # Trailing incidence
    J = inc[(t-1):(t-L), ]
    # J
    
    rj = tcrossprod(R,J)  # Same as R %*% t(J)

    tmp = sapply(1:A, FUN = xprod, g=g, rj=rj)
    # tmp
    
    inc[t,] = (tmp * K[t-1,])
    S[t, ] = pmax(0, S[t-1, ] - inc[t,])
    
  }

  inc.df = data.frame(inc)
  names(inc.df) = paste('inc', 1:A, sep='_')

  S.df = data.frame(S)
  names(S.df) = paste('S', 1:A, sep='_')

  res =cbind(time = 1:horizon, inc.df, S.df)

  return(res)

  # plot(inc[,1], typ = 'l', ylim = range(inc)) ; grid()
  # lines(inc[,2], col = 'orange')

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

  sr = simulate_r(prms) |> mutate(lang = 'R')
  sc = simulate_c(prms) |> mutate(lang = 'C')

  s = bind_rows(sr,sc)


  s |>
    tidyr::pivot_longer(-c(time, lang)) |>
    select(-starts_with('S')) |>
    ggplot(aes(x = time, y = value, color = lang))+
    facet_wrap(~name, scales = 'free')+
    geom_line(linewidth = 1, alpha = 0.3)

  sc |>
    tidyr::pivot_longer(-time) |>
    ggplot(aes(x = time, y = value, color = name))+
    geom_line(linewidth = 1)
  
  
  microbenchmark(
    r = simulate_r(prms), 
    c = simulate_c(prms),
    times = 200)
}

