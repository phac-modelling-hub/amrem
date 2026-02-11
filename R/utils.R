


#' Example of model parameters
#'
#' @returns List Example of model parameters
#' @export
#'
#' @examples prms = example_model_prms()
#'
example_model_prms <- function() {
  
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
  return(prms) 
}

#' Flatten data structure
#'
#' @param data List of dataframe having the same structure as 
#' the list of data passed as argument of the function \code{fit()}. 
#'
#' @returns A single, merged and long-format dataframe.
#' @export
#'
#' @examples
#' 
#' 
flatten_data <- function(data) {
  # data = fitobj$data
  res = dplyr::bind_rows(data, .id = 'source')
  return(res)
}
