
#' Helper to generate GI distributions
#'
#' @param gi.mean 
#' @param gi.var 
#' @param gi.max 
#'
#' @returns
#' @keywords internal
#'
helper_generate_gi <- function(A, gi.mean = 3, gi.var = 1, gi.max = 7) {
  gi.means = matrix(rep(gi.mean, A^2), ncol=A, byrow = TRUE)
  gi.vars  = matrix(rep(gi.var, A^2), ncol=A)
  gi.maxs  = matrix(rep(gi.max, times = A*A), ncol=A)
  
  gi = amrem::dist_create_matrix(means = gi.means, 
                                 vars = gi.vars, 
                                 maxs = gi.maxs)
  return(gi)
}


#' Helper to generate a contact matrix
#'
#' @param A Integer. Number of age groups
#' @param r0 
#' @param decay 
#' @param min 
#' @param max 
#'
#' @returns A contact matrix.
#' @keywords internal
#'
helper_generate_R <- function(A, 
                              r0 = 1.0, 
                              decay = 0.1, 
                              min = 0.1, max = 0.4) {
  m = matrix(data = runif(n = A^2, min = min, max = max), ncol = A)
  diag(m) <- exp(-decay * c(1:A))
  res = r0 * m
  return(res)
}

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
    date.start = as.Date.character("2026-01-01", format = "%Y-%m-%d"),
    horizon = 200,
    alpha = 0,
    # Contact matrix R0
    R = r0 * rbind(
      c(1.0, 0.2),
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

#' Example of model parameters with flexible number of age groups.
#'
#' @param r0 Numeric. Global multiplier for contact matrix.
#' @param N Numeric vector of population sizes.
#' @param S0.prop Numeric vector of proportion of initial susceptible populations.
#' @param h.prop Numeric vector of proportion of hospitalization rate for each age group.
#' @param odds.testpos Numeric vector of odds to test positive for each age group. 
#'
#' @returns List Example of model parameters
#' @export
#'
#' @examples prms = example_model_prms_ag()
#'
example_model_prms_ag <- function(
    r0 = 1.3,
    N = c(0.7, 2.4, 4.7, 3.9, 2.7) * 1e6,  # ON numbers
    S0.prop = c(0.99, 0.80, 0.3, 0.3, 0.6),
    h.prop       = c(0.02, 0.01, 0.001, 0.03, 0.05),
    odds.testpos = c(3, 2, 1, 5, 10)
) {
  
  if(0){ # DEBUG
    r0 = 1.3
    N = c(0.7, 2.4, 4.7, 3.9, 2.7) * 1e6  # ON numbers
    S0.prop = c(0.99, 0.80, 0.3, 0.3, 0.6)
    h.prop       = c(0.02, 0.01, 0.001, 0.03, 0.05)
    odds.testpos = c(2, 1, 0.2, 5, 10)
  }
  
  A = length(N)  # number of age groups
  stopifnot(A == length(S0.prop))
  stopifnot(A == length(h.prop))
  stopifnot(A == length(odds.testpos))
  
  
  # Generation interval distributions
  gi = helper_generate_gi(A)
  if(0) amrem::plot_dist_matrix(gi)
  gi.max = length(gi[[1]][[1]])
  
  # Initial susceptible populations
  S0 = round(N * S0.prop)
  
  # Contact matrix R0
  R = helper_generate_R(A)
  
  # Initial infectious population
  i0 = matrix(rep(1:gi.max, A), ncol = A)
  
  prms = list(
    N = N,
    S0 = S0,
    date.start = as.Date.character("2026-01-01", format = "%Y-%m-%d"),
    horizon = 200,
    alpha = 0,
    R = R,
    g = gi, 
    h.prop = h.prop,
    h.delay = amrem::dist_create(mean = 5, var = 2, max = 10),
    fec     = amrem::dist_create(mean = 4, var = 2, max = 10),
    odds.testpos = odds.testpos,
    i0 = i0
  ) 
  return(prms) 
}





helper_extract_data <- function(v, simobs) {
  # v = 'testpos'
  d = simobs |> 
    dplyr::select(date, starts_with(v)) |> 
    tidyr::pivot_longer(-date)
  
  a = list()
  u = unique(d$name)
  for(i in seq_along(u)){
    a[[i]] = d |> 
      dplyr::filter(name == u[i]) |> 
      dplyr::select(date, value)
  }
  names(a) = paste(v,seq_along(u),sep = '_')
  return(a)
}


#' Helper function.
#' Generate the expected names of elements 
#' in the `data` list, including age group.
#'
#' @param data.type String.
#' @param nag Integer. Number of age groups.
#' @keywords internal
#' 
get_data_names <- function(data.type, nag) {
  res = NA
  if(data.type == 'testpos') res = paste('testpos',1:nag,sep='_')
  if(data.type == 'hospadm') res = paste('hospadm',1:nag,sep='_')
  return(res)
}

#' Helper function.
#' Link data names with simulation variables names.
#'
#' @param data.type String.
#' @param nag Integer for the number of age groups.
#' @keywords internal
#' 
#' @examples 
#' get_sim_varnames('hospadm', 2) 
#' # returns: c("h_1", "h_2")
#' 
get_sim_varnames <- function(data.type, nag){
  stopifnot(length(data.type) == 1)
  res = paste(data.type, 1:nag, sep='_')
  return(res)
}



#' Return the number of age groups of a model
#'
#' @param obj List. Object as returned by the function \code{amrem::create()}.
#'
#' @returns Integer. Number of age groups.
#' @export
#'
#' @examples
#' 
#' model.prms = amrem::example_model_prms()
#' obj = amrem::create(model.prms)
#' print(get_nag(obj))
#' 
get_nag <- function(obj) {
  p   = obj[['prms']]
  nag = length(p[['N']])
  if(nag == 0) 
    stop('Model object is not correctly built: no age structure found.')
  return(nag)
}


#' Helper function. Return the last date of each data sets.
#'
#' @param fitobj List object as returned by the function \code{fit()}.
#'
#' @returns List of dates.
#' @keywords internal
#'
get_last_date_data <- function(fitobj) {
  data = fitobj$data
  v = lapply(data, function(x){max(x$date)})
  return(v)
}



#' Standardize labels for age groups
#'
#' @param x Character vector of age group labels
#'
#' @returns Character vector of standardized age group labels.
#' @export
#'
#' @examples
#' 
#'  x = c('0-4', '0 to 4', '65+', '65 +')
#'  standardize_age_groups(x)
#' 
#' 
standardize_age_groups <- function(x) {
  y = x |> 
    stringr::str_remove_all("\\s+") |> 
    stringr::str_extract_all(pattern ='\\d+\\+?') 
  # y
  res = sapply(y, paste, collapse='-')
  # res
  return(res)
}





