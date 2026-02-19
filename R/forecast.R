#' Forecast epidemic.
#'
#' @param fitobj List. Object as returned by the function \code{fit()}.
#' @param varnames String vector. Names of the variables to forecast. 
#' Can be data or simulation variable names (e.g. "hospadm" or "h").
#' Do not specify the age group, all age groups will be returned.
#' @param simulate Logical. If \code{FALSE}, will simply use the posterior
#' simulations of the fit object. This implicitly constrain the time horizon
#' and the number of forecast trajectories.
#' @param prm.fcst List. Forecast parameters.
#'
#' @returns
#' @export
#'
#' @examples
#' 
#' 
forecast <- function(
    fitobj, 
    varnames,
    simulate,
    prm.fcst) {
  
  if(0) {
    varnames = c('hospadm', 'testpos')
  }
  
  # Converts to simulation variable names
  nag = get_nag(fitobj[['obj']]) 
  v = sapply(varnames, get_sim_varnames, nag = nag ) |> as.vector()
  v
  if(!simulate){
    # Thu Feb 19 08:49:41 2026 ------------------------------
    # STOPPED HERE
    # IMPORTANT: Before implementing, change the simulate() function
    # such that "data" and "simulation" variable names are the SAME!
    # i.e. h_2 ==> hospadm_2, tau_1 ==> testpos_1
    # (Leave the C code with short name variable)
  }
  
}