


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

#' Create synthetic data from a simulated epidemic
#'
#' @param model.prms List. Model parameters as returned by the function \code{example_model_prms()}.
#' @param t.obs Integer vector of observation times. 
#'
#' @returns List of data sets as expected by the function \code{amrem::fit()}.
#' @export
#'
#' @examples
#' 
#' model.prms = example_model_prms()
#' t.obs = 12*c(1:10)    # observation times
#' dat = example_simulated_data(model.prms, t.obs)
#' 
example_simulated_data <- function(model.prms, 
                                   t.obs) {
  
   # set up simulation to generate "observations"
  prms0 = model.prms
  nag   = length(prms0$N)
  obj0  = amrem::create(prms0)
  
  # Model that generates data
  simobs = simulate(obj0)
   
  # Clinical test positivity
  data.testpos1 = simobs |> 
    dplyr::filter(time %in% t.obs )    |> 
    dplyr::select(time, value = testpos_1)
  data.testpos2 = simobs |> 
    dplyr::filter(time %in% t.obs )    |> 
    dplyr::select(time, value = testpos_2)
  
  # Hospital admissions
  data.hosp1 = simobs |> 
    dplyr::filter(time %in% t.obs )    |> 
    dplyr::select(time, value = hospadm_1)
  data.hosp2 = simobs |> 
    dplyr::filter(time %in% t.obs )    |> 
    dplyr::select(time, value = hospadm_2)
  
  data = list(
    testpos_1 = data.testpos1,
    testpos_2 = data.testpos2,
    hospadm_1 = data.hosp1,
    hospadm_2 = data.hosp2
  )
  return(data)
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

#' Flatten data structure
#'
#' @param data List of dataframe having the same structure as 
#' the list of data passed as argument of the function \code{fit()}. 
#'
#' @returns A single, merged and long-format dataframe.
#' @export
#'
#' @examples
#' # Create data for example
#' data = list(
#' testpos_1 = data.frame(time = 1:5, value = runif(n=5)),
#' testpos_2 = data.frame(time = 1:5, value = runif(n=5)),
#' hospadm_1 = data.frame(time = 1:5, value = runif(n=5)),
#' hospadm_2 = data.frame(time = 1:5, value = runif(n=5)))
#'
#' flatten_data(data)
#' 
flatten_data <- function(data) {
  # data = fitobj$data
  res = dplyr::bind_rows(data, .id = 'source')
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
