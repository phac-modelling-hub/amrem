
check_prms_create <- function(prms) {
  # Check if critical parameters are present
  required.names = c('N', 
                     'S0',
                     'i0',
                     'alpha', 
                     'R',
                     'g',
                     'fec')
  
  sapply(required.names, FUN = check_prms_name, prms=prms) 
  
  if(! all(prms[['N']] > 1)) stop('Population size (N) must be larger than 1')
  if(! is.numeric(prms[['alpha']])) stop('Parameter `alpha` must be a number.')
  
  g = prms[['g']]
  if(! is.list(g) ) 
    stop('Must input a list of list for generation interval distribution.')
  if(! is.list(g[[1]]) ) 
    stop('Must input a list of list for generation interval distribution.')
  
  g.lengths = unlist(lapply(g, function(inner) sapply(inner, length)))
  if(! all(g.lengths == g.lengths[1]))
    stop('All generation interval ditrib must have the same length (support).')
  
  
  # TODO: continue checks... 
}


#' Check if a parameter name is present. 
#'
#' @param x 
#' @param prms 
#'
#' @keywords internal
#'
check_prms_name <- function(x, prms) {
  if(! x %in% names(prms)) 
    stop('Parameter `', x, '` is missing from parameter list.')
}


#' Check the parameters for `simulate()` is valid.
#'
#' @param prms List of model parameters.
#'
#' @returns NULL if valid, an error if invalid.
#' @keywords internal
#'
check_prms_simulate <- function(prms) {
  
  # Check if all parameters are present
  required.names = c('N', 
                     'horizon',
                     'S0', 
                     'alpha', 
                     'R',
                     'g', 
                     'fec',
                     'i0')
  
  sapply(required.names, FUN = check_prms_name, prms=prms) 
  
  # Check types are valid
  stopifnot(class(prms[['N']]) == 'numeric')
  stopifnot(class(prms[['R']])[1] == 'matrix')
  
  
  # Check consistency with number of age groups
  chk.nag = 
    all(length(prms[['N']]) == length(prms[['S0']]),
        length(prms[['N']]) == nrow(prms[['R']]),
        length(prms[['N']]) == ncol(prms[['i0']]))
  
  if(!chk.nag){
    stop(
      'Inconsistent dimensions associated with number of age groups.\n',
      'All dimensions below must be equal:\n',
      paste0(
        'dim N  : ', length(prms[['N']]), '\n',
        'nrow R : ', nrow(prms[['R']]), '\n',
        'ncol i0: ', ncol(prms[['i0']]), '\n',
        'dim S0 : ', length(prms[['S0']]), '\n'
      )
    )
  }
  
  if(ncol(prms[['R']]) != nrow(prms[['R']]))
    stop('Input parameter `R` for `simulate()` must be a square matrix.')
  
  invisible(NULL)
}


#' Check if the parameters of `dist_create_matrix()` are valid.
#'
#' @param means matrix of means
#' @param vars matrix of variances
#' @param maxs matrix of maximums
#'
#' @keywords internal
#'
check_prms_dist_matrix <- function(means, vars, maxs) {
  
  stopifnot(all(dim(means) == dim(vars)))
  stopifnot(all(dim(means) == dim(maxs)))
 
  stopifnot(all(vars > 0))
  stopifnot(all(maxs > 0))

}

