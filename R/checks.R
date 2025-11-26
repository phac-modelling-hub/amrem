#' Check if a parameter name is present. 
#'
#' @param x 
#' @param prms 
#'
#' @keywords internal
#'
check_name <- function(x, prms) {
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
                     'i0')
  
  sapply(required.names, FUN = check_name, prms=prms) 
  
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

