
check_prms_create <- function(prms) {
  # Check if critical parameters are present
  required.names = c('N', 
                     'S0',
                     'i0',
                     'alpha', 
                     'R',
                     'g')
  
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
  L = g.lengths[1]
  
  if( nrow(prms$i0) != L)
    stop('Parameter `i0` must be a matrix with ', L, ' rows.',
         ' (currently nrow(i0)=',nrow(prms$i0),')')
  
  # TODO: continue checks... 
}


#' Check if a parameter name is present. 
#'
#' @param x Name of parameter
#' @param prms List of parameters.
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
                     'date.start',
                     'horizon',
                     'S0', 
                     'alpha', 
                     'R',
                     'g', 
                     'fec',
                     'odds.testpos',
                     'i0')
  
  sapply(required.names, FUN = check_prms_name, prms=prms) 
  
  # Check types are valid
  stopifnot(class(prms[['N']]) == 'numeric')
  stopifnot(length(prms[['N']]) > 0)
  stopifnot(is.matrix(prms[['R']]))
  stopifnot(class(prms[['date.start']]) == 'Date')
  
  # Check consistency with number of age groups
  chk.nag = 
    all(length(prms[['N']]) == length(prms[['S0']]),
        length(prms[['N']]) == nrow(prms[['R']]),
        length(prms[['N']]) == nrow(prms[['odds.testpos']]),
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
  if(is.matrix(prms[['R']])){
    if(ncol(prms[['R']]) != nrow(prms[['R']]))
      stop('Input parameter `R` for `simulate()` must be a square matrix.')
  }
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


#' Check the first element of character vector are letters.
#'
#' @param x 
#'
#' @keywords internal
#' 
check_first_letter <- function(x) {
  # x = c('unif', 1,2)
  return(grepl('^[A-Za-z]', x[1]))
}



#' Check the argument parameters for fit().
#'
#' @param prms.fit List of parameters
#' @param nag Integer. Number of age groups.
#'
#' @keywords internal
#'
check_prms_fit <- function(prms.fit, nag) {
  
  nam = names(prms.fit)
  
  check_prms_name(x = 'data.used.fit', prms = prms.fit)
  check_prms_name(x = 'n.priors', prms = prms.fit)
  check_prms_name(x = 'p.accept', prms = prms.fit)
  check_prms_name(x = 'n.cores', prms = prms.fit)
  check_prms_name(x = 'priors.dist', prms = prms.fit)
  
  pd = prms.fit[['priors.dist']]
  
  if(0){
    pd = list(
      R = list(
        r1c1 = c('unif', 0.1, 0.3),
        r2c1 = c('unif', 0.5, 0.9),
        # r1c2 = c(1233, 1.1, 1.5),
        r1c2 = c('unif', 1.1, 1.5),
        r2c2 = c('unif', 1.6, 2.1)
      ),
      odds.testpos = list(c('unif', 0.9, 100)),
      h.prop = list(
        c('unif', 0.2, 0.3),
        # c('unif', 0.2, 0.3),
        c('unif', 0.0, 0.1)
      )
    )
  }
  
  
  # The prior distributions are either
  #  - dim = 1: in that case all elements of the parameter
  #    have the same prior distribution
  #  - dim = nag: distribution specified for each element of the vector prm
  #  - dim = nag^2: distribution specified for each element of the matrix prm
  #
  ok = sapply(pd, length) %in% nag^c(0,1,2)
  if(any(!ok)){
    prob = paste(names(pd)[!ok], collapse = ', ')
    stop(paste0('The number of prior distributions for parameter(s) `',
                prob, '` is incorrect.\n',
                '(number should be either 1, ',nag,' or ',nag^2,').'))
  }
  
  # Check the list elements all have a distribution name as first sub-element.
  for(i in seq_along(pd)){
    y = sapply(pd[[i]], check_first_letter)
    if(any(!y)){
      stop('Prior distribution(s) for `', names(pd)[i], '` misspecified.')
    }
  }

}


