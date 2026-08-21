#' Create an `amrem` model object.
#'
#' @param prms List. Model parameters.
#' @param name String. Name of the model.
#' @param check.prms Logical. Check parameters (default = TRUE).
#'
#' @returns List
#' @export
#'
#' @examples  1+1 #TODO
#' 
create <- function(prms, name = "no_name", check.prms = TRUE) {
  
  if(check.prms) check_prms_create(prms)
  if(! is.character(name)) 
    stop('Argument `name` must be a character string.')
  
  prms = process_prms_i0prop(prms)
  
  return( list(
    name = name, 
    prms = prms
  ))
}

#' Process `i0prop` parameter
#'
#' @param prms List of model parameters.
#' @keywords internal
#' @returns List of model parameters with `i0` matrix if `i0prop` is present.
#
process_prms_i0prop <- function(prms) {
 has.i0.prop = 'i0prop' %in% names(prms)
if(has.i0.prop){
  message('Parameter `i0prop` is present. Converting to `i0` matrix.')
   L = length(prms[['g']][[1]][[1]])
   nag = length(prms[['N']])
   value = round(prms[['i0prop']] * prms[['N']], 0)
   i0 = matrix(value, nrow = L, ncol = nag, byrow = TRUE)
   prms[['i0']] = i0 
}
 return(prms)
}