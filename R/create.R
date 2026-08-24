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



#' Converts `i0.prop` into incidence matrix `i0`
#'
#' @param prms List of model parameters
#'
#' @returns List of model parameters
#' @keywords internal
#'
translate_i0prop_to_i0 <- function(prms) {
  
  if(any(prms[['i0.prop']] <= 0 | prms[['i0.prop']] >= 1)) 
    stop('Parameter `i0.prop` must be a numeric value between 0 and 1.')
  
  L     = length(prms[['g']][[1]][[1]])
  nag   = length(prms[['N']])
  value = round(prms[['i0.prop']] * prms[['N']], 0)
  i0    = matrix(value, nrow = L, ncol = nag, byrow = TRUE)
  
  prms[['i0']] = i0
  return(prms)
}





#' Process `i0.prop` parameter
#'
#' @param prms List of model parameters.
#' @keywords internal
#' @returns List of model parameters with `i0` matrix if `i0.prop` is present.
#
process_prms_i0prop <- function(prms) {
 has.i0.prop = 'i0.prop' %in% names(prms)
if(has.i0.prop){
  message('Parameter `i0.prop` is present. Converting to `i0` matrix.')
  prms = translate_i0prop_to_i0(prms)
}
 return(prms)
}