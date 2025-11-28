#' Create an `amrem` model object.
#'
#' @param prms List. Model parameters.
#' @param name String. Name of the model.
#'
#' @returns List
#' @export
#'
#' @examples  1+1 #TODO
#' 
create <- function(prms, name = "no_name") {
  
  check_prms_create(prms)
  if(! is.character(name)) 
    stop('Argument `name` must be a character string.')
  
  return( list(
    name = name, 
    prms = prms
  ))
}