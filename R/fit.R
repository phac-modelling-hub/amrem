#' Fit a model object to data.
#'
#' @param obj List Model object as returned by \code{amrem::create()}.
#' @param prms.fit List Parameters related to the fitting algorithm. 
#' @param data List Data to fit the model to. 
#'
#' @returns TODO
#' @export
#'
#' @examples foo = fit()
#' 
#' 
fit <- function(obj, prms.fit, data) {
 
  # Data that can be observed:
  # testpos -> tau_a --> odds.testpos
  # hospadm -> h_a --> h.prop
  # ww -> w_a 
  
   
  prms = example_model_prms()
  
  obj = amrem::create(prms)

  obj$prms$R = 1.5 * prms$R
  sim = amrem::simulate(obj)
  
  
  
  data.testpos = data.frame(
    time = 7*c(1:4),
    value = c(0.001, 0.01, 0.03, 0.11)
  )
  
  data = list(
    testpos = data.testpos
  )
  
  ggplot(data.testpos, aes(x=time, y = value)) + 
    geom_line()+ 
    geom_point()+
    geom_line(data = sim, aes(x=time, y = tau_1))
  
}