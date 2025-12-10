generate_priors <- function(priors, nag) {
  nag = 2
  priors = list(
    n.priors = 100,
    R = c('unif', 0.1, 3),
    odds.testpos = c('unif', 0.9, 100),
    h.prop = c('unif', 0.0, 0.8)
  )
  
  nmp = names(priors)
  
  if('R' %in% nmp){
    dist = paste0('r', priors[['R']][[1]])
    param = priors[['R']][2:length(priors[['R']])] |> as.numeric() |> as.list()
    n = nag^2 * priors$n.priors
    tmp = do.call(what = dist, args = c(n = n, param))
    priors.R = matrix(data = tmp, ncol = nag)  
    # Wed Dec 10 15:34:02 2025 ------------------------------
    # stopped here
    
}
  
  
}



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
  
  # --- DEBUG
  if(1){
    prms.fit = list(
      prms.to.fit = c('R', 'odds.testpos', 'h.prop'),
      data.used.fit = c('testpos', 'hospadm'),
      priors = list(
        R = c('unif', 0.1, 3),
        odds.testpos = c('unif', 0.9, 100),
        h.prop = c('unif', 0.0, 0.8)
        )
    )
    prms = example_model_prms()
    obj = amrem::create(prms)
    data.testpos = data.frame(
      time = 7*c(1:4),
      value = c(0.001, 0.01, 0.03, 0.11)
    )
    
    data = list(
      testpos = data.testpos
    )
  }  # --- end debug
  
  obj$prms$R = 1.5 * prms$R
  sim = amrem::simulate(obj)
  
  
  ggplot(data.testpos, aes(x=time, y = value)) + 
    geom_line()+ 
    geom_point()+
    geom_line(data = sim, aes(x=time, y = tau_1))
  
}