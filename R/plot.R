



#' Plot simulated time series
#'
#' @param sim Dataframe. Simulation returned by function \code{amrem::simulate()}.
#'
#' @returns A `ggplot` object. 
#' @export
#'
#' @examples
#' 
#' prms = amrem::example_model_prms()
#' obj  = amrem::create(prms) 
#' sim  = amrem::simulate(obj)
#' g    = amrem::plot_timeseries(sim)
#' plot(g)
#' 
plot_timeseries <- function(sim) {
  
  if(0){
    prms = example_model_prms()
    obj  = amrem::create(prms) 
    sim = simulate(obj)
  }
  
  df = sim |> 
    tidyr::pivot_longer(cols = -time) |> 
    tidyr::separate(col = name, into = c('name', 'age_group'), sep ='_')
  
  g = df |> 
    ggplot2::ggplot(ggplot2::aes(x=time, y = value, color = age_group))+
    ggplot2::geom_line()+
    ggplot2::facet_wrap(~name, scales = 'free_y')
  return(g)
}

