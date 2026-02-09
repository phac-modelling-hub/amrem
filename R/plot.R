
col.post = 'indianred'


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


helper_summarise_post <- function(df, ci) {
  dfs = df |> 
    dplyr::group_by(nameplot) |> 
    dplyr::summarise(
      m = mean(value),
      md = median(value),
      qlo = quantile(value, probs = 0.5 - ci/2),
      qhi = quantile(value, probs = 0.5 + ci/2))
  return(dfs)
}


helper_plot_post <- function(df, dfs, ncol = NULL) {
  g = df |> ggplot2::ggplot(ggplot2::aes(x=value)) + 
    ggplot2::facet_wrap(~nameplot, ncol = ncol,
                        scales = 'free') + 
    ggplot2::geom_histogram(
      bins = 20, 
      alpha = 0.3,
      fill = col.post,
      aes(y = ggplot2::after_stat(density) )) +
    ggplot2::geom_density(color = col.post,
                          linewidth = 1.2)+
    # Summary stats
    ggplot2::geom_segment(
      data = dfs,
      linewidth = 1,
      ggplot2::aes(x=qlo, xend = qhi,
                   y = 0, yend = 0),
      color = col.post, 
      arrow = grid::arrow(
        angle=90, 
        length = grid::unit(0.3, 'lines'),
        ends = 'both'))+
    ggplot2::geom_point(
      data = dfs, 
      color = col.post, 
      size = 3, shape = 4,
      ggplot2::aes(x=md, y=0)) + 
    ggplot2::geom_point(
      data = dfs, 
      color = col.post, 
      size = 3,
      ggplot2::aes(x=m, y=0)) + 
    ggplot2::theme_bw()+
    ggplot2::theme(
      panel.grid = ggplot2::element_line(color = 'grey97'),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )
  return(g)
}

plot_fit_post_vec <- function(prmname, post, ci = 0.95) {
  
  # prmname = 'h.prop'
  # prmname = 'odds.testpos'
  x = post[[prmname]]
  df = do.call(rbind, x) |> 
    as.data.frame() |> 
    tidyr::pivot_longer(everything()) |> 
    dplyr::mutate(
      nameplot = paste(prmname, 
                       stringr::str_extract(name, '\\d'),
                       sep = '_'))
 
  dfs = helper_summarise_post(df, ci)
   
  g =  helper_plot_post(df = df, dfs = dfs)
  g
  return(g)
}


plot_fit_post_matrix <- function(prmname, post, ci = 0.95) {
  
  # prmname = 'R'
  
  x = post[[prmname]]
  
  nag = ncol(x[[1]])
  npost = length(x)
  
  q = rep(c(1:nag), npost)
  
  df = do.call(rbind, x) |> 
    as.data.frame() |> 
    dplyr::mutate(rowidx = q) |> 
    tidyr::pivot_longer(tidyr::starts_with('V')) |> 
    dplyr::mutate(
      colidx = stringr::str_extract(name, '\\d'),
      nameplot = paste(prmname, 
                       rowidx, colidx,
                       sep = '_'))
  
  dfs = helper_summarise_post(df, ci)
 
  g =  helper_plot_post(df = df, dfs = dfs, ncol = nag)
  g
  
}


plot_fit_post <- function(fitobj) {
  
  
  post = fitobj$post
  
  names(post)
  
  
}
