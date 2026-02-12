
col.post = 'indianred'


col.ag = c(
  "#AFC3EE", 
  "#879EFF",
  "#5C78FF",
  "#4A51D8",
  "#6E2AAE"
)

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


#' Helper function to summarise posterior data.
#'
#' @param df dataframe of posteriors.
#' @param ci width of credible interval
#'
#' @returns dataframe of summary statistics
#' @keywords internal
#'
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


#' Helper function to plot posteriors.
#'
#' @param df dataframe of posterior data.
#' @param dfs dataframe of summary statistics.
#' @param ncol number of columns for faceted plots.
#'
#' @returns a ggplot object.
#' @keywords internal
#'
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

#' Plot posterior distributions for vector parameters.
#'
#' @param prmname String. Name of the parameter.
#' @param post Dataframe. Posterior data as returned by \code{fit(...)$post}.
#' @param ci Numeric. Width of the credible interval.
#'
#' @returns A ggplot object.
#' @keywords internal
#'
plot_fit_post_vec <- function(prmname, post, ci) {
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


#' Plot posterior distributions for matrix parameters.
#'
#' @param prmname String. Name of the parameter.
#' @param post Dataframe. Posterior data as returned by \code{fit(...)$post}.
#' @param ci Numeric. Width of the credible interval.
#'
#' @returns A ggplot object.
#' @keywords internal
#'
plot_fit_post_matrix <- function(prmname, post, ci) {
  
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
  return(g)
}


#' Plot fitted posteriors
#'
#' @param fitobj List. Object as returned by \code{fit()}.
#' @param ci (Optional) Numeric. Width (between 0 and 1) of the credible interval to plot. Default to 0.95.
#'
#' @returns A list of ggplot objects. One element per fitted parameters. 
#' @export
#'
#' @examples
#' # TO DO
#' 
plot_fit_post <- function(fitobj, ci = 0.95) {
  
  post = fitobj$post
  g = list() ; i=1
  
  for(a in names(post)){
    message(a)
    if(a %in% c('h.prop', 'odds.testpos')) 
      g[[i]] = plot_fit_post_vec(a, post,ci)
    if(a %in% c('R')) 
      g[[i]] = plot_fit_post_matrix(a, post,ci)
    i = i+1
  }
  names(g) <- names(post)
  return(g)
}


helper_summstat_traj <- function(s, varname, ci) {
  res = s |> 
    group_by(time) |> 
    summarise(
      m = across(starts_with(varname), mean),
      qlo = across(starts_with(varname), \(x) quantile(x,probs = 0.5 - ci/2)),
      qhi = across(starts_with(varname), \(x) quantile(x,probs = 0.5 + ci/2)),
      .groups = 'drop'
    ) |> 
    ungroup() |>
    unnest_wider(col = matches('^[mq]'), names_sep = '_') |>
    pivot_longer(-time) |>
    separate(col = name, into = c('stat', 'var', 'ag'))|>
    as.data.frame()
  
  return(res)
}


#' Plot fitted trajectories
#'
#' @param fitobj List as returned by the functino \code{fit()}.
#' @param ci Numeric. Width of the quantiles to plot for the fitted trajectories.
#'
#' @returns A ggplot object.
#' @export
#'
#' @examples
#' 
plot_fit_traj <- function(fitobj, ci = 0.95) {
  
  duf = fitobj$prms.fit$data.used.fit
  obs = fitobj$data |> 
    flatten_data() |>
    tidyr::separate_wider_delim(cols = source, 
                                names = c('v', 'ag'), 
                                delim = '_',
                                cols_remove = FALSE)
  
  s = fitobj$simtraj
  
  ss = list()
  
  if('testpos' %in% duf){
    ss[['testpos']] = helper_summstat_traj(s, 'tau', ci) 
  }
  if('hospadm' %in% duf){
    ss[['hosadm']] = helper_summstat_traj(s, 'h', ci)
  }
  ssall = dplyr::bind_rows(ss) |> 
    dplyr::mutate(type = 'fit',
                  source = case_when(
                    var == 'tau' ~ 'testpos',
                    var == 'h' ~ 'hospadm',
                    TRUE ~ NA
                  )) |>
    dplyr::mutate(source = paste(source, ag, sep='_'))
  
  g = ssall |>
    tidyr::pivot_wider(names_from = 'stat') |>
    ggplot2::ggplot(ggplot2::aes(x=time, color = ag, fill = ag))+
    ggplot2::facet_wrap( ~ source, scales = 'free') +
    ggplot2::geom_point(data = obs, ggplot2::aes(y = value),
                        color = 'black')+
    ggplot2::geom_ribbon(ggplot2::aes(ymin = qlo, ymax = qhi), 
                         alpha = 0.1, linewidth = 0.1)+
    ggplot2::geom_line(ggplot2::aes(y=m), linewidth = 1) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = 'Fitted trajectories') + 
    ggplot2::guides(color = 'none', fill = 'none') + 
    ggplot2::scale_color_manual(values = col.ag)+
    ggplot2::scale_fill_manual(values = col.ag)
  return(g)
}


#' Plot errors
#'
#' @param fitobj Object as returned by \code{fit()}.
#'
#' @returns list of ggplot objects
#' @export
#'
#' @examples
plot_fit_errors <- function(fitobj) {
  
  tot = fitobj$errorsTotal |> 
    dplyr::mutate(idx2 = row_number())

  pf = fitobj$prms.fit
  npost = pf$p.accept * pf$priors.dist$n.priors
  
  g.tot = tot |> 
    ggplot2::ggplot(ggplot2::aes(x=idx2, y = err.total))+
    ggplot2::geom_vline(xintercept = npost, linetype = 'dashed')+
    ggplot2::geom_step()+
    ggplot2::scale_y_log10()+
    ggplot2::scale_x_log10() + 
    ggplot2::labs(title = 'Total Error', x='iterations', y = 'total error')
  g.tot
  
  errs = fitobj$errors

  err.vars = errs |> 
    dplyr::group_by(idx, data.name) |> 
    dplyr::summarise(m = mean(error), .groups = 'drop') |> 
    tidyr::pivot_wider(names_from = data.name, 
                values_from = m) |> 
    dplyr::select(-idx) 
  nam = names(err.vars)
  nc = ncol(err.vars)
  p = list() ; k = 1
  for(i in 1:nc){
    for(j in 1:nc){
      if(i<j){
        
        p[[k]] =  err.vars |> 
          ggplot2::ggplot(aes(x=.data[[nam[i] ]], y=.data[[nam[j] ]]))+
          ggplot2::geom_abline(intercept = 0, slope = 1)+
          ggplot2::scale_x_log10()+
          ggplot2::scale_y_log10()+
          # geom_point(alpha = 0.3)+
          ggplot2::geom_density_2d_filled(alpha = 0.7 ) + 
          ggplot2::guides(fill = 'none')
        k = k+1
      }
    }
  }
    pp = patchwork::wrap_plots(p)
  
    return(list(
      paired = pp,
      total = g.tot))
}

