
col.post = 'indianred'


#' Return the color palette for age groups
#'
#' @param nag Integer. Number of age groups.
#'
#' @returns Vector of colors.
#' @keywords internal
#'
#' @examples get_colors_age_groups(3)
#' 
get_colors_age_groups <- function(nag) {
  col.ag = c(
    "#00C853", 
    "#00AFC6", 
    "#1F8FFF",
    "#3F6FE6", 
    "#6A4CCC", 
    "#9C27B0"
  )
 

  if(nag == 1) return('black')
  if(nag == 2) return(col.ag[c(2,5)])
  if(nag == 3) return(col.ag[c(1,3,6)])
  if(nag > 3) return(col.ag)
}

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
    tidyr::pivot_longer(cols = -c(time, date)) |> 
    tidyr::separate(col = name, into = c('name', 'age_group'), sep ='_')
  
  nag = length(unique(df$age_group))
  
  g = df |> 
    ggplot2::ggplot(ggplot2::aes(x=date, y = value, color = age_group))+
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::facet_wrap(~name, scales = 'free_y') +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid = ggplot2::element_line(color = 'gray97'),
      strip.text = ggplot2::element_text(face = 'bold'))+
    ggplot2::scale_color_manual(values = get_colors_age_groups(nag))+
    ggplot2::labs(title = 'Time series')
  g
  return(g)
}



#' Plot data inputted in list format.
#'
#' @param data List of data (as expected by the function \code{amrem::fit()}.)
#'
#' @returns A ggplot object
#' @export
#'
#' @examples
#' 
#' model.prms = example_model_prms()
#' t.obs = 12*c(1:10)    # observation times
#' date.obs = model.prms$date.start + 12*c(1:10)    # observation dates
#' data = example_simulated_data(model.prms, date.obs)  
#' g = plot_data_list(data)
#' plot(g)
#' 
plot_data_list <- function(data) {
  
  df = flatten_data(data) |> 
    tidyr::separate(col = 'source', 
                    into = c('variable', 'age_group'), 
                    sep = '_')
  
  nag = length(unique(df$age_group))
 
  g = df |> ggplot2::ggplot(
    ggplot2::aes(x = date, y = value, color = age_group))+
    ggplot2::geom_line(linewidth = 1) + 
    ggplot2::theme_bw()+
    ggplot2::facet_wrap(~ variable, scales = 'free_y', ncol = 1) + 
    ggplot2::scale_color_manual(values = get_colors_age_groups(nag))
  
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
helper_plot_post <- function(df, dfs, 
                             ncol = NULL, true.val = NULL) {
  
  g = df |> ggplot2::ggplot(ggplot2::aes(x=value)) + 
    ggplot2::facet_wrap(~nameplot, ncol = ncol,
                        scales = 'free') + 
    ggplot2::geom_histogram(
      bins = 20, 
      alpha = 0.15,
      fill = col.post,
      ggplot2::aes(y = ggplot2::after_stat(density) )) +
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
  
  if(!is.null(true.val)){
    
    varname = unique(dfs$nameplot) |> 
      stringr::str_remove_all('\\_') |> 
      stringr::str_remove_all('\\d') |> 
      unique()
    varname
    
    if(length(varname) == 1){
      tmp = list(); k=1
      
      if(is.vector(true.val) & !is.matrix(true.val)){
        for(i in 1:length(true.val)){
          tmp[[k]] = data.frame(
            nameplot = paste(varname,i,sep = '_'),
            value    = true.val[i])
          k = k+1
        }
      }
      
      if( is.matrix(true.val) ){
        for(i in 1:nrow(true.val)){
          for(j in 1:ncol(true.val)){
            tmp[[k]] = data.frame(
              nameplot = paste(varname,i,j,sep = '_'),
              value    = true.val[i,j])
            k = k+1
          }
        }
      }
      
      dftrue = dplyr::bind_rows(tmp)
      g = g + ggplot2::geom_vline(
        data = dftrue, 
        color = 'skyblue2',
        linewidth = 1,
        ggplot2::aes(xintercept = value))
      # g
    } 
  }
  # g
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
plot_fit_post_vec <- function(prmname, post, ci, true.values) {
  # prmname = 'h.prop'
  # prmname = 'odds.testpos'
  x = post[[prmname]]
  
  df0 = do.call(rbind, x) |> 
    as.data.frame()
  
  nag = length(x[[1]])
  names(df0) = paste(prmname, 1:nag, sep = '_')
  
  df =  df0 |> 
    tidyr::pivot_longer(everything()) |> 
    dplyr::rename(nameplot = name)
  
  dfs = helper_summarise_post(df, ci)
  
  true.val = NULL
  if(!is.null(true.values)){
    true.val = true.values[[prmname]]
  }
  
  g =  helper_plot_post(df = df, dfs = dfs, true.val = true.val)
  # g
  return(g)
}


helper_listvec_to_df <- function(post, prmname) {
  y = post[prmname]
  
  df00 = lapply(y, 
                function(z){
                  do.call(rbind, z) |> 
                    as.data.frame()
                })
  df0 = do.call(cbind, df00)
  
  names(df0) = stringr::str_replace(names(df0), 
                                    pattern = '.V', 
                                    replacement = '_')
  return(df0)
}


#' Helper function to Convert a
#' list of matrices into a dataframe.
#' 
#' @param x List of matrices.
#' @param pname String. Parameter name.
#' 
helper_listmat_to_df <- function(x, pname){
  
  nag = ncol(x[[1]])
  npost = length(x)
  
  q = rep(c(1:nag), npost)
  
  
  df = do.call(rbind, x) |> 
    as.data.frame() |> 
    dplyr::mutate(rowidx = q) |> 
    tidyr::pivot_longer(tidyr::starts_with('V')) |> 
    dplyr::mutate(
      colidx = stringr::str_extract(name, '\\d'),
      nameplot = paste(pname, 
                       rowidx, colidx,
                       sep = '_')) |> 
    dplyr::select(name = nameplot, value)
  df$idx = rep(1:npost, each=nag^2)
  
  return(df)
}

#' Plot 2D posterior distributions
#'
#' @param fitobj List. Fit object as returned by the function \code{\link{fit}()}.
#' @param true.values Not used.
#'
#' @returns List of ggplot objects.
#' @export
#'
#' @examples
#' 
plot_fit_post_2d <- function(fitobj, true.values = NULL) {
  
  post = fitobj[['post']]
  
  # Select vectors only:
  prm.names = names(post)
  prm.names.vec = prm.names[prm.names %in% c('h.prop', 'odds.testpos')]
  prm.names.mat = prm.names[prm.names %in% c('R')] 
  
  has.matrix =length(prm.names.mat) > 0
  
  df.vec = helper_listvec_to_df(post, prm.names.vec)
  
  df0 = df.vec
  
  if(has.matrix){
    # Note: if other matrix than 'R', change code above in a `lapply`
    xm = post[prm.names.mat]
    df.mat = helper_listmat_to_df(xm[['R']], pname = 'R') |> 
      tidyr::pivot_wider(names_from = 'name', values_from = 'value') |>
      dplyr::select(-idx)
    df0 = cbind(df.vec, df.mat)
  }
  
  df =  df0 |> 
    tidyr::pivot_longer(everything()) |> 
    dplyr::rename(nameplot = name)
  
  # dfs = helper_summarise_post(df, ci)
  
  true.val = NULL
  if(!is.null(true.values)){
    true.val = true.values[[prmname]]
  }
  
  k = 1 ; g2d = list()
  for(i in 1:ncol(df0)){
    for(j in i:ncol(df0)){
      if(i<j){
        dfij = df0[,c(i,j)]
        nx = names(df0)[i]
        ny = names(df0)[j]
        
        correlij = round(cor(dfij[,1], dfij[,2]),2)
        
        g2d[[k]] = dfij |> 
          ggplot2::ggplot(ggplot2::aes(x=.data[[nx]], y=.data[[ny]])) + 
          ggplot2::geom_density_2d_filled(bins = 12) + 
          ggplot2::theme_bw() + 
          ggplot2::scale_fill_viridis_d(option = "inferno")+
          ggplot2::theme(
            panel.grid = ggplot2::element_blank(),
            panel.border = ggplot2::element_blank(),
            axis.text = ggplot2::element_text(size = ggplot2::rel(0.6)),
            plot.subtitle = ggplot2::element_text(
              size = ggplot2::rel(0.8), 
              color = 'grey'),
            plot.title = ggplot2::element_text(face = 'bold'))+
          ggplot2::labs(title = paste(ny, nx,sep = '  |  '),
                        subtitle = paste0('correlation = ', correlij)) + 
          ggplot2::guides(fill = 'none')
        g2d[[k]]
        k = k+1
      }
    }
  }
  # patchwork::wrap_plots(g2d) 
  return(g2d) 
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
plot_fit_post_matrix <- function(prmname, post, ci, true.values) {
  
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
  
  true.val = NULL
  if(!is.null(true.values)){
    true.val = true.values[[prmname]]
  }
  
  g =  helper_plot_post(df = df, dfs = dfs, ncol = nag,
                        true.val = true.val)
  return(g)
}


#' Plot fitted posteriors
#'
#' @param fitobj List. Object as returned by \code{fit()}.
#' @param ci (Optional) Numeric. Width (between 0 and 1) of the credible interval to plot. Default to 0.95.
#' @param true.values (Optional) List of "true" model parameters (the model that was used 
#' to generate the synthetic data) as returned by 
#' the function \code{example_model_prms()}. Default = \code{NULL}.
#'
#' @returns A list of ggplot objects. One element per fitted parameters. 
#' @export
#'
#' @examples
#' # TO DO
#' 
plot_fit_post <- function(fitobj, ci = 0.95,
                          true.values = NULL) {
  
  # true.values = prms0
  
  post = fitobj$post
  g = list() ; i=1
  
  for(a in names(post)){
    message(a)
    if(a %in% c('h.prop', 'odds.testpos')) 
      g[[i]] = plot_fit_post_vec(a, post,ci, true.values)
    if(a %in% c('R')) 
      g[[i]] = plot_fit_post_matrix(a, post,ci, true.values)
    i = i+1
  }
  names(g) <- names(post)
  return(g)
}


helper_summstat_traj <- function(s, varname, ci) {
  res = s |> 
    dplyr::group_by(date) |> 
    dplyr::summarise(
      m   = dplyr::across(starts_with(varname), mean),
      qlo = dplyr::across(starts_with(varname), \(x) quantile(x,probs = 0.5 - ci/2)),
      qhi = dplyr::across(starts_with(varname), \(x) quantile(x,probs = 0.5 + ci/2)),
      .groups = 'drop'
    ) |> 
    dplyr::ungroup() |>
    tidyr::unnest_wider(col = matches('^[mq]'), names_sep = '_') |>
    tidyr::pivot_longer(-date) |>
    tidyr::separate(col = name, into = c('stat', 'var', 'ag'))|>
    as.data.frame()
  
  return(res)
}


#' Plot fitted trajectories
#'
#' @param fitobj List as returned by the function \code{fit()}.
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
  
  s = fitobj$simpost
  
  ss = list()
  
  if('testpos' %in% duf){
    ss[['testpos']] = helper_summstat_traj(s, 'testpos', ci) 
  }
  if('hospadm' %in% duf){
    ss[['hosadm']] = helper_summstat_traj(s, 'hospadm', ci)
  }
  ssall = dplyr::bind_rows(ss) |> 
    dplyr::mutate(type = 'fit', 
                  source = paste(var, ag, sep='_'))
  
  colag = get_colors_age_groups(nag = max(ssall$ag))
  
  g = ssall |>
    tidyr::pivot_wider(names_from = 'stat') |>
    ggplot2::ggplot(ggplot2::aes(x=date, color = ag, fill = ag))+
    ggplot2::facet_wrap( ~ source, scales = 'free') +
    ggplot2::geom_point(data = obs, ggplot2::aes(y = value),
                        color = 'black')+
    ggplot2::geom_ribbon(ggplot2::aes(ymin = qlo, ymax = qhi), 
                         alpha = 0.1, linewidth = 0.1)+
    ggplot2::geom_line(ggplot2::aes(y=m), linewidth = 1) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid = ggplot2::element_line(color = 'grey97'),
      strip.background = ggplot2::element_rect(fill = 'indianred4'),
      strip.text = ggplot2::element_text(color = 'white', face = 'bold'))+
    ggplot2::labs(title = 'Fitted posterior trajectories') + 
    ggplot2::guides(color = 'none', fill = 'none') + 
    ggplot2::scale_color_manual(values = colag)+
    ggplot2::scale_fill_manual(values = colag)
  #g
  return(g)
}


#' Plot fitted model errors.
#'
#' @param fitobj Object as returned by \code{fit()}.
#'
#' @returns list of ggplot objects
#' @export
#'
#' @examples
#' 
plot_fit_errors <- function(fitobj) {
  
  tot = fitobj$errorsTotal |> 
    dplyr::mutate(idx2 = dplyr::row_number())
  nprior = nrow(tot)
  
  pf = fitobj$prms.fit
  npost = pf$p.accept * pf$n.priors
  
  # Total errors
  
  errmax = tot$err.total[tot$idx2 == npost]
  
  g.tot = tot |> 
    ggplot2::ggplot(ggplot2::aes(x=idx2, y = err.total))+
    ggplot2::geom_vline(xintercept = npost, linetype = 'dashed')+
    ggplot2::geom_step()+
    ggplot2::annotate(geom = 'label',
                      x=0.8*npost, y = errmax,
                      label = round(errmax,4),vjust=0, hjust=1)+
    ggplot2::theme_bw()+
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank()
    ) + 
    ggplot2::scale_y_log10()+
    ggplot2::scale_x_log10() + 
    ggplot2::labs(
      title = 'Total Error',
      subtitle = paste('priors:',nprior, 
                       '; post:', npost,
                       '; accept ratio:', 
                       format(npost/nprior, 
                              scientific = TRUE, digits=2)),
      x='iterations', y = 'total error')
  g.tot
  
  # Errors by fitted variables
  
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
          ggplot2::ggplot(ggplot2::aes(x=.data[[nam[i] ]], 
                                       y=.data[[nam[j] ]]))+
          ggplot2::geom_abline(intercept = 0, slope = 1)+
          ggplot2::scale_x_log10()+
          ggplot2::scale_y_log10()+
          # geom_point(alpha = 0.3)+
          ggplot2::geom_density_2d_filled(alpha = 0.7 ) + 
          ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) +
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

