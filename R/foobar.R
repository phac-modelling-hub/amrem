
###
###  File for testing and debugging
###

if(FALSE){
  
  library(tidyr)
  library(dplyr)
  library(ggplot2) ; theme_set(theme_bw())
  library(lubridate)
  library(stringr)
  library(patchwork)
  
  library(amrem)
  # devtools::load_all()
  packageVersion('amrem')

  set.seed(12345) 
  
  
  # ---- One Age Group ----
  
 
  # --- Data
  
  data.long = readRDS('ab-rsv.rds') |> 
    mutate(age_group = 'all')
  
  dictionary = data.frame(
    amrem = c('testpos', 'hospadm'),
    name = c('positivity', 'hosp')
   )
      
  data = digest_long_data(data = data.long, dictionary = dictionary)
  
  ww = readRDS('ww.rds')
  ww.rsv.ab = ww |> 
    filter(between(date, ymd('2025-07-01'), ymd('2026-06-01')),  
           pathogen == 'rsv-all', 
           grepl('Calgary', site_name)) |> 
    group_by(date) |> 
    summarise(value = mean(concadj.mle, na.rm = TRUE)) |>
    filter(!is.nan(value))
  data[['ww_1']] = ww.rsv.ab
  
  g.data = amrem::plot_data_list(data)
  
  # --- Model parameters 
  
  model.prms = example_model_prms_ag(
    r0 = 1.7,
    N = 5e6, 
    S0.prop = 0.75,
    h.prop = 0.007, 
    odds.testpos = 20)
  
  model.prms$date.start = ymd('2025-07-01')
  model.prms$horizon = 360
  
  # Starting point model to feed fit
  obj = amrem::create(model.prms)
  sim = amrem::simulate(obj)
  plot_timeseries(sim) / g.data
  nag = amrem::get_nag(obj)

  prms.fit = list(
    data.used.fit = c('testpos', 'hospadm', 'ww'),
    p.accept      = 2e-3,
    n.priors      = 3e4,
    ranked.err    = TRUE,
    priors.dist = list(
      fec.scale    = list(c('exp', 200)),
      R            = list(c('unif', 1.00, 3.5)),
      alpha        = list(c('unif', 0, 5)),
      S0.prop      = list(c('unif', 0.3, 0.99)),
      odds.testpos = list(c('unif', 5, 90)),
      h.prop       = list(c('unif', 0.001, 0.3))  #  list(c('beta', 1 , 50))
    ),
    n.cores = 1
  )
 
  system.time({ 
    fitobj = fit(obj, 
                 prms.fit = prms.fit, 
                 data = data)  
  })
  ci = 0.90
  g.fit.traj = plot_fit_traj(fitobj = fitobj, ci = ci)
  # g.fit.traj
  
  gfp = plot_fit_post(fitobj = fitobj, ci = ci)
  
  g.fit.post = patchwork::wrap_plots(gfp, nrow = 2)
  
  tmp2d = plot_fit_post_2d(fitobj)
  g.fit.2d = patchwork::wrap_plots(tmp2d)
  
  gerr = plot_fit_errors(fitobj)
  g.fit.err = patchwork::wrap_plots(gerr)
  
  fname = paste0('amrem-plot-fit-',reem::timestamp_short(), '.pdf')
  
  message('saving plot...')
  pdf(fname, width=9*nag, height = 6*nag)
  plot(g.fit.traj)
  plot(g.fit.post)
  plot(g.fit.2d)
  plot(g.fit.err)
  dev.off()
  
  
}
