
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
  
 
  data.long = readRDS('ab-rsv.rds') |> 
    mutate(age_group = 'all')
  g.data = amrem::plot_data_long(data.long)
  
  dictionary = data.frame(
    amrem = c('testpos', 'hospadm'),
    name = c('positivity', 'hosp')
   )
      
  data = digest_long_data(data = data.long, dictionary = dictionary)
  
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
    data.used.fit = c('testpos', 'hospadm'),
    p.accept      = 1e-3,
    n.priors      = 5e4,
    priors.dist = list(
      R            = list(c('unif', 1.00, 2.3)),
      alpha        = list(c('unif', 0, 5)),
      S0           = list(c('unif', 2e6, 5e6)),
      odds.testpos = list(c('unif', 5, 90)),
      h.prop       = list(c('unif', 0.001, 0.1))  #  list(c('beta', 1 , 50))
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