test_that('forecast() works',{
  # devtools::load_all()
  # library(ggplot2)
  # library(dplyr)
  # library(tidyr)
  
  model.prms = example_model_prms()
  date.obs   = model.prms$date.start + 12*c(1:10)
  data       = example_simulated_data(model.prms = model.prms, 
                                      date.obs = date.obs)
  
  # Parameters for the fitting algorithm
  prms.fit = list(
    data.used.fit = c('testpos', 'hospadm'),
    p.accept      = 5e-2,
    n.priors     = 1e3,
    priors.dist = list(
      R            = list(c('unif', 0.1, 1.3)),
      odds.testpos = list(c('unif', 0.9, 10)),
      h.prop       = list(c('unif', 0.0, 0.08))
    ),
    n.cores = 1
  )
  
  # Starting point model to feed fit
  obj = amrem::create(model.prms)

  fitobj = amrem::fit(obj      = obj, 
                      prms.fit = prms.fit, 
                      data     = data)  
  
  # Forecasts
  
  prm.fcst = list(
    varnames = c('hospadm', 'testpos'),
    asof = as.Date.character('2026-05-15')
  )
  
  fcstobj = forecast(fitobj, 
                     prm.fcst = prm.fcst,
                     simulate = FALSE, 
                     silent = TRUE)
  
  expect_true(fcstobj$asof == prm.fcst$asof)
  expect_true(nrow(fcstobj$traj) > 100) 
  
  fcst.summ = summarize_forecast(fcstobj, ci = c(0.5, 0.9))
  
  expect_all_true( c('mean', 'quantile') %in% names(fcst.summ) )
  expect_true(nrow(fcst.summ$mean) > 100)
  expect_true(nrow(fcst.summ$quantile) > 100)

})