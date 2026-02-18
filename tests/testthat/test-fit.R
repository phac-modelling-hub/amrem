test_that("fit works", {
  
  model.prms = example_model_prms()
  t.obs = 12*c(1:10)
  data = example_simulated_data(
    model.prms = model.prms, 
    t.obs = t.obs)
  
  # Parameters for the fitting algorithm
  n.prior = 500 # <-- small for a fast test
  n.post  = 20
  
  prms.to.fit   = c('R', 'odds.testpos', 'h.prop')
  
  prms.fit = list(
    prms.to.fit   = prms.to.fit,
    data.used.fit = c('testpos', 'hospadm'),
    p.accept      = n.post / n.prior,
    priors.dist = list(
      n.priors     = n.prior,  
      R            = c('unif', 0.1, 1.3),
      odds.testpos = c('unif', 0.9, 10),
      h.prop       = c('unif', 0.0, 0.08)
    ),
    n.cores = 1  #  <-- if >1 test fails bc of sfExportAll().
  )
  obj0 = create(prms = model.prms)
  # Starting point model to feed fit
  fitobj = fit(obj      = obj0, 
               prms.fit = prms.fit, 
               data     = data)  
     
  # --- Checks
  
  expect_true(is.list(fitobj))
  expect_true(length(fitobj) >= 8)
  expect_equal(length(fitobj$post$h.prop), n.post)
  
  expect_all_true(names(fitobj$post) == prms.to.fit)
  expect_all_true(names(fitobj$priors) == prms.to.fit)
  
  expect_true(is.data.frame(fitobj$simtraj))
  expect_true(is.data.frame(fitobj$simpost))
  expect_true(is.data.frame(fitobj$errors))
  expect_true(is.data.frame(fitobj$errorsTotal))
  
  expect_true(nrow(fitobj$simtraj) > 100)
  expect_true(nrow(fitobj$simpost) > 100)
  expect_true(nrow(fitobj$errors) > 100)
  expect_true(nrow(fitobj$errorsTotal) > 100)
})