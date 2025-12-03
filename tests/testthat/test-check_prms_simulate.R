test_that("check_prms_simulate() works", {
  expect_equal(2 * 2, 4)

  # Define parameters
  N = c(1e5, 3e5)
  prms = list(
    N = N,
    S0 = round(N * c(0.98, 0.70)),
    horizon = 100,
    alpha = 0,
    # Contact matrix R0
    R = 1.3 * rbind(
      c(1.5, 1.0),
      c(1.0, 1.0)),
    g = dist_create_matrix(
      means = matrix(rep(4,4)), 
      vars = matrix(rep(1,4)), 
      maxs = matrix(rep(9,4))),
    i0 = cbind(1:5, 2*(1:5)),# length(g)
    fec = c(0, 1, 1, 3, 9, 5, 2, 1)
  )  
  
  expect_no_error(check_prms_simulate(prms))

  # Modify `prms` to throw errors
  prms2 = prms
  
  prms2$horizon <- NULL
  expect_error(check_prms_simulate(prms2))

  prms2 = prms
  prms2$R = cbind(prms$R, prms$R[,1])  
  expect_error(check_prms_simulate(prms2))
  
})
