test_that("Match the final size formula in a simple case", {
  
  
  # Theoretical final size formula for simple SIR model.
  # No analytical formula, must solve:
  # 1 - x = exp(-R0.x)
  finalsize <- function(r0) {
    x = seq(0.01,1, by = 0.01)
    y = abs(exp(-r0 * x) + x - 1)
    ii = which.min(y)
    x[ii]
  }
  
  # Parameters mirror a simple SIR model
  # with a single age group.
  
  N = c(1e6, 1e6)
  r0 = 1.1
  
  prms = list(
    N = N,
    S0 = N,
    horizon = 500,
    alpha = 0,
    # Contact matrix R0
    R = r0 * rbind(
      c(1.0, 0.0),
      c(0.0, 1.0)),
    g = c(0.1, 0.2, 0.6, 0.3, 0.1),
    i0 = cbind(1:5, N[2]/N[1]*(1:5))  # length(g)
  )

  sim = simulate(prms)  
  sim$Sall = sim$S_1 + sim$S_2
  
  fsize.sim  = 1 - sim$Sall[prms$horizon] / sum(N)
  fsize.theo = finalsize(r0)
  
  expect_equal(fsize.sim, expected = fsize.theo, tolerance = 0.1)  
  
  # now with a higher R0
  prms2 = prms
  mult = 2.0
  prms2$R = prms$R * mult
  
  sim = simulate(prms2)  
  sim$Sall = sim$S_1 + sim$S_2
  
  fsize.sim  = 1 - sim$Sall[prms2$horizon] / sum(N)
  fsize.theo = finalsize(mult * r0)
  
  expect_equal(fsize.sim, expected = fsize.theo, tolerance = 0.1)  
  
})
