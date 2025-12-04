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
  

  N = c(1e5, 1e5)
  r0 = 1.3
  A = length(N)  # number of age groups
  ng = 7
  
  gi.means = matrix(c(4,4,
                      4,4), 
                    ncol=A, byrow = TRUE)
  gi.vars  = matrix(c(1,1,1,1), ncol=A)
  gi.maxs  = matrix(rep(ng, times = A*A), ncol=A)
  
  gi = amrem::dist_create_matrix(means = gi.means, 
                                 vars = gi.vars, 
                                 maxs = gi.maxs)
  L = length(gi[[1]][[1]])
  
  prms = list(
    N = N,
    S0 = N,
    horizon = 500,
    alpha = 0,
    # Contact matrix R0
    R = r0 * rbind(
      c(1.0, 0.0),
      c(0.0, 1.0)),
     fec = c(0, 1, 1, 3, 9, 5, 2, 1),
    odds.testpos = c(2,3),
    g = gi,
    i0 = cbind(1:L, N[2]/N[1]*(1:L))
  )

  obj = create(prms)
  sim = simulate(obj = obj, ww = F, hosp = F, testpos = F)  
  sim$Sall = sim$S_1 + sim$S_2
  
  fsize.sim  = 1 - sim$Sall[prms$horizon] / sum(N)
  fsize.theo = finalsize(r0)
  
  expect_equal(fsize.sim, expected = fsize.theo, tolerance = 0.1)  
  
  # now with a higher R0
  prms2 = prms
  mult = 2.0
  prms2$R = prms$R * mult
  
  obj2 = create(prms = prms2)
  sim = simulate(obj2, ww = F, hosp = F, testpos = F)  
  sim$Sall = sim$S_1 + sim$S_2
  
  fsize.sim  = 1 - sim$Sall[prms2$horizon] / sum(N)
  fsize.theo = finalsize(mult * r0)
  
  expect_equal(fsize.sim, expected = fsize.theo, tolerance = 0.1)  
  
})
