#include <Rcpp.h>
#include <iomanip>  // for std::setprecision

using namespace Rcpp;



//' Simulate an epidemic model
//'
//' Runs a simulation using specified parameters.
//'
//' @param prms A list of parameters including N, S0, horizon, alpha, g, i0, R.
//' @return A data frame with incidence and susceptible counts over time.
//' @examples
//' simulate_c(list(N = c(1000, 1000), S0 = c(990, 990), horizon = 50,
//'                    alpha = 0.1, g = c(0.5, 0.5), i0 = matrix(1, nrow = 2, ncol = 2),
//'                    R = matrix(c(1, 0.5, 0.5, 1), nrow = 2)))
// [[Rcpp::export]]
DataFrame simulate_c(List prms) {
  // Extract parameters
  NumericVector N = prms["N"];
  NumericVector S0 = prms["S0"];
  int horizon = prms["horizon"];
  double alpha = prms["alpha"];
  NumericVector g = prms["g"];
  NumericMatrix i0 = prms["i0"];
  NumericMatrix R = prms["R"];
  
  double g_norm = sum(g);
  int L = g.size();
  int A = R.ncol(); // number of age groups
  double exp_alpha = exp(alpha);
  
  // Initialize matrices
  NumericMatrix inc(horizon, A);
  NumericMatrix S(horizon, A);
  NumericMatrix Nmat(horizon, A);
  
  for (int t = 0; t < horizon; t++) {
    for (int a = 0; a < A; a++) {
      Nmat(t, a) = N[a];
    }
  }
  
  // Fill initial values
  for (int t = 0; t < L; t++) {
    for (int a = 0; a < A; a++) {
      inc(t, a) = i0(t, a);
      S(t, a) = S0[a];
    }
  }
  
  // --- Simulation loop
  
  for (int t = L; t < horizon; t++) {
    
    // Compute K for previous time step
    NumericMatrix K(horizon, A);
    for (int i = 0; i < horizon; i++) {
      for (int a = 0; a < A; a++) {
        K(i, a) = pow(S(i, a) / Nmat(i, a), exp_alpha) / g_norm;
      }
    }
    
    // Trailing incidence J
    NumericMatrix J(L, A);
    for (int i = 0; i < L; i++) {
      for (int a = 0; a < A; a++) {
        J(i, a) = inc(t - 1 - i, a);
      }
    }
    
    // Compute rj = R %*% t(J)
    NumericMatrix rj(A, L);
    for (int i = 0; i < A; i++) {
      for (int j = 0; j < L; j++) {
        double sum_val = 0.0;
        for (int k = 0; k < A; k++) {
          sum_val += R(i, k) * J(j, k);
        }
        rj(i, j) = sum_val;
      }
    }
 
    // Compute incidence for each age group
    for (int a = 0; a < A; a++) {
      double tmp = 0.0;
      for (int i = 0; i < L; i++) {
        tmp += g[i] * rj(a,i);
      }
      // Warning: do NOT round `inc(t,a)` below,
      // this introduces numerical instabilities (when R0 is close to 1)
      inc(t, a) = tmp * K(t - 1, a);
      S(t, a) = std::max(0.0, S(t - 1, a) - inc(t, a));
    }
  }
  
  // Prepare output DataFrame
  std::vector<std::string> inc_names, S_names;
  for (int a = 0; a < A; a++) {
    inc_names.push_back("inc_" + std::to_string(a + 1));
    S_names.push_back("S_" + std::to_string(a + 1));
  }
  
  List out;
  out.push_back(seq(1, horizon), "time");
  for (int a = 0; a < A; a++) {
    NumericVector inc_col(horizon), S_col(horizon);
    for (int t = 0; t < horizon; t++) {
      inc_col[t] = inc(t, a);
      S_col[t] = S(t, a);
    }
    out.push_back(inc_col, inc_names[a]);
    out.push_back(S_col, S_names[a]);
  }
  
  return DataFrame(out);
}



// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R

*/
