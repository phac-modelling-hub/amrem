#include <Rcpp.h>
#include <iomanip>  // for std::setprecision

using namespace Rcpp;



//' Simulate an epidemic model
//'
//' Runs a simulation using specified parameters.
//'
//' @param prms A list of parameters including N, S0, horizon, alpha, g, i0, R.
//' @param ww A boolean to switch on/off fecal shedding into wastewater.
//' @return A data frame with incidence and susceptible counts over time.
//' @examples
//' simulate_c(list(N = c(1000, 1000), S0 = c(990, 990), horizon = 50,
//'                    alpha = 0.1, g = c(0.5, 0.5), 
//'                    i0 = matrix(1, nrow = 2, ncol = 2),
//'                    R = matrix(c(1, 0.5, 0.5, 1), nrow = 2)))
// [[Rcpp::export]]
 DataFrame simulate_c(List prms, 
                      bool ww) {
   
   // Extract parameters
   NumericVector N    = prms["N"];
   NumericVector S0   = prms["S0"];
   int horizon        = prms["horizon"];
   double alpha       = prms["alpha"];
   List g             = prms["g"];
   NumericMatrix i0   = prms["i0"];
   NumericMatrix R    = prms["R"];
   NumericVector fec  = prms["fec"];
   
   // double g_norm = sum(g);
   
   // retrieve the support of all GI (they must be all the same)
   List foo = g[0];
   NumericVector foo2 = foo[0];
   int L = foo2.size();
   
   int M = fec.size();
   int A = R.ncol(); // number of age groups
   double exp_alpha = exp(alpha);
   
   
   // DEBUG
   // Rcout << "L = "<<L <<std::endl;
   // 
   //   Rcout << "R: "<<std::endl;
   //   for(int i=0; i<A; i++){
   //     for(int j=0; j<A; j++){
   //       Rcout << R(i,j) << " ";
   //     }
   //     Rcout << std::endl;
   //   }
   // -----
   
   
   // ==== INITIALIZATION ==== 
   
   NumericMatrix inc(horizon, A);
   NumericMatrix S(horizon, A);
   
   for (int t = 0; t < L; t++) {
     for (int a = 0; a < A; a++) {
       inc(t, a) = i0(t, a);
       S(t, a) = S0[a];
     }
   }
   
   // ==== TIME LOOP ====
   
   double Kta;
   
   for (int t = L; t < horizon; t++) 
   {
     // ----- DEBUG
     
     // Rcout << "--- t = "<< t << std::endl;
     
     // ------------------
     
     
     // Compute incidence for each age group
     for (int a = 0; a < A; a++) {
       
       double tmp = 0.0;
       List g_a = g[a]; 
       
       for(int b = 0; b < A; b++){
         NumericVector g_ab = g_a[b];
         
         for (int k = 0; k < L; k++) {
           tmp += R(a,b) * g_ab[k] * inc(t-k, b);
         }
         
         // Rcout << "a = "<< a << std::endl;
         // Rcout << "b = "<< b << std::endl;
         // Rcout << "g_a[b].size() = " << g_ba.size() <<std::endl;
         // Rcout << "g = ";
         // for(int z =0; z< L; z++) Rcout << g_ba[z] << ",";
         // Rcout << std::endl;
         
       } // loop "b"
       
       Kta = pow( S(t-1, a) / N[a], exp_alpha);
       // Warning: do NOT round `inc(t,a)` below,
       // this introduces numerical instabilities 
       // (when R0 is close to 1)
       inc(t, a) = tmp * Kta;
       S(t, a)   = std::max(0.0, S(t - 1, a) - inc(t, a));
     } // loop "a"
   } // loop "t"
   
   
   
   // === By-products ===
   
   // fecal shedding in wastewater
   
   NumericMatrix w(horizon, A);
   
   if(ww){
     for(int t=0; t < horizon; t++){
       for(int a = 0; a < A; a++){
         double tmp = 0.0;
         for(int k = 0; k < M; k++){
           if(t >= k){
             // Rcout <<"DEBUG k: "<< k <<std::endl;
             tmp += fec[k] * inc(t-k, a);
           }
         }
         w(t,a) = tmp;
       }
     }
   }
   
   // ==== ENDING ====
   
   // Prepare output DataFrame
   
   std::vector<std::string> inc_names;
   std::vector<std::string> S_names;
   std::vector<std::string> w_names;
   
   for (int a = 0; a < A; a++) {
     inc_names.push_back("inc_" + std::to_string(a + 1));
     S_names.push_back("S_" + std::to_string(a + 1));
     w_names.push_back("w_" + std::to_string(a + 1));
   }
   
   List out;
   out.push_back(seq(1, horizon), "time");
   
   for (int a = 0; a < A; a++) {
     NumericVector inc_col(horizon);
     NumericVector S_col(horizon);
     NumericVector w_col(horizon);
     
     for (int t = 0; t < horizon; t++) {
       inc_col[t] = inc(t, a);
       S_col[t]   = S(t, a);
       w_col[t]   = w(t, a);
     }
     out.push_back(inc_col, inc_names[a]);
     out.push_back(S_col, S_names[a]);
     out.push_back(w_col, w_names[a]);
   }
   
   return DataFrame(out);
 }
 
 
 
 
 
 
 
 
//' Simulate an epidemic model -- BACKUP
//'
//' Runs a simulation using specified parameters.
//'
//' @param prms A list of parameters including N, S0, horizon, alpha, g, i0, R.
//' @param ww A boolean to switch on/off fecal shedding into wastewater.
//' @return A data frame with incidence and susceptible counts over time.
//' @examples
//' simulate_c(list(N = c(1000, 1000), S0 = c(990, 990), horizon = 50,
//'                    alpha = 0.1, g = c(0.5, 0.5), i0 = matrix(1, nrow = 2, ncol = 2),
//'                    R = matrix(c(1, 0.5, 0.5, 1), nrow = 2)))
// [[Rcpp::export]]
 DataFrame simulate_c_backup(List prms, 
                             bool ww) {
   
   // Extract parameters
   NumericVector N    = prms["N"];
   NumericVector S0   = prms["S0"];
   int horizon        = prms["horizon"];
   double alpha       = prms["alpha"];
   NumericVector g    = prms["g"];
   NumericMatrix i0   = prms["i0"];
   NumericMatrix R    = prms["R"];
   NumericVector fec  = prms["fec"];
   
   double g_norm = sum(g);
   int L = g.size();
   int M = fec.size();
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
   
   // NumericMatrix K(horizon, A);
   double Kta;
   
   for (int t = L; t < horizon; t++) {
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
         for (int b = 0; b < A; b++) {
           sum_val += R(i, b) * J(j, b);
         }
         rj(i, j) = sum_val;
       }
     }
     
     // Compute incidence for each age group
     for (int a = 0; a < A; a++) {
       Kta = pow(S(t-1, a) / Nmat(t-1, a), exp_alpha) / g_norm;
       
       double tmp = 0.0;
       for (int i = 0; i < L; i++) {
         tmp += g[i] * rj(a,i);
       }
       // Warning: do NOT round `inc(t,a)` below,
       // this introduces numerical instabilities (when R0 is close to 1)
       inc(t, a) = tmp * Kta;
       S(t, a) = std::max(0.0, S(t - 1, a) - inc(t, a));
     }
   }
   
   // === By-products ===
   
   // fecal shedding in wastewater
   
   NumericMatrix w(horizon, A);
   
   if(ww){
     for(int t=0; t < horizon; t++){
       for(int a = 0; a < A; a++){
         double tmp = 0.0;
         for(int k = 0; k < M; k++){
           if(t >= k){
             tmp += fec[k] * inc(t-k, a);
           }
         }
         w(t,a) = tmp;
       }
     }
   }
   
   
   // Prepare output DataFrame
   
   std::vector<std::string> inc_names;
   std::vector<std::string> S_names;
   std::vector<std::string> w_names;
   
   for (int a = 0; a < A; a++) {
     inc_names.push_back("inc_" + std::to_string(a + 1));
     S_names.push_back("S_" + std::to_string(a + 1));
     w_names.push_back("w_" + std::to_string(a + 1));
   }
   
   List out;
   out.push_back(seq(1, horizon), "time");
   
   for (int a = 0; a < A; a++) {
     NumericVector inc_col(horizon);
     NumericVector S_col(horizon);
     NumericVector w_col(horizon);
     
     for (int t = 0; t < horizon; t++) {
       inc_col[t] = inc(t, a);
       S_col[t]   = S(t, a);
       w_col[t]   = w(t, a);
     }
     out.push_back(inc_col, inc_names[a]);
     out.push_back(S_col, S_names[a]);
     out.push_back(w_col, w_names[a]);
   }
   
   return DataFrame(out);
 }
 
 
 
 // You can include R code blocks in C++ files processed with sourceCpp
 // (useful for testing and development). The R code will be automatically 
 // run after the compilation.
 //
 
 /*** R
 
 */
 