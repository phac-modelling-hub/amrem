
#' Create a Gamma distribution.
#'
#' @param mean Numeric. Mean of the distribution.
#' @param var Numeric. Variance of the distribution.
#' @param max Integer. Maximum support of the distribution.
#'
#' @returns Numerical vector. Normalized (sum = 1).
#' @export
#'
#' @examples dist_create(mean=3, var=1.2, max=5)
#' 
#' 
dist_create <- function(mean, var, max) {
  # mean = 3; var = 1 ; max = 5
  x = 1:max
  y = dgamma(x, shape = mean^2 / var, rate = mean/var) 
  return(y/sum(y))
}


#' Create a matrix of Gamma distributions.
#'
#' @param means Numeric matrix. Means of the distribution.
#' @param vars Numeric matrix. Variances of the distribution.
#' @param maxs Integer matrix. Maximums support of the distribution.
#'
#' @returns List of list of distribution. Element \code{[[i]][[j]]} is the distribution with mean \code{means[i,j]}.
#' @export
#'
#' @examples 
#'  means = matrix(c(2,3,2,2), ncol = 2)
#'  vars  = matrix(c(1.1, 1.8, 1.4, 0.9), ncol = 2)
#'  maxs  = matrix(c(5,5,5,6), ncol = 2)
#'  dist_create_matrix(means, vars, maxs)
#' 
dist_create_matrix <- function(means, vars, maxs) {
  
  if(0){ # DEBUG
    means = matrix(c(2,3,2,2), ncol = 2)
    vars  = matrix(c(1.1, 1.8, 1.4, 0.9), ncol = 2)
    maxs  = matrix(c(5,5,5,6), ncol = 2)
  }
  check_prms_dist_matrix(means, vars, maxs)
 print('DEBUG CHECK DONE') 
  d = list() 
 print('DEBUG 1') 
  for(i in 1:nrow(means)){
    print('DEBUG 2')
    d[[i]] = list()
    print('DEBUG 3')
    for(j in 1:ncol(means)){
      print(paste('i:',i))
      print(paste('j:',j))
      d[[i]][[j]] = dist_create(mean = means[i,j], 
                                var = vars[i,j],
                                max = maxs[i,j])
    }
  }
  return(d)
}





