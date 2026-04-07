
#' Flatten data structure
#'
#' @param data List of dataframe having the same structure as 
#' the list of data passed as argument of the function \code{fit()}. 
#'
#' @returns A single, merged and long-format dataframe.
#' @export
#'
#' @examples
#' # Create data for example
#' data = list(
#' testpos_1 = data.frame(time = 1:5, value = runif(n=5)),
#' testpos_2 = data.frame(time = 1:5, value = runif(n=5)),
#' hospadm_1 = data.frame(time = 1:5, value = runif(n=5)),
#' hospadm_2 = data.frame(time = 1:5, value = runif(n=5)))
#'
#' flatten_data(data)
#' 
flatten_data <- function(data) {
  # data = fitobj$data
  res = dplyr::bind_rows(data, .id = 'source')
  return(res)
}

#' Create synthetic data from a simulated epidemic
#'
#' @param model.prms List. Model parameters as returned by the function \code{example_model_prms()}.
#' @param date.obs Vector of observation dates. 
#'
#' @returns List of data sets as expected by the function \code{amrem::fit()}.
#' @export
#'
#' @examples
#' 
#' model.prms = example_model_prms()
#' t.obs = 12*c(1:10)    # observation times
#' date.obs = model.prms$date.start + 12*c(1:10)    # observation dates
#' dat = example_simulated_data(model.prms, date.obs)
#' 
example_simulated_data <- function(model.prms, 
                                   date.obs) {
  
  # set up simulation to generate "observations"
  prms0 = model.prms
  nag   = length(prms0$N)
  obj0  = amrem::create(prms0)
  
  # Model that generates data
  simobs = simulate(obj0)
  
  # Clinical test positivity
  data.testpos1 = simobs |> 
    dplyr::filter(date %in% date.obs )    |> 
    dplyr::select(date,, value = testpos_1)
  data.testpos2 = simobs |> 
    dplyr::filter(date %in% date.obs )    |> 
    dplyr::select(date,, value = testpos_2)
  
  # Hospital admissions
  data.hosp1 = simobs |> 
    dplyr::filter(date %in% date.obs )    |> 
    dplyr::select(date,, value = hospadm_1)
  data.hosp2 = simobs |> 
    dplyr::filter(date %in% date.obs )    |> 
    dplyr::select(date,, value = hospadm_2)
  
  data = list(
    testpos_1 = data.testpos1,
    testpos_2 = data.testpos2,
    hospadm_1 = data.hosp1,
    hospadm_2 = data.hosp2
  )
  return(data)
}




example_simulated_data2 <- function(model.prms, 
                                   date.obs) {
  
  # set up simulation to generate "observations"
  prms0 = model.prms
  nag   = length(prms0$N)
  obj0  = amrem::create(prms0)
  
  # Model that generates data
  simobs = simulate(obj0)
  
  dtp = helper_extract_data(v = 'testpos', simobs)
  dha = helper_extract_data(v = 'hospadm', simobs)
  
  res = c(dtp, dha)
 
  return(res)
}


#' Data example, with long format.
#'
#' @param n Integer. Number of data points by varable and age group.
#'
#' @returns List of data frame as expected by \code{amrem}.
#' @export
#'
#' @examples
#' data = example_longformat_data()
#' 
#' 
example_longformat_data <- function(n = 12) {
  
  dd = as.Date('2026-01-01') + 7*(1:n)
  
  tmp = list()
  
  tmp[[1]] = data.frame(
    date = dd, 
    value = runif(n=n, (1:n)/100, (1:n)/30),
    variable = 'positivity',
    age_group = "0_4"
  ) 
  tmp[[2]] = tmp[[1]]
  tmp[[2]]$age_group = "19_64"
  tmp[[2]]$value = 2* tmp[[2]]$value  
  
  tmp[[3]] = tmp[[1]]
  tmp[[3]]$variable = 'hosp'
  tmp[[3]]$value = round(tmp[[1]]$value * 1e2)
  
  tmp[[4]] = tmp[[2]]
  tmp[[4]]$variable = 'hosp'
  tmp[[4]]$value = round(tmp[[2]]$value * 2e2)
  
  data = do.call(rbind, tmp)
  
  return(data)
  
  if(0){
  data |> ggplot(aes(x=date, y=value, color = age_group))+
    geom_line()+
    facet_wrap(~variable, scales= 'free_y')
  }
}


#' Transform a long-format dataframe  
#' into a list of dataframes. 
#'
#' @param data Dataframe in a long format.
#' @param dictionary Dataframe providing the link
#' between the variable names of the input data
#' and the standard ones expected by \code{amrem}.
#'
#' @returns List of data frames. One element for each 
#' variable and age group (e.g., \code{testpos_1}, 
#' \code{hospadm_2}, etc.) 
#' @export
#'
#' @examples
#'  data = example_longformat_data()
#'  dictionary = data.frame(
#' amrem = c('testpos', 'hosp'),
#' name = c('positivity', 'hosp') )
#' 
#' 
#' 
digest_long_data <- function(data, dictionary) {
  
  if(0){
    data = example_longformat_data()
    dictionary = data.frame(
      amrem = c('testpos', 'hosp'),
      name = c('positivity', 'hosp')
    )
  }
  
  vu = unique(data$variable)
  res = list() 
  for(i in seq_along(vu)){
    q = data[data$variable == vu[i],]
    
    amrem.var = dictionary$amrem[dictionary$name == vu[i]]
    
    agu = unique(q$age_group)
    for(j in seq_along(agu)){
      z = q[q$age_group == agu[j],]
      # z
      nam = paste(amrem.var, j, sep = '_')
      res[[nam]] = z[,c('date', 'value')]
    }
  }  
  return(res)
}






