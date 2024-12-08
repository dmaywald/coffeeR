#' Calculate Acceptance Constant used in sample_inv_dist
#'
#' @param eta_bounds Bounds for random variable eta. Typically c(0,1)
#' @param omega_bounds Bounds for random variable omega. Typically c(1,T) for T = number of future values to predict.
#' @param phi_bounds Bounds for random variable phi. Typically of form (1 - a, 1 + a) for some 0 < a < 1.
#' @param N (Optional) Number of uniformly random values to sample in the bounds of eta/omega/phi to estimate the maximum value of the inverse distance function
#' @param TS_data Dataframe from empirical_growth_rates function. Should only be rows corresponding to last 'num_test' entries
#' @param kappa_const_dow Kappa Const+DOW described by equation 14 in https://arxiv.org/pdf/2110.01546
#' @param eta_coeff eta* value described by equation 15 in https://arxiv.org/pdf/2110.01546
#' @param num_test Number of testing values.
#'
#' @return
#' Estimated accept constant used Accept-Reject method within sample_inv_dist.
#' @export
#'
#' @examples
#' #'# Example dataset
#' data("jh_data_daily_confirm")
#' data("state_population")
#'
#' # Specify the count data to be days 1-550 of Pennsylvania's daily Covid-19 cases
#' count_data = jh_data_daily_confirm$Pennsylvania[1:550]
#'
#' # Specify the time data to be days 1-550
#' time_data = as.numeric(jh_data_daily_confirm$date[1:550])
#'
#' # Add a "day of week" effect to the model
#' by_factor = jh_data_daily_confirm$day_of_week[1:550]
#'
#' TS_data = adjust_outliers(count_data, time_data, by_factor, cooks_constant = 4, return_plot = FALSE)
#'
#' count_data = TS_data$data$adjusted_data
#'
#' num_train = 28
#' num_test = 14
#' num_forecast = 14
#'
#' # Get Pennsylvania Population and use .55 as susceptible population proportion
#' population = state_population$Pennsylvania[1]
#' susc_perc = .55
#'
#' # Empirical Growth Rate (EGR) and EGR Model is needed to calculate
#' # random vectors sampled from the empirical growth rate model
#' Penn_emp_grow_rate = empirical_growth_rates(count_data = count_data,
#'                                             time_data = time_data,
#'                                             num_train = num_train,
#'                                             num_test = num_test,
#'                                             population = population,
#'                                             susc_perc = susc_perc,
#'                                             by_factor = by_factor)
#'
#' TS_train_data = Penn_emp_grow_rate$data[1:num_train, ]
#' TS_test_data = Penn_emp_grow_rate$data[-(1:num_train), ]
#'
#' eta_bounds = c(0,1)
#' omega_bounds = c(1, num_forecast)
#' phi_bounds = c(.9,1.1)
#'
#' kappa_const_dow = TS_test_data$kappa_const + TS_test_data$kappa_dow
#'
#' eta_coeff = stats::median(TS_train_data$kappa_star[(num_train - 6):num_train])
#'
#' accept_const = calc_accept_const(eta_bounds = eta_bounds, omega_bounds = omega_bounds,
#'                                  phi_bounds = phi_bounds, N = 50000,
#'                                  TS_data = TS_test_data,
#'                                  kappa_const_dow = kappa_const_dow,
#'                                  eta_coeff = eta_coeff, num_test = num_test)

calc_accept_const <- function(eta_bounds, omega_bounds, phi_bounds, N = 50000, TS_data, kappa_const_dow, eta_coeff, num_test){
  # Temporary dataframe to calcualate h over
  if (nrow(TS_data) != num_test){
    warning("nrow(TS_data) not same as num_test. This can cause unexpected results")
  }

  temp = data.frame(eta = stats::runif(N, eta_bounds[1], eta_bounds[2]),
                    omega = stats::runif(N, omega_bounds[1], omega_bounds[2]),
                    phi = stats::runif(N, phi_bounds[1], phi_bounds[2]))

  f_x = f_x = sapply(1:N, function(num){inv_dist(x = c(temp[num,1], temp[num,2], temp[num,3]), TS_data$kappa_star, TS_data$kappa_orig, kappa_const_dow, eta_coeff, num_test)})
  # Accptance constant is going to be the maximal value over the N data points of
  # inv_dist(x)/h(x)
  accept_const = max(f_x/generate_z_dataframe(temp, eta_bounds, omega_bounds, phi_bounds))

  return(accept_const)
}
