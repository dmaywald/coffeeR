#' Accept-Reject Method with the Inverse Distance function.
#'
#' @param n Number of samples "(eta, omega, phi)" to draw from the pdf defined by the inverse distance function. Used in Kappa_Forecast function
#' @param TS_test_data Testing Time Series data. See example.
#' @param TS_train_data Training Time Series data. See example.
#' @param eta_bounds bounds for eta. Typically c(0,1)
#' @param omega_bounds bounds for omega. Typically c(1, 1.5*num_forecast)
#' @param phi_bounds bounds for phi. Typically of form (1 - a, 1 + a) for some 0 < a < 1.
#' @param max_draws Max number of draws for accept-reject method before a force exit.
#' @param accept_const (Optional). Accept-Reject constant. If left NULL, an accept-reject constant will be calculated.
#'
#' @return A n x 3 matrix of random samples drawn in accordance to the pdf defined by the inverse distance function.
#' @export
#'
#' @examples
#'
#' # Example dataset
#' data("jh_data_daily_confirm")
#' data("state_population")
#'
#' # Specify the count data to be days 800-900 of Rhode Island's daily Covid-19 cases
#' count_data = jh_data_daily_confirm$`Rhode Island`[800:900]
#'
#' # Specify the time data to be days 800-900
#' time_data = jh_data_daily_confirm$date[800:900]
#'
#' # Add a "day of week" effect to the model
#' by_factor = jh_data_daily_confirm$day_of_week[800:900]
#'
#' num_train_days = 28
#' num_test_days = 14
#'
#' # Get Rhode Island Population and use .55 as susceptible population proportion
#' population = state_population$Rhode.Island[1]
#' susc_perc = .55
#'
#' R_I_emp_grow_rate = empirical_growth_rates(count_data = count_data,
#'                                            time_data = time_data,
#'                                            num_train = num_train_days,
#'                                            num_test = num_test_days,
#'                                            population = population,
#'                                            susc_perc = susc_perc,
#'                                            by_factor = by_factor)
#'
#' TS_train_data = R_I_emp_grow_rate$data[1:num_train_days, ]
#' TS_test_data = R_I_emp_grow_rate$data[-(1:num_train_days), ]
#'
#' eta_bounds = c(0,1)
#' omega_bounds = c(1, 5)
#' phi_bounds = c(.5,1.5)
#'
#' # It is recommended that the function be run in parallel if multiple draws are needed.
#' # This is a small example.
#' x = sample_inv_dist(1, TS_test_data = TS_test_data, TS_train_data = TS_train_data,
#'                     eta_bounds = eta_bounds, omega_bounds = omega_bounds, phi_bounds = phi_bounds)
sample_inv_dist <- function(n, TS_test_data, TS_train_data, eta_bounds, omega_bounds, phi_bounds,
                            max_draws = 1e4, accept_const = NULL){
  # use accept-reject method to sample from inverse distance function

  num_test = nrow(TS_test_data)
  num_train = nrow(TS_train_data)

  # make sure eta/omega/phi_bounds make sense

  if(eta_bounds[1] >= eta_bounds[2] | length(eta_bounds) > 2){
    stop("eta_bounds needs to be a vector of length 2 with first argument strictly less than the second argument.")
  }

  if(omega_bounds[1] >= omega_bounds[2] | length(omega_bounds) > 2){
    stop("omega_bounds needs to be a vector of length 2 with first argument strictly less than the second argument.")
  }

  if(phi_bounds[1] >= phi_bounds[2] | length(phi_bounds) > 2){
    stop("phi_bounds needs to be a vector of length 2 with first argument strictly less than the second argument.")
  }

  kappa_const_dow = TS_test_data$kappa_const + TS_test_data$kappa_dow

  eta_coeff = stats::median(TS_train_data$kappa_star[(num_train - 6):num_train])

  # if no accept constant is given, calculate one
  if(is.null(accept_const)){
    accept_const = calc_accept_const(eta_bounds, omega_bounds, phi_bounds, N = 25000, TS_test_data, kappa_const_dow, eta_coeff, num_test)
  }

  z <- matrix(nrow = n, ncol = 3)

  itter = 0

  eta_low = eta_bounds[1]
  eta_high = eta_bounds[2]

  omega_low = omega_bounds[1]
  omega_high = omega_bounds[2]

  phi_low = phi_bounds[1]
  phi_high = phi_bounds[2]

  for(i in 1:n){
  # Accept Reject algorithm:
  # 1. Draw a candidate z from h(x) and draw u from Unif(0,1)
  # 2. If u <= inv_dist(z)/(accept_const*h(z)) return z
  # 3. Otherwise return to step 1.
  stopp = FALSE

  while(!stopp){
    z_val = c(runif(1, eta_low, eta_high),
              runif(1, omega_low, omega_high),
              runif(1, phi_low, phi_high))
    # print(eta_bounds)
    # print(omega_bounds)
    # print(phi_bounds)
    gen_val = generate_z_single_vector(z_val, eta_bounds, omega_bounds, phi_bounds)
    # print(inv_dist(z_val, TS_test_data, kappa_const_dow, eta_coeff, num_test))
    # print(accept_const)
    # print(gen_val)
    if(runif(1,0,1) <= inv_dist(z_val, TS_test_data, kappa_const_dow, eta_coeff, num_test) / (accept_const * gen_val)){
      z[i, ] = z_val
      stopp = TRUE
    }
    itter = itter + 1

    if (itter > n*max_draws){
      stopp = TRUE
    }
    }
  }
  return(z)
}
