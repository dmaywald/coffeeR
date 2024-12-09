#' Implement The COFFEE Procedure
#'
#' @param count_data Response vector of Count Data with (potential) outliers
#' @param time_data Covariate vector of Time Data
#' @param by_factor (Optional) Additional categorical covariate
#' @param pred_time_data Time data for future forecast values
#' @param pred_by_factor (Optional) by_factor data for future forecast values
#' @param population Total population (constant)
#' @param adjust_data (Optional) Adjust the given count data for outliers before doing COFFEE proceudure? Boolean TRUE/FALSE
#' @param num_train (Optional) Number of training data to take when constructing empirical growth rate models.
#' @param num_test (Optional) Number of testing data to take when constructing inverse distance function for random vector sampling.
#' @param total_cases (Optional) Total number of cases currently observed in the population (constant).
#'  If NULL, total_cases will be sum of count data which may not give intended results
#' @param susc_perc Constant between 0 and 1. Represents the percent of the population that is susceptible
#' @param random_vectors (Optional) Random samples of (eta, omega, phi) from inverse distance function. See sample_inv_dist.
#' If NULL, random vectors will be calculated (which may be time consuming).
#' @param num_random_vectors Number of samples "(eta, omega, phi)" to draw from the pdf defined by the inverse distance function.
#' @param eta_bounds (Optional) Bounds for random variable eta. Typically c(0,1)
#' @param omega_bounds (Optional) Bounds for random variable omega. Typically c(1,T) for T = number of future values to predict.
#' @param phi_bounds (Optional) Bounds for random variable phi. Typically of form (1 - a, 1 + a) for some 0 < a < 1.
#' @param accept_const (Optional). Accept-Reject constant. If left NULL, an accept-reject constant will be calculated.
#' @param max_draws Max number of draws for accept-reject method before a force exit.
#' @param attack_rate_bounds (Optional) Lower and Upper bounds to uniformly sample attack rate
#' @param cooks_constant (Optional) Specify the sensitivity to outliers. Lower values give higher sensitivity.
#' @param return_plot (Optional) Return ggplot visualization of forecast values
#'
#' @return
#' A list containing
#' \item{full_forecast_data}{For each random vector, a time series of future forecast values is calculated. These are
#'  saved row-wise in this matrix, along with the pred_time_data}
#' \item{simplified_data}{A dataframe of length(pred_time_data) containing pred_time_data,
#'  median of forecast count data, and 10/25/75/90 quantiles of forecast count data}
#' \item{plot}{(Optional) A ggplot2 object visualizing forecast data}
#' @export
#'
#'
#' @references
#' Castro (2020) COFFEE: COVID-19 Forecasts using Fast Evaluations and Estimation
#' \doi{10.48550/arXiv.2110.01546}.
#' @examples
#' # Example dataset
#' data("jh_data_daily_confirm")
#' data("state_population")
#'
#' num_train = 28
#' num_test = 14
#' num_forecast = 14
#' adjust_data = TRUE
#'
#' # Get New Mexico Population and use .55 as susceptible population proportion
#' population = state_population$New.Mexico[1]
#' susc_perc = .55
#'
#' # Specify the count data to be days 1-273 of New Mexico's daily Covid-19 cases
#' count_data = jh_data_daily_confirm$`New Mexico`[1:273]
#'
#' # Specify the time data to be days 1-273
#' time_data = jh_data_daily_confirm$date[1:273]
#'
#' # Add a "day of week" effect to the model
#' by_factor = jh_data_daily_confirm$day_of_week[1:273]
#'
#' # Get time data corresponding to forecastfuture values
#' first_pred_date = time_data[length(time_data)] + 1
#' last_pred_date = time_data[length(time_data)] + num_forecast
#'
#' pred_time_data = seq.Date(first_pred_date, last_pred_date, by = 'day')
#'
#' # Get the Day of week for each corresponding day
#' pred_by_factor = format(pred_time_data, '%a')
#'
#' data("new_mexico_random_vectors_example")
#' random_vector = new_mexico_random_vectors_example
#'
#' new_mexico_future_forecast = coffee(count_data = count_data, time_data = time_data,
#'                                     by_factor = by_factor, pred_time_data = pred_time_data,
#'                                     pred_by_factor = pred_by_factor, population = population,
#'                                     susc_perc = .55, random_vector = random_vector,
#'                                     adjust_data = TRUE, return_plot = TRUE)

coffee <- function(count_data, time_data, by_factor = NULL, pred_time_data, pred_by_factor = NULL, population,
                   adjust_data = TRUE, num_train = 28, num_test = 14, total_cases = NULL, susc_perc = .55,
                   random_vectors = NULL, num_random_vectors = 50, eta_bounds = c(0, 1),
                   omega_bounds = c(1, 14), phi_bounds = c(.9, 1.1), accept_const = NULL, max_draws = 1e4,
                   attack_rate_bounds = c(.4, .7), cooks_constant = 4, return_plot = TRUE){
  ## User checks ##
  if (!(cooks_constant > 0)) {
    stop("Bad value for cooks constant argument. Needs to be greater than 0")
  }

  if (!(num_train) > 0) {
    stop("Bad value for num_train. Needs to be greater than 0")
  }

  if (!(num_test) > 0) {
    stop("Bad value for num_train")
  }




  if (adjust_data) {
    # If adjust_data is TRUE, adjust the data
    adjusted_data = adjust_outliers(count_data = count_data, time_data = time_data, by_factor = by_factor)
    count_data = adjusted_data$data$adjusted_data
  }

  if(is.null(total_cases)){
    total_cases = sum(count_data)
  }


  # get initial empirical growth rate model/data
  emp_grow_rate_train = empirical_growth_rates(count_data = count_data,
                                               time_data = time_data,
                                               num_train = num_train,
                                               num_test = num_test,
                                               population = population,
                                               susc_perc = susc_perc,
                                               by_factor = by_factor)


  TS_train_data = emp_grow_rate_train$data[1:num_train, ]
  TS_test_data = emp_grow_rate_train$data[-(1:num_train), ]

  if(is.null(random_vectors)){
    if(is.null(num_random_vectors)){
      stop("If no random vectors are given, then num_random_vectors must be specified")
    }

    random_vectors = sample_inv_dist(n = num_random_vectors,
                                     TS_test_data = TS_test_data,
                                     TS_train_data = TS_train_data,
                                     eta_bounds = eta_bounds,
                                     omega_bounds = omega_bounds,
                                     phi_bounds = phi_bounds,
                                     max_draws = max_draws,
                                     accept_const = accept_const)
  }


  emp_grow_rate = empirical_growth_rates(count_data = count_data,
                                         time_data = time_data,
                                         num_train = num_train,
                                         num_test = 0,
                                         population = population,
                                         susc_perc = susc_perc,
                                         by_factor = by_factor)


  TS_data = emp_grow_rate$data
  emp_grow_model = emp_grow_rate$emp_grow_mod



  future_forecast = coffee_forecast(TS_data,
                                    emp_grow_model,
                                    population,
                                    total_cases,
                                    random_vectors,
                                    pred_time_data,
                                    pred_by_factor,
                                    attack_rate_bounds,
                                    return_plot)

  return(future_forecast)

}

