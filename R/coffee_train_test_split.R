#' Train/Test Split Data in the COFFEE procedure
#'
#' @param TS_data Time Series data (possibly containing data from empirical_growth_rates function)
#' @param num_train Number of training data observations desired
#' @param num_test Number of testing data observations desired
#'
#' @return
#' A list containing
#' \item{TS_train_data}{A dataframe of 'num_train' rows. If TS_data has N rows, this dataframe are rows (N - num_test - num_train + 1):(N - num_test) of TS_data}
#' \item{TS_test_data}{A dataframe of 'num_test' rows. If TS_data has N rows, this dataframe are rows (N - num_test + 1):N}
#' @export
#'
#' @examples
#' data("jh_data_daily_confirm")
#' data("state_population")
#'
#' # Specify the count data to be days 1-550 of Pennsylvania's daily Covid-19 cases
#' count_data = jh_data_daily_confirm$Pennsylvania[1:550]
#'
#' # Specify the time data to be days 1-550
#' time_data = jh_data_daily_confirm$date[1:550]
#'
#' # Add a "day of week" effect to the model
#' by_factor = jh_data_daily_confirm$day_of_week[1:550]
#'
#' num_train = 28
#' num_test = 14
#' num_forecast = 14
#'
#' # Get Florida Population and use .55 as susceptible population proportion
#' population = state_population$Pennsylvania[1]
#' susc_perc = .55
#' total_cases = sum(jh_data_daily_confirm$Pennsylvania[1:550])
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
#' # Split data
#' split = coffee_train_test_split(TS_data = Penn_emp_grow_rate$data,
#'                                 num_train = num_train,
#'                                 num_test = num_test)
#'
#' TS_test_data = split$TS_test_data
#' TS_train_data = split$TS_train_data

coffee_train_test_split <- function(TS_data, num_train, num_test){
  if(nrow(TS_data) < (num_train + num_test)){
    stop("Not enough data is given to do a train/test split")
  }

  if(nrow(TS_data) == (num_train + num_test)){
    TS_train_data = TS_data[1:num_train, , drop = FALSE]
    TS_test_data = TS_data[-(1:num_train), , drop = FALSE]
  }

  if (nrow(TS_data) > (num_train + num_test )){
    last_day = nrow(TS_data)
    train_days = (last_day - (num_test + num_train - 1)):(last_day - num_test)
    test_days  = (last_day - (num_test - 1)):last_day

    TS_train_data = TS_data[train_days, , drop = FALSE]
    TS_test_data = TS_data[test_days, , drop = FALSE]
  }


  return(list(TS_train_data = TS_train_data, TS_test_data = TS_test_data))
}
