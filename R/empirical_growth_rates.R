#' Empirical Growth Rate Calculation
#'
#' @param count_data Response vector of Count Data
#' @param time_data Covariate vector of Time Data
#' @param num_train Constant. Represents the number of training data points.
#' @param num_test Constant. Represents the number of testing data points. Use num_train/num_test = 0 for model employed in coffee_forecast()
#' @param population Constant. Represents the total population.
#' @param susc_perc Constant between 0 and 1. Represents the percent of the population that is susceptible
#' @param by_factor (Optional) Additional categorical covariate
#'
#' @return
#' A list containing
#' \item{data}{A dataframe containing time series data empirical growth rate estimates}
#' \item{emp_grow_mod}{Model used to calculate empirical growth rate estimates}
#' @export
#'
#' @examples
#' # Example dataset
#' data("jh_data_daily_confirm")
#' data("state_population")
#'
#' # Specify the count data to be days 200-400 of Florida's daily Covid-19 cases
#' count_data = jh_data_daily_confirm$Florida[200:400]
#'
#' # Specify the time data to be days 200-400
#' time_data = jh_data_daily_confirm$date[200:400]
#'
#' # Add a "day of week" effect to the model
#' by_factor = jh_data_daily_confirm$day_of_week[200:400]
#'
#' num_train = 28
#' num_test = 0
#'
#' # Get Florida Population and use .55 as susceptible population proportion
#' population = state_population$Florida[1]
#' susc_perc = .55
#'
#' Florida_emp_grow_rate = empirical_growth_rates(count_data = count_data,
#'                                                time_data = time_data,
#'                                                num_train = num_train,
#'                                                num_test = num_test,
#'                                                population = population,
#'                                                susc_perc = susc_perc,
#'                                                by_factor = by_factor)
empirical_growth_rates <- function(count_data, time_data, num_train, num_test, population, susc_perc, by_factor = NULL){

  TS_data = make_ts_data(count_data, time_data, by_factor)

  if(num_train == 0 & num_test == 0){
    stop("Both num_train and num_test cannot be 0")
  }

  # If either supplied num_train/num_test are 0, then set num_train to be the non-zero option
  # and set num_test to be 0
  if(num_train == 0 | num_test == 0){
    num_train = max(num_train, num_test)
    num_test = 0
  }



  # Check to see if num_train and num_test makes sense
  if (num_train + num_test > length(count_data)){
    stop("Number of training and testing data points exceeds number of data points")
  }

  # If susc_perc is greater than 1, give warning.
  if (susc_perc > 1){
    warning("susc_perc is usually a number between 0 and 1. For example, 55% should be entered as .55")
  }


  last_day = nrow(TS_data)

  train_days = (last_day - (num_test + num_train - 1)):(last_day - num_test)

  test_days  = (last_day - (num_test - 1)):last_day
  train_data = TS_data[train_days, ]
  test_data  = TS_data[test_days, ]

  kappa_vals = kappa_calc(TS_data$count_data)

  TS_data$total_count = kappa_vals$total_count

  train_data$kappa_star = kappa_vals$kappa_star[train_days]
  train_data$kappa_orig = kappa_vals$kappa_orig[train_days]

  test_data$kappa_star = kappa_vals$kappa_star[test_days]
  test_data$kappa_orig = kappa_vals$kappa_orig[test_days]


  if(!is.null(by_factor)){
    # Use cook distances for weights of unweighted regression as weights for kappa_trend model
    k_trend_unweight_mod <- stats::lm(kappa_star ~ time_data + by_factor, data = train_data)
    cooks_weights = 1/stats::cooks.distance(k_trend_unweight_mod)

    # They use variable selection to (possibly) reduce some of the predictors above.

    # I will do stepwise regression for variable selection with AIC since we care more about
    # prediction over model simplification
    # Encode dummy variables:
    X_train        = data.frame(stats::model.matrix( ~ time_data + by_factor, data = train_data))
    names(X_train) = c("Intercept", "time_data", levels(train_data$by_factor)[-1]) # Rename columns
    y_train        = train_data$kappa_star

    X_test         = data.frame(stats::model.matrix( ~ time_data + by_factor, data = test_data))
    names(X_test)  = c("Intercept", "time_data", levels(test_data$by_factor)[-1]) # Rename columns
    y_test         = test_data$kappa_star
  }

  if(is.null(by_factor)){
    # Use cook distances for weights of unweighted regression as weights for kappa_trend model
    k_trend_unweight_mod <- stats::lm(kappa_star ~ time_data, data = train_data)
    cooks_weights = 1/stats::cooks.distance(k_trend_unweight_mod)

    # They use variable selection to (possibly) reduce some of the predictors above.

    # I will do stepwise regression for variable selection with AIC since we care more about
    # prediction over model simplification
    # Encode dummy variables:
    X_train        = data.frame(stats::model.matrix( ~ time_data, data = train_data))
    names(X_train) = c("Intercept", "time_data") # Rename columns
    y_train        = train_data$kappa_star

    X_test         = data.frame(stats::model.matrix( ~ time_data, data = test_data))
    names(X_test)  = c("Intercept", "time_data") # Rename columns
    y_test         = test_data$kappa_star
  }



  # Initial model object
  k_trend_mod <- stats::lm(y_train ~ .-1, data = X_train, weights = cooks_weights) # -1 Since i've already baked in the intercept
  full_coefs  = names(stats::coef(k_trend_mod))

  # Backward step wise
  k_trend_mod_back = MASS::stepAIC(k_trend_mod, direction = "backward", trace = FALSE)

  if (num_test <= 0) {
    pred_data = X_train
  } else {
    pred_data = rbind(X_train, X_test)
  }

  k_trend_mod_back_fits = stats::predict(k_trend_mod_back, pred_data)

  # From the final step above, select all of the remaining predictors.
  # "(weights)" is one of the predictors, don't select this one
  if("(weights)" %in% names(k_trend_mod_back$"model")[-1]){
    weight_predictor_back = which(names(k_trend_mod_back$"model")[-1] == "(weights)")
    keep_predictors_back = names(k_trend_mod_back$"model")[-1][-weight_predictor_back]
  } else {
    keep_predictors_back = names(k_trend_mod_back$"model")[-1]
  }

  if(nrow(TS_data) > 7){
    mean_train = mean(TS_data$count_data[(last_day - (num_test + 6)):(last_day - num_test)])
  } else {
    mean_train = mean(utils::tail(TS_data$count_data))
  }

  if (length(population) > 1) {
    stop("More than one population value found")
  }
  true_sucs_0 = susc_perc * population

  k_const_calc_0 <- function(t){
    temp = mean_train / (((true_sucs_0  - TS_data$total_count[t - 1]) / (true_sucs_0))*(TS_data$total_count[t - 1]))
    return(
      stats::qlogis(temp)
    )
  }

  if(num_test > 0){
    k_const_0 = sapply((last_day - (num_test - 1)):last_day, k_const_calc_0)
  } else {
    # If number of testing days is 0, calculate k_const_0 over training data
    k_const_0 = sapply((last_day - (num_train - 1)):last_day, k_const_calc_0)
  }

  if(num_test > 0){
    test_data$kappa_const = k_const_0
    test_data$kappa_trend = k_trend_mod_back_fits[
      (length(k_trend_mod_back_fits) - (num_test - 1)):length(k_trend_mod_back_fits)
    ]

    # Equation 14 requires an empirical growth rate approximation over the testing data with only
    # the days of week taken into affect. This will be done by setting the days and intercept column
    # of the testing data design matrix to 0 and predicting with the backward stepwise regression model

    if(!is.null(by_factor)){
      X_test_dow           = data.frame(stats::model.matrix(~ time_data + by_factor, data = test_data))
      names(X_test_dow)    = c("Intercept", "time_data", levels(train_data$by_factor)[-1]) # Rename columns
      X_test_dow$Intercept = 0
      X_test_dow$time_data = 0
      test_data$kappa_dow  = stats::predict(k_trend_mod_back, X_test_dow)
    } else {
      X_test_dow           = data.frame(stats::model.matrix(~ time_data, data = test_data))
      names(X_test_dow)    = c("Intercept", "time_data") # Rename columns
      X_test_dow$Intercept = 0
      X_test_dow$time_data = 0
      test_data$kappa_dow  = stats::predict(k_trend_mod_back, X_test_dow)
    }



    # Fill out (now missing) values in training data so we can rbind it with testing data
    # since we need last 7 days of training data in step 7
    train_data$kappa_const = 0
    train_data$kappa_trend = k_trend_mod_back_fits[1:num_train]
    train_data$kappa_dow   = 0

    data_out = rbind(train_data, test_data)

    # return data and training model
    return(list(data = data_out, emp_grow_mod = k_trend_mod_back))

  } else { # Repeat the process, but with the training data (if number of test data is 0)
    train_data$kappa_const = k_const_0
    train_data$kappa_trend = k_trend_mod_back_fits[1:num_train]

    if(!is.null(by_factor)){
      X_train_dow           = data.frame(stats::model.matrix(~ time_data + by_factor, data = train_data))
      names(X_train_dow)    = c("Intercept", "time_data", levels(train_data$by_factor)[-1]) # Rename columns
      X_train_dow$Intercept = 0
      X_train_dow$time_data = 0
      train_data$kappa_dow  = stats::predict(k_trend_mod_back, X_train_dow)

    } else {

      X_train_dow           = data.frame(stats::model.matrix(~ time_data, data = train_data))
      names(X_train_dow)    = c("Intercept", "time_data") # Rename columns
      X_train_dow$Intercept = 0
      X_train_dow$time_data = 0
      train_data$kappa_dow  = stats::predict(k_trend_mod_back, X_train_dow)
    }

    data_out = train_data

    # return data and empirical growth rate model
    return(list(data = data_out, emp_grow_mod = k_trend_mod_back))
  }
}
