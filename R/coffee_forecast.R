#' Future Forecast using the COFFEE method
#'
#' @param TS_data Time Series data from empirical_growth_rates function
#' @param emp_grow_model Empirical growth model from empirical_growth_rates function
#' @param population Total population (constant)
#' @param total_cases Total number of cases currently observed in the population (constant)
#' @param random_vectors random samples of (eta, omega, phi) from inverse distance function. See sample_inv_dist.
#' @param pred_time_data time data for future forecast values
#' @param pred_by_factor (Optional) by_factor data for future forecast values
#' @param attack_rate_bounds (Optional) Lower and Upper bounds to uniformly sample attack rate
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
#' @examples

#' # Example dataset
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
#' num_forecast = 7
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
#' TS_train_data = Penn_emp_grow_rate$data[1:num_train, ]
#' TS_test_data = Penn_emp_grow_rate$data[-(1:num_train), ]
#'
#' eta_bounds = c(0,1)
#' omega_bounds = c(1, 10)
#' phi_bounds = c(.5,1.5)
#'
#' random_vectors = sample_inv_dist(n = 20,
#'                                  TS_test_data = TS_test_data,
#'                                  TS_train_data = TS_train_data,
#'                                  eta_bounds = eta_bounds,
#'                                  omega_bounds = omega_bounds,
#'                                  phi_bounds = phi_bounds)
#'
#'
#' # Empirical Growth Rate (EGR) and EGR Model is needed to recalculated
#' # with 0 testing days in order to retrieve the model used for future forecast
#' Penn_emp_grow_rate = empirical_growth_rates(count_data = count_data,
#'                                             time_data = time_data,
#'                                             num_train = num_train,
#'                                             num_test = 0,
#'                                             population = population,
#'                                             susc_perc = susc_perc,
#'                                             by_factor = by_factor)
#'
#' emp_grow_model = Penn_emp_grow_rate$emp_grow_mod
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
#' TS_data = Penn_emp_grow_rate$data
#'
#' attack_rate_bounds = c(.4, .7)
#'
#' return_plot = TRUE
#'
#' Penn_forecast = coffee_forecast(TS_data, emp_grow_model, population, total_cases, random_vectors,
#'                                 pred_time_data, pred_by_factor, attack_rate_bounds, return_plot)
coffee_forecast <- function(TS_data, emp_grow_model, population, total_cases, random_vectors,
                            pred_time_data, pred_by_factor = NULL,
                            attack_rate_bounds = c(.4,7), return_plot = FALSE){
  #########################
  # Make future forecasts

  # If more than 14 of the last 28 days had zero reported cases, take independent and
  # identically distributed (iid) samples of future reported cases from the empirical distribution
  # of outlier adjusted reported cases over the last 28 days.
  num_zero_reports = sum(TS_data$count_data == 0)
  num_forecast = length(pred_time_data)


  if(num_zero_reports > 14){
    if(num_zero_reports == 28){
      future_cases = stats::rbinom(n = num_forecast, size = 1, prob = 1 / 29)
      warning("Not enough non-zero cound data found in TS_data.")
      return(future_cases)
    } else {
      future_cases = sample(TS_data$count_data, size = num_forecast, replace = T)
      warning("Not enough non-zero cound data found in TS_data.")
      return(future_cases)
    }
  } else {
    # Make forecast data
    pred_data = data.frame(stats::model.matrix( ~ pred_time_data + pred_by_factor))
    names(pred_data)  = c("Intercept", "time_data", levels(TS_data$by_factor)[-1]) # Rename columns


    kappa_trend = stats::predict(emp_grow_model, pred_data)

    # Do Step 6 to compute k_const_dow replacing T_train with last_day in equations 11 and 12
    # Mean confirmed daily cases over the last week of the training data
    confirmed_mean_train = mean(TS_data$count_data[(nrow(TS_data)-6):nrow(TS_data)])


    k_const = rep(dplyr::last(TS_data$kappa_const), num_forecast)

    k_dow           = stats::predict(emp_grow_model, pred_data %>% dplyr::mutate(Intercept = 0, time_data = 0))
    kappa_const_dow = k_const + k_dow

    # Compute k_forecast following equation 13
    # Define eta_star function of equation 15
    eta_coeff = stats::median(TS_data$kappa_star[(nrow(TS_data)-6):nrow(TS_data)])

    if(any(is.na(random_vectors))){
      num_na = length(which(is.na(random_vectors$eta)))
      warning(paste("Missing Random Vectors found. Remvoing", num_na, "missing values"))

      random_vectors = random_vectors[-which(is.na(random_vectors$eta)),]
    }

    num_random_vector = nrow(random_vectors)

    # Give warning if num_random_vector doesn't seem large enough

    if (nrow(random_vectors) < length(pred_time_data)){
      warning("Forecasts made by COFFEE method depends on having an adequeate amount of random samples from the inverse distance function. \
    Increase number of random vectors to (at least) match the number of future Forecasts")
    }

    kappa_forecasts = sapply(1:num_random_vector, \(idx){kappa_forecast_t(t = 1:num_forecast,
                                                                          eta = random_vectors$eta[idx],
                                                                          omega = random_vectors$omega[idx],
                                                                          phi = random_vectors$phi[idx],
                                                                          eta_coeff = eta_coeff,
                                                                          kappa_star = TS_data$kappa_star,
                                                                          kappa_const_dow = kappa_const_dow)})

    # compute inverse logits
    expit_k_forecast = stats::plogis(kappa_forecasts)

    # draw an attack rate p ~ Unif(.4,.7) and set true_susc_0 = p*N, where N is state population
    attack_rates = stats::runif(num_random_vector, attack_rate_bounds[1], attack_rate_bounds[2])

    # Predefine vectors for storing intitial values of time stepping scheme
    true_susc_0_for = rep(NA, num_random_vector)
    true_tot_conf_T = rep(NA, num_random_vector)
    true_susc_T     = rep(NA, num_random_vector)

    # Predefine matrix for storing 'true' daily/total/susceptible solutions
    true_daily_for = matrix(nrow = num_forecast, ncol =  num_random_vector)
    true_total_for = matrix(nrow = num_forecast, ncol =  num_random_vector)
    true_susc_for  = matrix(nrow = num_forecast, ncol =  num_random_vector)


    for(i in 1:num_random_vector){
      true_susc_0_for[i] = attack_rates[i]*population
      true_tot_conf_T[i] = total_cases # Set 'true' total on last day to reported total
      true_susc_T[i]     = true_susc_0_for[i] - true_tot_conf_T[i]



      true_daily_for[1, i] = expit_k_forecast[1, i] * (true_susc_T[i] / true_susc_0_for[i]) * true_tot_conf_T[i]
      true_total_for[1, i] = true_tot_conf_T[i] + true_daily_for[1, i]
      true_susc_for[1, i]  = true_susc_T[i] - true_daily_for[1, i]


      for(t in 2:num_forecast){
        true_daily_for[t, i] =  expit_k_forecast[t, i] * (true_susc_for[(t - 1), i] / true_susc_0_for[i]) * true_total_for[(t - 1), i]
        true_total_for[t, i] = true_total_for[(t - 1), i] + true_daily_for[t, i]
        true_susc_for[t, i]  = true_susc_for[(t - 1), i] - true_daily_for[t, i]
      }
    }

    # Get range of values for true daily cases for plotting purposes.
    true_daily_low = rep(NA, num_forecast)
    true_daily_low = sapply(1:num_forecast, \(idx){true_daily_low[idx] = stats::quantile(true_daily_for[idx,],.25)})

    true_daily_low_low = rep(NA, num_forecast)
    true_daily_low_low = sapply(1:num_forecast, \(idx){true_daily_low_low[idx] = stats::quantile(true_daily_for[idx,],.1)})

    true_daily_high = rep(NA, num_forecast)
    true_daily_high = sapply(1:num_forecast, \(idx){true_daily_high[idx] = stats::quantile(true_daily_for[idx,],.75)})

    true_daily_high_high = rep(NA, num_forecast)
    true_daily_high_high = sapply(1:num_forecast, \(idx){true_daily_high_high[idx] = stats::quantile(true_daily_for[idx,],.9)})

    true_daily_mid = rep(NA, num_forecast)
    true_daily_mid = sapply(1:num_forecast, \(idx){true_daily_mid[idx] = stats::quantile(true_daily_for[idx,],.5)})

    true_daily_red = data.frame(forecast_time_data = pred_time_data,
                                forecast_count_median = true_daily_mid,
                                forecast_count_.1 = true_daily_low_low,
                                forecast_count_.25 = true_daily_low,
                                forecast_count_.75 = true_daily_high,
                                forecast_count_.9 = true_daily_high_high)

    new_row_names <- vector(mode = 'character', length = num_random_vector)

    for (i in 1:num_random_vector) {
      new_row_names[i] = paste("series_", as.character(i), sep = "")
    }

    data.out = rbind(pred_time_data, t(true_daily_for))
    rownames(data.out) <- c(rownames(data.out)[1], new_row_names)

    return_list <- list(full_forecast_data = data.out, simplified_data = true_daily_red)
    if(return_plot){

    reported_data = data.frame(time_data = TS_data$time_data,
                               count_data = TS_data$count_data,
                               by_factor = TS_data$by_factor)


    temp_df <- data.frame(matrix(nrow = num_forecast, ncol = ncol(reported_data)))
    colnames(temp_df) <- colnames(reported_data)
    reported_data = rbind(reported_data, temp_df)
    reported_data$time_data[-(1:nrow(TS_data))] = pred_time_data

    reported_data$by_factor[-(1:nrow(TS_data))] = pred_by_factor

    # Plot all the 'num_random_vector' many forecasts together
    reported_data$Type = c(rep("Past Data", nrow(TS_data)), rep("Future Data", num_forecast))

    data.new = cbind.data.frame(time_data = reported_data$time_data[reported_data$Type == "Future Data"], true_daily_for)
    data.new$Type = NULL
    data.new = reshape2::melt(data.new, id = 'time_data')
    data.new$eta = rep(random_vectors$eta, each = num_forecast)
    data.new$omega = rep(random_vectors$omega, each = num_forecast)
    data.new$phi = rep(random_vectors$phi, each = num_forecast)
    if(!is.null(random_vectors$prob)){
      data.new$prob = log(log(rep(random_vectors$prob,each = num_forecast))) # log log is taken for the sake of visualization
    }

    v_line_loc = .5*(as.numeric(reported_data$time_data[nrow(TS_data)]) + as.numeric(reported_data$time_data[nrow(TS_data) + 1]))

    # Plot the inner inner 80 percentile, 50 percentile and median
    g15 <- ggplot2::ggplot(data = reported_data, ggplot2::aes(x = time_data, y = count_data))+
      ggplot2::geom_point(ggplot2::aes(x = time_data, y = count_data), color = 'forestgreen', size = 1)+
      ggplot2::geom_line(data = true_daily_red, ggplot2::aes(x = forecast_time_data, y = forecast_count_median), inherit.aes = F)+
      ggplot2::geom_ribbon(data = true_daily_red, ggplot2::aes(x = forecast_time_data, ymin = forecast_count_.1, ymax = forecast_count_.9, fill = '80%'), inherit.aes = F, alpha = .3)+
      ggplot2::geom_ribbon(data = true_daily_red, ggplot2::aes(x = forecast_time_data, ymin = forecast_count_.25, ymax = forecast_count_.75, fill = '50%'),inherit.aes = F, alpha = .3,)+
      ggplot2::geom_vline(xintercept = v_line_loc, color = 'grey20', linetype = "dashed")+
      ggplot2::scale_color_manual("", values = c("red3",'pink3'))+
      ggplot2::scale_fill_manual("", values = c('red3','pink3'))+
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))

    return_list$plot = g15

    }

    return(return_list)
  }

}


