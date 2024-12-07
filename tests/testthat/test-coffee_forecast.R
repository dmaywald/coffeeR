test_that("Predict Data Checks Work", {

  data("jh_data_daily_confirm")
  data("state_population")

  # Specify the count data to be days 1-550 of Pennsylvania's daily Covid-19 cases
  count_data = jh_data_daily_confirm$Pennsylvania[1:550]

  # Specify the time data to be days 1-550
  time_data = jh_data_daily_confirm$date[1:550]

  # Add a "day of week" effect to the model
  by_factor = jh_data_daily_confirm$day_of_week[1:550]

  num_train = 28
  num_test = 14
  num_forecast = 14

  # Get Florida Population and use .55 as susceptible population proportion
  population = state_population$Pennsylvania[1]
  susc_perc = .55
  total_cases = sum(jh_data_daily_confirm$Pennsylvania[1:550])

  # Empirical Growth Rate (EGR) and EGR Model is needed to calculate
  # random vectors sampled from the empirical growth rate model
  Penn_emp_grow_rate = empirical_growth_rates(count_data = count_data,
                                              time_data = time_data,
                                              num_train = num_train,
                                              num_test = num_test,
                                              population = population,
                                              susc_perc = susc_perc,
                                              by_factor = by_factor)

  TS_train_data = Penn_emp_grow_rate$data[1:num_train, ]
  TS_test_data = Penn_emp_grow_rate$data[-(1:num_train), ]

  eta_bounds = c(0,1)
  omega_bounds = c(1, 10)
  phi_bounds = c(.5,1.5)

  # Get random vector data
  data("pennsylvania_random_vectors_example")
  random_vectors = pennsylvania_random_vectors_example

  # Empirical Growth Rate (EGR) and EGR Model is needed to recalculated
  # with 0 testing days in order to retrieve the model used for future forecast
  Penn_emp_grow_rate = empirical_growth_rates(count_data = count_data,
                                              time_data = time_data,
                                              num_train = num_train,
                                              num_test = 0,
                                              population = population,
                                              susc_perc = susc_perc,
                                              by_factor = by_factor)

  emp_grow_model = Penn_emp_grow_rate$emp_grow_mod

  # Get time data corresponding to forecastfuture values
  first_pred_date = time_data[length(time_data)] + 1
  last_pred_date = time_data[length(time_data)] + num_forecast

  pred_time_data = seq.Date(first_pred_date, last_pred_date, by = 'day')

  # Get the Day of week for each corresponding day
  pred_by_factor = format(pred_time_data, '%a')

  TS_data = Penn_emp_grow_rate$data

  attack_rate_bounds = c(.4, .7)

  # There should be an error when attempting to fit an empirical growth rate model
  # without the full set of covariates
  # Issue is that the emp_grow_model specified above has a 'by_factor'
  # but the prediction data that we're supplying does not have a 'by_factor'
  testthat::expect_error(coffee_forecast(TS_data,
                                         emp_grow_model,
                                         population,
                                         total_cases,
                                         random_vectors,
                                         pred_time_data,
                                         pred_by_factor = NULL,
                                         attack_rate_bounds,
                                         return_plot = FALSE))


  # Expect warning if there are too few random vectors
  testthat::expect_warning(coffee_forecast(TS_data,
                                           emp_grow_model,
                                           population,
                                           total_cases,
                                           random_vectors[1:3, ],
                                           pred_time_data,
                                           pred_by_factor,
                                           attack_rate_bounds,
                                           return_plot = FALSE))

  # Expect warning if not enough TS_data was given
  testthat::expect_warning(coffee_forecast(TS_data[1:6, ],
                                           emp_grow_model,
                                           population,
                                           total_cases,
                                           random_vectors,
                                           pred_time_data,
                                           pred_by_factor,
                                           attack_rate_bounds,
                                           return_plot = FALSE))


})
