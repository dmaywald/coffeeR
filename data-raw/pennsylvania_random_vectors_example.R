########
# Code used to generate pennslyvania_random_vectors_example
# Generating random samples, especially without parallel processing, can take
# a long time. This data set follows the COFFEE procedure up until the step
# where random vectors need to be generated. This way, one can demonstrate this
# package without recalculating random vectors everytime.
#########

# Example dataset
data("jh_data_daily_confirm")
data("state_population")

# Specify the count data to be days 1-550 of Pennsylvania's daily Covid-19 cases
count_data = jh_data_daily_confirm$Pennsylvania[1:550]

# Specify the time data to be days 1-550
time_data = as.numeric(jh_data_daily_confirm$date[1:550])

time_data = time_data

# Add a "day of week" effect to the model
by_factor = jh_data_daily_confirm$day_of_week[1:550]

TS_data = adjust_outliers(count_data, time_data, by_factor, cooks_constant = 4, return_plot = FALSE)

count_data = TS_data$data$adjusted_data

num_train = 28
num_test = 14
num_forecast = 14

# Get Florida Population and use .55 as susceptible population proportion
population = state_population$Pennsylvania[1]
susc_perc = .55
total_cases = sum(count_data)

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
omega_bounds = c(1, 14)
phi_bounds = c(.9,1.1)


usethis::use_data(pennsylvania_random_vectors_example, overwrite = TRUE)
