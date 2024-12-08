########
# Code used to generate new_mexico_random_vectors_example
# Generating random samples, especially without parallel processing, can take
# a long time. This data set follows the COFFEE procedure up until the step
# where random vectors need to be generated. This way, one can demonstrate this
# package without recalculating random vectors everytime.
#########

# Example dataset
data("jh_data_daily_confirm")
data("state_population")

# Specify the count data to be days 1-273 of Pennsylvania's daily Covid-19 cases
count_data = jh_data_daily_confirm$`New Mexico`[1:273]

# Specify the time data to be days 1-273
time_data = as.numeric(jh_data_daily_confirm$date[1:273])


# Add a "day of week" effect to the model
by_factor = jh_data_daily_confirm$day_of_week[1:273]

TS_data = adjust_outliers(count_data, time_data, by_factor, cooks_constant = 4, return_plot = FALSE)

count_data = TS_data$data$adjusted_data

num_train = 28
num_test = 14
num_forecast = 14
num_random_vector = 250

# Get Florida Population and use .55 as susceptible population proportion
population = state_population$New.Mexico[1]
susc_perc = .55
total_cases = sum(count_data)

# Empirical Growth Rate (EGR) and EGR Model is needed to calculate
# random vectors sampled from the empirical growth rate model
nm_emp_grow_rate = empirical_growth_rates(count_data = count_data,
                                            time_data = time_data,
                                            num_train = num_train,
                                            num_test = num_test,
                                            population = population,
                                            susc_perc = susc_perc,
                                            by_factor = by_factor)

TS_train_data = nm_emp_grow_rate$data[1:num_train, ]
TS_test_data = nm_emp_grow_rate$data[-(1:num_train), ]

eta_bounds = c(0,1)
omega_bounds = c(1, 14)
phi_bounds = c(.9,1.1)

kappa_const_dow = TS_test_data$kappa_const + TS_test_data$kappa_dow

eta_coeff = stats::median(TS_train_data$kappa_star[(num_train - 6):num_train])

accept_const = calc_accept_const(eta_bounds = eta_bounds, omega_bounds = omega_bounds,
                                 phi_bounds = phi_bounds, N = 50000,
                                 TS_data = TS_test_data, kappa_const_dow = kappa_const_dow,
                                 eta_coeff = eta_coeff, num_test = 14)

library(doParallel)

unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}


nworkers <- detectCores() - 1
cl <- makeCluster(nworkers)
registerDoParallel(cl)
random_vectors = foreach(idx = 1:num_random_vector, .combine = rbind, .packages = 'coffeeR') %dopar% {
  sample_inv_dist(1, TS_test_data = TS_test_data, TS_train_data = TS_train_data,
                  eta_bounds = eta_bounds, omega_bounds = omega_bounds,
                  phi_bounds = phi_bounds, accept_const = accept_const, max_draws = 1e7)
}
stopCluster(cl)
unregister_dopar()

new_mexico_random_vectors_example = random_vectors

usethis::use_data(new_mexico_random_vectors_example, overwrite = TRUE)
