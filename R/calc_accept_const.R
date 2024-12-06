calc_accept_const <- function(eta_bounds, omega_bounds, phi_bounds, N = 50000, TS_data, kappa_const_dow, eta_coeff, num_test){
  # Temporary dataframe to calcualate h over
  temp = data.frame(eta = stats::runif(N, eta_bounds[1], eta_bounds[2]),
                    omega = stats::runif(N, omega_bounds[1], omega_bounds[2]),
                    phi = stats::runif(N, phi_bounds[1], phi_bounds[2]))

  f_x = f_x = sapply(1:N, function(num){inv_dist(x = c(temp[num,1], temp[num,2], temp[num,3]), TS_data$kappa_star, TS_data$kappa_orig, kappa_const_dow, eta_coeff, num_test)})
  # Accptance constant is going to be the maximal value over the N data points of
  # inv_dist(x)/h(x)
  accept_const = max(f_x/generate_z_dataframe(temp, eta_bounds, omega_bounds, phi_bounds))

  return(accept_const)
}
