inv_dist <- function(x, TS_data, kappa_const_dow, eta_coeff, num_test){
  # x = [eta, omega, phi]
  return(
    1/(sum(
      (stats::plogis(lambda_t(1:num_test, x[3]) * (
        (omega_t(1:num_test, x[2])) * (pmin(eta_coeff*x[1], TS_data$kappa_star)) + (1 - omega_t(1:num_test, x[2])) * (kappa_const_dow[1:num_test])
      ))
       - TS_data$kappa_orig)^2
    ))
  )
}
