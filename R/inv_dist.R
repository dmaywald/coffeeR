inv_dist <- function(x, TS_data, kappa_const_dow, num_test_days, eta_star){
  # x = [eta, omega, phi]
  return(
    1/(sum(
      (stats::plogis(lambda_t(1:num_test_days, x[3]) * (
        (omega_t(1:num_test_days, x[2])) * (pmin(eta_star(x[1]), TS_data$kappa_star[1:num_test_days])) + (1 -
                                                                                                            omega_t(1:num_test_days, x[2])) * (kappa_const_dow[1:num_test_days])
      ))
       - TS_data$kappa_orig[1:num_test_days])^2
    ))
  )

}
