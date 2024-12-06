inv_dist <- function(x, kappa_star, kappa_orig, kappa_const_dow, eta_coeff, num_test){
  # x = [eta, omega, phi]
  return(
    1/(sum(
      (stats::plogis(lambda_t(1:num_test, x[3]) * (
        (omega_t(1:num_test, x[2])) * (pmin(eta_coeff*x[1], kappa_star)) + (1 - omega_t(1:num_test, x[2])) * (kappa_const_dow[1:num_test])
      ))
       - kappa_orig)^2
    ))
  )
}
