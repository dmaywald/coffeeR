# Define kappa_forcast_t function of equation 13
kappa_forecast_t <- function(t, eta, omega, phi, TS_data, eta_star, kappa_const_dow){
  # t in 1,...,num_test_days
  # eta in [0,1]
  # omega >= 1
  # phi > 0
  return(
    lambda_t(t,phi)*( (omega_t(t,omega))*(pmin(eta_star(eta),TS_data$kappa_star[t]))   +   (1-omega_t(t,omega))*(kappa_const_dow[t])  )
  )
}
