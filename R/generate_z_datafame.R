generate_z_dataframe <- function(df, eta_bounds, omega_bounds, phi_bounds){
  # df = [eta,omega,phi]
  # function for generating samples of eta omega and phi within accept-reject method
  return(stats::punif(q = df[,1], min = eta_bounds[1], max = eta_bounds[2])*stats::punif(q = df[,2], min = omega_bounds[1], max = omega_bounds[2])*
           stats::punif(q = df[,3], min = phi_bounds[1], max = phi_bounds[2]))
}
