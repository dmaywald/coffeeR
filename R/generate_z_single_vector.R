generate_z_single_vector <- function(z, eta_bounds, omega_bounds, phi_bounds){
  # z = [eta,omega,phi]
  # function for generating samples of eta omega and phi within accept-reject method
  return(stats::punif(q = z[1], min = eta_bounds[1], max = eta_bounds[2])*stats::punif(q = z[2], min = omega_bounds[1], max = omega_bounds[2])*
           stats::punif(q = z[3], min = phi_bounds[1], max = phi_bounds[2]))
}
