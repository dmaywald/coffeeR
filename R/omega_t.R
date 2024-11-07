omega_t <- function(k, omega){
  # k in (last_day - num_test_days:1)+1
  # omeaga >= 1

  return(pmax(0,(1 - ((k-1)/(omega))**2)))

}
