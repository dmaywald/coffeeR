lambda_t <- function(k, phi){
  # k in (last_day - num_test_days:1)+1
  # phi > 0
  return(1 + k*((phi-1)/(30)))
}
