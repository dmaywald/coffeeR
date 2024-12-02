kappa_calc <- function(count_data){

  # Calculate a total count for empirical growth calculations
  total_count = cumsum(count_data)

  # Equation 7 of document gives formula for empirical growth rate
  last_day = length(count_data)
  kappa_orig = total_count[2:last_day]/total_count[1:last_day-1] - 1

  # append kappa_orig with a mean of the first few entries in order to have an empirical growth rate for the first day.
  kappa_orig = c(mean(utils::head(kappa_orig)), kappa_orig)

  # Adjust logit function for kappa's that are 0 or 1 (or close)
  tau_c = .95*min(kappa_orig[kappa_orig > 0])

  # Apply truncated logit function to kappa_orig to get kappa_star
  kappa_star = logit_trunc(kappa_orig, tau_c, 1 - tau_c)

  return(list(kappa_orig = kappa_orig, kappa_star = kappa_star, total_count = total_count))
}

