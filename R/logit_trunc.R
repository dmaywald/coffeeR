# Logit function (qlogis) adjusted to truncate by low and high values
logit_trunc <- function(x, low, high) {



  out <- sapply(x,
    # Apply input x to a truncated 'qlogis' function
    \(num) {
      # If low < x < high, return qlogis(x)
      if ((num > low) & (num < high)) {
        return(qlogis(num))
      }
      if (num <= low) {
      # if x <= low, return qlogis(low)
        return(stats::qlogis(low))
      }
      if (num >= high)
      # if x >= high, return qlogis(high)
        return(stats::qlogis(high))

    }
  )
  return(out)
}
