make_ts_data <- function(count_data, time_data, by_factor = NULL){
  ########## Adversarial user checks ######################################

  # check count_data and time_data are of the same length
  if(length(count_data) != length(time_data)){
    stop("count_data and time_data are not of the same length")
  }

  # check count data is non-negative
  if(min(count_data) < 0){
    stop("count_data cannot have negative values")
  }

  # if by_factor is not null, check length of by factor
  if(!is.null(by_factor)){
    if(length(by_factor) != length(count_data)){
      stop("by_factor not of appropriate length. Needs to be the same length as count_data")
    }
  }


  # Make TS_data for empirical growth rate model
  TS_data <- data.frame(count_data, time_data)

  # add by_factor if it is not null
  if(!is.null(by_factor)){
    TS_data <- cbind(TS_data, by_factor)
  }
  return(TS_data)
}
