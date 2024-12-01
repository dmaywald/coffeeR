#' Adjust Time Series Data for Outliers
#'
#' @param count_data Response vector of Count Data with (potential) outliers
#' @param time_data Covariate vector of Time Data
#' @param by_factor (Optional) Additional categorical covariate
#' @param cooks_constant (Optional) Specify the sensitivity to outliers. Lower values give higher sensitivity.
#' @param return_plot  (Optional) Boolean argument to return a ggplot2 plot object
#'
#' @return
#' A list containing
#' \item{data}{A dataframe with original count_data, original time_data, original by_factor, and new outlier adjusted data}
#' \item{outliers}{Indices of data points flagged as outliers}
#' \item{models}{A list containing the 3 spline models used to fit the count data}
#' \item{plot}{A ggplot2 object visualizing the outlier adjusted data}
#' @export
#'
#' @examples
#' # Example dataset
#' data("jh_data_daily_confirm")
#'
#' # Specify the count data to be days 300-600 of Colorado's daily Covid-19 cases
#' count_data = jh_data_daily_confirm$Colorado[300:600]
#'
#' # Specify the time data to be the first 300 days
#' time_data = jh_data_daily_confirm$date[300:600]
#'
#' # Add a "day of week" effect to the model
#' by_factor = jh_data_daily_confirm$day_of_week[300:600]
#'
#' Colorado_data = adjust_outliers(count_data, time_data, by_factor, return_plot = TRUE)
adjust_outliers <- function(count_data, time_data, by_factor = NULL, cooks_constant = 4, return_plot = FALSE){

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


  # Make TS_data for building gam model
  TS_data <- data.frame(count_data, time_data)

  # add by_factor if it is not null
  if(!is.null(by_factor)){
    TS_data <- cbind(TS_data, by_factor)
  }

  ######## Fit the count data to the three models ###########################

  nb_mod <- fit_negative_binomial(count_data = count_data, time_data = time_data, by_factor = by_factor,
                                  cooks_constant = cooks_constant, return_plot = FALSE)

  pois_mod <- fit_poisson(count_data = count_data, time_data = time_data, by_factor = by_factor,
                          cooks_constant = cooks_constant, return_plot = FALSE)

  qpois_mod <- fit_qpois(count_data = count_data, time_data = time_data, by_factor = by_factor,
                         cooks_constant = cooks_constant, return_plot = FALSE)
  # Get the outliers and replace with weighted average of 3 models
  outliers1 = nb_mod$outliers
  outliers2 = pois_mod$outliers
  outliers3 = qpois_mod$outliers
  outliers  = c(outliers1, outliers2, outliers3)

  # Outliers are points tagged by at least 2 out of 3 of the outlier detectors
  out3 = which(sapply(1:length(outliers),
                      \(num){sum(outliers == outliers[num]) >= 2}))
  out3 = unique(outliers[out3])


  fits = data.frame(mod1 = nb_mod$data$fit_vals,
                    mod2 = pois_mod$data$fit_vals,
                    mod3 = qpois_mod$data$fit_vals)

  # For each model, calculate sum(|residuals|)
  model_errors = colSums(abs(fits - TS_data$count_data))

  # Model weights are 1/sum(|residuals|), giving more weight to 'more accurate' model
  weights = 1/model_errors
  weights = weights/sum(weights) # weights add up to 1

  nb_mod$weight = weights[[1]]
  pois_mod$weight = weights[[2]]
  qpois_mod$weight = weights[[3]]

  # Remove data from model fits to save storage
  within(nb_mod, rm(data))
  within(pois_mod, rm(data))
  within(qpois_mod, rm(data))

  adjusted_data = TS_data$count_data

  if(length(out3) > 0){
    for (num in out3) {
      weighted_average = sum(c(nb_mod$data$fit_vals[num], pois_mod$data$fit_vals[num],
                           qpois_mod$data$fit_vals[num]) * weights)
      adjusted_data[num] = round(weighted_average)
    }
  }


  data.out = cbind(TS_data, adjusted_data)
  if(return_plot){
    # Plot data points, red points are outliers, green points are their replacement
    gg <- ggplot2::ggplot(TS_data, ggplot2::aes(x = time_data, y = count_data))+
      ggplot2::geom_point(data = TS_data[-out3,], color = 'grey70')+

      ggplot2::geom_point(data = TS_data[out3,],
                          ggplot2::aes(x = time_data, y = count_data),
                                       color = 'firebrick')+
      ggplot2::geom_point(data = data.out[out3,],
                          ggplot2::aes(x = time_data, y = adjusted_data), color = 'green4')

    return(list(data = data.out,
                outliers = out3,
                models = list(nb_mod = nb_mod, pois_mod = pois_mod, qpois_mod = qpois_mod),
                plot = gg
                ))
  } else {
    return(list(data = data.out,
                outliers = out3,
                models = list(nb_mod = nb_mod, pois_mod = pois_mod, qpois_mod = qpois_mod)
    ))
  }
}
