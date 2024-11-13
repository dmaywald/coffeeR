
#' fit_poisson
#'
#' @param count_data Response vector of count data to be modeled
#' @param time_data Covariate of time data used in the model
#' @param by_factor (Optional) Additional categorical covariate
#' @param cooksConstant (Optional) Specify the sensitivity to outliers. Lower values give higher sensitivity.
#' @param return_plot  (Optional) Boolean argument to return a ggplot2 plot object
#'
#' @return
#' A list containing
#' \item{model}{mgcv::gam object. A spline model of count_data response variable and time_data covariate with by_factor as a possible factor}
#' \item{data}{Dataframe object. A data frame with binded count_data, time_data, by_factor (if used), and fitted value from model}
#' \item{cooks_distances}{Calculated cooks distances from model}
#' \item{outliers}{Flagged outliers based on cooks distances and cooksConstant}
#' \item{plot}{ggplot2 plot object of model fitted values}
#' @export
#'
#' @examples
#'
#' # Example dataset
#' data("jh_data_daily_confirm")
#'
#' # Specify the count data to be the first 275 days of Arizona's daily Covid-19 cases
#' count_data = jh_data_daily_confirm$Arizona[1:275]
#'
#' # Specify the time data to be the first 275 days
#' time_data = jh_data_daily_confirm$date[1:275]
#'
#' # Add a "day of week" effect to the model
#' by_factor = jh_data_daily_confirm$day_of_week[1:275]
#'
#'
#' Arizona_poisson = fit_poisson(count_data, time_data, by_factor, return_plot = FALSE)
#'
fit_poisson <- function(count_data, time_data, by_factor = NULL, cooksConstant = 4, return_plot = FALSE){

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

  # Model count data with poisson model with "by_factor"
  if(!is.null(by_factor)){
    poisson_mod <- mgcv::gam(count_data ~ s(as.numeric(time_data), bs = 'cr',
                                            k = 20, by = by_factor) + by_factor,
                        method = "REML", data = TS_data, family = stats::poisson())
  }

  # if no by_factor was given, make model with only data given
  if(is.null(by_factor)){
    poisson_mod <- mgcv::gam(count_data ~
                          s(as.numeric(time_data), bs = 'cr', k = 20),
                        method = "REML", data = TS_data, family = stats::poisson())
  }

  # Cooks distances for outlier detection
  cooksD = stats::cooks.distance(poisson_mod)
  outliers_poisson = which(cooksD > cooksConstant*mean(cooksD))


  # predicted values for model with standard errors
  if(!is.null(by_factor)){
    output_poisson_mod <- stats::predict(poisson_mod,
                                         newdata = data.frame(time_data = TS_data$time_data,
                                                              by_factor = TS_data$by_factor),
                                         se.fit = T,
                                         type = "response")}

  if(is.null(by_factor)){
    output_poisson_mod <- stats::predict(poisson_mod,
                                         newdata = data.frame(time_data = TS_data$time_data),
                                         se.fit = T,
                                         type = "response")}


  # Get fits, upper, and lower bounds for confidence intervals
  fit_vals = output_poisson_mod$fit
  fit_sd  = output_poisson_mod$se.fit

  # Make confidence intervals
  alpha = .05
  fit_lwr = fit_vals - stats::qnorm(1-alpha/2)*fit_sd
  fit_lwr[fit_lwr < 0] = 0 # lower bounds below 0 makes no sense to visualize

  fit_upp = fit_vals + stats::qnorm(1-alpha/2)*fit_sd

  # make visualization data and return data
  data.out = cbind(TS_data, fit_vals)
  data.new = cbind(TS_data, fit_vals, fit_lwr, fit_upp)


  # Plot model
  if(return_plot){
    if(!is.null(by_factor)){
      wrap = TRUE
      plot_with_factor = TRUE
    } else {
      wrap = FALSE
      plot_with_factor = FALSE
    }

    # store ggplot object to return
    gg <- ggplot2::ggplot(data.new, ggplot2::aes(x = time_data, y = count_data))+
      # add original data as points, marking outliers as red points
      {if(!wrap)ggplot2::geom_point()}+
      {if(!wrap)ggplot2::geom_point(data = TS_data[outliers_poisson,],
                                    ggplot2::aes(x = time_data, y = count_data),
                                    color = 'firebrick')}+
      # if by_factor was used, facet wrap plots by "by_factor"
      {if(wrap)ggplot2::geom_point(data = select(TS_data, -by_factor), color = 'grey80')}+
      {if(wrap)ggplot2::geom_point(ggplot2::aes(color = by_factor))}+
      {if(wrap)ggplot2::facet_wrap(~by_factor)}+
      {if(wrap)ggplot2::geom_point(data = TS_data[outliers_poisson,],
                                   ggplot2::aes(x = time_data, y = count_data),
                                   color = 'firebrick')}+
      # add fitted values of model as line. If no "by_factor" was used, add confidence interval.
      {if(plot_with_factor)ggplot2::geom_line(ggplot2::aes(x = time_data, y = fit_vals, color = by_factor), linewidth = 1)}+
      {if(!plot_with_factor)ggplot2::geom_line(ggplot2::aes(x = time_data, y = fit_vals), linewidth = 1, color = 'navyblue')}+
      {if(!plot_with_factor)ggplot2::geom_ribbon(ggplot2::aes(ymin = fit_lwr, ymax = fit_upp), alpha = .3, fill = 'skyblue')}

    return(list(model = poisson_mod, data = data.out, cooks_distances = cooksD, outliers = outliers_poisson, plot = gg))
  }
  return(list(model = poisson_mod, data = data.out, cooks_distances = cooksD, outliers = outliers_poisson))

}
