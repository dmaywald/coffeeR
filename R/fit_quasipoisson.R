#' Quasipoisson Spline Regression
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
#' # Specify the count data to be days 500-1000 of New York's daily Covid-19 cases
#' count_data = jh_data_daily_confirm$`New York`[500:1000]
#'
#' # Specify the time data to be the first 275 days
#' time_data = jh_data_daily_confirm$date[500:1000]
#'
#' # Add a "day of week" effect to the model
#' by_factor = jh_data_daily_confirm$day_of_week[500:1000]
#'
#'
#' New_York_qpois = fit_qpois(count_data, time_data, by_factor, return_plot = FALSE)
#'
fit_qpois <- function(count_data, time_data, by_factor = NULL, cooksConstant = 4, return_plot = FALSE){

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

  if(!is.null(by_factor)){
    # Model count data with  Quasipoisson (qpois) model with "by_factor"
    qpois_mod <- tryCatch(stats::glm(count_data ~ splines::ns(as.numeric(time_data),df = 10):by_factor,
                                  family = stats::quasipoisson, data = TS_data),
                              error = function(e) e,
                              warning = function(w) w)


    if(methods::is(qpois_mod,"warning")){
      # Sometimes model needs a few more iterations to converge
      qpois_mod <- tryCatch(stats::glm(count_data ~ splines::ns(as.numeric(time_data),df = 10):by_factor,
                                    family = stats::quasipoisson, data = TS_data,
                                    control = list(maxit = 100)),
                                error = function(e) e,
                                warning = function(w) w)
      # Sometimes additional basis dimensions are needed
      if(methods::is(qpois_mod,"error")){
        qpois_mod <- stats::glm(count_data ~ splines::ns(as.numeric(time_data),df = 20):by_factor,
                             family = stats::quasipoisson, data = TS_data,
                             control = list(maxit = 100))
      }
    }
  }

  # if no by_factor was given, make model with only data given
  if(is.null(by_factor)){
    # Model count data with Zero-Inflated Poisson (qpois) model with "by_factor"
    qpois_mod <- tryCatch(stats::glm(count_data ~ splines::ns(as.numeric(time_data),df = 10),
                                     family = stats::quasipoisson, data = TS_data),
                          error = function(e) e,
                          warning = function(w) w)


    if(methods::is(qpois_mod,"warning")){
      # Sometimes model needs a few more iterations to converge
      qpois_mod <- tryCatch(stats::glm(count_data ~ splines::ns(as.numeric(time_data),df = 10),
                                       family = stats::quasipoisson, data = TS_data,
                                       control = list(maxit = 100)),
                            error = function(e) e,
                            warning = function(w) w)
      # Sometimes additional basis dimensions are needed
      if(methods::is(qpois_mod,"error")){
        qpois_mod <- stats::glm(count_data ~ splines::ns(as.numeric(time_data),df = 20),
                                family = stats::quasipoisson, data = TS_data,
                                control = list(maxit = 100))
      }
    }
  }

  # Cooks distances for outlier detection
  cooksD = stats::cooks.distance(qpois_mod)
  outliers_qpois = which(cooksD > cooksConstant*mean(cooksD))


  # predicted values for model with standard errors
  if(!is.null(by_factor)){
    output_qpois_mod <- stats::predict(qpois_mod,
                                     newdata = data.frame(time_data = TS_data$time_data,
                                                          by_factor = TS_data$by_factor),
                                     se.fit = T,
                                     type = "response")}

  if(is.null(by_factor)){
    output_qpois_mod <- stats::predict(qpois_mod,
                                     newdata = data.frame(time_data = TS_data$time_data),
                                     se.fit = T,
                                     type = "response")}


  # Get fits, upper, and lower bounds for confidence intervals
  fit_vals = output_qpois_mod$fit
  fit_sd  = output_qpois_mod$se.fit

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
      {if(!wrap)ggplot2::geom_point(data = TS_data[outliers_qpois,],
                                    ggplot2::aes(x = time_data, y = count_data),
                                    color = 'firebrick')}+
      # if by_factor was used, facet wrap plots by "by_factor"
      {if(wrap)ggplot2::geom_point(data = dplyr::select(TS_data, -by_factor), color = 'grey80')}+
      {if(wrap)ggplot2::geom_point(ggplot2::aes(color = by_factor))}+
      {if(wrap)ggplot2::facet_wrap(~by_factor)}+
      {if(wrap)ggplot2::geom_point(data = TS_data[outliers_qpois,],
                                   ggplot2::aes(x = time_data, y = count_data),
                                   color = 'firebrick')}+
      # add fitted values of model as line. If no "by_factor" was used, add confidence interval.
      {if(plot_with_factor)ggplot2::geom_line(ggplot2::aes(x = time_data, y = fit_vals, color = by_factor), linewidth = 1)}+
      {if(!plot_with_factor)ggplot2::geom_line(ggplot2::aes(x = time_data, y = fit_vals), linewidth = 1, color = 'navyblue')}+
      {if(!plot_with_factor)ggplot2::geom_ribbon(ggplot2::aes(ymin = fit_lwr, ymax = fit_upp), alpha = .3, fill = 'skyblue')}

    return(list(model = qpois_mod, data = data.out, cooks_distances = cooksD, outliers = outliers_qpois, plot = gg))
  }
  return(list(model = qpois_mod, data = data.out, cooks_distances = cooksD, outliers = outliers_qpois))

}
