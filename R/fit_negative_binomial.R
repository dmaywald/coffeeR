
#' fit_negative_binomial
#'
#' @param count_data Response vector of count data to be modeled
#' @param time_data Covariate of time data used in the model
#' @param by_factor (Optional) Additional categorical covariate
#' @param cooksConstant (Optional) Specify the sensitivity to outliers. Lower values give higher sensitivity.
#' @param plot_model  (Optional) Boolean argument to plot the fitted values of the model
#'
#' @return
#' A list containing
#' \item{model}{mgcv::gam object. A spline model of count_data response variable and time_data covariate with by_factor as a possible factor}
#' \item{data}{Dataframe object. A data frame with binded count_data, time_data, by_factor (if used), and fitted value from model}
#' \item{cooks_distances}{Calculated cooks distances from model}
#' \item{outliers}{Flagged outliers based on cooks distances and cooksConstant}
#' @export
#'
#' @examples
fit_negative_binomial <- function(count_data, time_data, by_factor = NULL, cooksConstant = 4, plot_model = TRUE){

  # Adversarial user checks
  TS_data <- data.frame(count_data, time_data)

  if(!is.null(by_factor)){
    TS_data <- cbind(TS_data, by_factor)
  }
  # TS data needs 'day' and 'daily_confirmed'

  # if by is not null, check TS_data for "by_factor" variable

  # if by is vector, append TS_data with "by" variable

  # Model count data with negative binomial model with "by_factor"

  if(!is.null(by_factor)){
    nb_mod <- mgcv::gam(count_data ~
                            s(as.numeric(time_data), bs = 'cr', k = 20, by = by_factor) + by_factor,
                            method = "REML", data = TS_data, family = mgcv::nb())
  }

  if(is.null(by_factor)){
    nb_mod <- mgcv::gam(count_data ~
                          s(as.numeric(time_data), bs = 'cr', k = 20),
                        method = "REML", data = TS_data, family = nb())
  }
  cooksD = stats::cooks.distance(nb_mod) # Cooks distances for outlier detection
  outliers_nb = which(cooksD > cooksConstant*mean(cooksD))


  # predicted values for model with standard errors
  output_nb_mod <- stats::predict(nb_mod,
                           newdata = data.frame(time_data = TS_data$time_data,
                                                by_factor = TS_data$by_factor),
                           se.fit = T,
                           type = "response")

  # Get fits, upper, and lower bounds
  fit_vals = output_nb_mod$fit
  fit_sd  = output_nb_mod$se.fit


  alpha = .05
  fit_lwr = fit_vals - qnorm(1-alpha/2)*fit_sd
  fit_lwr[fit_lwr < 0] = 0 # lower bounds below 0 makes no sense to visualize

  fit_upp = fit_vals + qnorm(1-alpha/2)*fit_sd

  data.out = cbind(TS_data, fit_vals)
  data.new = cbind(TS_data, fit_vals, fit_lwr, fit_upp)




  # This seems to only happen with Florida, it has many zeros in conjunction with high spikes.
  # The other models can handle this, I'm replacing the model with a Poisson
  if(max(fit_vals) > 5*max(count_data)){
    # nb_dow_mod <- gam(daily_confirmed ~
    #                     s(day, bs = 'cr', k = 20, by = day_of_week),
    #                   method = "REML", data = TS_data, family = poisson)
    # second_title_str = ": Neg. Bin. Fail! (Poisson Dist. w/ DOW)"
    # daily_fit <- predict(nb_dow_mod,type = "response")

    warning("Detected poor fits in Negative Bonimial Model. fit_poisson_model() is recommended")
  }

  # Plot model
  if(plot_model){
    if(!is.null(by_factor)){
      wrap = TRUE
      plot_with_factor = TRUE
    }

    gg <- ggplot2::ggplot(data.new, ggplot2::aes(x = time_data, y = count_data))+
      {if(!wrap)ggplot2::geom_point()}+
      {if(!wrap)ggplot2::geom_point(data = TS_data[outliers_nb,],
                                        aes(x = time_data, y = count_data),
                                        color = 'firebrick')}+
      {if(wrap)ggplot2::geom_point(data = select(TS_data, -by_factor), color = 'grey80')}+
      {if(wrap)ggplot2::geom_point(aes(color = by_factor))}+
      {if(wrap)ggplot2::facet_wrap(~by_factor)}+
      {if(wrap)ggplot2::geom_point(data = TS_data[outliers_nb,],
                                   ggplot2::aes(x = time_data, y = count_data),
                                       color = 'firebrick')}+
      {if(plot_with_factor)ggplot2::geom_line(ggplot2::aes(x = time_data, y = fit_vals, color = by_factor), linewidth = 1)}+
      {if(!plot_with_factor)ggplot2::geom_line(ggplot2::aes(x = time_data, y = fit_vals), linewidth = 1, color = 'navyblue')}+
      {if(!plot_with_factor)ggplot2::geom_ribbon(ggplot2::aes(ymin = fit_lwr, ymax = fit_upp), alpha = .3, fill = 'skyblue')}

      plot(gg)
    return(list(model = nb_mod, data = data.out, cooks_distances = cooksD, outliers = outliers_nb, plot = gg))
  }
  return(list(model = nb_mod, data = data.out, cooks_distances = cooksD, outliers = outliers_nb))

}
