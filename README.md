
# coffeeR

## Package Description

This package loosely follows the procedure outlined in the paper: 

COFFEE: COVID-19 Forecasts using Fast Evaluations and Estimation[^1]


This package was developed independently of the Los Alamos National Laboratory and
original authors. 

During the COVID-19 Pandemic, many new epidemiological models were
developed to predict the number of cases and deaths a country or state
might experience in the near future. One such model is the COFFEE
(Covid-19 Forecasts using Fast Evaluations and Estimations) model
developed by the Los Alamos National Laboratory (Castro, 2020). Unlike
traditional disease models, COFFEE incorporates statistical methods to
dynamically parameterize a SIR model over time and across populations.
This approach reduces the amount of assumptions that must be made about
the virus’s spread. Additionally, this method allows for independent
forecasts on various scales– state-by-state, country-by-country, or even
city-by-city– while remaining sensitive to changes in virus conditions
such as transmission rates, social interaction rates, testing kit
availability, and many other factors, all without having to model them
directly. This allows for long-term predictions, upwards of 14 days or
even 4 weeks of predictions. Although originally designed for
COVID-19, the COFFEE method is adaptable to any pandemic data. This
package implements functions that will generate 14-day forecasts of
daily cases when given training data for any viral disease. A vignette
is available to demonstrate the functionality of my package using
COVID-19 data.

## Installation Instructions

Run the command below within your R console:

`devtools::install_github("dmaywald/coffeeR", build_vignettes = TRUE)`


## Package Purpose

This package serves to provide a more-or-less "hands off" implementation of the COFFEE
procedure. It provides functions that follow the steps described in the original 
paper. These functions include:

 - adjust_outliers()... Outlier adjustment of count data in time series
 - empirical_growth_rates()... Calculate empirical growth rates and empirical growth rate model
 - sample_inv_dist()... Sample random vectors from the density defined by the inverse distance function
 - coffee_forecast()... Implement forward time stepping procedure to produce forecast 
 
There is also a function that implements the entire procedure at once:
 - coffee()... Implement the COFFEE procedure

## Usage

```
library(coffeeR)

# Read in data
data("jh_data_daily_confirm")
data("state_population")

# Observed data
covid_cases = jh_data_daily_confirm$Pennsylvania[1:550]
days = jh_data_daily_confirm$date[1:550]
day_of_week = jh_data_daily_confirm$day_of_week[1:550]

# Number of forecast days
num_forecast = 14

# Random vectors computed in parallel (comes attached with package)
# See step 4 of vignette to see how to do this with the sample_inv_dist() function
data("pennsylvania_random_vectors_example")
random_vectors = pennsylvania_random_vectors_example

# Get time data corresponding to forecast values
first_pred_date = days[length(days)] + 1
last_pred_date = days[length(days)] + num_forecast
pred_time_data = seq.Date(first_pred_date, last_pred_date, by = 'day')

# Get the Day of week for each corresponding day
pred_by_factor = format(pred_time_data, '%a')

# Population of Pennsylvania 
penn_pop = state_population$Pennsylvania[1]

# Produce forecasts
penn_forecast = coffee(
  count_data = covid_cases,
  time_data = days,
  by_factor = day_of_week,
  pred_time_data = pred_time_data,
  pred_by_factor = pred_by_factor,
  population = penn_pop,
  adjust_data = TRUE,
  num_train = 28,
  num_test = 14,
  susc_perc = .55,
  random_vectors = random_vectors,
  attack_rate_bounds = c(.4, .7),
  return_plot = TRUE
)

```


## References

[^1]: COFFEE: COVID-19 Forecasts using Fast Evaluations and Estimation [(Castro, 2020)](https://doi.org/10.48550/arXiv.2110.01546)
