
# coffeeR

## Package Description

This package loosely follows the procedure outlined in the paper: 

COFFEE: COVID-19 Forecasts using Fast Evaluations and Estimation 
(https://arxiv.org/abs/2110.01546)

This package was developed independently of the Los Alamos National Laboratory. 

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

## Work in Progress

A vignette needs to be made that demonstrates the functionality of this
package with COVID-19 data. Outlier detection methods need to be added.
Empirical growth rate functions need to be added with sampling function.
Forecasting functions need to be implemented.
