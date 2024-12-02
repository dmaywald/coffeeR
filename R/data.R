#' John Hopkins Total Confirmed Cases - United States
#'
#' A processed data set of Covid-19 total confirmed cases from The Center for Systems Science and Engineering at John Hopkins University.
#' Separated by state
#'
#' @format ## `jh_data_total_confirm`
#' A data frame with 1,143 rows and 61 columns:
#' \describe{
#'   \item{state/territory}{Total Cases in State/Territory}
#'   \item{US}{Total Cases in US}
#'   \item{date}{Date in format "Y-m-d"}
#'   \item{year}{Year in numeric format with "2020" as 1}
#'   \item{month}{Month in numeric format with "January" as 1}
#'   \item{day_of_week}{Day of Week in format "a"}
#'   ...
#' }
#' @source <https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv>
"jh_data_total_confirm"


#' John Hopkins Daily Confirmed Cases - United States
#'
#' A processed data set of Covid-19 daily confirmed cases from The Center for Systems Science and Engineering at John Hopkins University.
#' Separated by state
#'
#' @format ## `jh_data_daily_confirm`
#' A data frame with 1,143 rows and 61 columns:
#' \describe{
#'   \item{state/territory}{Daily Cases in State/Territory}
#'   \item{US}{Daily Cases in US}
#'   \item{date}{Date in format "Y-m-d"}
#'   \item{year}{Year in numeric format with "2020" as 1}
#'   \item{month}{Month in numeric format with "January" as 1}
#'   \item{day_of_week}{Day of Week in format "a"}
#'   ...
#' }
#' @source <https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv>
"jh_data_daily_confirm"



#' State Population - United States
#'
#' A processed data set of US Population by State from The United States Census Bureau
#' Separated by state and Year 2020-203
#'
#' @format ## `state_population`
#' A data frame with 4 rows and 54 columns:
#' \describe{
#'   \item{state/territory}{Population of State/Territory}
#'   \item{US}{Population of US}
#'   \item{year}{Year in numeric format}
#'   ...
#' }
#' @source <https://www2.census.gov/programs-surveys/popest/tables/2020-2023/state/totals/NST-EST2023-POP.xlsx>
"state_population"
