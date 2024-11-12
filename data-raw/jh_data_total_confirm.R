###########
# code used to prepare `data/jh_data_total_confirm` data set
# This file takes daily total confirmed US cases data from John Hopkins and writes .rda
# of state by state daily confirmed cases.
# Also removes time series of towns that are always 0 or cruise ships,
# before adding them to the state's total
########

# read in time series data
jh_data_total_confirm = read.csv("data-raw/time_series_covid19_confirmed_US.csv")

# see if any of the time series are all <= 0
# ... This will detect entries like NaN's
just_dates        = data.frame(t(jh_data_total_confirm %>% dplyr::select(dplyr::starts_with('X'))))
names(just_dates) = jh_data_total_confirm$Combined_Key
remove_data       = which(((sapply(just_dates, function(vec){all(vec <= 0)})) == TRUE))

# subset the data, removing these entries
jh_data_total_confirm = jh_data_total_confirm[-remove_data,]
rm(list = c("just_dates", 'remove_data'))


# Split-apply-combine the data, summing up within state/territory total confirmed cases
jh_data_total_confirm = jh_data_total_confirm %>%
  dplyr::group_by(Province_State) %>%
  dplyr::summarise_at(dplyr::vars(starts_with('X')), sum)

# Transpose dataframe and get appropriate column names
jh_data_total_confirm        = data.frame(t(jh_data_total_confirm))
names(jh_data_total_confirm) = jh_data_total_confirm[1,]
jh_data_total_confirm        = jh_data_total_confirm[-1,]

# Get rid of "X" that's in front of every date and make it a column...
# ...instead of an index. Convert back to numeric data
jh_rownames           = gsub("X", "", rownames(jh_data_total_confirm))
jh_data_total_confirm = as.data.frame(sapply(jh_data_total_confirm, as.numeric))

# Assign a 'date' column: m-d-y -> Y-m-d
jh_data_total_confirm$date = as.Date(jh_rownames, tryFormats = c("%m.%d.%y"))

# Let's add a 'year' and 'month' as numeric
year  = format(jh_data_total_confirm$date, '%Y')
month = format(jh_data_total_confirm$date, '%m')

# Add to dataframe
jh_data_total_confirm$year  = as.numeric(year)
jh_data_total_confirm$month = as.numeric(month)

# Add "Day of Week"
jh_data_total_confirm$day_of_week = format(jh_data_total_confirm$date, '%a')


# Couple of 'states/territories' are cruise ships that provide uninformative data
# ... They're 0 confirmed cases one day, and maximal confirmed cases the next.
jh_data_total_confirm = dplyr::select(jh_data_total_confirm, -contains('Princess'))


# Sum up states/territories for US total
jh_data_total_confirm$US = jh_data_total_confirm %>% dplyr::select(-c(date, year, month, day_of_week)) %>% rowSums()

# 'Year' and 'Month' are being read as an integer, I want it to be a factor.
# Make sure factor levels make sense
jh_data_total_confirm$year  = factor(jh_data_total_confirm$year,levels = c(2020:2023))

jh_data_total_confirm$month = factor(jh_data_total_confirm$month,levels = c(1:12))

# Similarly for 'day_of_week', I want this to be a factor.
# Make sure factor level makes sense with paper
jh_data_total_confirm$day_of_week = factor(jh_data_total_confirm$day_of_week,
                                           levels =  c('Sun', 'Mon', 'Tue',
                                                       'Wed', 'Thu',
                                                       'Fri', 'Sat'))

# Make date a Date
jh_data_total_confirm$date = as.Date(jh_data_total_confirm$date)

usethis::use_data(jh_data_total_confirm, overwrite = TRUE)
