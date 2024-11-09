## Script to fit negative binomial with Day of Week effect spline model to data
data("jh_data_daily_confirm")
data("jh_data_total_confirm")

names(jh_data_daily_confirm)[1] = "day"
names(jh_data_total_confirm)[1] = "day"

state = "Texas"
first_day = 1
last_day = 273
cooksConstant = 4

# Structure Analysis
str(jh_data_daily_confirm)
str(jh_data_total_confirm)

# 'Year' and 'Month' are being read as an integer, I want it to be a factor.
# Make sure factor levels make sense
jh_data_daily_confirm$year  = factor(jh_data_daily_confirm$year,levels = c(2020:2023))
jh_data_total_confirm$year  = factor(jh_data_total_confirm$year,levels = c(2020:2023))

jh_data_daily_confirm$month = factor(jh_data_daily_confirm$month,levels = c(1:12))
jh_data_total_confirm$month = factor(jh_data_total_confirm$month,levels = c(1:12))

# Similarly for 'day_of_week', I want this to be a factor.
# Make sure factor level makes sense with paper
jh_data_daily_confirm$day_of_week = factor(jh_data_daily_confirm$day_of_week,
                                           levels =  c('Sun', 'Mon', 'Tue',
                                                       'Wed', 'Thu',
                                                       'Fri', 'Sat'))

jh_data_total_confirm$day_of_week = factor(jh_data_total_confirm$day_of_week,
                                           levels =  c('Sun', 'Mon', 'Tue',
                                                       'Wed', 'Thu',
                                                       'Fri', 'Sat'))


# Make date a Date
jh_data_daily_confirm$date = as.Date(jh_data_daily_confirm$date)
jh_data_total_confirm$date = as.Date(jh_data_total_confirm$date)

# Time Series Data
col_num  = which(names(jh_data_daily_confirm) == state)
TS_data = data.frame(day = jh_data_daily_confirm$day[first_day:last_day] ,
                     daily_confirmed = jh_data_daily_confirm[first_day:last_day,col_num],
                     total_confirmed = jh_data_total_confirm[first_day:last_day,col_num],
                     day_of_week = jh_data_total_confirm$day_of_week[first_day:last_day],
                     month = jh_data_total_confirm$month[first_day:last_day],
                     year = jh_data_total_confirm$year[first_day:last_day],
                     date = as.Date(jh_data_total_confirm$date[first_day:last_day],
                                    origin = jh_data_daily_confirm$date[first_day]))

# For making Month-Year labels in x-axis of plots. Needs to be done after each plot
reset_labels <- function(){
  day_label_loc <<- which(TS_data$date == lubridate::floor_date(TS_data$date,'month'))
  if(length(day_label_loc) >= 20){
    day_label_loc <<- day_label_loc[seq(from = 1, to = length(day_label_loc), by = 2)]
  }

  day_label <<- format(TS_data$date[day_label_loc], format = "%b %Y") # global variable
}

reset_labels()

# Sometimes daily confirmed is negative. Some model families cannot allow this
TS_data$daily_confirmed[TS_data$daily_confirmed < 0 ] = 0

# For confidence intervals
alpha = .05

# Initial Visual with just a LOESS fit
state_title = paste("Daily Confirmed Cases",
                    paste(strsplit(state, split = '.', fixed = T)[[1]], collapse = " "))


#############################################
# Model 1: Spline WITH day of week effect
# Seems to have computational problems for Florida between day 500-700

nb_dow_mod <- mgcv::gam(daily_confirmed ~
                  s(day, bs = 'cr', k = 20, by = day_of_week) + day_of_week,
                  method = "REML", data = TS_data, family = nb())


# gam.check(nb_dow_mod) # Check number of basis dimensions

summary(nb_dow_mod) # Summary model output

cooksD1 = stats::cooks.distance(nb_dow_mod) # Cooks distances for outlier detection

# nb_dow_mod$family$getTheta(TRUE) # Check for overdispersion

daily_fit <- stats::predict(nb_dow_mod, type = "response") # Predicted values


second_title_str = ": Negative Binomial Distribution (DOW)" # For plotting title
# This seems to only happen with Florida, it has many zeros in conjunction with high spikes.
# The other models can handle this, I'm replacing the model with a Poisson
if(max(daily_fit) > 5*max(TS_data$daily_confirmed)){
  nb_dow_mod <- gam(daily_confirmed ~
                      s(day, bs = 'cr', k = 20, by = day_of_week),
                    method = "REML", data = TS_data, family = poisson)
  second_title_str = ": Neg. Bin. Fail! (Poisson Dist. w/ DOW)"
  daily_fit <- predict(nb_dow_mod,type = "response")
}

# Plot the model
data.new = cbind(TS_data, daily_fit)


plot_with_dow = TRUE # Separate lines for day of week?
wrap_day      = TRUE # Separate plots for day of week?

# For the x-axis labels: if too many labels to fit on x axis, reduce labels
if(wrap_day & length(day_label)> 8){
  while(length(day_label)>8){
    day_label = day_label[seq(1,length(day_label),2)]
    day_label_loc = day_label_loc[seq(1,length(day_label_loc),2)]
  }
}

# Plot model
g1 <- ggplot(data.new, aes(x = day, y = daily_confirmed))+
  {if(!wrap_day)geom_point()}+
  {if(!wrap_day)geom_point(data = TS_data[which(cooksD1 > cooksConstant*mean(cooksD1)),],
                           aes(x = day, y = daily_confirmed),
                           color = 'firebrick')}+
  {if(wrap_day)geom_point(data = select(TS_data, -day_of_week), color = 'grey80')}+
  {if(wrap_day)geom_point(aes(color = day_of_week))}+
  {if(wrap_day)facet_wrap(~day_of_week)}+
  {if(wrap_day)geom_point(data = TS_data[which(cooksD1 > cooksConstant*mean(cooksD1)),],
                          aes(x = day, y = daily_confirmed),
                          color = 'firebrick')}+
  {if(plot_with_dow)geom_line(aes(x = day, y = daily_fit, color = day_of_week), linewidth = 1)}+
  {if(!plot_with_dow)geom_line(aes(x = day, y = daily_fit), linewidth = 1, color = 'navyblue')}+
  {if(!plot_with_dow)geom_ribbon(aes(ymin = daily_lwr, ymax = daily_upp), alpha = .3, fill = 'skyblue')}+
  ggtitle(paste(state_title, second_title_str, sep = ""))+
  scale_x_continuous(breaks = TS_data$day[day_label_loc],labels= day_label)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "bottom")+
  ylab('Daily Confirmed')

plot(g1)
reset_labels()
