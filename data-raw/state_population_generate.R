#########################
# Code used to prepare 'data/state_population' data ste.
# This file takes population data from the US Census Bureau in csv file "NST-EST2023-POP.xlsx".
# File is available for download at https://www2.census.gov/programs-surveys/popest/tables/2020-2023/state/totals/NST-EST2023-POP.xlsx
# Calculate the state's population. Take the states population by year.
#########################

# Read data and clean it up
data = t(readxl::read_xlsx("data-raw/NST-EST2023-POP.xlsx"))
data = data.frame(data[,9:61]) # Only need states/Provinces
data = data[,-52] # Some NA column

# Name the columns after the states
names(data) = data[1,]

# Remove excess rows
data = data[-c(1,2),]


colnames = sub(".", "", names(data)) # Remove period in front of state names
colnames = gsub(" ", ".", colnames) # Replace spaces with periods


# Rename columns
names(data) = colnames

# Make year column and put it in front of the data
data$year      = c(2020, 2021, 2022, 2023)
data           = cbind(data$year,data[1:(length(data)-1)])
names(data)[1] = 'year'

data_out = data.frame(sapply(1:ncol(data), \(num){as.numeric(data[[num]])}))
names(data_out) = names(data)


# Add us population
data_out$US = data_out %>% select(-year) %>% rowSums()

state_population = data_out

usethis::use_data(state_population, overwrite = TRUE)
