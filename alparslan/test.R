setwd("~\\sem 2\\Advanced programming in R\\myAPP\\covid19_vaccination-shiny")
source("alparslan/source.R")


# LOAD DATA
Vaccinated <- read.csv(url("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv"))

head(Vaccinated)
class(Vaccinated)

data_tur <- Data$new(total_vaccination = Vaccinated, country = "Poland")

data_tur$country
data_tur$create_country_data()
data_tur$create_ts()

data_tur$country_data
data_tur$ts_data
data_tur$ts_data_diff


## PLOT
plot <- Plot$new(ts_object = data_tur$ts_data)

plot$plot_time_series(ts_object_name = "Daily COVID Vaccination 2021")
plot$test_stationarity()
# Looking at the lower p values, we can use this data for further step.
plot$plot_acf_pacf(ts_object_name = "ACF - PACF Daily COVID Vaccination 2021")

# TRANSFORMING OUR DATA TO ADJUST FOR NON STATIONARY
# Get first difference of each day and check stationarity
# Ideally, we want to have a stationary time series for modelling.
# Of course, not all of them are stationary,
# but we can make different transformations to make them stationary.
plot_diff <- Plot$new(ts_object = data_tur$ts_data_diff)
plot_diff$plot_time_series(ts_object_name = "Daily COVID Vaccination 2021")
plot_diff$test_stationarity()
plot_diff$plot_acf_pacf(ts_object_name = "ACF - PACF Daily COVID Vaccination 2021")


## Model
model <- Model$new(ts_object = data_tur$ts_data)

model$fit

model$residual_diagnostics(header = 'Residual Diagnostic Plot')
model$plot_residual_fit()
model$plot_forecast(model_name = 'ARIMA')
model$get_forecast()

