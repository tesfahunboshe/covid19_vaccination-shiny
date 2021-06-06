# Please find the test cases how to use these 3 classes (Data, Plot, Model) with hands-on experiments
# in the test.R file. This classes also used in out own packege. Therefore, if you install our package
# from out project repository located in github, you can simply use these classes without any pre-condition.
## Project Root: https://github.com/tesfahunboshe/covid19_vaccination-shiny
## Package Root: https://github.com/tesfahunboshe/covid19_vaccination-shiny/tree/main/package/ArimaUW


# This class is created to read data and transform input data to able to use in Plot and Model classes.
# DATA
Data <- R6Class("Data",
                public = list(
                  total_vaccination = NULL,
                  country = NULL,
                  country_data = NULL,
                  ts_data = NULL,
                  ts_data_diff = NULL,
                  # Constructor method:
                  initialize = function(total_vaccination, country) {
                    if (!missing(total_vaccination)) self$total_vaccination <- total_vaccination
                    if (!missing(country)) self$country <- country
                    
                    self$country_data <- self$total_vaccination %>%
                      select(location:total_vaccinations) %>%
                      filter(.,location == self$country)%>%fill_data()
                    
                    # self$country_data$date = as.Date(create_country_data$date)
                    
                    inds <- seq(as.Date(self$country_data$date[1]),
                                as.Date(self$country_data$date[length(self$country_data$date)]), by = "day")
                    ts(self$country_data$total_vaccinations,
                       start = c(2021, as.numeric(format(inds[1], "%j"))),
                       frequency = 365) -> output
                    # Create ts object data
                    self$ts_data <- output
                    self$ts_data_diff <- diff(self$ts_data)
                  },
                  
                  # This is a print method for the class to observe informaiton about the data.
                  print_data = function(x) {
                    if (is.ts(x) == TRUE){
                      sprintf("Class of input data is..: %s", class(x))
                      sprintf("Start day of input data is..: %s", start(x))
                      sprintf("End day of input data is..: %s", end(x))
                      sprintf("Total missing values in input data is..: %s", sum(x))
                      sprintf("Summary of input data is..: %s", summary(x))
                    }
                    else {
                      warning('Make sure object entered is data.frame object!')
                    }
                  }
                )
)


# This class is creted to automate sketching plot of the given data.
# This class is using ts object data, so you can create available ts object from Data class,
# and simply input this to create object of Plot class.
## PLOT
Plot <- R6Class("Plot",
                public = list(
                  ts_object = NA,
                  # Constructor method
                  initialize = function(ts_object){
                    if (!missing(ts_object)) self$ts_object <- ts_object
                  },
                  
                  # Line plot
                  # This method is used to sketch line plot of ts object.
                  # Please mind your input, defensive programming is used.
                  plot_time_series = function(ts_object_name){
                    if (is.ts(self$ts_object) == TRUE){
                      if(missing(ts_object_name)) {
                        warning('Title for plot not entered!')
                      } else {
                        startYear <- start(self$ts_object) # Grabs start date
                        endYear <- end(self$ts_object) # Grabs end date
                        tsPlot <- autoplot(self$ts_object,
                                           ts.colour = 'red',
                                           size = 1,
                                           main = sprintf("Plot of %s Time Series (%s - %s)",
                                                          ts_object_name, startYear[2], endYear[2])) +
                          theme(axis.text.x = element_text(angle = 35, hjust = 1),
                                panel.background = element_rect(fill = "gray98"),
                                axis.line.x = element_line(colour="gray"),
                                axis.line.y = element_line(colour="gray")) +
                          labs(x = "Days", y = "Vaccine Number") 
                        return(tsPlot)
                      }
                    }
                    else {
                      warning('Make sure object entered is time-series object!')
                    }
                  },
                  
                  # FUNCTION FOR ACF AND PACF PLOTS
                  # This function is taken from the fiven source code by intertia7.
                  # Source https://github.com/inertia7/timeSeries_sp500_R/blob/master/src/helper_functions.R
                  plot_acf_pacf = function(ts_object, ts_object_name){
                    if (is.ts(self$ts_object) == TRUE){
                      if(missing(ts_object_name)) {
                        warning('Title for plot not entered!')
                      } else {
                        a <- autoplot(acf(self$ts_object, plot = FALSE),
                                      colour = 'turquoise4',
                                      conf.int.fill = '#4C4CFF',
                                      conf.int.value = 0.95, conf.int.type = 'ma') +
                          theme(panel.background = element_rect(fill = "gray98"),
                                axis.line.y   = element_line(colour="gray"),
                                axis.line.x = element_line(colour="gray")) +
                          ggtitle(sprintf("ACF plot of %s", ts_object_name))
                        
                        b <- autoplot(pacf(self$ts_object, plot = FALSE),
                                      colour = 'turquoise4',
                                      conf.int.fill = '#4C4CFF',
                                      conf.int.value = 0.95, conf.int.type = 'ma') +
                          theme(panel.background = element_rect(fill = "gray98"),
                                axis.line.y   = element_line(colour="gray"),
                                axis.line.x = element_line(colour="gray")) + labs(y="PACF") +
                          ggtitle(sprintf("PACF plot of %s", ts_object_name))
                        
                        grid.arrange(a, b)
                      }
                    } else {
                      warning('Make sure object entered is time-series object!')
                    }
                  },
                  
                  # DOTPLOT
                  # This method is used to sketch dot plot of ts object.
                  # Please mind your input, defensive programming is used.
                  plot_daily_dotplot = function(ts_object_name) {
                    if (is.ts(self$ts_object) == TRUE){
                      if(missing(ts_object_name)) {
                        warning('Title for plot not entered!')
                      } else {
                        a <- boxplot(self$ts_object~cycle(self$ts_object),
                                     xlab="date",
                                     ylab = "total_vaccinations",
                                     main = ts_object_name)
                      }
                    } else {
                      warning('Make sure object entered is time-series object!')
                    }
                  },
                  
                  # Analysis of Data
                  # This method is used to automate Box-test and adf-test together
                  # in one simple method and print the result of both tests.
                  test_stationarity = function(lag=20, type = 'Ljung-Box') {
                    print(Box.test(self$ts_object, lag = lag, type = type))
                    print(adf.test(self$ts_object))
                  }
                )
)


# This class is created to automate ARIMA model build process with some smart methods and
# feature itself. 
## Model
Model <- R6Class("Model",
                 public = list(
                   ts_object = NA,
                   fit = NA,
                   # Constructor method find the optimal ARIMA model.
                   initialize = function(ts_object){
                     if (!missing(ts_object)) self$ts_object <- ts_object
                     self$fit <- auto.arima(self$ts_object)
                   },
                   
                   # Method for residual diagnostics of ARIMA model
                   # Please mind your input, defensive programming is used.
                   residual_diagnostics = function(header){
                     if (is.Arima(self$fit) == TRUE){
                       if(missing(header)) {
                         warning('Name not entered!')
                       } else {
                         ggtsdiag_custom(self$fit, header) +
                           theme(panel.background = element_rect(fill = "gray98"),
                                 panel.grid.minor = element_blank(),
                                 axis.line.y = element_line(colour="gray"),
                                 axis.line.x = element_line(colour="gray"))
                       }
                     } else {
                       warning('Make sure object entered is Arima object!')
                     }
                   },
                   
                   # This method is plot the resudual fit
                   # Please mind your input, defensive programming is used.
                   plot_residual_fit = function(binwidth = 10000) {
                     if (is.Arima(self$fit) == TRUE){
                       residFit <- ggplot(data=self$fit, aes(residuals(self$fit))) +
                       geom_histogram(aes(y =..density..),
                                      binwidth = binwidth,
                                      col="turquoise4", fill="white") +
                       geom_density(col="turquoise4") +
                       theme(panel.background = element_rect(fill = "gray98"),
                             panel.grid.minor = element_blank(),
                             axis.line   = element_line(colour="gray"),
                             axis.line.x = element_line(colour="gray")) +
                       ggtitle("Model Residuals")
                       return(residFit)
                     } else {
                       warning('Make sure object entered is Arima object!')
                     }
                   },
                   
                   # This method is used to plot forecasted values with time series data.
                   # Please mind your input, defensive programming is used.
                   plot_forecast = function(model_name, ts_name='Covid Vaccination', h=36) {
                     if (is.Arima(self$fit) == TRUE){
                       if(missing(model_name)) {
                         warning('Name not entered!')
                       } else {
                         fit_arima <- forecast(self$fit, h = h)
                         forCovid <- autoplot(fit_arima,
                                            forc_name = model_name,
                                            ts_object_name = ts_name,
                                            title = "Time series")
                         
                         return(forCovid)
                       }
                     } else {
                       warning('Make sure object entered is Arima object!')
                     }
                   },
                   
                   # This method is used to get arima forecast values.
                   # Please mind your input, defensive programming is used.
                   get_forecast = function(h = 36) {
                     if (is.Arima(self$fit) == TRUE){
                       
                       fit_arima <- forecast(self$fit, h = h)
                       return(fit_arima)
                     } else {
                       warning('Make sure object entered is Arima object!')
                     }
                   }
                 )
)
