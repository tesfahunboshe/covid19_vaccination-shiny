# DATA
Data <- R6Class("Data",
                public = list(
                  total_vaccination = NULL,
                  country = NULL,
                  country_data = NULL,
                  ts_data = NULL,
                  ts_data_diff = NULL,
                  initialize = function(total_vaccination, country) {
                    if (!missing(total_vaccination)) self$total_vaccination <- total_vaccination
                    if (!missing(country)) self$country <- country
                  },
                  
                  create_country_data = function() {
                    if (is.data.frame(self$total_vaccination) == TRUE){
                      self$total_vaccination %>%
                        select(location:total_vaccinations) %>%
                        filter(.,
                               location == self$country) -> c_data
                      c_data <- fill_data(c_data)
                      c_data$date <- as.Date(c_data$date)
                      self$country_data <- c_data
                    }
                    else {
                      warning('Make sure object entered is data.frame object!')
                    }
                  },
                    
                  create_ts = function(){
                    if (is.data.frame(self$country_data) == TRUE){
                      inds <- seq(as.Date(self$country_data$date[1]),
                                  as.Date(self$country_data$date[length(self$country_data$date)]), by = "day")
                      ts(self$country_data$total_vaccinations,
                         start = c(2021, as.numeric(format(inds[1], "%j"))),
                         frequency = 365) -> output
                      self$ts_data <- output
                      self$ts_data_diff <- diff(self$ts_data)
                    }
                    else {
                      warning('Make sure object entered is data.frame object!')
                    }
                  },                
                  
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


## PLOT
Plot <- R6Class("Plot",
                public = list(
                  ts_object = NA,
                  initialize = function(ts_object){
                   if (!missing(ts_object)) self$ts_object <- ts_object
                 },
                 
                 # Line plot
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
                 test_stationarity = function(lag=20, type = 'Ljung-Box') {
                   print(Box.test(self$ts_object, lag = lag, type = type))
                   print(adf.test(self$ts_object))
                 }
                 
                )
)


## Model
Model <- R6Class("Model",
                 public = list(
                   ts_object = NA,
                   fit = NA,
                   initialize = function(ts_object){
                     if (!missing(ts_object)) self$ts_object <- ts_object
                     self$fit <- auto.arima(self$ts_object)
                   },
                   
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
                         ggtitle("Plot of COVID Vaccination Model Residuals")
                       return(residFit)
                     } else {
                       warning('Make sure object entered is Arima object!')
                     }
                   },
                   
                   plot_forecast = function(model_name, ts_name='Covid Vaccination', h=36) {
                     if (is.Arima(self$fit) == TRUE){
                       if(missing(model_name)) {
                         warning('Name not entered!')
                       } else {
                         fit_arima <- forecast(self$fit, h = h)
                         forCovid <- autoplot(fit_arima,
                                              forc_name = model_name, 
                                              ts_object_name = ts_name)
                         return(forCovid)
                       }
                     } else {
                       warning('Make sure object entered is Arima object!')
                     }
                   },
                   
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
