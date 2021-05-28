# install.packages("ggplot2")
# install.packages("forecast")
# install.packages("plotly")
# install.packages("ggfortify")
# install.packages("tseries")
# install.packages("gridExtra")
# install.packages("docstring")
# install.packages("here")
# install.packages("xfun")
# install.packages("R6")

# Load libraries
library(ggplot2)
library(forecast)
library(plotly)
library(ggfortify)
library(tseries)
library(gridExtra)
library(docstring)
library(readr)
library(here)
library(R6)
library(tidyr)


# Function to fill missing values inside the data.
fill_data <- function(x) {
  x %>%
    fill(total_vaccinations) -> x
  return(x)
}

# SOURCE: https://github.com/inertia7/timeSeries_sp500_R/blob/master/src/helper_functions.R
ggtsdiag_custom <- function(object, ts_object_name, gof.lag = 10,
                            conf.int = TRUE,
                            conf.int.colour = '#0000FF', conf.int.linetype = 'dashed',
                            conf.int.fill = NULL, conf.int.alpha = 0.3,
                            ad.colour = '#888888', ad.linetype = 'dashed', ad.size = .2,
                            nrow = NULL, ncol = 1, ...) {
  rs <- stats::residuals(object)
  if (is.null(rs)) {
    rs <- object$residuals
  }
  if (is.null(rs)) {
    rs <- object$resid
  }
  
  p.std <- ggplot2::autoplot(rs, na.action = stats::na.pass,
                             ts.colour = 'turquoise4', size = 1.05) +
    ggplot2::geom_hline(yintercept = 0,
                        linetype = ad.linetype, size = ad.size,
                        colour = ad.colour) +
    labs(subtitle = '') +
    ggplot2::ggtitle(sprintf("Residual Diagnostics for %s \nNon-Standardized Residuals",
                             ts_object_name))
  
  acfobj <- stats::acf(rs, plot = FALSE, na.action = stats::na.pass)
  p.acf <- autoplot(acfobj, conf.int = conf.int,
                    conf.int.colour = conf.int.colour,
                    conf.int.linetype = conf.int.linetype,
                    conf.int.fill = conf.int.fill,
                    conf.int.alpha = conf.int.alpha,
                    colour = 'turquoise4', size = 1.25)
  p.acf <- p.acf + ggplot2::ggtitle('ACF of Residuals')
  
  nlag <- gof.lag
  pval <- numeric(nlag)
  for (i in 1L:nlag) pval[i] <- stats::Box.test(rs, i, type = "Ljung-Box")$p.value
  lb.df <- data.frame(Lag = 1L:nlag, `p value` = pval,
                      lower = -0.05, upper = 0.05)
  # Unnable to create column with space by above expression
  colnames(lb.df) <- c('Lag', 'p value', 'lower', 'upper')
  p.lb <- ggplot2::ggplot(data = lb.df, mapping = ggplot2::aes_string(x = 'Lag')) +
    ggplot2::geom_point(mapping = ggplot2::aes_string(y = '`p value`'), na.rm = TRUE,
                        colour = 'turquoise4') +
    ggplot2::scale_y_continuous(limits=c(-0.1, 1)) +
    ggplot2::ggtitle('p values for Ljung-Box statistic')
  
  p.lb <- ggfortify:::plot_confint(p = p.lb, data = lb.df, conf.int = conf.int,
                                   conf.int.colour = conf.int.colour,
                                   conf.int.linetype = conf.int.linetype,
                                   conf.int.fill = conf.int.fill, conf.int.alpha = conf.int.alpha)
  
  if (is.null(ncol)) { ncol <- 0 }
  if (is.null(nrow)) { nrow <- 0 }
  new('ggmultiplot', plots = list(p.std, p.acf, p.lb), nrow = nrow, ncol = ncol)
}