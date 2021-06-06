\name{ggtsdiag_custom}
\alias{ggtsdiag_custom}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ggtsdiag_custom
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function is taken from the fiven source code by intertia7.
SOURCE: https://github.com/inertia7/timeSeries_sp500_R/blob/master/src/helper_functions.R
}
\usage{
ggtsdiag_custom(object, ts_object_name, gof.lag = 10, conf.int = TRUE, conf.int.colour = "#0000FF", conf.int.linetype = "dashed", conf.int.fill = NULL, conf.int.alpha = 0.3, ad.colour = "#888888", ad.linetype = "dashed", ad.size = 0.2, nrow = NULL, ncol = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{object} here~~
}
  \item{ts_object_name}{
%%     ~~Describe \code{ts_object_name} here~~
}
  \item{gof.lag}{
%%     ~~Describe \code{gof.lag} here~~
}
  \item{conf.int}{
%%     ~~Describe \code{conf.int} here~~
}
  \item{conf.int.colour}{
%%     ~~Describe \code{conf.int.colour} here~~
}
  \item{conf.int.linetype}{
%%     ~~Describe \code{conf.int.linetype} here~~
}
  \item{conf.int.fill}{
%%     ~~Describe \code{conf.int.fill} here~~
}
  \item{conf.int.alpha}{
%%     ~~Describe \code{conf.int.alpha} here~~
}
  \item{ad.colour}{
%%     ~~Describe \code{ad.colour} here~~
}
  \item{ad.linetype}{
%%     ~~Describe \code{ad.linetype} here~~
}
  \item{ad.size}{
%%     ~~Describe \code{ad.size} here~~
}
  \item{nrow}{
%%     ~~Describe \code{nrow} here~~
}
  \item{ncol}{
%%     ~~Describe \code{ncol} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (object, ts_object_name, gof.lag = 10, conf.int = TRUE,
    conf.int.colour = "#0000FF", conf.int.linetype = "dashed",
    conf.int.fill = NULL, conf.int.alpha = 0.3, ad.colour = "#888888",
    ad.linetype = "dashed", ad.size = 0.2, nrow = NULL, ncol = 1,
    ...)
{
    rs <- stats::residuals(object)
    if (is.null(rs)) {
        rs <- object$residuals
    }
    if (is.null(rs)) {
        rs <- object$resid
    }
    p.std <- ggplot2::autoplot(rs, na.action = stats::na.pass,
        ts.colour = "turquoise4", size = 1.05) + ggplot2::geom_hline(yintercept = 0,
        linetype = ad.linetype, size = ad.size, colour = ad.colour) +
        labs(subtitle = "") + ggplot2::ggtitle(sprintf("Residual Diagnostics for \%s \nNon-Standardized Residuals",
        ts_object_name))
    acfobj <- stats::acf(rs, plot = FALSE, na.action = stats::na.pass)
    p.acf <- autoplot(acfobj, conf.int = conf.int, conf.int.colour = conf.int.colour,
        conf.int.linetype = conf.int.linetype, conf.int.fill = conf.int.fill,
        conf.int.alpha = conf.int.alpha, colour = "turquoise4",
        size = 1.25)
    p.acf <- p.acf + ggplot2::ggtitle("ACF of Residuals")
    nlag <- gof.lag
    pval <- numeric(nlag)
    for (i in 1L:nlag) pval[i] <- stats::Box.test(rs, i, type = "Ljung-Box")$p.value
    lb.df <- data.frame(Lag = 1L:nlag, `p value` = pval, lower = -0.05,
        upper = 0.05)
    colnames(lb.df) <- c("Lag", "p value", "lower", "upper")
    p.lb <- ggplot2::ggplot(data = lb.df, mapping = ggplot2::aes_string(x = "Lag")) +
        ggplot2::geom_point(mapping = ggplot2::aes_string(y = "`p value`"),
            na.rm = TRUE, colour = "turquoise4") + ggplot2::scale_y_continuous(limits = c(-0.1,
        1)) + ggplot2::ggtitle("p values for Ljung-Box statistic")
    p.lb <- ggfortify:::plot_confint(p = p.lb, data = lb.df,
        conf.int = conf.int, conf.int.colour = conf.int.colour,
        conf.int.linetype = conf.int.linetype, conf.int.fill = conf.int.fill,
        conf.int.alpha = conf.int.alpha)
    if (is.null(ncol)) {
        ncol <- 0
    }
    if (is.null(nrow)) {
        nrow <- 0
    }
    new("ggmultiplot", plots = list(p.std, p.acf, p.lb), nrow = nrow,
        ncol = ncol)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
