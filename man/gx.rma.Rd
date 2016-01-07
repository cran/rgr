\name{gx.rma}
\alias{gx.rma}
\alias{gx.youden}
\alias{youden}
\alias{youden.plot}
\alias{youden-plot}
\alias{youden_plot}
\title{ Estimate the Coefficients of the Reduced Major Axis }
\description{
Function to estimate the coefficients and their standard errors of the Reduced Major Axis, the case of orthogonal regression, and also known as total least squares or errors in variables regression.  The procedure is based on the methodology described in Miller and Kahn (1962).  Function will also prepare a Youden plot. 
}
\usage{
gx.rma(xx1, xx2, x1lab = NULL, x2lab = NULL, log = FALSE, if.plot = FALSE,
if.rma = FALSE, if.coeffs = FALSE, ...)
}
\arguments{
  \item{xx1}{ the name of the first independent variable, or a n by 2 matrix of independent variables, for instance from \code{\link{alts2dups}}. }
  \item{xx2}{ the name of the second independent variable, omit if \code{xx1} is a matrix. }
  \item{x1lab}{ a title for the first independent variable, the default is the variable name, \cr\code{deparse(substitute(xx1))} or the name from the first column of the n by 2 matrix. It is often desirable to replace the default title of the input variable name text string with a more informative title, e.g., \cr\code{x1lab = "Magnetic Susceptibility - Measurement 1"}. }
  \item{x2lab}{ a title for the second independent variable, the default is the variable name, \code{deparse(substitute(xx2))} or the name from the second column of the n by 2 matrix. It is often desirable to replace the default title of the input variable name text string with a more informative title, e.g.,\cr\code{x2lab = "Magnetic Susceptibility - Measurement 2"}. }
  \item{log}{ if a logarithmic transformation (base 10) of the data is required to meet homogeneity of variance considerations (i.e. severe heteroscedasticity) set \code{log = TRUE}. This is also advisable if the range of the observations exceeds 1.5 orders of magnitude. }
  \item{if.plot}{ if a x-y plot of the independent variables is required set \code{if.plot = TRUE}.  The plot is equi-scaled and the 1:1 line is added. If \code{if.rma = TRUE} \code{if.plot} will be set to \code{TRUE}. }
  \item{if.rma}{ if the Reduced Major Axis is to be added to the plot set \code{if.rma = TRUE}.  }
 \item{if.coeffs}{ if the coefficients of the Reduced Major Axis and the result of the test for the RMA being (0,1) are to be displayed on the plot, set \code{if.coeffs = TRUE}.  The cursor will be activated, and the upper left corner of the text block may be placed where most appropriate, then \sQuote{left button} the pointing device. }
 \item{\dots}{ further arguments to be passed to methods concerning the plot.  For example, if it is required to add a title, add \code{main = "Youden Plot"}. }
}
\value{
A list comprising of:
  \item{alen}{ the data set size. }
  \item{mean}{ a two-element vector with the means of x1 and x2. }
  \item{sd}{ a two-element vector with the standard deviations of x1 and x2. }
  \item{corr}{ the Pearson correlation coefficient for x1 and x2. }
  \item{a0}{ the intercept of the reduced major axis. }
  \item{a1}{ the slope of the reduced major axis. }
  \item{sea0}{ the standard error of the intercept estimate. }
  \item{aea1}{ the standard error of the slope estimate. }
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any data pairs, \code{xx1,xx2}, containing any \code{NA}s are omitted from the calculations.

If a log transformation is undertaken and any less than or equal to zero values occur in the data the function will halt with a warning to that effect. 

The coefficients may be used to plot the RMA on a x-y plot of the two measures, see example below.
}
\references{
Miller, R.L. and Kahn, J.S., 1962. Statistical Analysis in the Geological Sciences, John Wiley & Sons, New York, U.S.A., 483 p. Specifically pp. 204-209.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{ltdl.fix.df}}, \code{\link{remove.na}} } 
\examples{
## Make test available
data(ms.data1)
attach(ms.data1)

## Estimate RMA coefficients for duplicate measurements on rock samples
gx.rma(MS.1, MS.2, x1lab = "MS - 1", x2lab = "MS - 2", log = TRUE)

## Display an x-y plot of the data and the RMA
gx.rma(MS.1, MS.2, x1lab = "MS - 1", x2lab = "MS - 2", log = TRUE, 
	if.plot = TRUE, if.rma = TRUE)

## Detach test data
detach(ms.data1)

## Using function alts2dups
data(ms.data3)
attach(ms.data3)
gx.rma(alts2dups(MS, ifalt = TRUE), log = TRUE, if.plot = TRUE,
	if.rma = TRUE)

## Detach test data
detach(ms.data3)
}
\keyword{ regression }
\keyword{ htest }

