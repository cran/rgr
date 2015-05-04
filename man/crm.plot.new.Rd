\name{crm.plot.new}
\alias{crm.plot.new}
\title{ Plot Results of Control Reference Material (CRM) Analyses Adding New Data }
\description{
Function to plot new CRM data with historic CRM data.  Summary statistics are estimated for the historic data, and if an established mean and SD are not provided these are used to estimate the tolerance bounds for the new data to be plotted.  The user may define the number of SDs for the tolerance bounds, the default is two.  The user must provide the number of new CRM determinations to be plotted so that the x-axis may be extended appropriately.  The inclusive range of the new data to be plotted must be provided to ensure all the data and tolerance bounds are plotted.  The new data are plotted into the display once the function has executed using the R \code{\link{plot}} command, see Examples.
}
\usage{
crm.plot.new(xx, xname = deparse(substitute(xx)), crm.mean = NULL, 
	crm.sd = NULL, n.sd = 2, crm.new = 0, ylim = NULL, ...)
}
\arguments{
  \item{xx}{ a column vector of determinations from a data frame or matrix for a measured parameter on a CRM. }
  \item{xname}{ a title can be displayed with the plot and results, e.g., \code{xname = "Cu (mg/kg)"}.  If this field is undefined the character string for \code{xx} is used as a default. }
  \item{crm.mean}{ the recommended value for the CRM.  If not defined the mean will be estimated from the input (historic) data. }
  \item{crm.sd}{ the standard deviation associated with the recommended value for the CRM.  If not defined the SD will be estimated from the input(historic) data.  Appropriate red dotted control lines are plotted above and below the mean. }
  \item{n.sd}{ by default 2 standard deviation limits are used on the Shewart plot, alternate values may be supplied. }
  \item{crm.new}{ the number of new CRM determinations to be added to the plot of historic data.  The new data may be in several subsets, \code{crm.new} has to be the total of all new determinations to be added. }
  \item{ylim}{ the inclusive range of the new determinations to be added.  The limits for the y-axis will be determined internally taking into account the limits of the historic data and the estimated 2 standard deviation limits. }
  \item{\dots}{ any additional arguments to be passed to the \code{plot} function for titling, etc. }
}
\details{
A mean and standard deviation for the historic CRM analyses should be provided.  If they are not, estimates are made from the input (historic) data.

Following the completion of the plot and the addition of the new data, the plot may be annotated using the R \code{text} function.  In the example below, to the right of the cursor position, in red and at 80\% font size:
\cr\code{text(locator(1), "New determinations as red asterisks", adj = 0, col = 2, cex = 0.8)}

If \code{crm.new} is undefined the historic data will be plotted using either the external, or internal, estimates of the historic mean and standard deviation.

Where the input data file contains determinations for more than one CRM, either a subset for the CRM of interest must be created, e.g., with \code{\link{gx.subset}}, or the R construct \code{Cu[CRM=="X"]} must be used to pass the data to the function.
}
\note{ Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}. }
\author{ Robert G. Garrett }
\seealso{ \code{\link{crm.plot}}, \code{\link{ltdl.fix.df}} }
\examples{
## Make test data available
data(crm.test)
attach(crm.test)
data(crm.test.new)

## Determine the range of the new determinations to be plotted
range(crm.test.new$Cu)

## Plot CRM analyses time-series for 25 CRM-X determinations using
## a historic CRM mean and SD estimate, and the default 2 SD
## tolerance bounds
crm.plot.new(Cu[CRM=="X"], "Cu(mg/kg) in CRM-X", crm.mean = 34.5, crm.sd = 2.19,
crm.new = 10, ylim = c(29, 39))

## Add the 10 new CRM-X determinations to the plot as red asterisks
points(seq(26,35), crm.test.new$Cu, pch = 8, col = 2)

## Plot CRM analyses time-series for 25 CRM-X determinations and
## base the CRM mean, SD estimate and confidence bounds on those
## data, using the default 2 SD tolerance bounds
crm.plot.new(Cu[CRM=="X"], "Cu(mg/kg) in CRM-X", crm.new = 10, ylim = c(29, 39))

## Add the 10 new CRM-X determinations to the plot as red asterisks
points(seq(26,35), crm.test.new$Cu, pch = 8, col = 2)

## Detach test data
detach(crm.test)
}
\keyword{ univar }
\keyword{ hplot }
