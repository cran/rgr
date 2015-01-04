\name{crm.plot}
\alias{crm.plot}
\title{ Plot Results of Control Reference Material (CRM) Analyses }
\description{
Function to plot the results of Control Reference material (CRM) analyses in the order in which they occur in a file, assuming that this order is a time-series, so that the presence of \sQuote{drift} may be recognized, in addition to the presence of gross outliers reflecting \sQuote{analytical problems}.  The data are plotted as either values, when the associated standard deviation of the CRM is provided, or percent absolute relative difference from the \sQuote{recommended} value when a target tolerance level is provided (see below).  The expected \sQuote{recommended} value (long-term mean) for the CRM being displayed must be supplied, together with its associated standard deviation, or a target tolerance level for percent absolute relative difference.  By default the CRM recommended value and standard deviation are used to plot red dashed lines at the recommended value +/- 2 standard deviations, and a green line for the recommended value, alternate standard deviation multiples may be provided.  This display is also known as a Shewart plot. 
}
\usage{
crm.plot(xx, xname = deparse(substitute(xx)), crm.mean = NULL, 
	crm.sd = NULL, n.sd = 2, crm.tol = NULL, ...)
}
\arguments{
  \item{xx}{ a column vector of determinations from a data frame or matrix for a measured parameter on a CRM. }
  \item{xname}{ a title can be displayed with the plot and results, e.g., \code{xname = "Cu (mg/kg)"}.  If this field is undefined the character string for \code{xx} is used as a default. }
  \item{crm.mean}{ the recommended value for the CRM.  A value must be provided, otherwise the function will terminate. }
  \item{crm.sd}{ the standard deviation associated with the recommended value for the CRM. Appropriate red dotted control lines are plotted above and below the mean. }
  \item{n.sd}{ by default 2 standard deviation limits are used on the Shewart plot, alternate values may be supplied. }
  \item{crm.tol}{ optionally a percentage tolerance level my be provided for the maximum acceptable absolute relative percent difference from the CRM recommended value, in which case a red dotted control line is added to the plot. }
  \item{\dots}{ any additional arguments to be passed to the \code{plot} function for titling, etc. }
}
\details{
Either a standard deviation for the CRM analyses or an upper limit tolerance level must be provided, otherwise the function will fail.  If both are provided an percentage absolute relative difference plot is displayed.

Where the input data file contains determinations for more than one CRM, either a subset for the CRM of interest must be created, e.g., with \code{\link{gx.subset}}, or the R construct \code{Cu[CRM=="X"]} must be used to pass the data to the function.
}
\note{ Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}. }
\author{ Robert G. Garrett }
\seealso{ \code{\link{ltdl.fix.df}} }
\examples{
## Make test data available
data(crm.test)
attach(crm.test)

## Plot CRM analyses time-series for CRM-X using the CRM SD estimate
## and the default 2 SD tolerance bounds
crm.plot(Cu[CRM=="X"], "Cu(mg/g) in CRM-X", crm.mean = 34.5, crm.sd = 2.19)

## Plot CRM analyses time-series for CRM-X using the CRM SD estimate
## and 2.5 SD tolerance bounds
crm.plot(Cu[CRM=="X"], "Cu(mg/g) in CRM-X", crm.mean = 34.5, crm.sd = 2.19,
n.sd = 2.5)

## Plot CRM analyses time-series for CRM-X using a maximum acceptable
## percantage tolerance bound
crm.plot(Cu[CRM=="X"], "Cu(mg/g) in CRM-X", crm.mean = 34.5, crm.tol = 15)

## Detach test data
detach(crm.test)
}
\keyword{ univar }
\keyword{ hplot }
