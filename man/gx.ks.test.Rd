\name{gx.ks.test}
\alias{gx.ks.test}
\title{ Kolmogorov-Smirnov test with ECDF Plot }
\description{
Function to plot the Empirical Cumulative Distribution Functions (ECDFs) of two distributions and undertake a Kolmogorov-Smirnov test for the Hypothesis that both distributions were drawn from the same underlying distribution.
}
\usage{
gx.ks.test(xx1, xx2, xlab = " ", x1lab = deparse(substitute(xx1)),
	x2lab = deparse(substitute(xx2)), 
	ylab = "Empirical Cumulative Distribution Function", log = FALSE, 
	main = "", pch1 = 3, col1 = 2, pch2 = 4, col2 = 4,  
	ifresult = TRUE, cex = 0.8, cexp = 0.9, ...)
}
\arguments{
  \item{xx1}{ name of the first variable to be plotted - distribution to be tested. }
  \item{xx2}{ name of the second variable to be plotted - distribution to be tested. }
  \item{xlab}{ a title for the x-axis, by default none is provided.  For example, \cr\code{xlab = "Cu (mg/kg) in <2 mm C-horizon soil"}. } 
  \item{x1lab}{ the name for the first distribution to be plotted, defaults to \cr\code{x1lab = deparse(substitute(xx1))}. }
  \item{x2lab}{ the name for the second distribution to be plotted, defaults to \cr\code{x2lab = deparse(substitute(xx2))}. }
  \item{ylab}{ defaults to \cr\code{ylab = "Empirical Cumulative Distribution Function"} and may be changed if required. }
  \item{log}{ if it is required to display the data with logarithmic (x-axis) scaling, set \code{log  = TRUE}.  The Kolmogorov-Smirnov test is undertaken on untransformed data.  If it is to be undertaken on transformed data, the transformation should be applied previously or in the call, e.g., \code{log10(xx1)}, \code{sqrt(xx1)}, etc. }
  \item{main}{ a plot title if one is required, e.g., \code{main = "Kola Ecogeochemistry Project, 1995"}. }
  \item{pch1}{ the plotting symbol for the first distribution, defaults to a \sQuote{+} sign, \code{pch = 3}, and may be changed if required, see \code{\link{display.marks}}. }
  \item{col1}{ the colour of the plotting symbol for the first distribution, defaults to red, \code{col1 = 2}, and may be changed if required, see \code{\link{display.lty}}. }
  \item{pch2}{ the plotting symbol for the second distribution, defaults to a \sQuote{x} sign, \code{pch = 4}, and may be changed if required, see \code{\link{display.marks}}. }
  \item{col2}{ the colour of the plotting symbol for the second distribution, defaults to blue, \code{col2 = 4}, and may be changed if required, see \code{\link{display.lty}}. }
  \item{ifresult}{ setting \code{ifresult = FALSE} suppresses the ability to add the results of the Kolmogorov-Smirnov test to the plot, the default is \code{ifresult = TRUE}. }
  \item{cex}{ the scaling factor for the test results and legend identifying the symbology for each distribution and its population size is set to \code{cex = 0.8} by default, it may be changed if required. }
  \item{cexp}{ the scaling factor for the plotting symbol size is set to \code{cexp = 0.9} by default, if may be changed if required. }
  \item{\dots}{ further arguments to be passed to methods.  For example, the size of the axis scale annotation can be change by setting \code{cex.axis}, the size of the axis titles by seetting \code{cex.lab}, and the size of the plot title by setting \code{cex.main}.  For example, if it is required to make the plot title smaller, add \code{cex.main = 0.9} to reduce the font size by 10\%. }  
}
\details{
By default the results of the Kolmogorov-Smirnov test are added to the plot.  On completion of the ECDF plotting the cursor is activated, locate it at the centre of the area where the results are to added and \sQuote{left button} on the pointing device.  When \code{ifresult = FALSE} the cursor is not activated for this annotation; this is sometimes convenient if there is insufficient space for the results without overprinting on the ECDFs and report quality plots are required.  Also by default a legend is added to the plot, the cursor is activated and should be placed at the top left corner of area where the legend is to be added and \sQuote{left button} on the pointing device. The legend consists of two lines indicating the symbology (symbol and colour), name and size of each distribution.
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any \code{NA}s in the data vectors are removed prior to displaying the plot and undertaking the Kolmogorov-Smirnov test.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{gx.cnpplts}}, \code{\link{display.marks}}, \code{\link{display.lty}}, \code{\link{ltdl.fix.df}}, \code{\link{text}}}
\examples{
## Make test data available
data(kola.c)
attach(kola.c)

## First select data for the variable to be plotted for the subsets, from
## dimnames(kola.c) we know that Be is the 19th column in the data frame
Norway <- gx.subset(kola.c,COUNTRY=="NOR")[,19]
Russia <- gx.subset(kola.c,COUNTRY=="RUS")[,19]
Finland <- gx.subset(kola.c,COUNTRY=="FIN")[,19]

## NOTE: the examples below are commented out as gx.ks.test makes a 
## call to the locator function that fails when the examples are run
## during package checking and building
## Initial plot
## gx.ks.test(Finland, Russia, xlab = "Be (mg/kg) in <2 mm Kola C-horizon soils",
##	log = TRUE, main  = "Kola Ecogeochemistry Project, 1995")

## The same plot as above, but with the results suppressed and the
## annotation better scaled, the legend and plot symbols at 75%, the
## plot title at 90% and the axis labelling at 80%
## gx.ks.test(Finland, Russia, xlab = "Be (mg/kg) in <2 mm Kola C-horizon soils",
##	log = TRUE, main  = "Kola Ecogeochemistry Project, 1995",
##	ifresult = F, cex = 0.75, cexp = 0.75, cex.main = 0.9, cex.lab = 0.8,
##	cex.axis = 0.8)

## Clean-up and detach test data
rm(Norway)
rm(Russia)
rm(Finland)
detach(kola.c)
}
\keyword{ hplot }
\keyword{ htest }
\keyword{ nonparametric }

