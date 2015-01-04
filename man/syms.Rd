\name{syms}
\alias{syms}
\title{ Function to Compute the Diameters of Proportional Symbols }
\description{
This function computes the diameters of the open circles to be plotted in a map or other display. 
}
\usage{
syms(z, zrange = c(NA, NA), p = 1)
}
\arguments{
  \item{z}{ name of the variable to be plotted for which diameters are to be computed. }
  \item{zrange}{ The mimimum and maximum values of z to be used as the lower and upper limits, respectively, for the computed symbol diameters.  By default the minimum and maximum values of the input data are used. }
  \item{p}{ a parameter that controls the rate of change of symbol diameter with changing value.  A default of \code{p = 1} is provided that results in a linear rate of change.  See Details below. }
}
\details{
The symbol diameter is computed as a function of the value \code{z} to be plotted:\cr
	\code{diameter = dmin + (dmax - dmin) * {(z - zmin)/(zmax - zmin)}^p}\cr
where dmin and dmax are defined as 0.1 and 1 units, so the symbol diameters range over an order of magnitude (and symbol areas over two); \code{zmin} and \code{zmax} are the observed range of the data, or the range over which the user wants the diameters to be computed; and p is a power defined by the user.  The value of \code{(z - zmin)/(zmax - zmin)} is the value of \code{z} normalized, 0 - 1, to the range over which the symbol diameters are to be computed.  After being raised to the power \code{p}, which will result in a number in the range 0 to 1, this value is multiplied by the permissable range of diameters and added to the minimum diameter.  This results in a diameter between 0.1 and 1 units that is proportional to the value of \code{z}.

A \code{p} value of \code{1} results in a linear rate of change.  Values of \code{p} less than unity lead to a rapid intial rate of change with increasing value of \code{z} which is often suitable for displaying negatively skewed data sets, see the example below.  In contrast, values of \code{p} greater than unity result in an initial slow rate of change with increasing value of \code{z} which is often suitable for displaying positively skewed data sets.  Experimentation is usually necessary to obtain a satisfactory visual effect.  See \code{\link{syms.pfunc}} for a graphic demonstrating the effect of varying the p parameter.

If \code{zmin} or \code{zmax} are defined this has the effect of setting a minimum or maximum value of \code{z}, respectively, beyond which changes in the value of \code{z} do not result in changes in symbol diameter.  This can be useful in limiting the effect of one or a few extreme outliers while still plotting them, they simply plot at the minimum or maximum symbol size and are not involved in the calculation of the range of \code{z} over which the diameter varies.
}
\value{
  \item{zdiam}{ the computed diameter of the symbol. }
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{syms.pfunc}} }  
\examples{ 
## Make test data available
data(kola.o)
attach(kola.o)

## Compute default symbol diameters
circle.diam <- syms(Cu, p = 0.3)
circle.diam

## Compute symbol diameters holding all symbols for values greater 
## than 1000 to the same size
circle.diam <- syms(Cu, zrange = c(NA, 1000), p = 0.3)
circle.diam 

## Clean-up and detach test data
rm(circle.diam)
detach(kola.o)
}
\keyword{ hplot }

