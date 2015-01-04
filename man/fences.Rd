\name{fences}
\alias{fences}
\title{ Generate and Display Fence Values }
\description{
Function to generate fence values to support the selection of the upper and lower bounds of background variability, i.e. threshold(s) or action levels, when an obvious graphical solution is not visually recognizable.
}
\usage{
fences(xx, units = "ppm", display = TRUE)
}
\arguments{
  \item{xx}{ name of the variable to be processed. }
  \item{units}{ the units of measurement, options are: \dQuote{pct}, \dQuote{ppm}, \dQuote{ppb}, \dQuote{ppt}.  The default is \dQuote{ppm}. }
  \item{display}{ the default is to display the tabular output on the current device, i.e. \code{display = TRUE}.  However, when the function is used by \code{\link{fences.summary}} and in order to suppress output to the current device \code{display = FALSE} as the displayed results will be saved to a text file for subsequent use/editing and reference. }
}
\details{
The fence values are computed by several procedures both with and without a logarithic data transformation and with a logit transformation, together with the 98th percentile of the data for display.  Fences are computed following Tukey's boxplot procedure, as median +/- 2 * MAD (Median Absolute Deviation), and mean +/- 2 * SD (Standard Deviation), see Reimann et al. (2005). It is essential that these estimates be viewed in the context of the graphical distributional displays, e.g., \code{\link{shape}} and its graphical components, \code{\link{gx.hist}}, \code{\link{gx.ecdf}}, \code{\link{cnpplt}} and \code{\link{bxplot}}, and if spatial coordinates for the sample sites are available \code{\link{map.eda7}}, \code{\link{map.eda8}} and \code{\link{caplot}}. The final selection of a range for background or the selection of a threshold level needs to take the statistical and spatial distributions of the data into account. It is also necessary to be aware that it might be appropriate to have more than one background range/threshold in a study or survey (Reimann and Garrett, 2005). The presence of relevant information in the data frame may permit the data to be subset on the basis of that information for display with the \code{\link{tbplots}}, \code{\link{bwplots}} and \code{\link{gx.cnpplts}} functions. If these indicate that the medians and middle 50\%s of the data are visibly different, multiple background ranges may be advisable.
}
\note{
The logit transformation requires that the input value be in the range zero to one.  This transformation takes into consideration the closed, constant sum, nature of geochemical analytical data (Filzmoser et al., 2009).  Therefore the measurement units must be defined so that the the value can be divided by the appropriate constant.  The default is \dQuote{ppm}, and other acceptable units are \dQuote{pct}, \dQuote{ppb} and \dQuote{ppt}.  However, it should be noted that at trace element levels the differences between fences computed with logarithmic and logit transformations are small, and in most applied geochemical applications the logarithmic transformation will suffice.  This is not the case for concentrations at major element levels, where the data are more \sQuote{normally} distributed and fences will be markedly different between untransformed and logit based estimates.

Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any \code{NA}s in the data vector are removed prior to computing the fences.
}
\author{ Robert G. Garrett }
\references{
Filzmoser, P., Hron, K. and Reimann, C., 2009. Univariate statistical analysis of environmental (compositional) data: Problems and possibilities. Science of the Total Environment, 407(1/3):6100-6108.

Reimann, C. and Garrett, R.G., 2005. Geochemical background - Concept and reality. Science of the Total Environment, 350(1-3):12-27. 

Reimann, C., Filzmoser, P. and Garrett, R.G., 2005. Background and threshold: critical comparison of methods of determination.  Science of the Total Environment, 346(1-3):1-16.

Reimann, C., Filzmoser, P., Garrett, R. and Dutter, R., 2008. Statistical Data Analysis Explained: Applied Environmental Statistics with R. John Wiley & Sons, Ltd., 362 p. 
}
\seealso{ \code{\link{fences.summary}}, \code{\link{ltdl.fix.df}}, \code{\link{remove.na}}, \code{\link{logit}}, \code{\link{expit}} }
\examples{
## Make test data available
data(kola.o)
attach(kola.o)

## Display the fences computed for Cu
fences(Cu)

## Detach test data
detach(kola.o)     
}
\keyword{ univar }

