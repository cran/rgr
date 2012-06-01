\name{fences.summary}
\alias{fences.summary}
\title{ Generate and Save Fence Values for Data Subsets }
\description{
Function to generate fences and save the values in the R working directory for subsets of the data for a variable when the data can be subdivided by some criterion (factor) such as EcoRegion, Province, physical sample parent material, etc.  The function supports the selection of the upper and lower bounds of background variability, and threshold(s) or action levels, when obvious graphical solutions are not visually recognizable.
}
\usage{
fences.summary(group, x, file = NULL, units = "ppm")
}
\arguments{
  \item{group}{ the name of the factor variable by which the data are to be subset. }
  \item{x}{ name of the variable to be processed. }
  \item{file}{ the first part of the file name identifying the data source for saving the function output in the R working directory, see Details below. }
  \item{units}{ the units of measurement, options are: \dQuote{pct}, \dQuote{ppm}, \dQuote{ppb}, \dQuote{ppt}.  The default is \dQuote{ppm}. }
}
\details{
The fence values are computed by several procedures both with and without a logarithic data transformation and with a logistic transformation, together with the 98th percentile of the data for display. These computations are based on results returned from function \code{\link{gx.stats}}. Fences are computed following Tukey's boxplot procedure, as median +/- 2 * MAD (Median Absolute Deviation), and mean +/- 2 * SD (Standard Deviation), see Reimann et al. (2005). It is essential that these estimates are viewed in the context of the graphical distributional displays, e.g., \code{\link{shape}} and its graphical components, \code{\link{gx.hist}}, \code{\link{gx.ecdf}}, \code{\link{cnpplt}} and \code{\link{bxplot}}, and if spatial coordinates for the sample sites are available \code{\link{map.eda7}}, \code{\link{map.eda8}} and \code{\link{caplot}}. The final selection of a range for background or the selection of a threshold level needs to take the statistical and spatial distributions of the data into account.  It is also necessary to be aware that it might be appropriate to have more than one background range/threshold in an area (Reimann and Garrett, 2005). The presence of relevant information in the data frame may permit the data to be subset on the basis of that information for display with the \code{\link{tbplots}}, \code{\link{bwplots}} and \code{\link{gx.cnpplts}} functions. If these indicate that the medians and middle 50\%s of the data are visibly different, multiple background ranges may be advisable.

A default file name is generated from the data frame, group and variable (x) names, \cr\code{data frame name_deparse(substitute(group))_deparse(substitute(x))_fences.txt}. If \code{file} contains text it is used as the first part of the file name identifying the data source for the file to be saved in the specified folder, for example, \code{file = "D://R_work//Project3//C_soils"}.  If no folder is specified the file is saved in the R working directory.

Output to the current device is suppressed.  The output file is formatted as a tab delimited file to be read with a spread sheet program.  It can be inspected with a text viewer, and column spacings edited for cosmetic purposes with an ASCII editor of the user's choice. 
}
\note{
The logit transformation requires that the input value be in the range zero to one.  This transformation takes into consideration the closed, constant sum, nature of geochemical analytical data (Filzmoser et al., 2009).  Therefore the measurement units must be defined so that the the value can be divided by the appropriate constant.  The default is \dQuote{ppm}, and other acceptable units are \dQuote{pct}, \dQuote{ppb} and \dQuote{ppt}.  However, it should be noted that at trace element levels the differences between fences computed with logarithmic and logit transformations are small, and in most applied geochemical applications the logarithmic transformation will suffice.  This is not the case for concentrations at major element levels, where the data are more \sQuote{normally} distributed and fences will be markedly different between untransformed and logit based estimates.

Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any \code{NA}s in the data vector are removed prior to computing the fences.

The function \code{\link{fences}} is employed to compute the statistical fence estimates.
}
\author{ Robert G. Garrett }
\references{
Filzmoser, P., Hron, K. and Reimann, C., 2009. Univariate statistical analysis of environmental (compositional) data: Problems and possibilities. Science of the Total Environment, 407(1/3):6100-6108.

Reimann, C. and Garrett, R.G., 2005. Geochemical background - Concept and reality. Science of the Total Environment, 350(1-3):12-27. 

Reimann, C., Filzmoser, P. and Garrett, R.G., 2005. Background and threshold: critical comparison of methods of determination. Science of the Total Environment, 346(1-3):1-16.

Reimann, C., Filzmoser, P., Garrett, R. and Dutter, R., 2008. Statistical Data Analysis Explained: Applied Environmental Statistics with R. John Wiley & Sons, Ltd., 362 p. 
} 
\seealso{ \code{\link{fences}}, \code{\link{ltdl.fix.df}}, \code{\link{remove.na}} }
\examples{
## Make test data available
data(kola.c)
attach(kola.c)

## Saves the file kola_c_COUNTRY_Cu_fences.txt for later use
## in the R working directory.
fences.summary(COUNTRY, Cu, file = "Kola_c_horizon")

## Detach test data 
detach(kola.c)
}
\keyword{ univar }
