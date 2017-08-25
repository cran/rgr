\name{rgr_1.1.14-package}
\alias{rgr_1.1.14}
\alias{rgr_package}
\alias{rgr-1.1.14-package}
\alias{rgr-1.1.14}
\alias{rgr-package}
\alias{rgr}
\docType{package}
\title{
Geological Survey of Canada Applied Geochemistry EDA
}
\description{
Functions for Exploratory Data Analysis with applied geochemical data.
}
\details{
This package supports the display and analysis of applied geochemical survey data, particularly in the context of estimating the ranges of background variation due to natural phenomena and the identification of outliers that may be due to natural processes or anthropogenic contamination.  Functions are provided for use with univariate and multivariate data, in the latter context tools are provided for compositional, constant sum, data.  Additional functions support analytical data QA/QC, ANOVA for investigations of field sampling and analytical variability, and utility tasks.  NOTE: function caplot for concentration-area plots employs package akima, however, akima is only licensed for not-for-profit use.  Therefore, not-for-profit users of rgr will have to independently make package akima available through library(akima); and use of function caplot by for-profit users will fail.
\tabular{ll}{
Package: \tab rgr_1.1.14\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2017-06-13\cr
License: \tab GPL-2\cr
}
}
\author{
Robert G. Garrett <robert.garrett@canada.ca>
}
\references{
Garrett, R.G., 2013. The 'rgr' package for the R Open Source statistical computing and graphics environment - a tool to support geochemical data interpretation. Geochemistry: Exploration, Environment, Analysis, 13(4):355-378, doi 10.1144/geochem2011-106

Reimann, C., Filzmoser, P. and Garrett, R.G., 2005. Background and threshold: critical comparison of methods of determination. Science of the Total Environment, 346(1/3):1-16.

Reimann, C. and Garrett, R.G., 2005. Geochemical background - Concept and reality. Science of the Total Environment, 350(1/3):12-27.

Reimann, C., Filzmoser, P., Garrett, R. and Dutter, R., 2008. Statistical Data Analysis Explained: Applied Environmental Statistics with R. John Wiley & Sons, Ltd., 362 p.

Venables, W.N. and Ripley, B.D., 2001. Modern Applied Statistics with S-Plus, 3rd Edition, Springer, 501 p.
}
\keyword{ package }

