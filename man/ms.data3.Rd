\name{ms.data3}
\docType{data}
\alias{ms.data3}
\title{ Measurement Variability Test Data }
\description{
A data frame of magnetic susceptibility data used by Stanley (2003) to demonstrate the Thompson-Howarth procedure for estimating analytical variability (Thompson and Howarth, 1973 & 1978).  They are used in the rgr package examples for the duplicate analysis ANOVA and Thompson-Howarth plot functions, \code{\link{anova2}} and \code{\link{thplot2}}, respectively.  See also Garrett and Grunsky (2003). 
}
\usage{ms.data3}
\source{
Stanley (2003), see below.
}
\format{ A data frame containing 2 measurements of magnetic susceptibility for each of 16 rock samples in 32 records. The measurements for the original and duplicate analyses alternate.  So the first duplicate pair are in records 1 and 2, and the last in records 31 and 32. }
\references{
Garrett, R.G. and Grunsky, E.C., 2003. S and R functions for the display of Thompson-Howarth plots. Computers & Geosciences, 29(2):239-242.

Stanley, C.R., 2003. THPLOT.M: a MATLAB function to implement generalized Thompson-Howarth error analysis using replicate data. Computers & Geosciences, 29(2):225-237.

Thompson, M. and Howarth, R.J, 1973. The rapid estimation and control of precision by duplicate determinations. The Analyst, 98(1164):153-160.

Thompson, M. and Howarth, R.J, 1978. A new approach to the estimation of analytical precision. Journal of Geochemical Exploration, 9(1):23-30.
}
\keyword{ datasets }








