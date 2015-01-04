\name{ltdl.fix}
\alias{ltdl.fix}
\title{ Replace Negative Values Representing Less Than Detects in a Vector }
\description{
Function to process a numeric vector to replace negative values representing less than detects (\code{<}DLs) with positive half that value.  This permits processing of these effectively categorical data as real numbers and their display on logarithmically scaled axes.  Some software packages replace blank fields that should be interpreted as \code{NA}s, i.e. no information, with zeros, the facility is provided to replace any zero values with \code{NA}s.  In other instances data files have been built using an integer code, e.g., -9999, to indicate 'no data', i.e. the equivalent of \code{NA}s.  The facility is provided to replace any so coded values with \code{NA}s.  If required, all \code{<}values and may be replaced with\code{NA}s, e.g. when estimating analytical precision with \code{\link{anova1}} using only duplicate analyses with \code{>}DL values.

A report of the changes made is displayed on the current device.

For processing data matrices or data frames, see \code{\link{ltdl.fix.df}}. 
}
\usage{
ltdl.fix(x, negs2na = FALSE, zero2na = FALSE, coded = NA)
}
\arguments{
  \item{x}{ name of the vector to be processed. }
  \item{negs2na}{ to replace any -ve values with \code{NA}s, set \code{negs2na = TRUE}. }
  \item{zero2na}{ to replace any zero values with \code{NA}s, set \code{zero2na = TRUE}. }
  \item{coded}{ to replace any numeric coded values, e.g., -9999 with \code{NA}s, set \code{coded = -9999}. }
}
\value{
A numeric vector identical to that input but where any negative values have been replaced by half their positive values, or optionally by \code{NA}s, and optionally any zero or numeric coded values have been replaced by \code{NA}s.
}
\note{
If data are being accessed through an ODBC link to a database, rather than from a data frame that can be processed by \code{\link{ltdl.fix.df}}, it may be important to run this function on the retrieved vector prior to any subsequent processing.  The necessity for such vector processing can be ascertained using the range function, e.g., \code{range(na.omit(x))} or \code{range(x, na.rm = TRUE)}, where x is the variable name, to determine the presence of any negative values.  The presence of any \code{NA}s in the vector will return \code{NA}s by \code{\link{range}} if they are not removed.

Great care needs to be taken when processing data where a large proportion of the data are less than detects (\code{<}value).  In such cases parametric statistics have limited value, and can be missleading.  Records should be kept of variables containing \code{<}values, and the fixed replacement values changed in tables for reports to the appropriate \code{<}values.  Thus, in tables of percentiles the \code{<}value should replace the fixed value computed from absolute(-value)/2.  Various rules have been proposed as to how many less than detects treated in this way can be tolerated before means, variances, etc. become biassed and of little value.  Less than 5\% in a large data set is usually tolerable, with greater than 10\% concern increases, and with greater than 20\% alternate procedures for processing the data should be sought, for example, the procedures outlined in Helsel (2005).  Alternately replacement non-detect values may be imputed with R packages \code{robCompositions} (Hron et al., 2010; Templ et al., 2011) or \code{zCompositions} (Martin-Fernandez et al., 2012; Palarea-Albaladejo and Martin-Fernandez, 2013), specifically function \code{lrEM} in the latter, both may be used with closed, constant-sum, data.
}
\author{ Robert G. Garrett }
\references{
Helsel, D.R., 2005. Nondetects and Data Analysis: Statistics for Censored Data. John Wiley & Sons, Ltd., 250 p.

Hron, K., Templ, M. and Filzmoser, P., 2010. Imputation of missing values for compositional data using classical and robust methods. Computational Statistics and Data Analysis, 54(12):3095-3107.
3107.

Templ, M., Hron, K. and Filzmoser, P., 2010. robCompositions: An R-package for Robust Statistical Analysis of Composition data. In: Compositional Data Analysis: Theory and Applications, V. Pawowsky and A. Buccianti (Eds.), Chapter 25, pp. 342-355.  John Wiley & Sons, Ltd. 

Martin-Fernadez, J.A., Hron, K., Templ, M., Filzmoser, P. and Palarea-Albaladejo, J., 2012. Model-based replacement of rounded zeros in compositional data: classical and robust approaches. Computational Statistics and Data Analysis, 56(9):2688-2704.

Palarea-Albaladejo, J. and Martin-Fernandez, J.A., 2013. Values below detection limit in compositional chemical data.  Analytica Chemica Acta, 764:32-43.
}
\seealso{ \code{\link{ltdl.fix.df}} }
\examples{
## Replace any missing data coded as -9999 with NAs and any remaining
## negative values representing less than detects with Abs(value)/2
data(fix.test)
x.fixed <- ltdl.fix(fix.test[, 3], coded = -9999)
x.fixed

## As above, and replace any zero values with NAs
x.fixed <- ltdl.fix(fix.test[, 3], coded = -9999, zero2na = TRUE)
x.fixed

## As above, but replace any negative values with NAs
x.fixed <- ltdl.fix(fix.test[, 3], negs2na = TRUE, coded = -9999, zero2na = TRUE)
x.fixed

## Make test data kola.o available, setting a -9999, indicating a
## missing pH measurement, to NA
data(kola.o)
attach(kola.o)
pH.fixed <- ltdl.fix(pH, coded = -9999)

## Display relationship between pH in one pH unit intervals and Cu in 
## O-horizon (humus) soil, extending the whiskers to the 2nd and 98th
## percentiles, finally removing the temporary data vector pH.fixed
bwplots(split(Cu,trunc(pH.fixed+0.5)), log = TRUE, wend = 0.02, 
	xlab = "Soil pH to the nearest pH unit",
	ylab = "Cu (mg/kg) in < 2 mm Kola O-horizon soil")
rm(pH.fixed)

## Or directly
bwplots(split(Cu,trunc(ltdl.fix(pH, coded = -9999)+0.5)), log = TRUE,  
	wend = 0.02, xlab = "Soil pH to the nearest pH unit",
	ylab = "Cu (mg/kg) in < 2 mm Kola O-horizon soil")

## Clean-up and detach test data
rm(x)
rm(x.fixed)
rm(pH.fixed)
detach(kola.o)
}
\keyword{ misc }
 

