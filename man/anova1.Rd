\name{anova1}
\alias{anova1}
\title{ Analysis of Variance (ANOVA) }
\description{
Undertakes a random effects model Analysis of Variance (ANOVA) on a set of duplicate measurements to determine if the analytical, or combined sampling and analytical, (within) variability is significantly smaller than the variability across the duplicates.
}
\usage{
anova1(x1, x2, name = deparse(substitute(x1)), log = FALSE)
}
\arguments{
  \item{x1}{ a column vector from a matrix or dataframe, \code{x1[1], ..., x1[n]}. }
  \item{x2}{ another column vector from a matrix or dataframe,  \code{x2[1], ..., x2[n]}. \code{x1} and \code{x2} must be of identical length, \code{n}, where \code{x2} is a duplicate measurement of \code{x1}. }
  \item{name}{ a title can be displayed with the results, e.g., \dQuote{name = Duplicate measurements of Magnetic Susceptibility}.  If this field is undefined the character string for \code{x1} is used as a default. }
  \item{log}{ if a logarithmic transformation of the data is required to meet homogeneity of variance considerations (i.e. severe heteroscedasticity) set \code{log = TRUE}. This is also advisable if the range of the observations exceeds 1.5 orders of magnitude. }
}
\details{
In field geochemical surveys the combined sampling and analytical variability is more important than analytical variability alone. If the at site (within) variability is not significantly smaller than the between duplicate sites variability it cannot be stated that there are statistically significant spatial patterns in the data, and they are likely not suitable for mapping. This may not mean that the data cannot be used to recognize individuals with above threshold or action level observations. However, under these conditions there also may be above threshold or action level instances that the survey data have failed to detect (Garrett, 1983).

A random effects ANOVA is undertaken, the ANOVA table is displayed, together with estimates of the variance components, i.e. how much of the total variability is between and within the duplicate measurements, and the USGS mapping reliability measures of V and Vm (Miesch et al., 1976).  Additionally, the data are investigated through a two-way model following the procedure of Bolviken and Sinding-Larsen (1973).

If the data are as a single concatenated vector from a matrix or dataframe as \code{x1[1], ..., x1[n]} followed by \code{x[n+1], ..., x[2n]}, or alternated as \code{x[1]} and \code{x[2]} being a pair through to \code{x[2*i+1]} and \code{x[2*i+2]}, for the \code{i in 1:n} duplicate pairs use function \code{\link{anova2}}.
}
\note{
The script does not follow a standard computation of Mean Squares, but is based on a procedure developed after Garrett (1969) for use in the field in the 1970s when pocket calculators first had mean and standard deviation functions.

Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data vector, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Duplicate pairs \code{x1,x2} containing any \code{NA}s are omitted from the calculations.

If a log transformation is undertaken and any less than or equal to zero values occur in the data the function will halt with a warning to that effect. 
}
\author{ Robert G. Garrett }
\references{
Bolviken, B. and Sinding-Larsen, R., 1973. Total error and other criteria in the interpretation of stream sediment data. In Geochemical Exploration 1972, Institution of Mining and Metallurgy, London, pp. 285-295.

Garrett, R.G., 1969. The determination of sampling and analytical errors in exploration geochemistry. Economic Geology, 64(4):568-569.
 
Garrett, R.G., 1983. Sampling methodology. In Chapter 4 of Handbook of Exploration Geochemistry, Vol. 2, Statistics and Data Analysis in Geochemical Prospecting (Ed. R.J. Howarth), Elsevier, pp. 83-110.

Miesch, A.T. et al., 1976. Geochemical survey of Missouri - methods of sampling, analysis and statistical reduction of data. U.S. Geological Survey Professional Paper 954A, 39 p.
}
\seealso{ \code{\link{anova2}}, \code{\link{ltdl.fix.df}} }
\examples{
## Make test data available
data(ms.data1)
attach(ms.data1)

## Undertake an ANOVA for duplicate measurements on rock samples
anova1(MS.1, MS.2, log = TRUE, 
	name = "Duplicate measurements of Magnetic Susceptibility")

## Detach test data 
detach(ms.data1)
}
\keyword{ univar }
\keyword{ htest }

