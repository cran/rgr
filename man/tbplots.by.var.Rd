\name{tbplots.by.var}
\alias{tbplots.by.var}
\title{ Plot Vertical Tukey Boxplots for Variables}
\description{
Plots a series of vertical Tukey boxplots where the individual boxplots represent the data subdivided by variables.  Optionally the y-axis may be scaled logarithmically (base 10).  A variety of other plot options are available, see Details and Note below.
}
\usage{
tbplots.by.var(xmat, log = FALSE, logx = FALSE, notch = FALSE, 
	xlab = "Measured Variables", ylab = "Reported Values", 
	main = "", label = NULL, plot.order = NULL, xpos = NA, 
	las = 1, cex = 1, adj = 0.5, colr = 8, ...)
}
\arguments{
  \item{xmat}{ the data matrix or data frame containing the data (variables). }
  \item{log}{ if it is required to display the data with logarithmic (y-axis) scaling, set \code{log = TRUE}. } 
  \item{logx}{ if the positions of the Tukey boxplot fences are to be computed on the basis of log transformed data set \code{logx = TRUE}. For general usage, if \code{log = TRUE} then set \code{logx = TRUE}. }
  \item{notch}{ determines if the boxplots are to be \dQuote{notched} such that the notches indicate the 95\% confidence intervals for the medians. The default is not to notch the boxplots, to have notches set \code{notch = TRUE}. }
  \item{xlab}{ a title for the x-axis, by default \code{xlab = "Measured Variables"}. }
  \item{ylab}{ a title for the y-axis, by default \code{ylab = "Reported Values"}. }
  \item{main}{ a main title may be added optionally above the display by setting \code{main}, e.g., \code{main = "Kola Project, 1995"}. }
  \item{label}{ by default the character strings defining the variables are used to label the boxplots along the x-axis.  Alternate labels can be provided with \code{label = c("Alt1", "Alt2", "Alt3")}, see Examples. }
  \item{plot.order}{ provides an alternate order for the boxplots.  By default the boxplot are plotted in alphabetical order of the factor variables.  Thus, \code{plot.order = c(2, 1, 3)} will plot the 2nd alphabetically ordered factor in the 1st position, the 1st in the 2nd, and the 3rd in its alphabetically 3rd ordered postion. }
  \item{xpos}{ the locations along the x-axis for the individual vertical boxplots to be plotted.  By default this is set to \code{NA}, which causes default equally spaced positions to be used, i.e. boxplot 1 plots at value 1 on the x-axis, boxplot 2 at value 2, etc., up to boxplot \dQuote{n} at value \dQuote{n}.  See Details below for defining \code{xpos}. }
  \item{las}{ controls whether the x-axis labels are written parallel to the x-axis, the default \code{las = 1}, or are written down from the x-axis by setting \code{las = 2}.  See also, Details below.}
  \item{cex}{ controls the size of the font used for the factor labels plotted along the x-axis.  By default this is \code{1}, however, if the labels are long it is sometimes necessary to use a smaller font, for example \code{cex = 0.8} results in a font 80\% of normal size. }
  \item{adj}{ controls the justification of the x-axis labels.  By default they are centred, \code{adj = 0.5}, to left justify them if the labels are written downwards set \code{adj = 0}. }
  \item{colr}{ by default the boxes are infilled in grey, \code{colr = 8} .  If no infill is required, set \code{colr = 0}.  See \code{\link{display.lty}} for the range of available colours. }
  \item{\dots}{ further arguments to be passed to methods.  For example, the size of the axis titles by setting \code{cex.lab}, and the size of the plot title by setting \code{cex.main}.  For example, if it is required to make the plot title smaller, add \code{cex.main = 0.9} to reduce the font size by 10\%. }
}
\details{
There are two ways to provide data to this function.  Firstly, if all the variables in a data frame are to be displayed, and there are no factor variables, the data frame name can be entered for \code{xmat}.  However, if there are factor variables, or only a subset of the variables are to be displayed, the data are entered via the \code{\link{cbind}} construct, see Examples below.

Long variable names can lead to display problems, changing the \code{las} parameter from its default of \code{las = 1} which plots subset labels parallel to the axis to \code{las = 2}, to plot perpendicular to the axis, can help.  It may also help to use \code{label} and split the character string into two lines, e.g., by changing the string \code{"Specific Conductivity"} that was supplied to replace the variable name \code{SC} to \code{"Specific\\nConductivity"}.  If this, or setting \code{las = 2}, causes a conflict with the x-axis title, if one is needed, the title can be moved down a line by using \code{xlab = "\\nPhysical soil properties"}.  In both cases the \code{\\n} forces the following text to be placed on the next lower line.

If there are more than 7 labels (variables) and no alternate labels are provided \code{las} is set to \code{2}, otherwise some variable names may fail to be displayed.

The notches in the boxplots indicate the 95\% confidence intervals for the medians and can extend beyond the upper and lower limits of the boxes indicating the middle 50\% of the data when subset population sizes are small.  The confidence intervals are estimated using the binomial theorem.  It can be argued that for small populations a normal approximation would be better.  However, it was decided to remain with a non-parametric estimate despite the fact that the calculation of the Tukey fence values involves normality assumptions.
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any \code{NA}s in the data vectors are removed prior to preparing the boxplots.

For a summary statistics display to complement the graphics see \code{\link{gx.summary.mat}}.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{tbplots}}, \code{\link{var2fact}}, \code{\link{ltdl.fix.df}} } 
\examples{
## Make test data kola.c available
data(kola.c)
attach(kola.c)

## Display a simple Tukey boxplot for measured variables
tbplots.by.var(cbind(Co,Cu,Ni))

## Display a more appropriately labelled and scaled Tukey boxplot
tbplots.by.var(cbind(Co,Cu,Ni), log = TRUE, logx = TRUE,
	ylab = "Contentrations (mg/kg) in <2 mm Kola C-horizon soil")

## Detach test data kola.c
detach(kola.c)

## Make test data ms.data1 available
data(ms.data1)

## Display variables in a data frame, remembering to omit the
## sample IDs
tbplots.by.var(ms.data1[, -1], log=TRUE, logx = TRUE)
}
\keyword{ hplot }

