\name{bwplots}
\alias{bwplots}
\title{ Plot Vertical Box-and-Whisker Plots }
\description{
Plots a series of vertical box-and-whisker plots (Garrett, 1988) where the individual plots represent the data subdivided by the value of some factor.  Optionally the y-axis may be scaled logarithmically (base 10). A variety of other plot options are available, see Details and Note below.
}
\usage{
bwplots(x, by, log = FALSE, wend = 0.05, notch = TRUE, xlab = "", 
	ylab = deparse(substitute(x)), ylim = NULL, main = "",
	label = NULL, plot.order = NULL, xpos = NA, width, 
	space = 0.25, las = 1, cex.axis = 1, adj = 0.5, add = FALSE, 
	ssll = 1, colr = 8, pch = 3, ...)
}
\arguments{
  \item{x}{ name of the variable to be plotted. }
  \item{by}{ the name of the factor variable to be used to subdivide the data.  See Details below for when \code{by} is undefined. }
  \item{log}{ to display the data with logarithmic (y-axis) scaling, set \code{log = TRUE}. } 
  \item{wend}{ the locations of the whisker-ends have to be defined.  By default these are at the 5th and 95th percentiles of the data.  Setting \code{wend = 0.02} plots the whisker ends at the 2nd and 98th percentiles. }
  \item{notch}{ determines if the boxplots are to be \dQuote{notched} such that the notches indicate the 95\% confidence intervals for the medians. The default is to notch the boxplots, to suppress the notches set \code{notch = FALSE}.  See Details below. }
  \item{xlab}{ a title for the x-axis, by default none is provided. A title may be provided, see Examples.}
  \item{ylab}{ by default the character string for \code{x} is used for the y-axis title.  An alternate title can be displayed with \code{ylab = "text string"}, see Examples. }
  \item{ylim}{ only for \code{log = FALSE}, defines the limits of the y-axis if the default limits based on the range of the data are unsatisfactory. It can be used to ensure the y-axis scaling in multiple sets of boxplots are the same to facilitate visual comparison. }
  \item{main}{ a main title may be added optionally above the display by setting \code{main}, e.g., \code{main = "Kola Project, 1995"}. }
  \item{label}{ by default the character strings defining the factors are used to label the boxplots along the x-axis.  Alternate labels can be provided with \cr\code{label = c("Alt1", "Alt2", "Alt3")}, see Examples. }
  \item{plot.order}{ provides an alternate order for the boxplots. Thus, \code{plot.order = c(2, 1, 3)} will plot the 2nd ordered factor in the 1st position, the 1st in the 2nd, and the 3rd in its 3rd ordered postion, see Details and Examples below. }
  \item{xpos}{ the locations along the x-axis for the individual vertical boxplots to be plotted.  By default this is set to \code{NA}, which causes default equally spaced positions to be used, i.e. boxplot 1 plots at value 1 on the x-axis, boxplot 2 at value 2, etc., up to boxplot \dQuote{n} at value \dQuote{n}.  See Details below for defining \code{xpos}. }
  \item{width}{ the width of the boxes, by default this is set to the minimum distance between all adjacent boxplots times the value of \code{space}.  With the default values of \code{xpos} this results in a minimum difference of \code{1}, and with the default of \code{space = 0.25} the width is computed as 0.25.  To specify different widths for all boxplots use, for example, \code{width = c(0.3)}.  See Details below for changing individual boxplot widths. }
  \item{space}{ the space between the individual boxplots, by default this is 0.25 x-axis units.  }
  \item{las}{ controls whether the x-axis labels are written parallel to the x-axis, the default \code{las = 1}, or are written down from the x-axis by setting \code{las = 2}.  See also, Details below.}
  \item{cex.axis}{ controls the size of the font used for the factor labels plotted along the x-axis.  By default this is \code{1}, however, if the labels are long it is sometimes necessary to use a smaller font, for example \code{cex.axis = 0.8} results in a font 80\% of normal size. }
  \item{adj}{ controls justification of the x-axis labels.  By default they are centred, \code{adj = 0.5}, to left justify them if the labels are written downwards set \code{adj = 0}. }
  \item{add}{ permits the user to plot additional boxplots into an existing display.  It is recommended that this option is left as \code{add = FALSE}. }
  \item{ssll}{ determines the minimum data subset size for which a subset will be plotted.  By default this is set to \code{1}, which leads to only a circle with a median bar being plotted, as the subset size increases additional features of the box-and-whisker plot are displayed.  If \code{ssll} results in subset boxplots not being plotted, a gap is left and the factor label is still plotted on the x-axis. }
  \item{colr}{ by default the boxes are infilled in grey, \code{colr = 8} .  If no infill is required, set \code{colr = 0}.  See \code{\link{display.lty}} for the range of available colours. }
  \item{pch}{ by default the plotting symbol for the subset maxima and minima are set to a plus, \code{pch =} 3, alternate plotting symbols may be chosen from those displayed by \code{\link{display.marks}}. } 
  \item{\dots}{ further arguments to be passed to methods.  For example, the size of the axis titles by setting \code{cex.lab}, and the size of the plot title by setting \code{cex.main}.  For example, if it is required to make the plot title smaller, add \code{cex.main = 0.9} to reduce the font size by 10\%. }
}
\details{
There are two ways to execute this function.  Firstly by defining \code{x} and \code{by}, and secondly by combining the two variables with the \code{\link{split}} function.  See the first two examples below.  The \code{\link{split}} function can be useful if the factors to use in the boxplot are to be generated at run-time, see the last example below.  Note that when the \code{\link{split}} construct is used instead of \code{by} the whole \code{split} statement will be displayed as the default y-axis title.  Also note that when using \code{by} the subsets are listed in the order that the factors are encountered in the data, but when using \code{split} the subsets are listed alphabetically.  In either case they can be re-ordered using \code{plot.order}, see Examples. 

In a box-and-whisker plot there are two special cases.  When \code{wend = 0} the whiskers extend to the observed minima and maxima that are not plotted with the plus symbol.  When \code{wend = 0.25} no whiskers or the data minima and maxima are plotted, only the medians and boxes representing the span of the middle 50\% of the data are displayed.

The \code{width} option can be used to define different widths for the individual boxplots.  For example, the widths could be scaled to be proportional to the subset population sizes as some function of the square root (const * sqrt(n)) or logarithm (const * log10(n)) of those sizes (n).  The constant, const, would need to be chosen so that on average the width of the individual boxes would be approximately 0.25, see Example below.  It may be desirable for cosmetic purposes to adjust the positions of the boxes along the x-axis, this can be achieved by specifying \code{xpos}. 

Long subset (factor) names can lead to display problems, changing the \code{las} parameter from its default of \code{las = 1} which plots subset labels parallel to the axis to \code{las = 2}, to plot perpendicular to the axis, can help.  It may also help to use \code{label} and split the character string into two lines, e.g., by changing the string \code{"Granodiorite"} that was supplied to replace the coded factor variable \code{GRDR} to \code{"Grano-\\ndiorite"}.  If this, or setting \code{las = 2}, causes a conflict with the x-axis title, if one is needed, the title can be moved down a line by using \code{xlab = "\\nLithological Units"}.  In both cases the \code{\\n} forces the following text to be placed on the next lower line.

If there are more than 7 labels (subsets) and no alternate labels are provided \code{las} is set to 2, otherwise some labels may fail to be displayed.

The notches in the boxplots indicate the 95\% confidence intervals for the medians and can extend beyond the upper and lower limits of the boxes indicating the middle 50\% of the data when subset population sizes are small.  The confidence intervals are estimated using the binomial theorem.  It can be argued that for small populations a normal approximation would be better.  However, it was decided to remain with a non-parametric estimate to be consistent with the use of non-parametric statistics in this display.
}
\note{
This function is based on a script shared by Doug Nychka on S-News, April 28, 1992. 

Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any \code{NA}s in the data vector are removed prior to preparing the boxplots.

For summary statistics displays to complement the graphics see \code{\link{gx.summary.groups}} or\cr\code{\link{framework.summary}}.
}
\author{ Douglas W. Nychka and Robert G. Garrett }
\references{ 
Garrett, R.G., 1988. IDEAS - An Interactive Computer Graphics Tool to Assist the Exploration Geochemist. In Current Research Part F, Geological Survey of Canada Paper 88-1F, pp. 1-13 for a description of box-and-whisker plots. 
}
\seealso{ \code{\link{cat2list}}, \code{\link{ltdl.fix.df}} } 
\examples{
## Make test data available
data(kola.c)
attach(kola.c)

## Display a simple box-and-whisker plot
bwplots(Cu, by = COUNTRY)
bwplots(split(Cu,COUNTRY))

## Display a more appropriately labelled and scaled box-and-whisker plot
bwplots(Cu, by = COUNTRY, log = TRUE, xlab = "Country", 
	ylab = "Cu (mg/kg) in <2 mm C-horizon soil")

## Display a west-to-east re-ordered plot using the full country names
bwplots(split(Cu, COUNTRY), log = TRUE,
	ylab = "Cu (mg/kg) in <2 mm C-horizon soil", 
	label = c("Finland", "Norway", "Russia"), 
	plot.order = c(2, 1, 3))

## Detach test data
detach(kola.c)

## Make test data kola.o available, setting a -9999, indicating a
## missing pH measurement, to NA
data(kola.o)
kola.o.fixed <- ltdl.fix.df(kola.o, coded = -9999)
attach(kola.o.fixed)

## Display relationship between pH in one pH unit intervals and Cu in 
## O-horizon (humus) soil, extending the whiskers to the 2nd and 98th
## percentiles
bwplots(split(Cu,trunc(pH+0.5)), log = TRUE, wend = 0.02, 
	xlab = "O-horizon soil pH to the nearest pH unit",
	ylab = "Cu (mg/kg) in <2 mm Kola O-horizon soil")

## As above, but demonstrating the use of variable box widths and the
## suppression of 95\% confidence interval notches.  The box widths are
## computed as (Log10(n)+0.1)/5, the 0.1 is added as one subset has a
## population of 1.  Note: paste is used in constructing xlab, below,
## as the label is long and overflows the text line length
table(trunc(pH+0.5))
bwplots(split(Cu,trunc(pH+0.5)), log=TRUE, wend = 0.02, notch = FALSE,
	xlab = paste("O-horizon soil pH to the nearest pH unit,",
	"\nbox widths proportional to Log(subset_size)"),
	ylab = "Cu (mg/kg) in <2 mm Kola O-horizon soil",
	width = c(0.26, 0.58, 0.24, 0.02))


## Detach test data
detach(kola.o.fixed)
}
\keyword{ hplot }

