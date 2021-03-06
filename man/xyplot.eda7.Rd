\name{xyplot.eda7}
\alias{xyplot.eda7}
\title{ Display a Third Variable in a X-Y Plot using Tukey Boxplot Symbology }
\description{
Displays a third variable where the data are represented by symbols using Tukey boxplot-based symbology. Tukey boxplots divide data into 7 groups, the middle 50\%, and three lower and higher groupings, see Details below. The computation of the fences used to subdivide the data may be carried out following a logarithmic transformation of the data. The colours of the symbols may be optionally changed.  The x-y plot axes may be optionally displayed with logarithmic (base 10) scaling.  Optionally a legend may be added to the plot.

Where the x-y data are parts from a composition they should be plotted as symmetric coordinates, and the third variable as a log-ratio if it is from the same composition.
}
\usage{
xyplot.eda7(xx, yy, zz, sfact = 1, xlim = NULL, ylim = NULL,  
	log = NULL, logz = FALSE, xlab = deparse(substitute(xx)), 
	ylab = deparse(substitute(yy)), zlab = deparse(substitute(zz)), 
	main = "", ifgrey = FALSE, symcolr = NULL, iflgnd = FALSE,
	title = deparse(substitute(zz)), cex.lgnd = 0.8, ...)
}
\arguments{
  \item{xx}{ name of the x-axis variable. }
  \item{yy}{ name of the y-axis variable. }
  \item{zz}{ name of the third variable to be plotted. }
  \item{sfact}{ controls the absolute size of the plotted symbols, by default \code{sfact = 1}.  Increasing \code{sfact} results in larger symbols. }
  \item{xlim}{ user defined limits for the x-axis, see Details below. }
  \item{ylim}{ user defined limits for the y-axis, see Details below. }
  \item{log}{ if it is required to display the data with logarithmic axis scaling, set \code{log = "x"} for a logarithmically scaled x-axis, \code{log = "y"} for a logarithmically scaled y-axis, and \code{log = "xy"} for both axes logarithmically scaled. }
  \item{logz}{ if it is required to undertake the Tukey Boxplot computations after a logarithmic data transform, set \code{logz = TRUE}. }
  \item{xlab}{ by default the character string for \code{xx} is used for the x-axis title.  An alternate title can be displayed with \code{xlab = "text string"}, see Examples. }
  \item{ylab}{ by default the character string for \code{yy} is used for the x-axis title.  An alternate title can be displayed with \code{ylab = "text string"}, see Examples. }
  \item{zlab}{ by default the character string for \code{zz} is appended to the text string \dQuote{EDA Tukey Boxplot Based Plot for} for the plot title}.  An alternate title can be displayed with \code{zlab = "text string", see Details below. }
  \item{main}{ an alternative plot title, see Details below.}
  \item{ifgrey}{ set \code{ifgrey = TRUE} if a grey-scale plot is required, see Details below. }
  \item{symcolr}{ the default is a colour plot and default colours are provided, deeper blues for lower values, green for the middle 50\% of the data, and oranges and reds for higher values.  A set of alternate symbol colours can be provided by defining \code{symcolr}, see Details below. }
  \item{iflgnd}{ the default is no legend.  If a legend is required set \code{iflgnd = TRUE}, following the plotting of the data the cursor will be activated, locate that at the top left corner of the desired legend position and \sQuote{left button} on the pointing device. }
  \item{title}{ a short title for the legend, e.g., \code{title = "Zn (mg/kg)"}.  The default is the character string for \code{zz}. } 
  \item{cex.lgnd}{ controls the scaling of the legend box and text, but not the symbols so that they match those on the plot, the default is \code{cex.lgnd = 0.8}. } 
  \item{\dots}{ further arguments to be passed to methods.  For example, if it is required to make the plot title smaller, add \code{cex.main = 0.9} to reduce the font size by 10\%. }
}
\details{
Tukey boxplots divide data into 7 groups, the middle 50\%, and three lower and higher groupings: within the whisker, near outliers and far outliers, respectively.  Symbols for values below the first quartile (Q1) are plotted as increasingly larger circles, while symbols for values above the third quartile are plotted as increasingly larger squares, a \sQuote{+} is used to plot the data falling in the middle 50\%.  For the higher groupings, the whisker contains values \code{>Q3 and <(Q3 + 1.5 * HW)}, where \code{HW = (Q3 - Q1)}, the interquartile range; near outliers lie between \code{(Q3 + 1.5 * HW)} and \code{(Q3 + 3 * HW)}; and far outliers have values \code{>(Q3 + 3 * HW)}.  For the lower groupings the group boundaries, fences, fall similarly spaced below Q1.  The computation of the fences used to subdivide the data may be carried out following a logarithmic transformation of the data, set \code{logz = TRUE}.

A summary table of the values of the symbol intervals, the number of values plotting as each symbol, and symbol shapes, sizes and colours is displayed on the current device.

If \code{zlab} and \code{main} are undefined a default a plot title is generated by appending the input variable name text string to \code{"EDA Tukey Boxplot-Based Plot for "}.  If no plot title is required set \code{zlab = " "}, and if some user defined plot title is required it should be defined in \code{main}, e.g. \code{main = "Plot Title Text"}.

If the grey-scale option is chosen the symbols are plotted 100\% black for the far outliers, 85\% black for the near outliers, 70\% black for values within the whiskers, and 60\% black for values falling within the middle 50\% of the data.

The default colours, \code{symcolr = c(25, 22, 20, 13, 6, 4, 1)}, are selected from the \code{rainbow(36)} pallette, and alternate colour schemes need to be selected from the same palette.  See \code{\link{display.rainbow}} for the available colours.  It is essential that 7 colours be provided, e.g., \cr\code{symcolr = c(27, 24, 22, 12, 5, 3, 36)}, if exactly 7 are not provided the default colours will be displayed.

For compositional data the function \code{\link{gx.symm.coords.mat}} is required, and arguments \code{xx} and \code{yy} are replaced by a call to that function which returms the symmetric coordinated for those parts of the composition, and the axes are labelled appropriately.  See example below.
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any data vectors including \code{NA}s are removed prior to displaying the plot.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{display.rainbow}}, \code{\link{ltdl.fix.df}}, \code{\link{remove.na}}, \code{\link{gx.symm.coords.mat}} }
\examples{
## Make test data available
data(sind)
attach(sind)

## Plot a default Tukey boxplot-based display
xyplot.eda7(Fe, Mn, Zn)

## Plot with logarithmically scaled boxplot fences and more
## appropriate axis scaling and labelling with a user specified title
xyplot.eda7(Fe, Mn, Zn, sfact = 2, log = "y", logz = TRUE,
	xlab = "Fe (pct) in stream sediment", 
	ylab = "Mn (mg/kg) in stream sediment",
	main = "Howarth & Sinding-Larsen Test Data\nLog10(Zn) (mg/kg)") 

## Display a grey-scale equivalent of the above plot
xyplot.eda7(Fe, Mn, Zn, sfact = 2, log = "y", logz = TRUE, ifgrey = TRUE,
	xlab = "Fe (pct) in stream sediment", 
	ylab = "Mn (mg/kg) in stream sediment",
	main = "Howarth & Sinding-Larsen Test Data\nLog10(Zn) (mg/kg)") 

## Plot the same display with an alternate colour scheme
xyplot.eda7(Fe, Mn, Zn, sfact = 2, log = "y", logz = TRUE,
	xlab = "Fe (pct) in stream sediment", 
	ylab = "Mn (mg/kg) in stream sediment",
	main = "Howarth & Sinding-Larsen Test Data\nLog10(Zn) (mg/kg)",
	symcolr = c(27, 24, 22, 12, 5, 3, 36)) 

## Detach test data
detach(sind)
rm(sind)

## More appropriately for compositional data
data(nockolds)
attach(nockolds)

## Plot a minor element log ratio against the symmetric coordinates for
## Si and Al
xyplot.eda7(gx.symm.coords.mat(nockolds, 1, 2), log(P/Ti))

## Detach test data
detach(nockolds)
rm(nockolds)
}
\keyword{ hplot }

