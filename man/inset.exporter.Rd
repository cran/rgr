\name{inset.exporter}
\alias{inset.exporter}
\title{ Saves an EDA Graphical and Statistical Summary }
\description{
Saves the output from functio \code{\link{inset}} as a graphics file in the R working directory for use in report or map preparation. 
}
\usage{
inset.exporter(x, xlab = deparse(substitute(x)), log = FALSE, 
	xlim = NULL, nclass = NULL, ifnright = TRUE, file = NULL, 
	table.cex = 0.7, gtype = "wmf", ...)
}
\arguments{
  \item{x}{ name of the variable to be plotted. }
  \item{xlab}{ a label for the x-axis. It is often desirable to replace the default x-axis label of the input variable name text string with a more informative label, e.g., \code{xlab = "Cu (mg/kg) in <2 mm O-horizon soil"}. }
  \item{log}{ if it is required to display the data with logarithmic (x-axis) scaling, set \code{log = TRUE}. }
  \item{xlim}{default limits of the x-axis are determined in the function.  However when used stand-alone the limits may be user-defined by setting \code{xlim}, see Note below. }
  \item{nclass}{ the default procedure for preparing the histogram is to use the Scott (1979) rule. This usually provides an informative histogram, other optional rules are \code{nclass = "sturges"} or \code{nclass = "fd"}; the later standing for Freedman-Diaconis (1981), a rule that is resistant to the presence of outliers in the data.  See \code{\link{inset}} or \code{\link{gx.hist}}. }
  \item{ifnright}{ controls where the sample size is plotted in the histogram display, by default this in the upper right corner of the plot.  If the data distribution is such that the upper left corner would be preferable, set \code{ifnright = FALSE}. }
  \item{file}{ the first part of the file name identifying the data source for saving the function output in the R working directory, see Details below. }
  \item{table.cex}{ on some display devices the table may be \sQuote{cramped}
and the text lines overlap.  If this is true \code{table.cex} can be decreased,
the default is \code{table.cex = 0.85}, conversely it can be increased if the
table appears \sQuote{skinny}. }
  \item{gtype}{ the format of the graphics file to be saved.  By default \code{gtype = "wmf"} for a Windows metafile.  Other alternatives are \code{gtype = "jpg"} for a jpeg file, \code{gtype = "png"} for a portable graphics file, \code{gtype = "ps"} for a postscript file, or \code{gtype = "pdf"} for a pdf file.
}
  \item{\dots}{ further arguments to be passed to methods.  For example, by default individual data points in the CPP plot are marked by a plus sign, \code{pch = 3}, if a cross or open circle is desired, then set \code{pch = 4} or \code{pch = 1}, respectively.  See \code{\link{display.marks}} for all available symbols. Adding \code{ifqs = TRUE} results in horizontal and vertical dotted lines being plotted at the three central quartiles and their values, respectively, in the CPP plot. }
}
\details{
See \code{\link{inset}} for details concerning the \code{inset} parameters.

\code{file} contains the first part of the file name identifying the data source for the output file to be saved in the R working directory, see Note below.  The function concatenates the working directory name with \code{file}\code{_}\code{deparse(substitute(x))}\code{_inset} as a character string for the file name.  Subsequently the \sQuote{value} of \code{gtype} is appended as the file type and the file saved in the R working directory. }
\note{
To set the R working directory, if it has not already been set in a \code{first} function, use at the R command line, for example, \code{setwd("C:\\\R\\\WDn")}, where \sQuote{n} is some number, which will result in all saved output being placed in that folder.

Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data vector, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any NAs in the data vector are removed prior to displaying and saving the plot.

If the default selection for \code{xlim} is inappropriate it can be set, e.g., \code{xlim = c(0, 200)} or \code{c(2, 200)}.  If the defined limits lie within the observed data range a truncated plot will be displayed.  If this occurs the number of data points omitted is displayed below the total number of observations.

In some instances if the graphics window has been resized the last line(s) of the table may not be displayed. Resizing the window to be smaller will display the whole table. If the whole table is not visible it will not be saved properly to the graphics file in \code{\link{inset.exporter}}. Once as a complete graphics file the image may be resized in the receiving document.

In some R installations the generation of multi-panel displays and the use of function eqscplot from package MASS causes warning messages related to graphics parameters to be displayed on the current device. These may be suppressed by entering \code{options(warn = -1)} on the R command line, or that line may be included in a \sQuote{first} function prepared by the user that loads the rgr package, etc.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{inset}}, \code{\link{ltdl.fix.df}} 
#ifdef WINDOWS
  For Windows installations see also \code{\link{savePlot}}
#endif
} 
\examples{
## Make test data available
data(kola.o)
attach(kola.o)

## Usage is as follows
## inset.exporter(Cu, xlab = "Cu (mg/kg) in\n<2 mm O-horizon soil", 
##	log = TRUE, gtype = "wmf", file = "kola_o")

#ifdef WINDOWS

## Saves a graphics file, kola_o_Cu_inset.wmf, in the R working
## directory in wmf format for later use or inclusion in a
## report or map.
inset.exporter(Cu, xlab = "Cu (mg/kg) in\n<2 mm O-horizon soil", 
	log = TRUE, gtype = "wmf", file = "kola_o")

#endif

## Detach test data
detach(kola.o)
}
\keyword{ hplot }
 

