\name{inset.exporter}
\alias{inset.exporter}
\title{ Saves an EDA Graphical and Statistical Summary }
\description{
Saves the output from function \code{\link{inset}} as a graphics file in the R Working Directory for use in report or map preparation.  Optionally the EDA graphics may be plotted with base 10 logarithmic scaling. 
}
\usage{
inset.exporter(x, xlab = deparse(substitute(x)), log = FALSE, 
	xlim = NULL, nclass = NULL, ifnright = TRUE, file = NULL, 
	table.cex = 0.7, gtype = "emf", ...)
}
\arguments{
  \item{x}{ name of the variable to be plotted. }
  \item{xlab}{ a label for the x-axis. It is often desirable to replace the default x-axis label of the input variable name text string with a more informative label, e.g., \cr\code{xlab = "Cu (mg/kg) in <2 mm O-horizon soil"}. }
  \item{log}{ to display the data with logarithmic (x-axis) scaling, set \code{log = TRUE}. }
  \item{xlim}{default limits of the x-axis are determined in the function.  However when used stand-alone the limits may be user-defined by setting \code{xlim}, see Note below. }
  \item{nclass}{ the default procedure for preparing the histogram is to use the Scott (1979) rule. This usually provides an informative histogram, other optional rules are \code{nclass = "sturges"} or \code{nclass = "fd"}; the later standing for Freedman-Diaconis (1981), a rule that is resistant to the presence of outliers in the data.  See Venables and Ripley (2001) for details, or \code{\link{inset}} or \code{\link{gx.hist}}. }
  \item{ifnright}{ controls where the sample size is plotted in the histogram display, by default this in the upper right corner of the plot.  If the data distribution is such that the upper left corner would be preferable, set \code{ifnright = FALSE}.  If neither option generates an acceptable plot, setting \code{ifnright = NULL} suppresses the display of the data set size.  }
  \item{file}{ the name of the folder where the files are to be saved, a \sQuote{/} is appended before the synthesized file name, details of which are displayed on the current device.  If no file is specified, the files are saved to the Working Directory, see Details below. }
  \item{table.cex}{ controls the size of the text in the central panel of summary statistics table, the default is \code{table.cex = 0.7}, which has been found to be optimal.  If the entire table does not display, just parts of the columns, see Note below. }
  \item{gtype}{ the format of the graphics file to be saved.  By default \code{gtype = "emf"} for a Windows extended metafile.  Other alternatives are \code{gtype = "jpg"} for a jpeg file, \code{gtype = "png"} for a portable network graphics file, \code{gtype = "ps"} for a postscript file, or \code{gtype = "pdf"} for a pdf file. }
  \item{\dots}{ further arguments to be passed to methods.  For example, by default individual data points in the CPP plot are marked by a plus sign, \code{pch = 3}, if a cross or open circle is desired, then set \code{pch = 4} or \code{pch = 1}, respectively.  See \code{\link{display.marks}} for all available symbols. Adding \code{ifqs = TRUE} results in horizontal and vertical dotted lines being plotted at the three central quartiles and their values, respectively, in the CPP plot. }
}
\details{
See \code{\link{inset}} for details concerning the \code{inset} parameters.

The graphics file will be saved, by default, in the Working Directory.  Alternately a folder name may be provided in \code{file}.  The file name is synthesized from the data frame name, \sQuote{_inset_}, the variable name and \sQuote{\code{.gtype}}.  This is prefixed with a \sQuote{\\} and concatenated with the folder location.  
}
\note{
The synthesis of the file name uses the data frame name which it is assumed is located in \code{search()} position \code{[[2]]}.

Sometimes the table between the two plots my be left and/or right truncated, or incompletely displayed.  Reducing the size of the graphics window will lead to a complete display.  If this needs to be done the function needs to be repeated to ensure a correctly dimensioned display is saved.  Once as a complete saved graphics file the image may be resized in the receiving document.

Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any NAs in the data vector are removed prior to displaying and saving the plot.

If the default selection for \code{xlim} is inappropriate it can be set, e.g., \code{xlim = c(0, 200)} or \code{c(2, 200)}, the latter being apprpriate for a logarithmically scaled plot, i.e. \code{log = TRUE}.  If the defined limits lie within the observed data range a truncated plot will be displayed.  If this occurs the number of data points omitted is displayed below the total number of observations.

In some R installations the generation of multi-panel displays and the use of function eqscplot from package MASS causes warning messages related to graphics parameters to be displayed on the current device. These may be suppressed by entering \code{options(warn = -1)} on the R command line, or that line may be included in a \sQuote{first} function prepared by the user that loads the \sQuote{rgr} package, etc.
}
\author{ Robert G. Garrett }
\references{ 
Venables, W.N. and Ripley, B.D., 2001. Modern Applied Statistsis with S-Plus, 3rd Edition, Springer, 501 p. See pp. 119 for a description of histogram bin selection computations.
}
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
## Saves the graphics as a pdf file to the defined folde.r
## inset.exporter(Cu, xlab = "Cu (mg/kg) in\n<2 mm O-horizon soil", 
##	log = TRUE, gtype = "pdf", file = "D://R_work//Project3")

#ifdef WINDOWS

## Saves a default emf graphics file, kola_o_inset_Cu.emf, in the
## R Working Directory for later use or inclusion in a report or
## map.
inset.exporter(Cu, xlab = "Cu (mg/kg) in\n<2 mm O-horizon soil", 
	log = TRUE)

#endif

## Detach test data
detach(kola.o)
}
\keyword{ hplot }
 

