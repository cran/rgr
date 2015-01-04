\name{gx.2dproj.plot}
\alias{gx.2dproj.plot}
\title{ Function to Display a Saved 2-d Projection Object }
\description{ 
Displays the 2-d projection saved from \code{\link{gx.2dproj}}, optionally the row numbers of the input matrix or the row identifiers, if available, may be displayed instead of the default plotting symbol.
}
\usage{
gx.2dproj.plot(save, rowids = NULL, main = "", pch = 3, cex = 0.8, col = 1, ...)
}
\arguments{
  \item{save}{ the saved object from \code{\link{gx.2dproj}}. }
  \item{rowids}{ to display the input matrix row numbers set \code{rowids = TRUE}.  Setting \code{rowids = FALSE} replaces the row numbers with the row identifiers from the input matrix.  The default \code{rowids = NULL} causes the default symbol, a \sQuote{+} to be plotted. }
  \item{main}{ an alternative plot title to that in the saved object from \code{\link{gx.2dproj}}, see Details below. }
  \item{pch}{ by default a \sQuote{+}, \code{pch = 3} will be plotted.  See \bold{Note} below for alternate plotting symbols. }
  \item{cex}{ by default symbols and characters are plotted at \code{cex = 0.8}, a 20\% reduction in font size.  }
  \item{col}{ by default symbols and charcaters are plotted in black, \code{col = 1}, specify \code{col = 2} to obtain red (see \code{\link{display.lty}} for the default colour palette).  See \bold{Note} below for alternate colours. }
  \item{\dots}{ further arguments to be passed to methods concerning the plot.  For example, if it is required to make the plot title smaller, add \code{cex.main = 0.9} to reduce the font size by 10\%. }
}
\details{
If \code{main} is undefined the plot title from the saved object from \code{\link{gx.2dproj}} is displayed.  If no plot title is required set \code{main = " "}, or if a user defined plot title is required it should be defined in \code{main}, e.g., \code{main = "Plot Title Text"}.

The x- and y-axis labels are those in the saved object from \code{\link{gx.2dproj}} and indicate the type of 2-d projection in the display.
}
\references{
Reimann, C., Filzmoser, P., Garrett, R. and Dutter, R., 2008. Statistical Data Analysis Explained: Applied Environmental Statistics with R. John Wiley & Sons, Ltd., 362 p.

Venables, W.N. and Ripley, B.D., 2001. Modern Applied Statistics with S-Plus, 3rd Edition, Springer,  501 p.
}
\note{
The available symbols are:\cr
pch: 0 = square, 1 = circle, 2 = triangle, 3 = plus, 4 = X,\cr
     5 = diamond, 6 = upside-down triangle, 7 = square with X,\cr
     8 = asterisk, 9 = diamond with plus, 10 = circle with plus,\cr
     11 = double triangles, 12 = square with plus,\cr
     13 = circle with X, 14 = square with upside-down triangle.\cr
     Symbols 15 to 18 are solid in the colour specified:\cr
     15 = square, 16 = circle, 17 = triangle, 18 = diamond.

The available colours from the default \sQuote{palette} are:\cr
Col: 0 = none, 1 = black, 2 = red, 3 = green, 4 = dark blue,\cr
     5 = turquoise, 6 = pink, 7 = yellow, 8 = grey, 9 = black.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{gx.2dproj}} }
\examples{
## Make test data available
data(sind.mat2open)

## Display default 2-d projection
sind.save <- gx.2dproj(sind.mat2open, ifilr = TRUE)

## Display saved object with more informative title
gx.2dproj.plot(sind.save,
main = "Howarth & Sinding-Larsen\nStream Sediments, ilr Transformed Data")

## Display saved object with row identifiers in a smaller blue font
gx.2dproj.plot(sind.save,
main = "Howarth & Sinding-Larsen\nStream Sediments, ilr Transformed Data",
rowids = TRUE, cex = 0.7, col = 4)

## Display 2-d projection after trimming the 3 most extreme samples
sind.2dproj.trim3 <- gx.2dproj(sind.mat2open, ifilr = TRUE, row.omits = c(13,15,16))
gx.2dproj.plot(sind.save,
main = "Howarth & Sinding-Larsen\nStream Sediments, ilr Transformed Data",
rowids = FALSE, cex = 0.7, col = 4)

## Clean-up
rm(sind.save)
rm(sind.2dproj.trim3)
}
\keyword{ hplot }
