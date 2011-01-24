\name{gx.cnpplts.setup}
\alias{gx.cnpplts.setup}
\title{ Set Up and Display Symbolgy for function \code{gx.cnpplts} }
\description{
Permits a user to display the symbol mark and colour combinations to be used in function \code{gx.cnpplts}, and change them and the legend font size, if required.  Any changes require editing the function and some elementary R-scripting skills, see Note below.
}
\usage{
gx.cnpplts.setup(display = FALSE)
}
\arguments{
  \item{display}{ if \code{display = TRUE} the symbol mark and colour combinations are displayed on the current device.  If \code{display = FALSE} output is suppressed.}
}
\details{
The available symbols are:\cr
pch: 0 = square, 1 = circle, 2 = triangle, 3 = plus, 4 = X,\cr
     5 = diamond, 6 = upside-down triangle, 7 = square with X,\cr
     8 = asterisk, 9 = diamond with plus, 10 = circle with plus,\cr
     11 = double triangles, 12 = square with plus,\cr
     13 = circle with X, 14 = square with upside-down triangle.\cr
     Symbols 15 to 18 are solid in the colour specified:\cr
     15 = square, 16 = circle, 17 = triangle, 18 = diamond.\cr
The available colours from the default \sQuote{palette} are:\cr
Col: 0 = none, 1 = black, 2 = red, 3 = green, 4 = dark blue,\cr
     5 = turquoise, 6 = pink, 7 = yellow, 8 = grey, 9 = black.
}
\value{
  \item{pchs}{ a vector of 9 elements defining the symbols, marks, to use for plotting the 1 to 9 permissable subsets. }
  \item{symcols}{ a vector of 9 elements defining the colours from the \sQuote{default} palette to use for the colours of the 1 to 9 permissable subset symbols. }
  \item{cex}{ the text scale expansion factor to use in the optional legend for function \code{gx.cnpplts}, the default is 0.8. }
  \item{cexp}{ the scale expansion factor for the plotting symbols in function \code{gx.cnpplts}, the default is 0.9. }
}
\author{ Robert G. Garrett }
\note{
To edit the function use \code{fix(gx.cnpplts.setup)} to extract a copy of the function from the \sQuote{rgr} library for editing.  It will help to have a colour printed copy of the display, \code{display = TRUE}, from this function at hand.  Note that after editing and saving the function will remain in the workspace and you may get warning messages that can be ignored.
}
\seealso{ \code{\link{display.marks}}, \code{\link{points}}, \code{\link{display.lty}} }
\keyword{ color }
\keyword{ misc }
