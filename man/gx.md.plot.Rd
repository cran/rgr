\name{gx.md.plot}
\alias{gx.md.plot}
\title{ Function to Display Chi-square Plots of Mahalanobis Distances }
\description{
Function to display Chi-square plots of Mahalanobis distances from objects saved from \code{\link{gx.mva}}, \code{\link{gx.mva.closed}}, \code{\link{gx.robmva}}, \code{\link{gx.robmva.closed}}, \code{\link{gx.md.gait}} and \code{\link{gx.md.gait.closed}}.  The actual plotting of the displays is undertaken by function \code{\link{gx.md.plt0}}.  The function facilitates making \sQuote{cosmetic} changes to the Chi-square plots not so easily achieved in function \code{\link{gx.md.gait}} and \code{\link{gx.md.gait.closed}}, and not possible in functions \code{\link{gx.mva}}, \code{\link{gx.mva.closed}}, \code{\link{gx.robmva}} or \code{\link{gx.robmva.closed}}.
}
\usage{
gx.md.plot(save, main = "", ifadd = c(0.98, 0.95, 0.9), cexf = 0.6,
	cex = 0.8, ...)
}
\arguments{
  \item{save}{ a saved object from the execution of function \code{\link{gx.mva}}, \code{\link{gx.mva.closed}}, \cr\code{\link{gx.robmva}}, \code{\link{gx.robmva.closed}}, \code{\link{gx.md.gait}} or \code{\link{gx.md.gait.closed}}. }
  \item{main}{ an alternate Chi-square plot title to that in the saved object, see Details below. }
  \item{ifadd}{ if probability based fences are to be displayed on the Chi-square plots enter the probabilities here, see Details below.  For no fences set \code{ifadd = NULL}. }
  \item{cexf}{ the text scale expansion factor for the annotation of the probability based fences, by default \code{cexf = 0.6}. } 
  \item{cex}{ the text scale expansion factor for the other annotation within the \sQuote{frame} of the Chi-square plot, by default \code{cex = 0.8}. }
  \item{\dots}{ further arguments to be passed to methods concerning the generated plots.  For example, if some colour other than black is required for the plotting characters, specify \code{col = 2} to obtain red (see \code{\link{display.lty}} for the default colour palette).  If it is required to make the plot title or axis labelling smaller, add \code{cex.main = 0.9} or \code{cex.lab = 0.9}, respectively, to reduce the font size by 10\%. }
}
\details{
If \code{main} is undefined the name of the matrix object from which the Mahalanobis distances were derived is passed to the function via the saved object.  Using the matrix name is the recommended procedure as it helps to track the progression during a GAIT exercise, and acts as a record of the data source.  However, at a presentation stage an alternate plot title may preferred and can be defined in this function, e.g., \code{main = "Plot Title Text"}.  If no plot title is required set \code{main = " "}.

By default three fences are placed on the Chi-square plots at probabilities of membership of the current \sQuote{core} data subset, or total data if appropriate, with \code{ifadd = c(0.98, 0.95, 0.9)}.  Alternate probabilities may be defined as best for the display.  If no fences are required set \code{ifadd = NULL}.

The Mahalanobis distance, Chi-square, plot x-axis label is set appropriately to indicated the type of robust start or trim using the value of \code{proc} from the saved object passed to the function.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{gx.md.gait}}, \code{\link{gx.md.gait.closed}}, \code{\link{gx.mva}}, \code{\link{gx.robmva}}, \code{\link{gx.robmva.closed}}, \code{\link{gx.add.chisq}} }
\examples{
## Make test data available
data(sind.mat2open)

## Save and display Chi-square plot
sind.save <- gx.mva(ilr(sind.mat2open))
gx.md.plot(sind.save)
gx.md.plot(sind.save,
main = "Howarth & Sinding Larsen Stream Sediments\nilr transform",
cexf = 0.8, cex = 1, col = 2)

## Save and display Chi-square plot with a
## mcd robust start and ilr transformation
sind.save <- gx.md.gait(ilr(sind.mat2open), mcdstart = TRUE, mvtstart = TRUE,
trim = 3, ifadd = NULL)
gx.md.plot(sind.save)
gx.md.plot(sind.save,
main = paste("Howarth & Sinding-Larsen\nStream Sediments, ilr Transformed Data",
"\nMCD robust start"), ifadd = 0.9, cex.main = 0.8)

## Clean-up
rm(sind.save)
}
\keyword{ hplot }

