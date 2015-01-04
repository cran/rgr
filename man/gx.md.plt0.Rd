\name{gx.md.plt0}
\alias{gx.md.plt0}
\title{ Function to Display Chi-square plots of Mahalanobis Distances }
\description{
This function is not called directly by the user but from functions that plot Mahalanobis distances, i.e. \code{\link{gx.md.gait}} and \code{\link{gx.md.plot}}.
}
\usage{ 
gx.md.plt0(md, n, p, trim = trim, ptrim = -1, proc = proc,
	main = main, ifadd = ifadd, cexf = cexf, cex = cex, ...) 
}
\arguments{
  \item{md}{ a vector of Mahalanobis distances of length n. }
  \item{n}{ the length of the vector of Mahalanobis distances. }
  \item{p}{ the number of variables upon which the Mahalanobis distances are based. }
  \item{trim}{ the number of individuals (observations or samples) that have been trimmed, and did not contribute to the estimation of covariance and means. }
  \item{ptrim}{ the percentage trim requested, if a percentage (MVT) trim was requested. }
  \item{proc}{ the procedure by which the Mahalanobis distances were estimated, used to ensure appropriate labelling of the Chi-square plot x-axis. }
  \item{main}{ the title for the Chi-square plot. }
  \item{ifadd}{ the probability based fences to be displayed on the Chi-square plots, set by the calling function and the user. }
  \item{cexf}{ the text scale expansion factor for the annotation of the probability based fences, set by the calling function and the user. } 
  \item{cex}{ the text scale expansion factor for the other annotation within the \sQuote{frame} of the Chi-square plot, set by the calling function and the user. }
  \item{\dots}{ further arguments to be passed to methods concerning the generated plots.  For example, if some colour other than black is required for the plotting characters, specify \code{col = 2} to obtain red (see \code{\link{display.lty}} for the default colour palette).  If it is required to make the plot title or axis labelling smaller, add \code{cex.main = 0.9} or \code{cex.lab = 0.9}, respectively, to reduce the font size by 10\%. }
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{gx.md.gait}}, \code{\link{gx.md.plot}}, \code{\link{gx.add.chisq}} }
\examples{
## Make test data available
data(sind.mat2open)

## Generate and display sets of Mahalanobis distances
gx.md.gait(ilr(sind.mat2open))
gx.md.gait(ilr(sind.mat2open), mcdstart = TRUE, ifadd = NULL)
gx.md.gait(ilr(sind.mat2open), mcdstart = TRUE, mvtstart = TRUE, trim = 3,
ifadd = 0.9)
}
\keyword{ hplot }

