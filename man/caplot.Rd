\name{caplot}
\alias{caplot}
\title{ Prepare a Concentration-Area (C-A) Plot }
\description{
Displays a concentration-area (C-A) plot to assess whether the data are spatially multi-fractal (Cheng et al., 1994; Cheng and Agterberg, 1995) as a part of a four panel display.  This procedure is useful for determining if multiple populations that are spatially dependent are present in a data set.  It can be used to determine the practical limits, upper or lower bounds, of the influence of the biogeochemical processes behind the spatial distribution of the data.  Optionally the data may be logarithmically transformed (base 10) prior to interpolation, the points may be \sQuote{jittered} (see Arguments below), the size of the interpolated grid may be modified, and alternate colour schemes can be chosen for display of the interpolated data.
}
\usage{
caplot(x, y, z, zname = deparse(substitute(z)),
	caname = deparse(substitute(z)), log =  TRUE, ifjit = FALSE,
	ifrev = FALSE, ngrid = 100, colr = topo.colors(16), 
	xcoord = "Easting", ycoord = "Northing")
}
\arguments{
  \item{x}{ name of the x-axis spatial coordinate, the eastings. }
  \item{y}{ name of the y-axis spatial coordinate, the northings. }
  \item{z}{ name of the variable to be processed and plotted. }
  \item{zname}{ a title for the x-axes of the CPP (Cumulative Normal Percentage Probability) and C-A plot panels. It is often desirable to replace the default x-axis titles of the input variable name text string with a more informative title, e.g., \cr\code{zname = "Cu (mg/kg) in <2 mm O-horizon soil"}. }
  \item{caname}{ a title for the image of the interpolated data. It is often desirable to replace the default title of the input variable name text string with a more informative title, e.g., \code{caname = "Kola Project, 1995\nCu (mg/kg) in <2 mm O-horizon soil"}.  For no title, set \code{caname = ""}. }
  \item{log}{ the default is set to \code{log = TRUE} as in most cases this function is used with positively skewed data, where a logarithmic data transform is appropriate.  If it is required to undertake the C-A plot interpolation without a logarithmic data transformation, set \code{log = FALSE}.  This also results in the accompanying probability (CPP) plots being arithmically scaled (x-axes). }
  \item{ifjit}{ if there is a possibility that the data set contains multiple measurements at an identical spatial (x,y) location set \code{ifjit = TRUE}.  The presence of multiple data at an identical location will cause the Akima (1996) interpolation function to fail. }
  \item{ifrev}{ by default the empirical C-A function is plotted from highest value to lowest, \code{ifrev = FALSE}.  As the C-A plot is a log-log display this provides greater detail for the highest values.  The direction of accumulation can be key in detecting multi-fractal patterns, it is usually informative to also prepare a plot with \code{ifrev = TRUE}, i.e. accumulation from lowest to highest values.  To see a dramatic example of this, run the Examples below. }
  \item{ngrid}{ by default \code{ngrid = 100}, this results in the data being interpolated into a 100 x 100 grid that extends between the data set's spatial extremes determined for the (x,y) spatial coordinates for the data.  See Details below. }
  \item{colr}{ by default the \code{topo.colors(16)} pallette is used to render the interpolated grid as an image.  For alternative palettes see \code{\link{colors}}, and see Details below. }
  \item{xcoord}{ a title for the x-axis, defaults to \code{Easting}. }
  \item{ycoord}{ a title for the y-axis, defaults to \code{Northing}. }
}
\details{
The function creates a four panel display.  The percentage cumulative probabity (CPP) plot of the data in the upper left, and the CPP plot of the interpolated data to be used in the C-A plot in the upper right.  The lower left panel contains an image of the interpolated data, and the lower right the C-A plot.

Akima's (1978, 1996) interpolation function is used to obtain a linear interpolation between the spatial data values.  If the data are positively skewed the use of a logarithmic data transformation, \code{log = TRUE}, is highly recommended, as noted above this is commonly the case and is the default.  Following generation of the interpolated grid and prior to futher processing the interpolated grid values are clipped by the convex-hull of the spatial locations, therefore there is no interpolation beyond the spatial extent, support, of the data is displayed.

The use of the \code{topo.colors(16)} palette to display the image of the interpolated values leads to low values being plotted in blue, and as the interpolated values increase they take on green, yellow and orange colors.  For a grey-scale display for black-and-white use set \code{colr = grey(0:8/8)}.  This leads to lowest interpolated values being plotted in black and the highest in white, using \code{colr = grey(8:0/8)} reverses this, with the lowest values being plotted in white and the highest in black.  In either case, if the values plotted in white occur at the study area boundary, i.e. at the convex hull, the difference between no data and white cannot be discerned.

For preparation of the C-A plot the ordered vector of interpolated values is used as a surrogate for the measurement of area greater than, or less than, a stated interpolated value.  The cumulative percentage count of the interpolated values being plotted on the y-axis of the C-A plot.  As noted above, it is both informative and important to display the C-A plot accumulated both upwards and downwards.
}
\note{
This wrapper function was developed from a S-Plus function to prepare C-A plots using Akima's (1978, 1996) interpolation procedure written by Graeme Bonham-Carter, Geological Survey of Canada, in April 2004.

Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any sites with \code{NA}s in their (x,y,z) data are removed prior to spatial interpolation and preparation of the C-A plot.

In some R installations the generation of multi-panel displays and the use of function eqscplot from package MASS causes warning messages related to graphics parameters to be displayed on the current device. These may be suppressed by entering \code{options(warn = -1)} on the R command line, or that line may be included in a \code{first} function prepared by the user for loading the \sQuote{rgr} package, etc.
}
\author{ Graham F. Bonham-Carter and Robert G. Garrett }
\references{
Akima, H. (1978). A Method of Bivariate Interpolation and Smooth Surface Fitting for Irregularly Distributed Data Points. ACM Transactions on Mathematical Software *4*, 148-164.

Akima, H. (1996). Algorithm 761: scattered-data surface fitting that has the accuracy of a cubic polynomial. ACM Transactions on Mathematical Software *22*, 362-371.

Cheng, Q. and Agterberg, F.P., 1995. Multifractal modeling and spatial point processes. Mathematical Geology 27(7):831-845.

Cheng, Q., Agterberg, F.P. and Ballantyne, S.B., 1994. The separation of geochemical anomalies from background by fractal methods.  Journal of Geochemical Exploration, 51(2):109-130.
}
\seealso{  \code{\link{cnpplt}}, \code{\link[akima]{interp}}, \code{\link{colors}}, \code{\link{ltdl.fix.df}} }
\examples{ 
## Make test data available
data(kola.o)
attach(kola.o)

## A default (uninformative) C-A plot
caplot(UTME/1000, UTMN/1000, Cu)

## Plot a more appropriately scaled (log transformed data) and 
## titled display
caplot(UTME/1000, UTMN/1000, Cu, log = TRUE,
	zname = "Cu (mg/kg) in\n<2 mm O-horizon soil",
	caname = "Kola Project, 1995\nCu (mg/kg) in <2 mm O-horizon soil") 

## Plot as above but with the C-A plot accumulation reversed
caplot(UTME/1000, UTMN/1000, Cu, log = TRUE, ifrev = TRUE,
	zname = "Cu (mg/kg) in\n<2 mm O-horizon soil",
	caname = "Kola Project, 1995\nCu (mg/kg) in <2 mm O-horizon soil") 

## Detach test data
detach(kola.o)
} 
\keyword{ hplot }


