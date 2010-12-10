\name{gx.cnpplts}
\alias{gx.cnpplts}
\title{ Multiple (max 9) Cumulative Normal Probability (CPP) plots }
\description{
Displays cumulative normal probability (CPP) plots for up to nine data subsets, using combinations of symbols and colours to identify each subset.  Note CPP plots are equivalent to Q-Q plots and are more frequently used by physical scientists and engineers.
}
\usage{
gx.cnpplts(xlab = " ", log = FALSE, xlim = NULL, main = "",
	iflgnd = FALSE, ...)
}
\arguments{
  \item{xlab}{  a title for the x-axis must be provided, even if it is \sQuote{no title}, i.e. \code{xlab = ""}. It is often desirable to use an informative title, e.g., \code{xlab = "Cu (mg/kg) in <2 mm C-horizon soil"}. }
  \item{log}{ \code{log} must be specified, \code{TRUE} or \code{FALSE}.  If it is required to display the data with logarithmic (x-axis) scaling, set \code{log  = TRUE}. If log scaling is not required, set \code{log = FALSE}. }
  \item{xlim}{ if the internally generated values for \code{xlim} are to be replaced see the Note below.  If the internally generated x-axis limits are satisfactory omit any reference to \code{xlim} in the call to the function. }
  \item{main}{ a title must be provided, even if it is \sQuote{no title}, i.e. \code{main = ""}.  If \code{main} is specified a tile will be added above the plot, e.g., \code{main = "Kola Project, 1995"}. }
  \item{iflgnd}{ \code{iflgnd} must be specified, \code{TRUE} or \code{FALSE}.  If a R generated legend is to be be placed on the plot, set \code{iflgnd = TRUE}.  On completion of CPP plotting the cursor is activated, locate it at the top left of the space where the legend is to be added and \sQuote{left button} on the pointing device.   The legend comprises the symbol/colour combination, the name of the subset plotted and the data subset size; this information is also displayed on the current device. If no legend is required, set \code{iflgnd = FALSE}. }
  \item{\dots}{ the names of the data subsets (objects), separated by commas, to be plotted, up to a maximum of nine.  See the example below for subset pre-processing steps that lead to a more presentable legend. }
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data matrix, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any \code{NA}s in the data vector are removed prior to displaying the plot.

Although this function is most frequently used to compare the frequency distributions for the same element in multiple subsets of the data, it may also be used to display frequency distributions for multiple elements.

If it is required to set the x-axis limits to specific values they can be set, e.g., \code{xlim = c(0, 200)} or \code{c(2, 200)}, the latter being appropriate for a logarithmically scaled plot, i.e. \code{log  = TRUE}. If the defined limits lie within the observed data range a truncated plot will be displayed.  Setting the limits wider than the default limits can provide additional space for annotation of the display.

By setting \code{iflgnd = FALSE} no internally generated legend will be added.  Alternately, a legend can be constructed with the \code{text} function and placed with the \code{locator} at execution of the \code{text} function.

A default allocation of symbols and colours, and the size of the legend text, is provided in \cr\code{\link{gx.cnpplts.setup}}.  These may be edited if required, they are imported into \code{gx.cnpplts} at function run time.

Unlike most other functions in \sQuote{rgr} all the arguments must be specified explicitly, except \code{xlim}.  This is the cost of being able to append up to nine subset names in the function call.  The function needs to know where subset names start in the list passed to the function.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{gx.cnpplts.setup}}, \code{\link{display.marks}}, \code{\link{display.lty}}, \code{\link{ltdl.fix.df}}, \code{\link{text}} }
\examples{
## Make test data available
data(kola.c)
attach(kola.c)

## An example
gx.cnpplts(xlab = "Cu (mg/kg) in <2 mm C-horizon soil", log = TRUE, 
	xlim = NULL, main = "", iflgnd = FALSE, Cu[COUNTRY == "RUS"], 
	Cu[COUNTRY == "FIN"], Cu[COUNTRY == "NOR"])

## An example that leads to a cleaner legend
## First select data for the variable to be plotted for the subsets, from
## dimnames(kola.c) we know that Be is the 19th column in the dataframe
Norway <- gx.subset(kola.c,COUNTRY=="NOR")[,19]
Russia <- gx.subset(kola.c,COUNTRY=="RUS")[,19]
Finland <- gx.subset(kola.c,COUNTRY=="FIN")[,19]
gx.cnpplts(xlab = "Be (mg/kg) in <2 mm Kola C-horizon soils", log = TRUE,
	xlim = NULL, main  ="", iflgnd = FALSE, Finland, Russia, Norway)

## An example where the limts of the x-axis are provided
gx.cnpplts(xlab = "Be (mg/kg) in <2 mm Kola C-horizon soils", log = TRUE,
	xlim = c(0.02, 20), main  ="", iflgnd = FALSE, Finland, Russia, Norway)

## An example of a multi-element display
gx.cnpplts(xlab = "Concentrations (mg/kg) in <2 mm C-horizon soils",
	log = TRUE, xlim = NULL, main = "Kola Project, 1995", 
	iflgnd = FALSE ,Cu, Ni, Co)

## Clean up and detach test data
rm(Norway)
rm(Russia)
rm(Finland)
detach(kola.c)
}
\keyword{ hplot }

