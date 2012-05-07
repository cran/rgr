\name{framework.summary}
\alias{framework.summary}
\title{ Generate and Save Framework/Subset Summary Statistics }
\description{
Function to generate \sQuote{framework} or subset summary statistics and save them as a \sQuote{.csv} file in the R working directory.  The file can be directly imported into a spreadsheet, e.g., MS Excel, for inspection, or into other software, e.g., a Geographical Information System (GIS) where the spatial information concerning the \sQuote{framework} units is available, e.g., ecoclassification units.
}
\usage{
framework.summary(group, x, file = NULL)
}
\arguments{
  \item{group}{ the name of the factor variable by which the data are to be subset. }
  \item{x}{ name of the variable to be processed. }
  \item{file}{ the first part of the file name identifying the data source for saving the function output in the R working directory, see Details below. }
}
\details{
A default file name is generated from the data frame, group and variable (x) names, \code{data frame name_deparse(substitute(group))_deparse(substitute(x)).csv}.  If \code{file} contains text it is used as the first part of the file name identifying the data source for the file to be saved in the specified folder, for example, \code{file = "D://R_work//Project3//C_soils"}.  If no folder is specified the file is saved in the R working directory.

Output to the current device is suppressed. The output file can be inspected with spread sheet software or a viewer of the user's choice.
}
\note{
To set the R working directory, if it has not already been set in a \code{first} function, use at the R command line, for example, \code{setwd("C:\\\R\\\WDn")}, where \sQuote{n} is some number, which will result in all saved output being placed in that folder.
 
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any \code{NA}s in the data vector are counted and then removed prior to computing the summary statistics.

The function \code{\link{framework.stats}} is employed to compute the summary statistics.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{framework.stats}}, \code{\link{ltdl.fix.df}}, \code{\link{remove.na}} }
\examples{
## Make test data available
data(kola.c)
attach(kola.c)

## Saves the file kola_c_COUNTRY_Cu_summary.csv for later use
## in the R working directory.
framework.summary(COUNTRY, Cu, file = "Kola_c_horizon")

## Detach test data
detach(kola.c)  
}
\keyword{ univar }

