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
  \item{file}{ the name of the folder where the file is to be saved, a \sQuote{/} is appended before the synthesized file name, details of which are displayed on the current device.  If no file is specified, the files are saved to the Working Directory, see Details below. }
}
\details{
The file name is synthesized by concatenating the data frame name, \code{group} and variable, \code{x}, names, separated by \code{_}s, and \cr\code{.csv}.  If a folder name has been provided in \code{file}, for example, \code{file = "D://R_work//Project3"}, a \sQuote{/} is appended prior to the synthesized file name.

The file name for the \code{.csv} file is displayed on the current device, and all other output is suppressed. The output file can be inspected with spread sheet software or a viewer of the user's choice.
}
\note{
The synthesis of the file name uses the data frame name which it is assumed is located in \code{search()} position \code{[[2]]}.

To set the R working directory, if it has not already been set in a \code{first} function, use at the R command line, for example, \code{setwd("C:\\R\\WDn")}, where \sQuote{n} is some number, which will result in all saved output being placed in that folder.  The folder must be created before the R session commences.
 
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
## in the R Working Directory.
framework.summary(COUNTRY, Cu)

## Detach test data
detach(kola.c)  
}
\keyword{ univar }

