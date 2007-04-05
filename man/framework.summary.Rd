\name{framework.summary}
\alias{framework.summary}
\title{ Generate and Save Framework/Subset Summary Statistics }
\description{
Function to generate \sQuote{framework} or subset summary statistics and save them in as a .csv file that can be directly imported into a spreadsheet, e.g., MS Excel®, for inspection or other software, for example, a Geographical Information System (GIS) where the spatial information concerning the \sQuote{framework} units is available, e.g., ecoclassification units.
}
\usage{
framework.summary(group, x, file = NULL)
}
\arguments{
  \item{group}{ the name of the factor variable by which the data are to be subset. }
  \item{x}{ name of the variable to be processed. }
  \item{file}{ the folder and first part of the file name for saving the function output. }
}
\details{
\code{file} contains the folder name where the output file is to be saved and the first part of the unique file name that will be created and used to store the .csv file of results. The function concatenates this with an \code{_}, \code{group} as a character string, another \code{_}, and \code{x} as a character string. Subsequently the suffix \code{.csv} is appended and file saved in the folder indicated.

Output to the current device is suppressed. The output file can be inspected with spread sheet software or a viewer of the user's choice.
}
\author{ Robert G. Garrett }
\note{ 
Any less than detection limit values represented by negative values or zeros or other numeric codes representing blanks in the data vector must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any \code{NA}s in the data vector are counted and then removed prior to computing the summary statistics.

The function \code{\link{framework.stats}} is employed to compute the summary statistics.
}
\seealso{ \code{\link{framework.stats}}, \code{\link{ltdl.fix.df}}, \code{\link{remove.na}} }
\examples{
## Make test data available
data(kola.c)
attach(kola.c)

## Saves the file c:\temp\kola_c_summary_COUNTRY_Cu.csv for later use.
## Note the necessity of using double back-slashes in the file name.
framework.summary(COUNTRY, Cu, file = "c:\\\temp\\\kola_c_summary")

## Detach test data
detach(kola.c)  
}
\keyword{ univar }

