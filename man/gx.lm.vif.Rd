\name{gx.lm.vif}
\alias{gx.lm.vif}
\title{ Estimate Variance Inflation Factosr (VIFs). }
\description{
Function estimates Variance Inflation Factors (VIFs), measures of collinearity in a linear model.  The VIF provides a measure of how much the variance of an estimated regression coefficient is increased because of collinearity.  Collinearity is present when there is a high correlation between the independent, predictor, variables in a model, i.e. they tell the same \sQuote{story}.  Where collinearity exists it is often best to remove predictor variables with high VIFs from the model.
}
\usage{
gx.lm.vif(object, ...)
}
\arguments{
  \item{object}{ a \code{\link{lm}} object. }
  \item{\dots}{ any additional parameters. }
}
\value{
A (structure) table of Variable Inflation Factors for the predictor variables.
}
\note{
VIFs >5 are indicative of collinearity, and the information conveyed in that variable is also in the subset of the remaining variables.
}
\references{
http://www.biostat.wustl.edu/archives/html/s-news/2001-10/msg00164.html
}
\author{ W.N. Venables,function shared on S-News, October 21, 2002
}
\examples{
## Make test data available
data(sind)
attach(sind)

## Model 1
sind.1 <- lm(log(Zn) ~ Fe + log(Mn) + log(Cu) + log(Cd))
summary(sind.1)
gx.lm.vif(sind.1)

## Model 2
sind.2 <- lm(log(Zn) ~ Fe + log(Mn))
summary(sind.2)
gx.lm.vif(sind.2)
AIC(sind.1, sind.2)

## Model 3
sind.3 <- lm(log(Zn) ~ log(Mn) + log(Cu))
summary(sind.3)
gx.lm.vif(sind.3)
AIC(sind.1, sind.2, sind.3) 

## Clean-up and detach test data
rm(sind.1)
rm(sind.2)
rm(sind.3)
detach(sind) 
}
\keyword{ models }

