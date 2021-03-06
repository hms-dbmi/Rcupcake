% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comparison2b2.R
\name{comparison2b2}
\alias{comparison2b2}
\title{Comparison by pairs of variables in a  \code{cupcakeData} object}
\usage{
comparison2b2(input, variable1, variable2, nfactor = 10,
  verbose = FALSE)
}
\arguments{
\item{input}{A \code{cupcakeData} object, obtained after applying the my.data function.}

\item{variable1}{The name of the first variable that has to be the same that 
the colname of the variable in the input data.frame.}

\item{variable2}{The name of the second variable that has to be the same that 
the colname of the variable in the input data.frame.}

\item{nfactor}{By default 10. Change it into other number if you consider there is any
continuous variable with less than \code{nfactor} values.}

\item{verbose}{By default \code{FALSE}. Change it to \code{TRUE} to get an
on-time log from the function.}
}
\value{
An object of class \code{cupcakeResults}
}
\description{
Given an object of type \code{cupcakeData} and two variables of the 
data.frame a statistically comparison is performed. According to the 
type of variable different test will be run (fisher test, t-test or 
phearson test). As a result the p-value and some other information 
will be return.
}
\examples{
load(system.file("extdata", "RcupcakeExData.RData", package="Rcupcake"))
comparison <- comparison2b2( 
              input         = RcupcakeExData, 
              variable1     = "Age",
              variable2     = "Gender"
              )
}
