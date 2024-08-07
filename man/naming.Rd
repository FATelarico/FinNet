% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/00.helpers.R
\name{naming}
\alias{naming}
\title{Function to extract the symbols of the objects passed as \code{...}}
\usage{
naming(...)
}
\arguments{
\item{...}{Objects on which to operate}
}
\value{
A vector of strings matching the provided objects' symbols.
}
\description{
This function will not work when called inside the function to which the objects were explicitly passsed as \code{...}.
}
\references{
\url{https://stackoverflow.com/a/11892680}
}
\author{
\enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
}
\keyword{internal}
