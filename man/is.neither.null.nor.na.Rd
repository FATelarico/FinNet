% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/00.helpers.R
\name{is.neither.null.nor.na}
\alias{is.neither.null.nor.na}
\title{Function to check whether an object is neither \code{NA} nor \code{NULL}}
\usage{
is.neither.null.nor.na(x)
}
\arguments{
\item{x}{Object on which to operate}
}
\value{
Logical: \code{TRUE} if \code{x} is neither \code{NA} or \code{NULL}, \code{FALSE} otherwise.
}
\description{
Combines \code{base::is.na(x)|> magrittr::not()} and \code{base::is.null(x)|> magrittr::not()}.
}
\author{
\enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
}
\keyword{internal}
