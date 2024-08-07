% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/00.helpers.R
\name{is.null.na}
\alias{is.null.na}
\title{Function to check whether an object is \code{NA} or \code{NULL}}
\usage{
is.null.na(x, negating = FALSE)
}
\arguments{
\item{x}{Object on which to operate}

\item{negating}{Whether to return the negation of the result}
}
\value{
Logical, depending on \code{negating}:
\itemize{
 \item if \code{negating} is \code{FALSE}, it returns \code{TRUE} if \code{x} is \code{NA} or \code{NULL};
 \item if \code{negating} is \code{TRUE}, it returns \code{TRUE} if \code{x} is \strong{neither} \code{NA} \strong{nor} \code{NULL}.
}
}
\description{
Combines \code{base::is.na(x)}, \code{base::is.null(x)}. When \code{negating} is \code{TRUE}, it integrates also \code{f(x)|> magrittr::not()}.
}
\author{
\enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
}
\keyword{internal}
