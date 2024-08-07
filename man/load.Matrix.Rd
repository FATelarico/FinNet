% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/00.helpers.R
\name{load.Matrix}
\alias{load.Matrix}
\title{Function to  load the package \code{Matrix}}
\usage{
load.Matrix(x, pos = 1L)
}
\arguments{
\item{x}{The object on which to base the decision}

\item{pos}{Integer specifying position to attach}
}
\value{
A \code{logical} whether to load the package \code{Matrix}
}
\description{
Checks if the package is required and check whether it is available
}
\seealso{
Other Internal matrix (de)constructrs: 
\code{\link{extract.matrix}()},
\code{\link{get.matrix.adjacency}()},
\code{\link{get.matrix.adjacency.both}()}
}
\author{
\enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
}
\concept{Internal matrix (de)constructrs}
\keyword{internal}
