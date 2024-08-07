% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/05.1.graph_methods.R
\name{plot_igraph-methods}
\alias{plot_igraph-methods}
\alias{plot_igraph,igraph-method}
\alias{plot_igraph,igraph_financial-method}
\title{igraph plotting for \code{igraph_financial} objects}
\usage{
\S4method{plot_igraph}{igraph_financial}(x, ...)

\S4method{plot_igraph}{igraph}(x, ...)
}
\arguments{
\item{x}{The \code{igraph_financial} object}

\item{...}{Other parameters passed to the corresponding method and/or \code{igraph} functions (see Details).}
}
\value{
For both \code{igraph} and \code{igraph_financial} objects, returns NULL invisibly. It is called to print the graph to any R device. (see method and \href{https://rdrr.io/cran/igraph/man/plot.igraph.html}{igraph::plot.igraph})
}
\description{
Methods to extend \code{igraph}'s plotting functions to \code{igraph_financial} objects
}
\author{
\enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
}
