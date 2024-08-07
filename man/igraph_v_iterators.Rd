% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/05.1.graph_methods.R
\name{igraph_v_iterators}
\alias{igraph_v_iterators}
\alias{V,igraph-method}
\alias{V,igraph_financial-method}
\alias{vcount,igraph-method}
\alias{vcount,igraph_financial-method}
\alias{gorder,igraph-method}
\alias{gorder,igraph_financial-method}
\title{igraph vertex iterators for \code{igraph_financial} objects}
\usage{
\S4method{V}{igraph_financial}(x)

\S4method{V}{igraph}(x)

\S4method{vcount}{igraph_financial}(x)

\S4method{vcount}{igraph}(x)

\S4method{gorder}{igraph_financial}(x)

\S4method{gorder}{igraph}(x)
}
\arguments{
\item{x}{The \code{igraph_financial} object}
}
\value{
The same result for both \code{igraph} and \code{igraph_financial} objects
\itemize{
 \item \code{V}: A vertex sequence containing all vertices, in the order of their numeric vertex ids
 \item \code{vcount} and \code{gorder}: Number of vertices, numeric scalar
}
}
\description{
Methods to extend igraph vertex iterators and functions to \code{igraph_financial} objects
}
\author{
\enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
}
