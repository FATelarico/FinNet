% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/00.helpers.R
\name{SCC2}
\alias{SCC2}
\title{Tarjan's algorithm for finding strongly connected components in C++}
\usage{
SCC2(x)
}
\arguments{
\item{x}{A square adjacency matrix representing the directed network.}
}
\value{
A list containing two elements:
\itemize{
 \item{\code{n} - number of strongly connected components}
 \item{\code{sizes} - size of each strongly connected component, in order of discovery}
}
}
\description{
This function performs a depth-first search (DFS) on a directed graph to identify
strongly connected components (SCCs) and their size
}
\details{
This function is a modified version of the C++ implementation of Tarjan's
algorithm for finding strongly connected components in directed graphs made
available by the webiste Geeks for Geeks (see References).

The function consists of several internal steps:
\enumerate{
  \item{Node Labeling - All nodes are labeled with two-digit names for clarity in referencing.}
  \item{Successor List Creation - For each node, lists of direct successors are compiled.}
  \item{Utilization Table Setup - A table is set up for tracking exploration details such as depth and backtracking information.}
  \item{Main DFS Loop - The core loop where DFS occurs, including node visitation and backtracking logic to determine SCCs.}
  \item{Stack Management - Nodes are managed in a stack to keep track of the current path of exploration and to facilitate backtracking.}
  \item{SCC Identification - Upon finishing exploration of an SCC, it is identified and nodes are popped from the stack.}
}
}
\references{
Geeks for Geeks. ‘Strongly Connected Components’. C++, 17 January 2024. \url{https://www.geeksforgeeks.org/strongly-connected-components/}.
}
\seealso{
Other SCC finders: 
\code{\link{SCC}()}
}
\author{
\enc{Geeks for Geeks}{GeeksForGeeks}

\enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
}
\concept{SCC finders}
\keyword{internal}
