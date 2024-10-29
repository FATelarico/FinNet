#' Calculate network efficiency
#'
#' Network efficiency quantifies how efficiently information (management relations)
#' and/or money capital (ownership relations) flow through a network. It is essential in
#' systemic-risk identification, resilience assessment, and crisis-propagation analysis.
#'
#' The function is implemented both for \code{igraph} users and in base \code{R} using the Floyd-Warshall algorithm.
#' However, the latter runs in \eqn{O(n^3)}, which may not be efficient for very large networks.
#'
#' The distances enter into play in the formal definition of efficiency:
#'
#' \deqn{E = \frac{1}{N(N-1)}\sum_{i\ne j \in \mathcal{N}}\frac{1}{d_{i,\ j}}}
#'
#' where:
#' \itemize{
#'  \item{\eqn{\mathcal{N}} is the set of all nodes};
#'  \item{\eqn{N} is the number of nodes (i.e., the number of elements in \eqn{\mathcal{N}};}
#'  \item{\eqn{d_{i,\ j}} is the shortest (weighted and directed) path distance between the nodes \eqn{i} and \eqn{j}.}
#'}
#'
#' @param ... Firm-Firm network in one of the following classes:
#' \itemize{
#'  \item{\code{financial_matrix} produced by \code{FF} and family;}
#'  \item{\code{network_financial} or \code{network} if the relevant package is installed;}
#'  \item{\code{igraph_financial} or \code{igraph} if the  relevant package is installed.}
#' }
#' @param ignore.weights Optional parameter, defaults to \code{FALSE}. If \code{TRUE}, ignore ties weights in the computation.
#' @param use.igraph Whether to use igraph to speed-up the computation. See 'Details'.
#'
#' @return A \code{numeric}, the global efficiency value.
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @references Latora, Vito, and Masimo Marchiori. 'Economic Small-World Behavior in Weighted Networks'. The European Physical Journal B - Condensed Matter and Complex Systems 32, no. 2 (1 March 2003): 249–63. \doi{10.1140/epjb/e2003-00095-5}.
#' @references Floyd, Robert W. 'Algorithm 97: Shortest path'. Communications of the ACM, 5, no. 6 (1962): 345.
#'
#' @examples
#' # Load some data
#' data('firms_BKB')
#'
#' # Create a FF matrix
#' mat <- FF(firms_BKB, who = 'b', ties =  'n')
#' # Use the built-in Floyd-Warshall algorithm
#' network.efficiency(mat, use.igraph = FALSE)
#'
#' #' # Create a FF graph
#' if(!require('igraph')){
#'   g <- FF.graph(mat, 'simple')
#'   # Use igraph's implementation, which gives the same result
#'   # as the built-in Floyd-Warshall algorithm, but is faster
#'   network.efficiency(g, use.igraph = TRUE)==network.efficiency(mat, use.igraph = FALSE)
#' }
#'
#' @export


network.efficiency <- function(..., ignore.weights = FALSE,
                             use.igraph = isTRUE(requireNamespace('igraph', quietly = TRUE))){
  x <- ...elt(1)

  x <- extract.matrix(x, .except.igraph = TRUE)


  # Load `Matrix` if required and check whether it is available
  load.Matrix(x)
  weighted <- any(!{as.vector(x)%in%0:1})&&{!ignore.weights}

  if(use.igraph){
    mat <- igraph::graph_from_adjacency_matrix(
      adjmatrix = as.matrix(x),
      mode = 'directed',
      weighted = if(weighted){TRUE}else{NULL}
    )
    d <- igraph::distances(graph = mat, mode = 'out',
                           weights = if(weighted){NULL}else{NA},
                           algorithm = 'dijkstra')

  } else {
    # Convert adjacency matrix to distance matrix
    # Note that 0s are turned to Inf (except diagonal elements)
    d <- ifelse(x == 0, Inf, x)

    # Distance from each node to itself is zero
    diag(d) <- 0

    # Stage repetitions
    n <- nrow(d)

    for (k in 1:n) { # For each unit
      for (i in (1:n)[-k]) { # Starting from each other unit
        for (j in (1:n)[-c(i, k)]) { # Going to each third unit
          # Take the shortest path
          d[i, j] <- min(d[i, j], d[i, k] + d[k, j])
        }
      }
    }; rm(k, i, j, n)
  }

  d <- 1/d
  d[is.infinite(d)] <- 0
  sum(d) / (nrow(d) * (ncol(d) - 1))
}

