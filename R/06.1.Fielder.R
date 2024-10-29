#' Calculate the Fiedler value (algebraic connectivity)
#'
#' This function expresses the algebraic connectivity of a Firm-Firm network as
#' its Fiedler value. The Fiedler value, named after Miroslav Fiedler, who
#' explored its significance, summarises the connectivity and robustness of a
#' network. Mathematically, it is the second smallest eigenvalue
#' of the network's Laplacian matrix
#'
#' The Fiedler value is specifically defined for undirected graphs. For directed
#' or asymmetrical networks like the Firm-Firm ones, the Laplacian is not
#' necessarily symmetric, and its eigenvalues can be complex. In practical applications,
#' this is more likely to happen in naively valued networks, due to the presence of
#' large off-diagonal values. But it can happen also under other circumstances.
#'
#' There are three main workarounds:
#'
#' 1. Symmetrisation - Simply considers the underlying undirected graph.
#' This involves ignoring the direction of edges and calculating the Laplacian
#' matrix and its eigenvalues as if the graph were undirected.
#'
#' 2. The generalised Laplacian calculated as the sum of the diagonal matrices for
#' in-degree and out-degree \eqn{\left([D]_{in}+[D]_{out}\right)/2}
#'
#' 3. The Hermitian part of the Laplacian - Uses the Hermitian part of the
#' Laplacian matrix of the directed network \eqn{\left([M]+{[M]^\text{C}}^\top)\right)/2},
#' where the second addendum is the conjugate transpose of the adjacency matrix.
#'
#' Practically, the third method is excessive here, as the values of the ties
#' cannot be complex numbers. Indeed, the Hermitian is actually the Laplacian
#' of the underlying symmetric network with the value of the ties being split
#' equally in both directions because the conjugate of a real number is that number.
#' Moreover, symmetrising before calculating the Laplacian or generalising the matrix
#' \eqn{[L]} returns the same result. So, the parameter \code{generalise} is logical
#' and takes the following values:
#' \enumerate{
#'  \item{\code{TRUE} for the generalised Laplacian;}
#'  \item{\code{FALSE} for the possibly complex (and uninterpretable) eigenvalue of the as-is Laplacian.}
#'  \item{\code{NULL} will take the generalised Laplacian only if necessary.}
#' }
#'
#' @param ... Firm-Firm network in one of the following classes:
#' \itemize{
#'  \item{\code{financial_matrix} produced by \code{FF} and family;}
#'  \item{\code{network_financial} or \code{network} if the relevant package is installed;}
#'  \item{\code{igraph_financial} or \code{igraph} if the  relevant package is installed.}
#' }
#' @param ignore.weights Optional parameter, defaults to \code{FALSE}. If \code{TRUE}, ignore ties weights in the computation.
#' @param generalise See Details for more information. Defaults to \code{NULL}.
#'
#' @return A numeric, the Fiedler value.
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @references Fiedler, Miroslav. ‘Laplacian of Graphs and Algebraic Connectivity’. Banach Center Publications 25, no. 1 (1989): 57–70. \url{https://eudml.org/doc/267812}.
#' @references Guo, Krystal, and Bojan Mohar. ‘Hermitian Adjacency Matrix of Digraphs and Mixed Graphs’. Journal of Graph Theory 85, no. 1 (May 2017): 217–48. \doi{10.1002/jgt.22057}.
#'
#' @examples
#' # Load some data
#' data('firms_BKB')
#' # Create a FF matrix
#' mat <- FF(firms_BKB, who = 'b', ties =  'n')
#' fiedler(mat)
#'
#' # Create a FF network
#' if(!require('network')){
#'   net <- FF.net(mat, 'simple')
#'   fiedler(net)==fiedler(mat)
#' }
#'
#' # Create a FF graph
#' if(!require('igraph')){
#'   g <- FF.graph(mat, 'simple')
#'   fiedler(g)==fiedler(mat)
#' }
#'
#' @export


fiedler <- function(..., ignore.weights = FALSE,
                    generalise = NULL){
  x <- ...elt(1)

  x <- extract.matrix(x)

  # Whether to ignore the weights
  if(ignore.weights&&any(!as.vector(x)%in%c(0, 1))){
    x[x>1] <- 1
  }

  # For automatic or no generalisation
  if(!isTRUE(generalise)){
    # The Laplacian is [L] = [D] - [M] where
    #   [D] is a diagonal matrix in which the terms are
    #       the total degrees by vertex
    #   [M] is the square/one-mode adjacency matrix
    # The Fiedler value is [L]'s second-smallest eigenvalue
    y <- eigen(rowSums(x)|> diag() - x)$values[2]

    # Set automatic generalisation
    if(is.null(generalise))generalise <- is.complex(y)
  }

  if(!generalise){# Generalisation (auto or manual)
    # The generalised Laplacian is [L] = ([D]_{in}+[D]_out)/2-([M]-[M]^T)/2
    eigen(
      (diag(rowSums(x))+diag(colSums(x)))/2-
        (x+t(x))/2
    )$values[2]
  } else {  # No generalisation (auto or manual)
    y
  }
}

