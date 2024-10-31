#' Compute Bonacich centrality on a Firm-Firm network
#'
#' Based in part on the earlier work of Katz (1953) and Hubbell (1965),
#' Bonacich (1972a, 1972b) argued that a point's centrality in a network
#' should depend on three criteria: (1) the number of links to other
#' points; (2) the intensity of the links; and (3) the centrality of those
#' with whom one is linked. This centrality has been applied to board
#' interlocks for the first time by Mizruchi and Bunting (1981).
#'
#' @param FF An object of class \code{financial_matrix} or an object of class \code{igraph_financial} (or  \code{network_financial}) produced by \code{\link{FF}} and related functions to represent the relations between firms (legal person).
#' @param exo A value for the exogenous parameter of the Bonacich centrality. Defaults to \code{1}.
#' @param alpha A value for the alpha parameter of the Bonacich centrality. Defaults to a function of the matrix's largest eigenvalue.
#' @param tol A value for the tolerance of the Bonacich centrality. Defaults to \code{1e-07}.
#'
#' @return A scalar vector with the Bonacich centrality of each firm.
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#'
#' @references Katz, Leo. 1953. “A new status index derived from sociometric analysis.” Psychometrika 18: 39-43 \doi{10.1007/BF02289026}.
#' @references Bonacich, Phillip. 1972a. “Technique for Analyzing Overlapping Memberships.” Sociological Methodology 4: 176–85. \doi{10.2307/270732}.
#' @references Bonacich, Phillip. 1972b. “Factoring and Weighting Approaches to Status Scores and Clique Identification.” The Journal of Mathematical Sociology 2 (1): 113–20. \doi{10.1080/0022250X.1972.9989806}.
#' @references Mizruchi, Mark S., and David Bunting. 1981. “Influence in Corporate Networks: An Examination of Four Measures.” Administrative Science Quarterly 26, no. 3: 475–89. \doi{10.2307/2392519}.
#'
#' @examples
#' # Find the Bonacich centralities for all the companies that Berkshire Hathaway holds
#' data('firms_BKB')
#' FF_BKB <- FF.norm.ownership(firms_BKB)
#' bonacich(FF_BKB)
#'
#' @name bonacich
#' @export

bonacich <- function(FF, exo = 1, alpha = NULL, tol = 1e-07) {
  mat <- if('financial_net'%in%is(FF)){
    extract.matrix(FF)
  } else if('financial_matrix'%in%is(FF)){
    FF@M
  } else {
    paste0('The object `FF` must be of class `financial_matrix`,',
          ' `igraph_financial` or `network_financial`, not ',
          is(FF)[1], '!')|> stop()
  }
  # Density
  if(is.null(alpha))alpha <- round({1/max(eigen(mat)$values)}*.9, 4)
  ({sum(rescale.numeric(mat, c(0, 1)))/ # Total normalised value of present ties
      ifelse(all(diag(mat)==0), # If there are no self-loops
             nrow(mat)*(ncol(mat)-1), # Divide by $n(n-1)$
             prod(dim(mat))) # Else, divide by $n^2$
  }<=.2)|> # Threshold for sparsity
    # Assign correct function
    ifelse(bonacich.sparse, bonacich.dense) -> f

  # Execute computation
  f(mat, exo = exo, alpha = alpha, tol = tol)
}

#' Function to compute Bonacich centrality efficiently on sparse and dense matrices
#'
#' Called upon by \code{\link{bonacich}}.
#'
#' @param mat A \code{matrix} object.
#' @param exo A value for the exogenous parameter of the Bonacich centrality. Defaults to \code{1}.
#' @param alpha A value for the alpha parameter of the Bonacich centrality. Defaults to a function of the matrix's largest eigenvalue.
#' @param tol A value for the tolerance of the Bonacich centrality. Defaults to \code{1e-07}.
#'
#' @return A scalar vector with the Bonacich centrality of each unit.
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @name bonacich-internal

#' @keywords internal
#' @rdname bonacich-internal
bonacich.sparse <- function(mat, exo = 1, alpha = 1, tol = 1e-07){
  n <- nrow(mat)
  M2 <- Matrix::sparseMatrix(dims = c(n, n), i = 1:n, j = 1:n, x = rep(1, n))
  exo <- cbind(rep(exo, length.out = n))
  M3 <- M2 - alpha * mat
  Matrix::solve(M3, tol = tol, exo)|> as.vector()
}

#' @keywords internal
#' @rdname bonacich-internal
bonacich.dense <- function(mat, exo = 1, alpha = 1, tol = 1e-07){
  n <- nrow(mat)
  exo <- rep(exo, length.out = n)
  exo <- matrix(exo, ncol = 1)
  id <- matrix(0, nrow = n, ncol = n)
  diag(id) <- 1
  as.vector(solve(id - alpha * mat, tol = tol) %*% exo)
}
