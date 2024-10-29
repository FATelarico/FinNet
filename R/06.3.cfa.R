#' Perform cascade failure analysis
#'
#' Cascade failure analysis (CFA) involves understanding how failures in one
#' part of the network might cascade to other parts. Networks capable of
#' isolating such failures or minimizing their effects demonstrate higher
#' robustness.
#'
#' @param ... Firm-Firm network in one of the following classes:
#' \itemize{
#'  \item{\code{financial_matrix} produced by \code{FF} and family;}
#'  \item{\code{network_financial} or \code{network} if the relevant package is installed;}
#'  \item{\code{igraph_financial} or \code{igraph} if the  relevant package is installed.}
#' }
#' @param ordering In what order to remove the firms, the completing ordering is always returned as part of the result. Take the following values:
#' \itemize{
#'  \item{\code{random} - Random order, corresponds to percolation theory}
#'  \item{\code{out} - By out-degree}
#'  \item{\code{in} - By in-degree}
#'  \item{\code{tot} - By sum of in- and out-degre (default)}
#'  \item{\code{custom} - Customised ordering via \code{custom.order}}
#' }
#' @param custom.order Order in which to remove the firms. If \code{ordering} is not 'custom', it is ignored. Defaults to \code{NULL}.
#' @param decreasing Logical, defaults to \code{TRUE}. Only evaluated if \code{ordering} is neither 'custom' nor 'random'.
#' \itemize{
#'  \item{if \code{TRUE} - the ordering is by decreasing degree and higher in-/out-/total-degree firms are removed first}
#'  \item{if \code{FALSE} - the ordering is by increasing degree and higher in-/out-/total-degree firms are removed last}
#'}
#' @param Rcpp Whether to use the \code{C++} or native-\code{R} version of the search algorithm. Defaults to \code{TRUE} if the package \code{Rcpp} is installed.
#'
#' @return A \code{data.frame} with one row for the result of the CFA after each node is removed. The columns report:
#' \itemize{
#'  \item{\code{l_scc} - Size of the largest strongly connected component}
#'  \item{\code{rem_id} - ID of the firm removed}
#'  \item{\code{rem_pos} - Position of the firm removed (row/column number)}
#'  \item{\code{n_scc} - Number of strongly connected components}
#'  \item{\code{n_rem} - Number of firms removed}
#'  \item{\code{n_left} - Number of firms left}
#' }
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @references Elliott, Matthew, Benjamin Golub, and Matthew O. Jackson. ‘Financial Networks and Contagion’. American Economic Review 104, no. 10 (1 October 2014): 3115–53. \doi{10.1257/aer.104.10.3115}.
#'
#' @examples
#' # Create a matrix
#' mat <- matrix(c(
#'     0, 1, 0, 1, 0, 1, 0, 0,
#'     0, 0, 1, 0, 0, 0, 0, 0,
#'     1, 0, 0, 0, 0, 0, 0, 0,
#'     0, 1, 1, 0, 1, 0, 0, 0,
#'     0, 0, 0, 1, 0, 1, 0, 0,
#'     0, 0, 1, 0, 0, 0, 1, 0,
#'    0, 0, 0, 0, 0, 1, 0, 0,
#'     0, 0, 0, 0, 1, 0, 1, 1
#'   ),ncol = 8, byrow = TRUE)
#' # Add rownames
#' rownames(mat) <- paste0("Firm", LETTERS[1:ncol(mat)])
#'
#' # Create a FF matrix
#' mat <- methods::new('financial_matrix',
#'                     M = mat,
#'                     relation = c('own'),
#'                     legal_form = c('JSC'),
#'                     sector = c('A.01'),
#'                     revenues = c(NA),
#'                     capitalisation = c(NA),
#'                     currency = c('USD'))
#'
#' # Notice the differnce between:
#' # a. CFA with ordering by in-degree (decreasing)
# cfa(mat, ordering = 'in')
#' # b. CFA with ordering by in-degree (increasing)
#' cfa(mat, ordering = 'in', decreasing = FALSE)
#' # But ordering by increasing (decreasing) in-degree is the
#' # same as ordering by decreasing (increasing) out-degree and
#' # vice versa!
#' cfa(mat, ordering = 'out', decreasing = FALSE) # By out-degree (increasing)
#'
#' @export


cfa <- function(..., ordering = 'tot', custom.order = NULL,
                decreasing = TRUE,
                Rcpp = ifelse(requireNamespace('Rcpp', quietly = TRUE),
                              yes =  TRUE, no = FALSE)) {

  x <- ...elt(1)

  x <- extract.matrix(x)

  # Load `Matrix` if required and check whether it is available
  load.Matrix(x)
  if(ordering!='random')x <- as.matrix(x)
  # Number of units
  n <- nrow(x)

  # Units' ids
  ids <- rownames(x)

  # Order of node failures
  nodes <- switch(ordering,
                  'random' =  sample(n), # Random
                  'out' =  rowSums(x), # Outdegree
                  'in' =  colSums(x), # Intdegree
                  'tot' = rowSums(x)+colSums(x), # Tot
                  'custom' = if(is.null(custom.order)){
                    stop('If `ordering` is \'custom\', then `custom.order` cannot be `NULL`')
                  } else {
                    if(length(custom.order)!=n){
                      stop('If `ordering` is \'custom\', then `custom.order` must order all firms!')
                    }
                    custom.order
                  }
  )
  # For degree-ordering
  if(ordering %in% c('out', 'in', 'tot')){
    if({!is.logical(decreasing)}&&!{decreasing%in%c(0,1)})stop(paste0(
      'If the `ordering` is \'out\', \'in\', or \'tot\',',
      '`decreasing` must be logical (or `1` or `0`), not ',
      decreasing, '!'
    ))
    # Turn degrees into orders
    nodes <-  order(nodes, decreasing = decreasing)
  }

  # Prepare outputs
  sizes <- num <- numeric(n)

  # Sort the units so that they are removed in the chosen order
  x <- x[nodes, nodes]

  # Adapt inputs for C++ function if necesary
  f <- ifelse(Rcpp, SCC2, SCC)
  y <- f(x)
  sizes[1] <- max(y$sizes)
  num[1] <- y$n
  # For each node in the ordering
  for(i in 1:{ncol(x)-1}){
    # Remove the $i^{th}$ node
    x <- matrix(x[-1, -1], nrow = nrow(x)-1, ncol = ncol(x)-1)
    # Apply the function
    y <- f(x)
    sizes[i+1] <- max(y$sizes)
    num[i+1] <- y$n
  }; rm(i, y)

  # Produce final data frame
  data.frame(l_scc = sizes,
             rem_id = if(!is.null(ids)){
               c('', ids[nodes[-n]])
             } else {
               ''
             },
             rem_pos = c(NA, nodes[-n]),
             n_scc = num,
             n_rem = 0:(n-1),
             n_left = n-(0:(n-1)))
}

