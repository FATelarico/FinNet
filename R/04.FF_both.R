#' Create a firm-firm (FF) matrix
#'
#'
#' Function to create a firm-firm (FF) matrix based on both common ownership and board interlocks
#'
#' The possible values of \code{ties} and their effect relative to the value of \code{who} are:
#'
#'   \code{binary} or \code{1} for binary ties: \eqn{1} if there is at least one common manager or owner, \eqn{0} otherwise.
#'
#'   \code{naive}, or \code{2} for 'naively' valued ties indicating the number of common managers and owners.
#'
#'   \code{share} or \code{3} (the default) for 'normalised' tie values. The actual value depends on \code{combining}:
#'   \itemize{
#'    \item for \code{sum} (the default), the sum of the share of \eqn{i}'s owners who are also amongst \eqn{j}'s owners and the share of \eqn{i}'s managers who are also amongst \eqn{j}'s managers (normalised to 2);
#'    \item for \code{mean} or \code{avg}, the mean of the share of \eqn{i}'s owners who are also amongst \eqn{j}'s owners and the share of \eqn{i}'s managers who are also amongst \eqn{j}'s managers (normalised to 1);
#'    \item for \code{max}, the maximum between the share of \eqn{i}'s owners who are also amongst \eqn{j}'s owners and the share of \eqn{i}'s managers who are also amongst \eqn{j}'s managers (normalised to 1);
#'    \item for \code{min}, the minimum between the share of \eqn{i}'s owners who are also amongst \eqn{j}'s owners and the share of \eqn{i}'s managers who are also amongst \eqn{j}'s managers (normalised to 1);
#'   }
#'
#' @param ... Either multiple objects of class \code{firm} or a list of such objects
#' @param ties Type of ties to create. Defaults to \code{2}; for other possible values, see details.
#' @param id_as_firm_name Whether to use the ticker as the firm's name. Defaults to \code{TRUE} if all firms' id is neither \code{NULL} nor \code{NA}.
#' @param Matrix Whether to use the \href{https://cran.r-project.org/package=Matrix}{\code{Matrix} package}. Defaults to \code{TRUE} when any matrix in the pipeline contains more than 10,000 cells and the package is installed.
#' @param self_ties Whether to allow self-ties (a 'loop' in graph theory). Defaults to \code{FALSE}.
#' @param combining How to combine the FF matrix for managers and that for owners (see Details);
#'
#' @return A matrix object of class \code{financial_matrix}(possibly using the \href{https://cran.r-project.org/package=Matrix}{\code{Matrix} package})
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @family Internal matrix (de)constructrs
#'
#' @seealso \link{FF}.
#'
#' @keywords internal

get.matrix.adjacency.both <- function(...,  ties = 3, id_as_firm_name = NULL,
                                      Matrix = NULL, self_ties = FALSE,
                                      combining = 'sum'){
  if(...length()==1){
    firms <- ...elt(1)
    names(firms) <- names(...elt(1))
  } else {
    firms <- listing(...)
    names(firms) <-
      as.character(match.call(expand.dots = TRUE))[-1][1:length(firms)]
  }

  if(!combining %in% c('sum', 'mean', 'avg', 'min', 'max')){
    warning(paste0('The parameter `combining` cannot be: ',
                   combining, '! Please refer to the documentation'))
  }

  if(is.null.na(Matrix))Matrix <- length(firms)>100
  if(Matrix)requireNamespace('Matrix', quietly = TRUE)

  mat_man <- get.matrix.adjacency(firms, who = 'man', ties = ties,
                                  id_as_firm_name = id_as_firm_name,
                                  Matrix = Matrix, self_ties = self_ties)

  mat_own <- get.matrix.adjacency(firms, who = 'own', ties = ties,
                                  id_as_firm_name = id_as_firm_name,
                                  Matrix = Matrix, self_ties = self_ties)

  mat <- if(combining == 'sum'){
    mat_own@M+mat_man@M
  } else {
    temp <- mat_own@M
    f <- ifelse(combining %in% c('mean', 'avg'), mean,
                ifelse(combining == 'min', min, max))
    for(i in seq_len(nrow(mat_own@M))){
      for(j in (i+1):ncol(mat_own@M)){
        temp[i, j] <- temp[i, j] <-
          f(mat_own@M[i, j], mat_man@M[i, j])
      }
    }
    temp
  }

  dimnames(mat) <- dimnames(mat_own)

  methods::new('financial_matrix',
               M = mat,
               relation = 'both',
               legal_form = query.firms(firms, which = 'legal_form',
                                        naming = FALSE)|> unlist(),
               sector = query.firms(firms, which = 'sector',
                                    naming = FALSE)|> unlist(),
               revenues = query.firms(firms, which = 'revenues',
                                      naming = FALSE)|> unlist(),
               capitalisation = query.firms(firms, which = 'capitalisation',
                                            naming = FALSE)|> unlist(),
               currency = query.firms(firms, which = 'currency',
                                      naming = FALSE)|> unlist())
}


#' Create a complete binary firm-firm (FF) matrix
#'
#' Function to create a binary firm-firm (FF) matrix based on both common ownership and board interlocks
#'
#' The ties' value will be: \eqn{1} if there is at least one common manager or owner, \eqn{0} otherwise.
#'
#' @param ... Either multiple objects of class \code{firm} or a list of such objects
#' @param id_as_firm_name Whether to use the ticker as the firm's name. Defaults to \code{TRUE} if all firms' id is neither \code{NULL} nor \code{NA}.
#' @param Matrix Whether to use the \href{https://cran.r-project.org/package=Matrix}{\code{Matrix} package}. Defaults to \code{TRUE} when any matrix in the pipeline contains more than 10,000 cells and the package is installed.
#' @param self_ties Whether to allow self-ties (a 'loop' in graph theory). Defaults to \code{FALSE}.
#' @param combining How to combine the FF matrix for managers and that for owners. Possible values:
#'  \itemize{
#'  \item \code{sum};
#'  \item \code{mean} or \code{average};
#'  \item \code{min};
#'  \item \code{max};
#' }
#'
#' @return A matrix object of class \code{financial_matrix}(possibly using the \href{https://cran.r-project.org/package=Matrix}{\code{Matrix} package})
#'
#' @family Financial_matrix builders
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @examples
#'
#' # Create the complete binary firm-firm matrix for the companies held by Berkshire Hathaway
#' data('firms_BKB')
#' FF <- FF.binary.both(firms_BKB)
#'
#'
#' @export

FF.binary.both <- function(..., id_as_firm_name = NULL,
                           Matrix = NULL, self_ties = FALSE,
                           combining = 'sum'){

  get.matrix.adjacency.both(...,  ties = 1, id_as_firm_name = id_as_firm_name,
                            Matrix = Matrix, self_ties = self_ties,
                            combining = combining)
}

#' Create a complete naive-valued firm-firm (FF) matrix
#'
#' Function to create a naive-valued firm-firm (FF) matrix based on both common ownership and board interlocks
#'
#' The ties' value will reflect the count of common owners and membership depending on \code{combining}:
#' \itemize{
#'  \item \code{sum}: sum of the counts;
#'  \item \code{mean} or \code{average}: average of the counts;
#'  \item \code{min}: minimum of the counts;
#'  \item \code{max}: maximum of the counts.
#' }
#'
#' @param ... Either multiple objects of class \code{firm} or a list of such objects
#' @param id_as_firm_name Whether to use the ticker as the firm's name. Defaults to \code{TRUE} if all firms' id is neither \code{NULL} nor \code{NA}.
#' @param Matrix Whether to use the \href{https://cran.r-project.org/package=Matrix}{\code{Matrix} package}. Defaults to \code{TRUE} when any matrix in the pipeline contains more than 10,000 cells and the package is installed.
#' @param self_ties Whether to allow self-ties (a 'loop' in graph theory). Defaults to \code{FALSE}.
#' @param combining How to combine the FF matrix for managers and that for owners. Possible values:
#'  \itemize{
#'  \item \code{sum};
#'  \item \code{mean} or \code{average};
#'  \item \code{min};
#'  \item \code{max};
#' }
#'
#' @return A matrix object of class \code{financial_matrix}(possibly using the \href{https://cran.r-project.org/package=Matrix}{\code{Matrix} package})
#'
#' @family Financial_matrix builders
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @examples
#'
#' # Create the complete naive firm-firm matrix for the companies held by Berkshire Hathaway
#' data('firms_BKB')
#' FF <- FF.naive.both(firms_BKB)
#'
#' @export

FF.naive.both <- function(..., id_as_firm_name = NULL,
                          Matrix = NULL, self_ties = FALSE,
                          combining = 'sum'){

  get.matrix.adjacency.both(...,  ties = 2, id_as_firm_name = id_as_firm_name,
                            Matrix = Matrix, self_ties = self_ties,
                            combining = combining)
}

#' Create a complete normalised-valued firm-firm (FF) matrix
#'
#' Function to create a normalised-valued firm-firm (FF) matrix based on both common ownership and board interlocks
#'
#' The ties' value will reflect the count of common owners and membership depending on \code{combining}:
#'  -\code{sum}: sum of the shares (normalised on 2);
#'  -\code{mean} or \code{average}: average of the shares (normalised on 1);
#'  -\code{min}: minimum of the shares (normalised on 1);
#'  -\code{max}: maximum of the shares (normalised on 1).
#'
#' @param ... Either multiple objects of class \code{firm} or a list of such objects
#' @param id_as_firm_name Whether to use the ticker as the firm's name. Defaults to \code{TRUE} if all firms' id is neither \code{NULL} nor \code{NA}.
#' @param Matrix Whether to use the \href{https://cran.r-project.org/package=Matrix}{\code{Matrix} package}. Defaults to \code{TRUE} when any matrix in the pipeline contains more than 10,000 cells and the package is installed.
#' @param self_ties Whether to allow self-ties (a 'loop' in graph theory). Defaults to \code{FALSE}.
#' @param combining How to combine the FF matrix for managers and that for owners. Possible values:
#' \itemize{
#'  \item \code{sum};
#'  \item \code{mean} or \code{average};
#'  \item \code{min};
#'  \item \code{max};
#'  }
#'
#' @return A matrix object of class \code{financial_matrix}(possibly using the \href{https://cran.r-project.org/package=Matrix}{\code{Matrix} package})
#'
#' @family Financial_matrix builders
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @examples
#'
#' # Create the complete normalised firm-firm matrix for the companies held by Berkshire Hathaway
#' data('firms_BKB')
#' FF <- FF.norm.both(firms_BKB)
#'
#'
#' @export

FF.norm.both <- function(..., id_as_firm_name = NULL,
                         Matrix = NULL, self_ties = FALSE,
                         combining = 'sum'){

  get.matrix.adjacency.both(...,  ties = 3, id_as_firm_name = id_as_firm_name,
                            Matrix = Matrix, self_ties = self_ties,
                            combining = combining)
}
