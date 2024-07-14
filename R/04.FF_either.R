#' Create any firm-firm (FF) matrix for common ownership or board interlocks
#'
#' Function to create a firm-firm (FF) matrix based on ownership or board interlocks:
#'
#' The possible values of \code{ties} and their effect relative to the value of \code{who} are:
#'
#' \code{binary} or \code{0} for binary ties. Namely:
#'  \itemize{
#'   \item for \code{owners}: \eqn{1} if there is at least one common owner, \eqn{0} otherwise;
#'   \item for \code{managers}: \eqn{1} if there is at least one common manager, \eqn{0} otherwise.
#'  }
#'
#'  \code{naive}, or \code{1} for 'naively' valued ties. Namely:
#'  \itemize{
#'    \item  for \code{owners}, the number of common owners;
#'    \item  for \code{managers}, the number of common managers.
#'   }
#'
#' \code{share} or \code{2} (the default) for 'normalised' tie values. Namely:
#' \itemize{
#'  \item for \code{owners}, the share of \eqn{i}'s owners who are also amongst \eqn{j}'s owners;
#'  \item for \code{managers}, the share of \eqn{i}'s managers who are also amongst \eqn{j}'s managers.
#' }
#'
#'
#' @param ... Either multiple objects of class \code{firm} or a list of such objects
#' @param who Whether to extract the 'managers' or the 'owners' (minimum unambiguous string)
#' @param ties Type of ties to create. Defaults to \code{2}; for other possible values, see details.
#' @param id_as_firm_name Whether to use the ticker as the firm's name. Defaults to \code{TRUE} if all firms' id is neither \code{NULL} nor \code{NA}.
#' @param Matrix Whether to use the \href{https://cran.r-project.org/package=Matrix}{\code{Matrix} package}. Defaults to \code{TRUE} when any matrix in the pipeline contains more than 10,000 cells and the package is installed.
#' @param self_ties Whether to allow self-ties (a 'loop' in graph theory). Defaults to \code{FALSE}.
#'
#' @return A matrix object of class \code{financial_matrix}(possibly using the \href{https://cran.r-project.org/package=Matrix}{\code{Matrix} package})
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @family Internal matrix (de)constructrs
#'
#' @seealso \link{FF}
#'
#' @keywords internal

get.matrix.adjacency <- function(..., who = c('managers', 'owners'),
                                 ties = 3, id_as_firm_name = NULL,
                                 Matrix = NULL, self_ties = FALSE){
  if(...length()==1){
    firms <- ...elt(1)
    names(firms) <- names(...elt(1))
  } else {
    firms <- listing(...)
    names(firms) <-
      as.character(match.call(expand.dots = TRUE))[-1][1:length(firms)]
  }

  id_as_firm_name <-
    ifelse(!is.null.na(id_as_firm_name),
           id_as_firm_name,
           ifelse(
             test = any(query.firms(firms, which = 'id')|>unlist()|> is.na()),
             yes = FALSE, no = TRUE
           ))


  id_as_firm_name <- ifelse(id_as_firm_name, 'id', 'names')

  who <-
    (match.arg(arg = who, choices = c('managers', 'owners'))=='managers')|>
    ifelse(test =  _, yes = 'management', no = 'ownership')
  rows <- query.firms(firms, which = who)
  cols <- find.people(firms, who = substr(who, 1, 3), sorting = TRUE)


  Matrix <- ifelse(!is.null.na(Matrix),
                   yes = Matrix,
                   no = ifelse(requireNamespace("Matrix", quietly = TRUE)&&
                                 length(cols)*length(firms)>10000,
                               yes = TRUE, no = FALSE))

  if(Matrix)requireNamespace('Matrix', quietly = TRUE)

  if(as.numeric(ties)|> suppressWarnings()|> is.na()){
    ties <- switch(ties,
                   'binary' = 1,
                   'weighted' = 2,
                   'valued' = 2,
                   'share' = 3)
  }

  if(ties == 1){
    mat <- get.matrix.twomode(firms, who = substr(who, 1, 3), ties = ties,
                              id_as_firm_name = (id_as_firm_name=='id'),
                              Matrix = Matrix)|> suppressWarnings()
    mat <- if(Matrix){
      Matrix::tcrossprod(mat, mat)
    } else {
      mat%*%t(mat)
    }

  } else {
    f <- ifelse(Matrix, Matrix::Matrix, matrix)

    mat <- lapply(seq_along(rows), function(r1){
      lapply(seq_along(rows), function(r2){
        if(r1==r2) return(as.numeric(self_ties))
        common <- match(rows[[r1]], rows[[r2]])
        common <- is.neither.null.nor.na(common)|> sum()
        if(ties == 2){
          ifelse(is.null.na(common), 0, common)
        } else {
          if(length(rows[[r1]])==0) return(0)
          common/length(rows[[r1]])
        }

      })
    })|> unlist()|>
      f(data = _, nrow = length(firms),
        ncol = length(firms), byrow = FALSE)
  }

  if(!self_ties){
    diag(mat) <- 0
  }

  colnames <- if(is.null.na(names(firms))){
    query.firms(firms, which = id_as_firm_name)|> unlist()
  } else {
    names(firms)
  }

  dimnames(mat) <- list(colnames, colnames)


  methods::new('financial_matrix',
               M = mat,
               relation = match.arg(substr(who, 1, 3), c('management',
                                                         'ownership')),
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

#' Create a binary firm-firm (FF) matrix for common ownership
#'
#' Function to create a binary firm-firm (FF) matrix based on common ownership
#'
#'
#' @param ... Either multiple objects of class \code{firm} or a list of such objects
#' @param id_as_firm_name Whether to use the ticker as the firm's name. Defaults to \code{TRUE} if all firms' id is neither \code{NULL} nor \code{NA}.
#' @param Matrix Whether to use the \href{https://cran.r-project.org/package=Matrix}{\code{Matrix} package}. Defaults to \code{TRUE} when any matrix in the pipeline contains more than 10,000 cells and the package is installed.
#' @param self_ties Whether to allow self-ties (a 'loop' in graph theory). Defaults to \code{FALSE}.
#'
#' @return A matrix object of class \code{financial_matrix}(possibly using the \href{https://cran.r-project.org/package=Matrix}{\code{Matrix} package})
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @examples
#'
#' # Create the binary FF matrix of Berkshire Hathaway's holdings by common ownership
#' data('firms_BKB')
#' FF <- FF.binary.ownership(firms_BKB)
#'
#'
#' @family Financial_matrix builders
#'
#' @export

FF.binary.ownership <- function(..., id_as_firm_name = NULL,
                                Matrix = NULL, self_ties = FALSE){

  if(...length()==1){
    firms <- ...elt(1)
    names(firms) <- names(...elt(1))
  } else {
    firms <- listing(...)
    names(firms) <-
      as.character(match.call(expand.dots = TRUE))[-1][1:length(firms)]
  }

  get.matrix.adjacency(firms, who ='own',
                       ties = 1, id_as_firm_name = id_as_firm_name,
                       Matrix = Matrix, self_ties = self_ties)
}

#' Create a binary firm-firm (FF) matrix for board interlocks
#'
#' Function to create a binary firm-firm (FF) matrix based on board interlocks
#'
#'
#' @param ... Either multiple objects of class \code{firm} or a list of such objects
#' @param id_as_firm_name Whether to use the ticker as the firm's name. Defaults to \code{TRUE} if all firms' id is neither \code{NULL} nor \code{NA}.
#' @param Matrix Whether to use the \href{https://cran.r-project.org/package=Matrix}{\code{Matrix} package}. Defaults to \code{TRUE} when any matrix in the pipeline contains more than 10,000 cells and the package is installed.
#' @param self_ties Whether to allow self-ties (a 'loop' in graph theory). Defaults to \code{FALSE}.
#'
#' @return A matrix object of class \code{financial_matrix}(possibly using the \href{https://cran.r-project.org/package=Matrix}{\code{Matrix} package})
#'
#' @family Financial_matrix builders
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @examples
#'
#' # Create the binary FF matrix of Berkshire Hathaway's holdings by boards interlock
#' data('firms_BKB')
#' FF <- FF.binary.management(firms_BKB)
#'
#'
#' @export
FF.binary.management <- function(..., id_as_firm_name = NULL,
                                 Matrix = NULL, self_ties = FALSE){

  if(...length()==1){
    firms <- ...elt(1)
    names(firms) <- names(...elt(1))
  } else {
    firms <- listing(...)
    names(firms) <-
      as.character(match.call(expand.dots = TRUE))[-1][1:length(firms)]
  }

  get.matrix.adjacency(firms, who ='man',
                       ties = 1, id_as_firm_name = id_as_firm_name,
                       Matrix = Matrix, self_ties = self_ties)
}

#' Create a naive-valued firm-firm (FF) matrix for common ownership
#'
#' Function to create a naive-valued firm-firm (FF) matrix based on common ownership
#'
#' Naive-valued means simply counting the number of common owners
#'
#' @param ... Either multiple objects of class \code{firm} or a list of such objects
#' @param id_as_firm_name Whether to use the ticker as the firm's name. Defaults to \code{TRUE} if all firms' id is neither \code{NULL} nor \code{NA}.
#' @param Matrix Whether to use the \href{https://cran.r-project.org/package=Matrix}{\code{Matrix} package}. Defaults to \code{TRUE} when any matrix in the pipeline contains more than 10,000 cells and the package is installed.
#' @param self_ties Whether to allow self-ties (a 'loop' in graph theory). Defaults to \code{FALSE}.
#'
#' @return A matrix object of class \code{financial_matrix}(possibly using the \href{https://cran.r-project.org/package=Matrix}{\code{Matrix} package})
#'
#' @family Financial_matrix builders
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @examples
#'
#' # Create the naive FF matrix of Berkshire Hathaway's holdings by common ownership
#' data('firms_BKB')
#' FF <- FF.naive.ownership(firms_BKB)
#'
#' @export

FF.naive.ownership <- function(..., id_as_firm_name = NULL,
                               Matrix = NULL, self_ties = FALSE){

  if(...length()==1){
    firms <- ...elt(1)
    names(firms) <- names(...elt(1))
  } else {
    firms <- listing(...)
    names(firms) <-
      as.character(match.call(expand.dots = TRUE))[-1][1:length(firms)]
  }

  get.matrix.adjacency(firms, who ='own',
                       ties = 2, id_as_firm_name = id_as_firm_name,
                       Matrix = Matrix, self_ties = self_ties)
}

#' Create a naive-valued firm-firm (FF) matrix for boards interlocks
#'
#' Function to create a naive-valued firm-firm (FF) matrix based on boards interlocks
#'
#' Naive-valued means simply counting the number of common managers.
#'
#' @param ... Either multiple objects of class \code{firm} or a list of such objects
#' @param id_as_firm_name Whether to use the ticker as the firm's name. Defaults to \code{TRUE} if all firms' id is neither \code{NULL} nor \code{NA}.
#' @param Matrix Whether to use the \href{https://cran.r-project.org/package=Matrix}{\code{Matrix} package}. Defaults to \code{TRUE} when any matrix in the pipeline contains more than 10,000 cells and the package is installed.
#' @param self_ties Whether to allow self-ties (a 'loop' in graph theory). Defaults to \code{FALSE}.
#'
#' @return A matrix object of class \code{financial_matrix}(possibly using the \href{https://cran.r-project.org/package=Matrix}{\code{Matrix} package})
#'
#' @family Financial_matrix builders
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @examples
#'
#' # Create the naive FF matrix of Berkshire Hathaway's holdings by boards interlocks
#' data('firms_BKB')
#' FF <- FF.naive.management(firms_BKB)
#'
#' @export

FF.naive.management <- function(..., id_as_firm_name = NULL,
                                Matrix = NULL, self_ties = FALSE){

  if(...length()==1){
    firms <- ...elt(1)
    names(firms) <- names(...elt(1))
  } else {
    firms <- listing(...)
    names(firms) <-
      as.character(match.call(expand.dots = TRUE))[-1][1:length(firms)]
  }

  get.matrix.adjacency(firms, who ='man',
                       ties = 2, id_as_firm_name = id_as_firm_name,
                       Matrix = Matrix, self_ties = self_ties)
}

#' Create a normalised-valued firm-firm (FF) matrix for common ownership
#'
#' Function to create a normalised-valued firm-firm (FF) matrix based on common ownership
#'
#' Normalised-valued means that weights represent the share of common managers.
#'
#' @param ... Either multiple objects of class \code{firm} or a list of such objects
#' @param id_as_firm_name Whether to use the ticker as the firm's name. Defaults to \code{TRUE} if all firms' id is neither \code{NULL} nor \code{NA}.
#' @param Matrix Whether to use the \href{https://cran.r-project.org/package=Matrix}{\code{Matrix} package}. Defaults to \code{TRUE} when any matrix in the pipeline contains more than 10,000 cells and the package is installed.
#' @param self_ties Whether to allow self-ties (a 'loop' in graph theory). Defaults to \code{FALSE}.
#'
#' @return A matrix object of class \code{financial_matrix}(possibly using the \href{https://cran.r-project.org/package=Matrix}{\code{Matrix} package})
#'
#' @family Financial_matrix builders
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @examples
#'
#' # Create the normalised FF matrix of Berkshire Hathaway's holdings by common ownership
#' data('firms_BKB')
#' FF <- FF.norm.ownership(firms_BKB)
#'
#' @export

FF.norm.ownership <- function(..., id_as_firm_name = NULL,
                              Matrix = NULL, self_ties = FALSE){

  if(...length()==1){
    firms <- ...elt(1)
    names(firms) <- names(...elt(1))
  } else {
    firms <- listing(...)
    names(firms) <-
      as.character(match.call(expand.dots = TRUE))[-1][1:length(firms)]
  }

  get.matrix.adjacency(firms, who ='own',
                       ties = 3, id_as_firm_name = id_as_firm_name,
                       Matrix = Matrix, self_ties = self_ties)
}

#' Create a normalised-valued firm-firm (FF) matrix for boards interlocks
#'
#' Function to create a normalised-valued firm-firm (FF) matrix based on boards interlocks
#'
#' Normalised-valued means that weights represent the share of common managers.
#'
#' @param ... Either multiple objects of class \code{firm} or a list of such objects
#' @param id_as_firm_name Whether to use the ticker as the firm's name. Defaults to \code{TRUE} if all firms' id is neither \code{NULL} nor \code{NA}.
#' @param Matrix Whether to use the \href{https://cran.r-project.org/package=Matrix}{\code{Matrix} package}. Defaults to \code{TRUE} when any matrix in the pipeline contains more than 10,000 cells and the package is installed.
#' @param self_ties Whether to allow self-ties (a 'loop' in graph theory). Defaults to \code{FALSE}.
#'
#' @return A matrix object of class \code{financial_matrix}(possibly using the \href{https://cran.r-project.org/package=Matrix}{\code{Matrix} package})
#'
#' @family Financial_matrix builders
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @examples
#'
#' # Create the normalised FF matrix of Berkshire Hathaway's holdings by boards interlocks
#' data('firms_BKB')
#' FF <- FF.norm.management(firms_BKB)
#'
#' @export

FF.norm.management <- function(..., id_as_firm_name = NULL,
                               Matrix = NULL, self_ties = FALSE){

  if(...length()==1){
    firms <- ...elt(1)
    names(firms) <- names(...elt(1))
  } else {
    firms <- listing(...)
    names(firms) <-
      as.character(match.call(expand.dots = TRUE))[-1][1:length(firms)]
  }

  get.matrix.adjacency(firms, who ='man',
                       ties = 3, id_as_firm_name = id_as_firm_name,
                       Matrix = Matrix, self_ties = self_ties)
}

#' Create any firm-firm (FF) matrix
#'
#' General function to create a firm-firm (FF) matrix
#'
#' See more specific functions for a detailed overview:
#'
#'  for board interlocks (\code{who == 'management'}):
#'  \itemize{
#'   \item \code{\link{FF.binary.management}}, if \code{ties = 'binary'};
#'   \item \code{\link{FF.binary.management}}, if \code{ties = 'naive'};
#'   \item \code{\link{FF.norm.management}}, if \code{ties = 'share'}.
#'  }
#'
#'  for co-ownership (\code{who == 'ownership'}):
#'  \itemize{
#'   \item \code{\link{FF.binary.ownership}}, if \code{ties = 'binary'};
#'   \item \code{\link{FF.naive.ownership}}, if \code{ties = 'naive'};
#'   \item \code{\link{FF.norm.ownership}}, if \code{ties = 'share'}.
#'  }
#'
#' for both co-ownership and board interlocks (\code{who == 'both'}):
#' \itemize{
#'   \item \code{\link{FF.binary.both}}, if \code{ties = 'binary'};
#'   \item \code{\link{FF.naive.both}}, if \code{ties = 'naive'};
#'   \item \code{\link{FF.norm.both}}, if \code{ties = 'share'}.
#' }
#'
#' @param ... Either multiple objects of class \code{firm} or a list of such objects
#' @param who Whether to take into account: (\code{ownership}) co-ownership ; (\code{management}) board interlocks, or \code{both} (recognises minimum unambiguous strings).
#' @param ties Type of ties to create. Possible values: \code{binary}; \code{naive}; \code{share} (see Details).
#' @param id_as_firm_name Whether to use the ticker as the firm's name. Defaults to \code{TRUE} if all firms' id is neither \code{NULL} nor \code{NA}.
#' @param Matrix Whether to use the \href{https://cran.r-project.org/package=Matrix}{\code{Matrix} package}. Defaults to \code{TRUE} when any matrix in the pipeline contains more than 10,000 cells and the package is installed.
#' @param self_ties Whether to allow self-ties (a 'loop' in graph theory). Defaults to \code{FALSE}.
#'
#' @return A matrix object of class \code{financial_matrix}(possibly using the \href{https://cran.r-project.org/package=Matrix}{\code{Matrix} package})
#'
#' @family Financial_matrix builders
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @examples
#'
#' # Create the normalised FF matrix of Berkshire Hathaway's holdings by boards interlocks
#' data('firms_BKB')
#' FF <- FF(firms_BKB, who = 'man', ties = 'share')
#'
#' @export

FF <- function(..., who, ties, id_as_firm_name = NULL,
               Matrix = NULL, self_ties = FALSE){

  if(...length()==1){
    firms <- ...elt(1)
    names(firms) <- names(...elt(1))
  } else {
    firms <- listing(...)
    names(firms) <-
      as.character(match.call(expand.dots = TRUE))[-1][1:length(firms)]
  }

  who <- match.arg(who, c('management', 'ownership', 'both'))

  ties <- c('binary', 'naive', 'norm')[
    match.arg(ties, c('binary', 'naive', 'share'))|>
      match(c('binary', 'naive', 'share'))
  ]

  f <- paste('FF', ties, who, sep = '.')|> parse(text = _)|> eval()

  f(firms, id_as_firm_name = id_as_firm_name,
    Matrix = Matrix, self_ties = self_ties)
}
