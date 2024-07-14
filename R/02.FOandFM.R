#' Function to create a firm-owner (FO) or firm-manager (FM) matrix
#'
#' The possible values of \code{ties} are:
#' \itemize{
#'   \item \code{binary} or \code{1} for binary ties;
#'   \item \code{weighted}, \code{valued}, or \code{2} to weight the tie between company \eqn{i} and the person \eqn{j} as the value of \eqn{j}'s share of \eqn{i}'s capital;
#'   \item \code{share} or \code{3} (the default) to weight the tie between company \eqn{i} and the person \eqn{j} as the share of \eqn{i}'s capital owned by \eqn{j}.
#' }
#'
#' @param ... Either multiple objects of class \code{firm} or a list of such objects
#' @param who Whether to extract the 'managers' or the 'owners' (minimum unambiguous string)
#' @param ties Type of ties to create. Defaults to \code{3}; for other possible values, see details.
#' @param id_as_firm_name Whether to use the ticker as the firm's name. Defaults to \code{TRUE} if all firms' id is neither \code{NULL} nor \code{NA}.
#' @param Matrix Whether to use the \href{https://cran.r-project.org/package=Matrix}{\code{Matrix} package}. Defaults to \code{TRUE} when there are more than 10,000 combinations and the package is installed.
#'
#' @return A matrix object of class \code{financial_matrix} (possibly using the \href{https://cran.r-project.org/package=Matrix}{\code{Matrix} package}) in which:
#' \describe{
#'  \item{the rows}{Represent firms;}
#'  \item{the columns}{Represent managers/owners (physical and legal persons).}
#' }
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @seealso \link{FM} \link{FO.binary} \link{FO.naive} \link{FO.norm}
#'
#' @keywords internal

get.matrix.twomode <- function(..., who, ties = 3,
                               id_as_firm_name = NULL,
                               Matrix = NULL){
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

  cols <- find.people(firms, who = who, sorting = TRUE)

  Matrix <- ifelse(!is.null.na(Matrix),
                   yes = Matrix,
                   no = ifelse(requireNamespace("Matrix", quietly = TRUE)&&
                                 length(cols)*length(firms)>10000,
                               yes = TRUE, no = FALSE))

  who <-
    (match.arg(arg = who, choices = c('managers', 'owners'))=='managers')|>
    ifelse(test =  _, yes = 'management', no = 'ownership')
  rows <- query.firms(firms, which = who)

  if(as.numeric(ties)|> suppressWarnings()|> is.na()){
    ties <- switch(ties,
                   'binary' = 1,
                   'weighted' = 2,
                   'valued' = 2,
                   'share' = 3)
  }

  vals <- if(ties == 1){ # Binary ties
    get.binarary.values(firms, who, cols)
  } else {
    if(who == 'management'){
      warning('Only binary ties are possible in the FM matrix')
      get.binarary.values(firms, who, cols)
    } else {
      shares <- query.firms(firms, which = 'share')

      if(ties == 2){
        y <- shares
      } else {
        cap <- query.firms(firms, which = 'cap')|> unlist()
        y <- lapply(seq_along(cap), function(i)shares[[i]]*cap[i])
      }

      lapply(seq_along(rows), function(r){
        out <- rep(0, length(cols))
        row <- rows[[r]]
        out[match(row, cols)] <- y[[r]]
        out
      })|> unlist()
    }
  }

  f <- ifelse(Matrix, Matrix::Matrix, matrix)

  mat <-
    f(data = vals, nrow = length(firms),
      ncol = length(cols), byrow = FALSE)

  dimnames(mat) <- list(
    # rows
    query.firms(firms, which = id_as_firm_name)|> unlist(),
    # cols
    cols
  )

  mat
}

#' Function to create a (necessarily binary) firm-manager (FM) matrix
#'
#' @param ... Either multiple objects of class \code{firm} or a list of such objects
#' @param id_as_firm_name Whether to use the ticker as the firm's name. Defaults to \code{TRUE} if all firms' id is neither \code{NULL} nor \code{NA}.
#' @param Matrix Whether to use the \href{https://cran.r-project.org/package=Matrix}{\code{Matrix} package}. Defaults to \code{TRUE} when there are more than 10,000 combinations and the package is installed.
#'
#' @return A matrix object of class \code{financial_matrix} (possibly using the \href{https://cran.r-project.org/package=Matrix}{\code{Matrix} package}) in which:
#' \describe{
#'  \item{the rows}{Represent firms;}
#'  \item{the columns}{Represent managers (usually physical persons).}
#' }
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @examples
#'
#' # Create the FM matrix of Berkshire Hathaway's holdings
#' \donttest{
#' data('firms_BKB')
#'   FM <- FM(firms_BKB)
#' }
#'
#' @seealso \link{FO.binary} \link{FO.naive} \link{FO.norm}
#'
#' @export

FM <- function(...,
               id_as_firm_name = NULL,
               Matrix = NULL){

  if(...length()==1){
    firms <- ...elt(1)
    names(firms) <- names(...elt(1))
  } else {
    firms <- listing(...)
    names(firms) <-
      as.character(match.call(expand.dots = TRUE))[-1][1:length(firms)]
  }

  get.matrix.twomode(firms, who = 'man', ties = 1,
                     id_as_firm_name = NULL,
                     Matrix = NULL)
}

#' Function to create a binary firm-owner (FO) matrix
#'
#' @param ... Either multiple objects of class \code{firm} or a list of such objects
#' @param id_as_firm_name Whether to use the ticker as the firm's name. Defaults to \code{TRUE} if all firms' id is neither \code{NULL} nor \code{NA}.
#' @param Matrix Whether to use the \href{https://cran.r-project.org/package=Matrix}{\code{Matrix} package}. Defaults to \code{TRUE} when there are more than 10,000 combinations and the package is installed.
#'
#' @return A matrix object of class \code{financial_matrix} (possibly using the \href{https://cran.r-project.org/package=Matrix}{\code{Matrix} package}) in which:
#' \describe{
#'  \item{the rows}{Represent firms;}
#'  \item{the columns}{Represent owners (physical and legal persons).}
#' }
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @examples
#'
#' # Create the binary FO matrix of Berkshire Hathaway's holdings
#' \donttest{
#' data('firms_BKB')
#' FO <- FO.binary(firms_BKB)
#' }
#'
#' @seealso \link{FM} \link{FO.naive} \link{FO.norm}
#'
#' @export

FO.binary <- function(...,
                      id_as_firm_name = NULL,
                      Matrix = NULL){

  if(...length()==1){
    firms <- ...elt(1)
    names(firms) <- names(...elt(1))
  } else {
    firms <- listing(...)
    names(firms) <-
      as.character(match.call(expand.dots = TRUE))[-1][1:length(firms)]
  }

  get.matrix.twomode(firms, who = 'own', ties = 1,
                     id_as_firm_name = NULL,
                     Matrix = NULL)
}


#' Function to create a naive-valued firm-owner (FO) matrix
#'
#' The values are simply the value of the owner \eqn{j}'s stake in firm \eqn{i}.
#'
#' @param ... Either multiple objects of class \code{firm} or a list of such objects
#' @param id_as_firm_name Whether to use the ticker as the firm's name. Defaults to \code{TRUE} if all firms' id is neither \code{NULL} nor \code{NA}.
#' @param Matrix Whether to use the \href{https://cran.r-project.org/package=Matrix}{\code{Matrix} package}. Defaults to \code{TRUE} when there are more than 10,000 combinations and the package is installed.
#'
#' @return A matrix object of class \code{financial_matrix} (possibly using the \href{https://cran.r-project.org/package=Matrix}{\code{Matrix} package}) in which:
#' \describe{
#'  \item{the rows}{Represent firms;}
#'  \item{the columns}{Represent owners (physical and legal persons).}
#' }
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @examples
#'
#' # Create the naive FO matrix of Berkshire Hathaway's holdings
#' \donttest{
#' data('firms_BKB')
#' FO <- FO.naive(firms_BKB)
#' }
#'
#' @seealso \link{FM} \link{FO.binary} \link{FO.norm}
#'
#' @export

FO.naive <- function(...,
                     id_as_firm_name = NULL,
                     Matrix = NULL){

  if(...length()==1){
    firms <- ...elt(1)
    names(firms) <- names(...elt(1))
  } else {
    firms <- listing(...)
    names(firms) <-
      as.character(match.call(expand.dots = TRUE))[-1][1:length(firms)]
  }

  get.matrix.twomode(firms, who = 'own', ties = 2,
                     id_as_firm_name = NULL,
                     Matrix = NULL)
}

#' Function to create a naive-valued firm-owner (FO) matrix
#'
#' The values represent the share of firm \eqn{i}'s capital owned by \eqn{j}.
#'
#' @param ... Either multiple objects of class \code{firm} or a list of such objects
#' @param id_as_firm_name Whether to use the ticker as the firm's name. Defaults to \code{TRUE} if all firms' id is neither \code{NULL} nor \code{NA}.
#' @param Matrix Whether to use the \href{https://cran.r-project.org/package=Matrix}{\code{Matrix} package}. Defaults to \code{TRUE} when there are more than 10,000 combinations and the package is installed.
#'
#' @return A matrix object of class \code{financial_matrix} (possibly using the \href{https://cran.r-project.org/package=Matrix}{\code{Matrix} package}) in which:
#' \describe{
#'  \item{the rows}{Represent firms;}
#'  \item{the columns}{Represent owners (physical and legal persons).}
#' }
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @examples
#'
#' # Create the normalised FO matrix of Berkshire Hathaway's holdings
#' \donttest{
#' data('firms_BKB')
#' FO <- FO.norm(firms_BKB)
#' }
#'
#' @seealso \link{FM} \link{FO.binary} \link{FO.naive}
#'
#' @export

FO.norm <- function(...,
                    id_as_firm_name = NULL,
                    Matrix = NULL){

  if(...length()==1){
    firms <- ...elt(1)
    names(firms) <- names(...elt(1))
  } else {
    firms <- listing(...)
    names(firms) <-
      as.character(match.call(expand.dots = TRUE))[-1][1:length(firms)]
  }

  get.matrix.twomode(firms, who = 'own', ties = 3,
                     id_as_firm_name = NULL,
                     Matrix = NULL)
}
