#' Function to extract information from a \code{firm} object (legal person)
#'
#'
#' @param firm Firm which to extract information from
#' @param which Information to extract, minimum unambiguous substring. Possible values (one or more):
#' - \code{name} Name of the firm
#' - \code{id} ID of the firm, usually the ticker (if provided or otherwise known)
#' - \code{legal_form} Legal form of the firm
#' - \code{sector} Sector in which the firm operates
#' - \code{revenues} Yearly revenues
#' - \code{capitalisation} Capitalisation
#' - \code{management} Members of the board
#' - \code{ownership} Owner(s)
#' - \code{shares} Share owned by (each of) the owner(s)
#' - \code{currency} Currency in which revenues and capitalisation are denominated
#' @param naming Whether to name the result after the querie information (defaults to \code{TRUE})
#'
#' @return Depends on the information queried. One (or, if \code{length(which)>=2}, a \code{\link{list}} of two or more) of the following:
#' \item{name}{A string representing the name of the firm}
#' \item{id}{A string representing the ID of the firm (usually its ticker)}
#' \item{legal_form}{A string representing the firm's legal form}
#' \item{sector}{A string indicating the sector in which the firm operates (possibly a NACE rev. 2 code)}
#' \item{revenues}{A numeric (double) quantifying yearly revenues}
#' \item{capitalisation}{A numeric (double) quantifying capitalisation}
#' \item{management}{A vector of strings representing the members of the board}
#' \item{ownership}{A vector of strings representing the owner(s)}
#' \item{shares}{A numeric (double) vector indicating the shares controlled by (each of) the owner(s)}
#' \item{currency}{A string indicating the currency in which revenues and capitalisation are denominated}
#'
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#'
#' @examples
#' \donttest{
#' # Query Apple's capitalisation
#' data('firms_US')
#' list2env(firms_US, parent.frame())
#' query.firm(AAPL, which = 'capitalisation')
#'
#' # Query British-American Tobacco's capitalisation using the common abbreviation 'cap'
#' data('firms_US')
#' list2env(firms_US, parent.frame())
#' query.firm(BTI, 'cap')
#'
#' # Query General Motors's owners and their shares, but return an unnamed \code{\link{list}}
#' data('firms_US')
#' list2env(firms_US, parent.frame())
#' query.firm(GM, c('own', 'sha'), naming = FALSE)
#' }
#'
#' @seealso \link{query.firms} \link{query.firms.dataframe}
#'
#' @export

query.firm <- function(firm, which, naming = TRUE){
  which <- match.arg(which, methods::slotNames(firm), several.ok = TRUE)

  out <- if(length(which) == 1){
    methods::slot(firm, which)
  } else {
    lapply(which, function(this)methods::slot(firm, this))
  }

  if(naming&length(out)>0)names(out) <- which

  out
}

#' Function to extract information from multiple \code{firm} object (legal person)
#'
#' This function can be fed either:
#' - a (possibly named) \code{\link{list}} of objects of class \code{firm} (see examples 1 and 2); or
#' - multiple objects of class \code{firm}(see example 3)
#'
#' @param ... Object/s which to extract information from (see 'Details')
#' @param which Information to extract, minimum unambiguous sub-string. Possible values (one or more):
#' - \code{name} Name of the firm
#' - \code{id} ID of the firm, usually the ticker (if provided or otherwise known)
#' - \code{legal_form} Legal form of the firm
#' - \code{sector} Sector in which the firm operates
#' - \code{revenues} Yearly revenues
#' - \code{capitalisation} Capitalisation
#' - \code{management} Members of the board
#' - \code{ownership} Owner(s)
#' - \code{shares} Share owned by (each of) the owner(s)
#' - \code{currency} Currency in which revenues and capitalisation are denominated
#' @param naming Whether to name the result after the querie information (defaults to \code{TRUE})
#'
#'
#' @return Depends on the information queried. An object of class \code{\link{list}} (that, if \code{length(which)>=2}, contain multiple sub-lists) of the following:
#' \item{name}{A string representing the name of the firm}
#' \item{id}{A string representing the ID of the firm (usually its ticker)}
#' \item{legal_form}{A string representing the firm's legal form}
#' \item{sector}{A string indicating the sector in which the firm operates (possibly a NACE rev. 2 code)}
#' \item{revenues}{A numeric (double) quantifying yearly revenues}
#' \item{capitalisation}{A numeric (double) quantifying capitalisation}
#' \item{management}{A vector of strings representing the members of the board}
#' \item{ownership}{A vector of strings representing the owner(s)}
#' \item{shares}{A numeric (double) vector indicating the shares controlled by (each of) the owner(s)}
#' \item{currency}{A string indicating the currency in which revenues and capitalisation are denominated}
#'
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @examples
#' \donttest{
#' # Query Apple's, GM's, and BTI's market cap and revenues
#' data('firms_US')
#' query.firms(firms_US, which = c('cap', 'rev'))
#'
#' # Query GM's and BTI's management
#' data('firms_US')
#' query.firms(firms_US, which = 'man')
#'
#' # Query Appple's and GM's revenues and currency
#' data('firms_US')
#' list2env(firms_US, envir = parent.frame())
#' query.firms(AAPL, GM, which = c('rev', 'curr'))
#' }
#'
#' @seealso \link{query.firm} \link{query.firms.dataframe}
#'
#' @export

query.firms <- function(..., which, naming = TRUE){
  if(...length()==1){
    firms <- ...elt(1)
    names(firms) <- names(...elt(1))
  } else {
    firms <- list(...)
    names(firms) <-
      as.character(match.call(expand.dots = TRUE))[-1][1:length(firms)]
  }

  which <- match.arg(which, methods::slotNames(firms[[1]]),
                     several.ok = TRUE)

  out <- lapply(firms, function(firm){
    query.firms_switch(firm, which, naming)
  })

  if(length(out)>0&&(naming&&!is.null.na(names(firms)))){
    names(out) <- names(firms)
  }

  out
}

#' Function to extract information from multiple \code{firm} object (legal person) as a data frame
#'
#' This function can be fed either:
#' - a (possibly named) \code{\link{list}} of objects of class \code{firm} (see example 1); or
#'
#' It is not recommended to use this function with \code{management}, \code{ownership}, or \code{shares} unless \code{transposing == FALSE}.
#'
#' @param ... Object/s which to extract information from (see 'Details')
#' @param which Information to extract, minimum unambiguous sub-string. Possible values (one or more):
#' - \code{name} Name of the firm
#' - \code{id} ID of the firm, usually the ticker (if provided or otherwise known)
#' - \code{legal_form} Legal form of the firm
#' - \code{sector} Sector in which the firm operates
#' - \code{revenues} Yearly revenues
#' - \code{capitalisation} Capitalisation
#' - \code{management} Members of the board
#' - \code{ownership} Owner(s)
#' - \code{shares} Share owned by (each of) the owner(s)
#' - \code{currency} Currency in which revenues and capitalisation are denominated
#' @param naming Whether to name the result after the queried information (defaults to \code{TRUE})
#' @param transposing If \code{TRUE} (default) each row will correspond to a firm and each column to a variable.
#'
#'
#' @return A data frame in structured as follows (or vice versa if \code{transposing == TRUE}):
#' \describe{
#'  \item{a row}{for each queried information; and}
#'  \item{a column}{for each number of firm}.
#' }
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @examples
#' \donttest{
#' # Query Apple's, GM's, and BTI's market cap and revenues
#' data('firms_US')
#' query.firms.dataframe(firms_US, which = c('cap', 'rev'))
#'
#' # Query GM's and BTI's market cap and revenues
#' data('firms_US')
#' list2env(firms_US, envir = parent.frame())
#' query.firms.dataframe(GM, BTI, which = c('cap', 'rev'))
#' }
#'
#' @seealso \link{query.firm} \link{query.firms}
#'
#' @export

query.firms.dataframe <- function(..., which, naming = TRUE,
                                  transposing = TRUE){
  if(...length()==1){
    firms <- ...elt(1)
    names(firms) <- names(...elt(1))
  } else {
    firms <- listing(...)
    names(firms) <-
      as.character(match.call(expand.dots = TRUE))[-1][1:length(firms)]
  }

  which <- match.arg(which, methods::slotNames(firms[[1]]),
                     several.ok = TRUE)

  out <- lapply(firms, function(firm){
    query.firms_switch(firm, which, naming, unlisting = TRUE)
  })

  if(length(out)>0&&(naming&&!is.null.na(names(firms)))){
    names(out) <- names(firms)
  } else {
    stop('Not all firms have all attributes!')
  }

  out <- list2DF(out)

  if(transposing)out <- t(out)|> as.data.frame()

  if(naming){
    f1 <- ifelse(transposing, 'col', 'row')

    out <- .set_names(x = out, names = which, where = f1)

    if(!is.null(names(firms))){
      out <- .set_names(x = out, names = names(firms),
                        where =  base::setdiff(c('col', 'row'), f1))
    }
  }

  out
}
