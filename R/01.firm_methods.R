#' Print information on a class \code{firm} object
#'
#' Print method for the S4 class representing a firm (legal person)
#'
#' @param x The \code{firm} object to show
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @returns No return value, called to print to the console \emph{detail} information about the \code{firm} object including:
#' \itemize{
#'   \item in the first paragraph:
#'   \itemize{
#'    \item legal form (if any),
#'    \item revenues (if known),
#'    \item capitalisation (if known).
#'   }
#'   \item in the second paragraph, the names of the board members/managers;
#'   \item in the third paragraph, a data frame with two columns:
#'   \itemize{
#'    \item First, the names of the owners
#'    \item The, their respective share of the firm's capital (normalised to 1)
#'   }
#'
#' }
#'
#' @export

setMethod(f = 'print', signature = 'firm',
          definition = function(x){
            currency = ifelse(is.null.na(x@currency)||length(x@currency)==0,
                              yes = '', no = x@currency)

            cat(sep = '', 'A ', x@legal_form, ' with total revenue totalling ',
                currency, format(x@revenues, scientific = FALSE, big.mark = ','),
                ' and a capitalisation of ',
                currency, format(x@capitalisation, scientific = FALSE, big.mark = ','),'.\n')

            cat(rep(' ', getOption('width')/5*1), '>',
                rep('-', getOption('width')/5*3-2), '<',
                rep(' ', getOption('width')/5*1), '\n\n', sep = '')

            plural <- ifelse(length(x@management)>1, 's are', ' is')
            cat(sep = '', 'Its manager', plural, ': ',
                ifelse(length(x@management)>1,
                       paste0(x@management, collapse = ', '),
                       x@management), '.\n')

            cat(rep(' ', getOption('width')/5*1), '>',
                rep('-', getOption('width')/5*3-2), '<',
                rep(' ', getOption('width')/5*1), '\n\n', sep = '')

            cat(sep = '', 'Ownership structure')
            out <- data.frame(Owner = x@ownership,
                              Share = x@shares)
            out <- if(requireNamespace("knitr", quietly = TRUE)){
              knitr::kable(out, format = 'pipe')
            } else if(requireNamespace("pander", quietly = TRUE)){
              pander::pander(out)
            } else {
              out
            }

            print(out)
          })

#' Show information on a class \code{firm} object
#'
#' Unexported function behind the \code{show} methods for objects of class \code{firm}
#'
#' @param object The object of class \code{firm} to be shown
#'
#' @returns No return value, called to print to the console information about the \code{firm} object including:
#' \itemize{
#'   \item legal form (if any),
#'   \item revenues (if known),
#'   \item capitalisation (if known),
#'   \item number of members of the board/managers; and
#'   \item number of owners/shareholders.
#' }
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @keywords internal

show_firm <- function(object){
  currency = ifelse(is.null.na(object@currency)||length(object@currency)==0,
                    '', object@currency)
  legal_form <- ifelse(is.null.na(object@legal_form, negating = TRUE),
                       paste('A ', object@legal_form, ' with t'), 'T')
  revenues <- ifelse(is.null.na(object@legal_form, negating = TRUE),
                     paste('totalling', currency,
                           format(object@revenues, scientific = FALSE,
                                  big.mark = ',')),
                     'unkown')

  capitalisation <- ifelse(is.null.na(object@legal_form, negating = TRUE),
                           paste('valued at', currency,
                                 format(object@capitalisation,
                                        scientific = FALSE,
                                        big.mark = ',')),
                           'unkown')

  cat(sep = '', legal_form, 'otal revenues ', revenues,
      ' and capitalisation ', capitalisation,'.\n\n')


  cat(sep = '', 'Manager', ifelse(length(object@management)>1, 's',''),
      ': ', length(object@management))
  cat('\n')
  cat(sep = '', 'Owner',ifelse(length(object@ownership)>1, 's',''),
      ': ', length(object@ownership))

}

#' Show information on a \code{firm}
#'
#' Show method for the S4 class \code{firm} representing a firm (legal person)
#'
#' @param object The \code{firm} object to show
#'
#' @returns No return value, called to print to the console information about the \code{firm} object including:
#' \itemize{
#'   \item legal form (if any),
#'   \item revenues (if known),
#'   \item capitalisation (if known),
#'   \item number of members of the board/managers; and
#'   \item number of owners/shareholders.
#' }
#'
#' @importFrom methods show
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @keywords internal

setMethod(f = 'show', signature = signature('firm'),
          definition = function(object){
            show_firm(object)
          })
