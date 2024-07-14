#' Print information on a \code{financial_matrix}
#'
#' Un-exported function behind the \code{print} methods for objects of class \code{financial_matrix}
#'
#' @param x The object of class \code{financial_matrix} to be shown
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @keywords internal

print_financial_matrix <- function(x){
  cat(ifelse(x@relation=='management', 'Board interlocks',
             ifelse(x@relation == 'ownership', 'Common ownership',
                    'Common ownership and board interlocks')),
      'between', format(nrow(x@M), big.mark = ','), 'firms.')

  cat('\n\n')

  if(!is.null(x@capitalisation)&&!all(is.na(x@capitalisation))){
    cap <- x@capitalisation[!is.na(x@capitalisation)]
    currency <- x@currency[!is.na(x@capitalisation)]

    if(length(unique(currency))>1){
      cap <- split(cap, currency)
      lapply(seq_along(cap), function(i){
        cat(sep ='', 'Total capitalisation (data denominated in ',
            unique(x@currency)[i],'): ',
            sum(cap[[i]]), '\n')
      })
    } else {
      cat('Total capitalisation:',
          sum(cap, na.rm = TRUE)|> format(big.mark = ','),
          currency[1])
    }
  }


  cat('\n\n')
  if(!is.null(x@revenues)&&!all(is.na(x@revenues))){
    rvns <- x@revenues[!is.na(x@revenues)]
    currency <- x@currency[!is.na(x@revenues)]

    if(length(unique(currency))>1){
      rvns <- split(rvns, currency)
      lapply(seq_along(rvns), function(i){
        cat(sep ='', 'Average revenues (data denominated in ',
            unique(currency)[i],'): ',
            mean(rvns[[i]], na.rm = TRUE),
            '\n')
      })
    } else {
      cat('Average revenues:', mean(rvns)|> format(big.mark = ','), currency[1])
    }
  }
}

#' Print information on a \code{financial_matrix}
#'
#' Print method for the objects of class \code{financial_matrix}
#'
#' @param x The \code{financial_matrix} object to print
#'
#' @returns No return value, called to print to the console information about the \code{financial_matrix} object:
#' \itemize{
#'   \item type of relation detail (common ownership, board interlocks, or both),
#'   \item Total capitalisation of the firms present (if known), broke down by currency (if more than one);
#'   \item Average total revenues (if known), broke down by currency (if more than one)
#'   \item A print-out of the matrix, up to the number of character given by \code{options('max.print')}.
#' }
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @keywords internal

setMethod(f = 'print', signature = 'financial_matrix',
          definition = function(x){
            print_financial_matrix(x)
            cat('\n\n')
            print(x@M)
          })

#' Show information on a \code{financial_matrix}
#'
#' Show method for the objects of class \code{financial_matrix}
#'
#' @param object The \code{financial_matrix} object to show
#'
#' @returns No return value, called to print to the console information about the \code{financial_matrix} object:
#' \itemize{
#'   \item type of relation detail (common ownership, board interlocks, or both),
#'   \item Total capitalisation of the firms present (if known), broke down by currency (if more than one);
#'   \item Average total revenues (if known), broke down by currency (if more than one)
#'   \item A print-out of the matrix, up to six columns by six rows (i.e., 36 cells). An additional row of text is printed out to inform on the truncation of the matrix displayed
#' }
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @keywords internal

setMethod(f = 'show', signature = 'financial_matrix',
          definition = function(object){
            print_financial_matrix(object)
            cat('\n\n')
            i <- ifelse(nrow(object@M)>=6, 6, nrow(object@M))
            print(object@M[1:i, 1:i])
            if(nrow(object@M)>6)cat('* Only the first six firms shown')
          })


#' Coerce a \code{financial_matrix} object into a list of \code{firm} objects
#'
#' Un-exported function behind the \code{as.firm} method for objects of class \code{financial_matrix}
#'
#' @param x The object of class \code{financial_matrix} to be coerced
#' @param ... Optional argument to the method
#'
#' @returns A (usually named) list of \code{firm} objects the length of which equals the number of rows and columns of the provided \code{financial_matrix}
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @keywords internal

as.firm.financial_matrix <- function(x, ...){
  # Extract information from the FF
  a <- lapply(methods::slotNames(x), function(i){
    by_slot <- methods::slot(x, i)

    if(i=='M'){
      by_slot <- rownames(by_slot)
    }

    by_slot
  })

  # Assign names corresponding to the slot of a firm object
  names(a) <- c('name', methods::slotNames(x)[-1])

  # Check whether there is an id
  if(names(rownames(x@M))[1]=='id'){
    a[[2]] <- a$name
    names(a)[2] <- 'id'
  } else {
    a <- a[-2]
  }

  # Turn a list by information type into a list by firm
  a <- list2DF(a)|> t()|> as.data.frame()|> as.list()|>
    lapply(function(x){names(x)<-names(a); x})

  # Create the firms and assign the available information
  a <- lapply(a, function(x){
    y <- methods::new(Class = 'firm')
    for(i in seq_along(x)){
      what <- ifelse(names(x)[i] %in% c('revenues', 'capitalisation'),
                     as.numeric(x[i]), as.character(x[i]))
      methods::slot(y, names(x)[i]) <- what
    }
    y
  })

  names(a) <- if(is.na(query.firm(a[[1]], 'id'))){
    query.firms(a, which = 'id')
  } else {
    query.firms(a, which = 'name')
  }

  a
}

#' Coerce a \code{financial_matrix} object into a list of \code{firm} objects
#'
#' `as.firm` method for an object of class \code{financial_matrix}
#'
#' @param x The \code{financial_matrix} object to coerce
#' @param ... Optional arguments
#'
#' @returns A (usually named) list of \code{firm} objects the length of which equals the number of rows and columns of the provided \code{financial_matrix}
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @export

setMethod(f = 'as.firm', signature = 'financial_matrix',
          definition = function(x, ...){
            as.firm.financial_matrix(FF = x)
          })

#' Subset a \code{financial_matrix} object
#'
#' Un-exported function behind the \code{subset} method for objects of class \code{financial_matrix}
#'
#' @param x The object of class \code{financial_matrix} to be subset
#' @param ... Additional arguments to the \code{matrix} method for \code{subset()} (see under 'See also')
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @keywords internal

subset_financial_matrix <- function(x, ...){
  y <- x # Copy the FF

  y@M <- subset(x@M, ...) # subset the matrix part

  pos <- match(rownames(y@M), rownames(x@M)) # Which firms were kept?

  # Subset the other parts
  for(i in methods::slotNames(x)[-1:-2]){
    methods::slot(y, i) <- methods::slot(y, i)[pos]
  }

  y # return
}

#' Determine if all the non-\code{NA} slots of a \code{financial_matrix} object are of a given length
#'
#' Un-exported function called by the \code{ncol} and \code{nrow} methods for objects of class \code{financial_matrix}
#'
#' @param x The object of class \code{financial_matrix} which to extract unique elements from
#' @param number The number against which to check the length of \code{x}'s non-\code{NA} slots
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @keywords internal

check.against.other.slots <- function(x, number){
  check <- lapply(methods::slotNames(x)[-1:-2], function(i){
    y <- methods::slot(x, i)
    if(is.null.na(y)|> all()){
      number
    } else {
      length(y[!is.na(y)] )
    }
  })|> unlist()|> c(number)|> unique()|> length()

  if(check!=1){
    warning('The FF is malformed')
  }
}

#' Mathematical methods for \code{financial_matrix} objects
#'
#' Mathematical methods for \code{financial_matrix} objects
#'
#' @description
#' \code{isSymmetric} checks only the matrix-like part
#' \code{summary} operates on all numeric attributes and the matrix-like part
#'
#' @param object The \code{financial_matrix} object to operate on
#' @param ... Arguments passed to the relevant \code{matrix} method
#'
#' @returns \itemize{
#'  \item \code{isSymmetric}: a boolean, \code{TRUE} if the matrix is symmetric, \code{FALSE} otherwise;
#'  \item \code{summary}: a list of length equal to the number of numeric attributes possed by the \code{financial_matrix} (maximum three, the matrix itself, revenues, and capitalisation) assumed as measured on the same scale and denominated in the same currency). Each element of the list of class \code{c('summaryDefault', 'table')} which has specialized \link[base]{format} and \link[base]{print} methods
#' }
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @aliases isSymmetric,financial_matrix-method summary,financial_matrix-method
#'
#' @name FF-math-methods

#' @rdname FF-math-methods
#' @export
setMethod(f = 'isSymmetric', signature = 'financial_matrix',
          definition = function(object, ...){
            isSymmetric(object@M)
          })

#' @rdname FF-math-methods
#' @export
setMethod(f = 'summary', signature = 'financial_matrix',
          definition = function(object, ...){
            y <- list()
            j <- 0
            for(i in methods::slotNames(object)){
              x <- methods::slot(object, i)
              x <- x[!is.na(x)]
              if(is.numeric(x)){
                j <- j + 1
                y[[j]] <- summary(x, ...)
                print(y[[j]])
              }
            }
            y
          })

#' Basic methods for objects of class \code{financial_matrix}
#'
#' Basic methods for objects of class \code{financial_matrix}
#'
#' Mind that usually the rows and columns are named after the firm's tickers.
#'
#' @param x The \code{financial_matrix} object to operate on
#' @param do.NULL Whether to use \code{NULL} names. Defaults to \code{FALSE}
#' @param prefix Prefix for created names (if \code{do.NULL} is \code{FALSE} and names are \code{NULL})
#'
#' @returns A character vector of length equal to the number of rows (or columns) in the \code{financial_matrix} corresponding to the names of the rows (or columns)
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @name FF-basic-methods
#'
#' @aliases rownames,financial_matrix-method colnames,financial_matrix-method

#' @rdname FF-basic-methods
#' @exportMethod rownames
setMethod(f = 'rownames', signature = 'financial_matrix',
          definition = function(x, do.NULL = TRUE, prefix = "row"){
            rownames(x@M, do.NULL = do.NULL, prefix = prefix)
          })

#' @rdname FF-basic-methods
#' @exportMethod colnames
setMethod(f = 'colnames', signature = 'financial_matrix',
          definition = function(x, do.NULL = TRUE, prefix = "row"){
            colnames(x@M, do.NULL = do.NULL, prefix = prefix)
          })

#' Number of rows/columns in a \code{financial_matrix} object
#'
#' Unlike most other methods (i.e., \code{duplicated}, \code{isSymmetric}, \code{summary}, \code{rownames}, and \code{colnames}), these methods act on both the matrix-like and the other components of a \code{financial_matrix} object.
#'
#' @details
#' Checks if the length of the names matches that of the other attributes that are not \code{NA} or structurally of unitary length (i.e., the slots \code{M} and \code{relation}).
#'
#' @param x The \code{financial_matrix} object to operate on
#'
#' @returns A single numeric, the number of rows (columns) in the matrix. It also prints a message to the console if any of the object's other attributes (e.g., capitalisation) is not conformed to the matrix's dimensions
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @name FF-nrow-ncol
#'
#' @aliases ncol,financial_matrix-method nrow,financial_matrix-method

#' @rdname FF-nrow-ncol
#' @exportMethod ncol
setMethod(f = 'ncol', signature = 'financial_matrix',
          definition = function(x){
            y <- ncol(x@M)
            check.against.other.slots(x, number = y)
            y
          })

#' @rdname FF-nrow-ncol
#' @exportMethod nrow
setMethod(f = 'nrow', signature = 'financial_matrix',
          definition = function(x){
            y <- nrow(x@M)
            check.against.other.slots(x, number = y)
            y
          })

#' Basic method to check to compare values in a \code{financial_matrix} object
#'
#' @param x The \code{financial_matrix} object to operate on
#' @param incomparables Either:
#' \itemize{
#'  \item a vector of values that cannot be compared
#'  \item or \code{FALSE}, in which case all values can be compared
#' }
#' @param ... Arguments passed to the relevant \code{matrix} method
#'
#' @returns \itemize{
#'  \item \code{duplicated}: A logical array with the same dimensions and \code{dimnames} of the \code{financial_matrix}'s matrix component.
#'  \item \code{unique}: The matrix component is coerced into a vector and then returned, but with only one copy of each duplicated element.
#' }
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @name FF-comparison-methods
#'
#' @aliases duplicated,financial_matrix,logical-method unique,financial_matrix,logical-method

#' @rdname FF-comparison-methods
#' @export
setMethod(f = 'duplicated', signature = c(x = 'financial_matrix',
                                          incomparables = 'logical'),
          definition = function(x, incomparables = FALSE, ...){
            duplicated(x@M, incomparables = incomparables, ...)
          })

#' @rdname FF-comparison-methods
#' @export
setMethod(f = 'unique', signature = c(x = 'financial_matrix',
                                      incomparables = 'logical'),
          definition = function(x, incomparables = FALSE, ...){
            as.vector(x@M)|> unique.matrix()
          })

#' Method to subset a \code{financial_matrix}
#'
#' Subsets all components of a \code{financial_matrix} object
#'
#' @param x The \code{financial_matrix} object to operate on
#' @param ... Arguments passed to the relevant \code{matrix} method
#'
#' @returns A \code{financial_matrix} object, subsetted to the desired firms
#'
#' @aliases subset,financial_matrix-method
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @name FF-subset-method

#' @rdname FF-subset-method
#' @export
setMethod(f = 'subset', signature = 'financial_matrix',
          definition = function(x, ...){
            subset_financial_matrix(x, ...)
          })
