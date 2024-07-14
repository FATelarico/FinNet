#' Print information on a \code{network_financial} object
#'
#' Method to \code{print} objects of class \code{network_financial}
#'
#' @param x The object of class \code{network_financial} to be shown
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @returns Prints the list of global attributes of the network as if \code{network::print.network()} had been called. See \href{https://rdrr.io/cran/network/man/network.html}{print.network} for more details.
#'
#' @keywords internal

setMethod(f = 'print', signature = 'network_financial',
          definition = function(x){
            print(x@data)
          })

#' Show information on a \code{network_financial}
#'
#' Un-exported function behind the \code{show} method for objects of class \code{network_financial}
#'
#' @param x The object of class \code{network_financial} to be shown
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @inherit show,igraph_financial-method return
#'
#' @keywords internal

show_network_financial <- function(x){
  methods::show(x@data)
  cat('\n\n')
  cat('A network representing',
      ifelse(x@relation == 'management', 'board interlocks',
             ifelse(x@relation == 'ownership', 'common ownership',
                    'common ownership and board interlocks')), '\n')

  if(!is.null.na(x@vertex.size)){
    cat('\n')
    cat('The size of the nodes represents the firms', x@vertex.size)
  }

  if(!is.null.na(x@vertex.colour)){
    cat('\n')
    cat('The colour of the nodes represents the firms', x@vertex.colour)
  }

  if(!is.null.na(x@edge.width)){
    cat('\n')
    cat('The width of the ties represents their strength')
  }

  if(!is.null.na(x@edge.greyscale)){
    cat('\n')
    cat('The colour (greyscale) of the ties represents their strength')
  }
}

#' Show information on a \code{network_financial}
#'
#' Show method for the objects of class \code{network_financial}
#'
#' @param object The \code{network_financial} object to show
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @inherit show,igraph_financial-method return
#'
#' @keywords internal

setMethod(f = 'show', signature = 'network_financial',
          definition = function(object){
            show_network_financial(object)
          })
