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
#' @inherit show,network_financial-method return
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
#' @inherit show,network_financial-method return
#'
#' @keywords internal

setMethod(f = 'show', signature = 'network_financial',
          definition = function(object){
            show_network_financial(object)
          })

#' Extending \code{newtwork} functions to \code{newtwork_financial} objects
#'
#' Implementing most basic iterators from the package \code{newtwork} for objects of class \code{newtwork_financial}
#'
#' @description
#' The following functions are implemented:
#' \itemize{
#'  \item \code{edgecount} to count the number of eges (\code{network::network.edgecount});
#'  \item \code{vertex.names} to retrieve the vertices' names (\code{network::network.vertex.names});
#'  \item \code{network.size} to count the edges (\code{network::network::network.size});
#'  \item \code{plot_network} to plot networks (\code{network::plot.network}))
#' }
#'
#' @param x The \code{newtwork_financial} object
#' @param ... Other parameters passed to the corresponding \code{newtwork} functions (see Details).
#'
#' @returns The same result for both \code{newtwork} and \code{newtwork_financial} objects
#' \itemize{
#'  \item \code{edgecount}: Number of edges, numeric scalar
#'  \item \code{vertex.names}: Names/Labels of the vertices, character vector
#'  \item \code{network.size}: Number of vertices, numeric scalar
#'  \item \code{plot_network}: Returns a two-column matrix containing the vertex positions as \code{(x,y)} coordinates, invisibly. Called to print the graph to any \code{R} device.)
#' }
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @name network_methods

#' @rdname fun-network_financial
#' @export
edgecount <- function(x, ...)UseMethod('edgecount')

#' @rdname fun-network_financial
#' @export
network.size <- function(x, ...)UseMethod('network.size')

#' @rdname fun-network_financial
#' @export
vertex.names <- function(x, ...)UseMethod('vertex.names')

#' @rdname fun-network_financial
#' @export
plot_network <- function(x, ...)UseMethod('plot_network')


#' Operators for \code{network_financial} objects
#'
#' Methods to extend operators from the package \code{network} to \code{network_financial} objects
#'
#' @param x The \code{network_financial} object
#' @param ... Other parameters passed to the corresponding method and/or \code{network} functions (see Details).
#'
#' @returns The same result for both \code{network} and \code{network_financial} objects
#' \itemize{
#'  \item \code{network.edgecount}: Number of edges, numeric scalar
#'  \item \code{network.vertex.names}: Names/Labels of the vertices, character vector
#'  \item \code{network::network.size}: Number of vertices, numeric scalar
#' }
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @name network_operators
#'
#' @aliases edgecount,network_financial-method edgecount,network-method vertex.names,network-method vertex.names,network_financial-method network.size,network-method network.size,network_financial-method


#' @rdname network_operators
#' @exportMethod edgecount
setMethod(f = 'edgecount', signature = 'network_financial',
          definition = function(x, ...){
            if(requireNamespace('network', quietly = TRUE)|> isFALSE()){
              stop('`network` is not installed!')
            }

            network::network.edgecount(x@data, ...)
          })

#' @rdname network_operators
#' @exportMethod edgecount
setMethod(f = 'edgecount', signature = 'network',
          definition = function(x, ...){
            if(requireNamespace('network', quietly = TRUE)|> isFALSE()){
              stop('`network` is not installed!')
            }

            network::network.edgecount(x, ...)
          })

#' @export
#' @rdname network_operators
setMethod(f = 'vertex.names', signature = 'network_financial',
          definition = function(x, ...){
            if(requireNamespace('network', quietly = TRUE)|> isFALSE()){
              stop('`network` is not installed!')
            }

            network::network.vertex.names(x@data, ...)
          })

#' @export
#' @rdname network_operators
setMethod(f = 'vertex.names', signature = 'network',
          definition = function(x, ...){
            if(requireNamespace('network', quietly = TRUE)|> isFALSE()){
              stop('`network` is not installed!')
            }

            network::network.vertex.names(x, ...)
          })

#' @export
#' @rdname network_operators
setMethod(f = 'network.size', signature = 'network_financial',
          definition = function(x, ...){
            if(requireNamespace('network', quietly = TRUE)|> isFALSE()){
              stop('`network` is not installed!')
            }

            network::network.size(x@data, ...)
          })

#' @export
#' @rdname network_operators
setMethod(f = 'network.size', signature = 'network',
          definition = function(x, ...){
            if(requireNamespace('network', quietly = TRUE)|> isFALSE()){
              stop('`network` is not installed!')
            }

            network::network.size(x, ...)
          })

#' network plotting for \code{igraph_financial} objects
#'
#' Methods to extend \code{network}'s plotting functions to \code{network_financial} objects
#'
#' @param x The \code{network_financial} object
#' @param ... Other parameters passed to the corresponding method and/or \code{network} functions (see Details).
#'
#' @returns For both \code{igraph} and \code{igraph_financial} objects, returns NULL invisibly. It is called to print the graph to any R device. (see method and \href{https://rdrr.io/cran/igraph/man/plot.igraph.html}{igraph::plot.igraph})
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @name plot_network-methods
#'
#' @aliases plot_network,network_financial-method plot_network,network-method

#' @export
#' @rdname plot_network-methods
setMethod(f = 'plot_network', signature = 'network_financial',
          definition = function(x, ...){
            if(requireNamespace('network', quietly = TRUE)|> isFALSE()){
              stop('`network` is not installed!')
            }

            network::plot.network(x@data, ...)
          })

#' @export
#' @rdname plot_network-methods
setMethod(f = 'plot_network', signature = 'network',
          definition = function(x, ...){
            if(requireNamespace('network', quietly = TRUE)|> isFALSE()){
              stop('`network` is not installed!')
            }

            network::plot.network(x, ...)
          })
