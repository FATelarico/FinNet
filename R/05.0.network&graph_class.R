#' A virtual class for relational data extending the packages \href{https://statnet.org/}{network}and \href{https://igraph.org/}{igraph}
#'
#' This is the un-exported \link[methods:BasicClasses]{virtual} base for the S4 classes \code{igraph_financial} and \code{network_financial} to represent the relations between firms (legal person)
#'
#' @slot relation Relations represented by the matrix
#' @slot vertex.size Attribute used to determine the vertexes' size
#' @slot vertex.color Attribute used to determine the vertexes' colour
#' @slot edge.width Whether the edges' width reflect tie weight
#' @slot edge.greyscale Whether the edges' color reflects tie weight
#'
#' @name financial_net
#' @importFrom methods new
#'
#' @aliases financial_net
#'
#' @rdname financial_net
#'
#' @keywords internal
setClass('financial_net',
         slots = c(relation = 'character',
                   vertex.size = 'character',
                   vertex.colour = 'character',
                   edge.width = 'character',
                   edge.greyscale = 'logical'),
         contains = 'VIRTUAL')


#' An S4 class for relational data extending the package \href{https://statnet.org/}{network}
#'
#' An S4 class for the network objects produced by the \code{\link{FF.net}} and \code{\link{FF.net.custom}} functions to represent the relations between firms (legal person)
#'
#' @slot data The representation of the network as a \href{https://rdrr.io/cran/network/man/network.html}{network} object
#'
#' @name network_financial
#'
#' @importFrom methods new
#'
#' @aliases network_financial

if(requireNamespace('network', quietly = TRUE)){
  setOldClass('network', prototype = network::network.initialize(n=0))
} else {
  setOldClass('network', prototype = '')
}

#' @rdname network_financial
#' @exportClass network_financial
setClass('network_financial',
         slots = c(data = 'network'),
         contains = 'financial_net')

#' An S4 class for relational data extending the package \href{https://igraph.org/}{igraph}
#'
#' An S4 class for the network objects produced by the \code{\link{FF.graph}} and \code{\link{FF.graph.custom}} to represent the relations between firms (legal person)
#'
#' @slot data The representation of the network as a \href{https://rdrr.io/cran/igraph/man/make_graph.html}{igraph} object
#'
#' @name igraph_financial
#' @importFrom methods new
#' @aliases igraph_financial

if(requireNamespace('igraph', quietly = TRUE)){
  setOldClass('igraph', prototype = igraph::make_empty_graph())
} else {
  setOldClass('igraph', prototype = '')
}

#' @rdname igraph_financial
#' @exportClass igraph_financial
setClass('igraph_financial',
         slots = c(data = 'igraph'),
         contains = 'financial_net')
