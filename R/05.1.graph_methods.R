#' Print information on a \code{igraph_financial} object
#'
#' Method to \code{print} objects of class \code{igraph_financial}
#'
#' For more information see the relevant igraph function: \href{https://rdrr.io/cran/igraph/man/print.igraph.html}{print.igraph}
#'
#' @param x The object of class \code{igraph_financial} to be shown
#'
#' @returns Nothing, called for printing to the console the following information:
#' \itemize{
#'  \item The first line contains the basic properties of the graph;
#'  \item The rest of the of the print-out contains the graph's attributes. After the attribute names, the kind of the attribute – graph (‘g’), vertex (‘v’) or edge (‘e’) – is denoted, and the type of the attribute as well, character (‘c’), numeric (‘n’), logical (‘l’), or other (‘x’).
#' }
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @keywords internal

setMethod(f = 'print', signature = 'igraph_financial',
          definition = function(x){
            print(x@data)
          })

#' Show information on a \code{igraph_financial}
#'
#' Un-exported function behind the \code{show} method for objects of class \code{igraph_financial}
#'
#' @param x The object of class \code{igraph_financial} to be shown
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @keywords internal

show_igraph_financial <- function(x){
  methods::show(x@data)
  cat('\n\n')
  cat('A network representing',
      ifelse(x@relation == 'management', 'board interlocks',
             ifelse(x@relation == 'ownership', 'common ownership',
                    'common ownership and board interlocks')))

  cat('\n\n')
  cat('The size of the vertexes',
      ifelse(is.null.na(x@vertex.size), 'does not represent an attribute',
             paste('represents the firms', x@vertex.size)))

  cat('\n\n')
  cat('The colour of the vertexes',
      ifelse(is.null.na(x@vertex.colour), 'does not represent an attribute',
             paste('represents the firms', x@vertex.colour)))

    cat('\n\n')
    cat('The width of the edges',
        ifelse(is.null.na(x@edge.width), 'does not represent', 'represents'),
        'their strength')

    cat('\n\n')
    cat('The colour (greyscale) of the edges',
        ifelse(is.null.na(x@edge.greyscale),
               'does not represent', 'represents'),
        'their strength')
}

#' Show information on a \code{igraph_financial}
#'
#' Show method for the objects of class \code{igraph_financial}
#'
#' @param object The \code{igraph_financial} object to show
#'
#' @return Nothing, called for printing to the console the following information:
#' \itemize{
#'  \item The first line contains the basic properties of the graph;
#'  \item The rest of the of the first paragrah contains the graph's attributes. After the attribute names, the kind of the attribute – graph (‘g’), vertex (‘v’) or edge (‘e’) – is denoted, and the type of the attribute as well, character (‘c’), numeric (‘n’), logical (‘l’), or other (‘x’);
#'  \item The second paragraph decribes the type of relation represented in the network (common ownership, board interlocks, both);
#'  \item The third paragraph informs on whether the size of the vertexes represents an attribute (and, if yes, which one);
#'  \item The fourth paragraph informs on whether the colour of the vertexes represents an attribute (and, if yes, which one);
#'  \item The fifth paragraph informs on whether the width of the edges represents their strength;
#'  \item The sixth paragraph informs on whether the colour of the edges represents their strength.
#' }
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @keywords internal

setMethod(f = 'show', signature = 'igraph_financial',
          definition = function(object){
            show_igraph_financial(object)
          })


#' Extending \code{igraph} functions to \code{igraph_financial} objects
#'
#' Implementing most basic iterators from the package \code{igraph} for objects of class \code{igraph_financial}
#'
#' @description
#' The following functions are implemented:
#' \itemize{
#'  \item \code{V_fin} to retrieve the vertexes (\code{igraph::V});
#'  \item \code{vcount_fin} to count the vertexes (\code{igraph::vcount});
#'  \item \code{gorder_fin} as an alias to \code{vcount_fin} (\code{igraph::gorder});
#'  \item \code{E_fin} to retrieve the edges (\code{igraph::E});
#'  \item \code{gsize_fin} to count the edges (\code{igraph::gsize});
#'  \item \code{ecount_fin} as an alias to \code{gsize_fin} (\code{igraph::ecount})
#'  \item \code{plot_igraph_fin} to plot graphs (\code{igraph::plot.igraph}))
#' }
#'
#' @param x The \code{igraph_financial} object
#' @param ... Other parameters passed to the corresponding \code{igraph} functions (see Details).
#'
#' @returns The same result for both \code{igraph} and \code{igraph_financial} objects
#' \itemize{
#'  \item \code{V}: A vertex sequence containing all vertices, in the order of their numeric vertex ids.
#'  \item \code{vcount} and \code{gorder}: Number of vertices, numeric scalar.
#'  \item \code{E}: An edge sequence of the graph
#'  \item \code{ecount} and \code{gsize}: Number of edges, numeric scalar.
#'  \item \code{plot_igraph}: Returns NULL, invisibly. Called to print the graph to any R device. (see method and \href{https://rdrr.io/cran/igraph/man/plot.igraph.html}{igraph::plot.igraph})
#' }
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @name graph_methods

#' @rdname fun-igraph_financial
#' @export
V <- function(x)UseMethod('V')

#' @rdname fun-igraph_financial
#' @export
vcount <- function(x)UseMethod('vcount')

#' @rdname fun-igraph_financial
#' @export
gorder <- function(x)UseMethod('gorder')

#' @rdname fun-igraph_financial
#' @export
E <- function(x, ...)UseMethod('E')

#' @rdname fun-igraph_financial
#' @export
ecount <- function(x, ...)UseMethod('ecount')

#' @rdname fun-igraph_financial
#' @export
gsize <- function(x, ...)UseMethod('gsize')

#' @rdname fun-igraph_financial
#' @export
plot_igraph <- function(x, ...)UseMethod('plot_igraph')


#' igraph vertex iterators for \code{igraph_financial} objects
#'
#' Methods to extend igraph vertex iterators and functions to \code{igraph_financial} objects
#'
#' @param x The \code{igraph_financial} object
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @returns The same result for both \code{igraph} and \code{igraph_financial} objects
#' \itemize{
#'  \item \code{V}: A vertex sequence containing all vertices, in the order of their numeric vertex ids
#'  \item \code{vcount} and \code{gorder}: Number of vertices, numeric scalar
#' }
#'
#' @name igraph_v_iterators
#'
#' @aliases V,igraph-method V,igraph_financial-method vcount,igraph-method vcount,igraph_financial-method gorder,igraph-method gorder,igraph_financial-method

#' @rdname igraph_v_iterators
#' @exportMethod V
setMethod(f = 'V', signature = 'igraph_financial',
          definition = function(x){
            if(requireNamespace('igraph', quietly = TRUE)|> isFALSE()){
              stop('`igraph` is not installed!')
            }

            igraph::V(x@data)
          })

#' @exportMethod V
#' @rdname igraph_v_iterators
setMethod(f = 'V', signature = 'igraph',
          definition = function(x){
            if(requireNamespace('igraph', quietly = TRUE)|> isFALSE()){
              stop('`igraph` is not installed!')
            }

            igraph::V(x)
          })

#' @exportMethod vcount
#' @rdname igraph_v_iterators
setMethod(f = 'vcount', signature = 'igraph_financial',
          definition = function(x){
            if(requireNamespace('igraph', quietly = TRUE)|> isFALSE()){
              stop('`igraph` is not installed!')
            }

            igraph::vcount(x@data)
          })

#' @exportMethod vcount
#' @rdname igraph_v_iterators
setMethod(f = 'vcount', signature = 'igraph',
          definition = function(x){
            if(requireNamespace('igraph', quietly = TRUE)|> isFALSE()){
              stop('`igraph` is not installed!')
            }

            igraph::vcount(x)
          })

#' @export
#' @rdname igraph_v_iterators
setMethod(f = 'gorder', signature = 'igraph_financial',
          definition = function(x){
            if(requireNamespace('igraph', quietly = TRUE)|> isFALSE()){
              stop('`igraph` is not installed!')
            }

            igraph::gorder(x@data)
          })

#' @export
#' @rdname igraph_v_iterators
setMethod(f = 'gorder', signature = 'igraph',
          definition = function(x){
            if(requireNamespace('igraph', quietly = TRUE)|> isFALSE()){
              stop('`igraph` is not installed!')
            }

            igraph::gorder(x)
          })

#' igraph edge iterators for \code{igraph_financial} objects
#'
#' Methods to extend igraph edge iterators and functions to \code{igraph_financial} objects
#'
#' @param x The \code{igraph_financial} object
#' @param ... Other parameters passed to the corresponding method and/or \code{igraph} functions (see Details).
#'
#' @returns The same result for both \code{igraph} and \code{igraph_financial} objects
#' \itemize{
#'  \item \code{E}: An edge sequence of the graph
#'  \item \code{ecount} and \code{gsize}: Number of edges, numeric scalar
#' }
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @name igraph_E_iterators
#'
#' @aliases E,igraph-method  E,igraph_financial-method  ecount,igraph_financial-method ecount,igraph-method gsize,igraph_financial-method gsize,igraph-method

#' @rdname igraph_E_iterators
#' @exportMethod E
setMethod(f = 'E', signature = 'igraph_financial',
          definition = function(x, ...){
            if(requireNamespace('igraph', quietly = TRUE)|> isFALSE()){
              stop('`igraph` is not installed!')
            }

            igraph::E(x@data, ...)
          })

#' @export
#' @rdname igraph_E_iterators
setMethod(f = 'E', signature = 'igraph',
          definition = function(x, ...){
            if(requireNamespace('igraph', quietly = TRUE)|> isFALSE()){
              stop('`igraph` is not installed!')
            }

            igraph::E(x, ...)
          })

#' @export
#' @rdname igraph_E_iterators
setMethod(f = 'ecount', signature = 'igraph_financial',
          definition = function(x, ...){
            if(requireNamespace('igraph', quietly = TRUE)|> isFALSE()){
              stop('`igraph` is not installed!')
            }

            igraph::ecount(x@data, ...)
          })

#' @export
#' @rdname igraph_E_iterators
setMethod(f = 'ecount', signature = 'igraph',
          definition = function(x, ...){
            if(requireNamespace('igraph', quietly = TRUE)|> isFALSE()){
              stop('`igraph` is not installed!')
            }

            igraph::ecount(x, ...)
          })

#' @export
#' @rdname igraph_E_iterators
setMethod(f = 'gsize', signature = 'igraph_financial',
          definition = function(x, ...){
            if(requireNamespace('igraph', quietly = TRUE)|> isFALSE()){
              stop('`igraph` is not installed!')
            }

            igraph::gsize(x@data, ...)
          })

#' @export
#' @rdname igraph_E_iterators
setMethod(f = 'gsize', signature = 'igraph',
          definition = function(x, ...){
            if(requireNamespace('igraph', quietly = TRUE)|> isFALSE()){
              stop('`igraph` is not installed!')
            }

            igraph::gsize(x, ...)
          })

#' igraph plotting for \code{igraph_financial} objects
#'
#' Methods to extend \code{igraph}'s plotting functions to \code{igraph_financial} objects
#'
#' @param x The \code{igraph_financial} object
#' @param ... Other parameters passed to the corresponding method and/or \code{igraph} functions (see Details).
#'
#' @returns For both \code{igraph} and \code{igraph_financial} objects, returns NULL invisibly. It is called to print the graph to any R device. (see method and \href{https://rdrr.io/cran/igraph/man/plot.igraph.html}{igraph::plot.igraph})
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @name plot_igraph-methods
#'
#' @aliases plot_igraph,igraph-method  plot_igraph,igraph_financial-method

#' @export
#' @rdname plot_igraph-methods
setMethod(f = 'plot_igraph', signature = 'igraph_financial',
          definition = function(x, ...){
            if(requireNamespace('igraph', quietly = TRUE)|> isFALSE()){
              stop('`igraph` is not installed!')
            }

            igraph::plot.igraph(x@data, ...)
          })

#' @export
#' @rdname plot_igraph-methods
setMethod(f = 'plot_igraph', signature = 'igraph',
          definition = function(x, ...){
            if(requireNamespace('igraph', quietly = TRUE)|> isFALSE()){
              stop('`igraph` is not installed!')
            }

            igraph::plot.igraph(x, ...)
          })
