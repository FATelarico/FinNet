#' Create any firm-firm (FF) network
#'
#' Create a network (either an object of class \code{network} from the package \code{network} or a \code{graph} object from the package \code{igraph}) from a FF, FO, or FM matrix
#'
#' This function allows for a number of additional arguments.
#'
#' @section What can be passed to \code{vertex.colour} and \code{vertex.size}:
#'
#' The pieces of information that is possible to pass to \code{vertex.size} and \code{vertex.colour} are:
#' \itemize{
#'  \item \code{capitalisation}, will be arranged into steps (see \code{capitalisation.bins} below)
#'  \item \code{revenue}, will be arranged into steps (see \code{revenues.bins} below)
#'  \item \code{legal_form}
#'  \item \code{sector}
#'  \item \code{currency}
#' }
#'
#' @section What can be passed to \code{edge.width} and \code{edge.greyscale}:
#'
#' The pieces of information that is possible to pass to \code{edge.width} and \code{edge.greyscale} are:
#' - \code{capitalisation}
#' - \code{revenue}
#'
#' @section Additional parameters related to \code{vertex.size}:
#'
#' The effect of the additional parameters that modify the behaviour of \code{vertex.size} are:
#'
#' \code{vertex.size.max} (defaults to \code{5}) :
#'  \itemize{
#'   \item if \code{vertex.size} or one of its aliases is specified, this is the size of the biggest vertex;
#'   \item if neither \code{vertex.size} nor any of its aliases is given, this is the size of ALL vertices.
#'  }
#'
#' \code{vertex.size.min} (defaults to \code{1}):
#'  \itemize{
#'   \item if \code{vertex.size} or one of its aliases is specified, this is the size of the smallest vertex;
#'   \item if neither \code{vertex.size} nor any of its aliases is given, it is ignored.
#'  }
#'
#' @section Additional parameters related to \code{vertex.colour}:
#'
#' The only additional parameter related to \code{vertex.colour} is \code{vertex.colour.palette}.
#' It supports a vector of RGB or named colours (see \code{\link[grDevices]{colours}} for all named colours in \code{R}).
#' It also accepts complete calls to functions that return a such a vector like \code{RColorBrewer::brewer.pal(n, name)} or \code{viridisLite::viridis(n, option)}.
#' If the palette is too short, it will be extended automatically using \code{\link[grDevices]{colorRampPalette}}.
#' If the palette is not declared, but this arguemnt is \code{TRUE}, it will defaulr to the following vector of colours:
#' \itemize{
#'    \item \code{#00204D}, \href{https://www.color-name.com/hex/00204d}{Oxford Blue}
#'    \item \code{#31446B}, \href{https://www.color-name.com/hex/31446B}{Police Blue}
#'    \item \code{#666970}, \href{https://www.color-name.com/hex/666970}{Dim Gray}
#'    \item \code{#958F78}, \href{https://www.color-name.com/hex/958F78}{Artichoke}
#'    \item \code{#CBBA69}, \href{https://www.color-name.com/hex/CBBA69}{Dark Khaki}
#'    \item \code{#FFEA46}, \href{https://www.color-name.com/hex/FFEA46}{Gargoyle Gas}
#' }
#' If the argument is \code{FALSE}, \code{NULL} or \code{NA}, the vertex will be coloured of \code{#081677} (\href{https://www.color-name.com/gentian-blue.color}{Gentian blue}).
#'
#' @section Additional parameters related to \code{edge.width}:
#'
#' \code{edge.width.max} (defaults to \code{5}) :
#'  \itemize{
#'   \item if \code{edge.width} or one of its aliases is specified, this is the thickness of the thickest edge;
#'   \item if neither \code{edge.width} nor any of its aliases is given, this is the thickness of ALL edges
#'  }
#'
#' \code{edge.width.min} (defaults to \code{1}):
#'  \itemize{
#'   \item if \code{edge.width} or one of its aliases is specified, this is the thickness of the slimmest edge;
#'   \item if neither \code{edge.width} nor any of its aliases is given, it is ignored.
#'  }
#'
#' @section Additional parameters related to \code{edge.greyscale}:
#'
#' \code{edge.greyscale.darkest} (defaults to \code{5}):
#'  \itemize{
#'   \item if \code{edge.greyscale} or one of its aliases is specified, this is the thickness of the thickest edge;
#'   \item if neither \code{edge.greyscale} nor any of its aliases is given, this is the thickness of ALL edges
#'  }
#'
#' \code{edge.greyscale.fairest} (defaults to \code{1}):
#'  \itemize{
#'   \item if \code{edge.greyscale} or one of its aliases is specified, this is the thickness of the slimmest edge;
#'   \item if neither \code{edge.greyscale} nor any of its aliases is given, it is ignored.
#'  }
#'
#' Several aliases are accepted for all arguments, except \code{M}:
#' \itemize{
#'  \item for \code{vertex.size}: \code{node.size}
#'  \item for \code{vertex.colour}: \code{vertex.color}, \code{node.colour}, and \code{node.color};
#'  \item for \code{edge.width}: \code{tie.width}
#'  \item for \code{edge.greyscale}: \code{tie.grayscale}, \code{tie.greyscale}, and \code{edge.grayscale}
#' }
#'
#' @param x A matrix-like object produced by \code{\link{FF}} and related functions
#' @param vertex.size Which piece of information on the firms should be used to represent the nodes' size (see Details).
#' @param vertex.colour Which piece of  information on the firms should be used to represent the nodes' colours (see Details).
#' @param edge.width Whether to use the edges' width to represent tie strength. Defaults to \code{FALSE}.
#' @param edge.greyscale Whether to use the edges' colour to represent tie strength through a grey scale. Defaults to \code{TRUE} if the matrix is valued.
#' @param format Which format to use for the network. Possible values: \code{igraph} or \code{network}
#' @param directed Whether the network should be directed. Defaults to \code{TRUE}
#' @param loops Whether the network should have loops. Defaults to \code{FALSE}
#' @param weighted Whether the ties/edges should be weighted. Defaults to \code{TRUE} if any element of the matrix equals neither 0 nor 1
#' @param ... Aliases to the other parameters and additional settings (see Details).
#'
#' @return A network in the desired format
#'
#' @seealso \link{FF.net} \link{FF.net.custom} \link{FF.graph} \link{FF.graph.custom}
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @importFrom grDevices grey.colors
#' @importFrom methods is
#'
#' @keywords internal

get.net <- function(x, vertex.size = NULL, vertex.colour = NULL,
                    edge.width = NULL, edge.greyscale = NULL,
                    format = c('igraph', 'network'),
                    directed = TRUE, loops = FALSE,
                    weighted = any(!x@M%in%c(0, 1)),
                    ...){

  M <- x@M
  if(is(M, 'Matrix'))requireNamespace('Matrix', quietly = TRUE)

  if(...length()>0){
    nms <- ...names()
    for(i in 1:...length()){
      assign(nms[i], ...elt(i))
    }
  }

  if(is.null(vertex.size)){
    if(exists('node.size')){
      vertex.size <- node.size
    } else {
      vertex.size <- node.size <- NA
    }
  }

  if(is.numeric(vertex.size)){
    warning('vertex.size should indicate the attribute to represent,
              not the size. Correcting...')
    vertex.size.max <- vertex.size
    vertex.size <- NA
  } else if(!exists('vertex.size.max')){
    vertex.size.max <- 5
  }

  if(!exists('vertex.size.min')&&
     !is.null.na(vertex.size))vertex.size.min <- 1

  if(is.null(vertex.colour)){
    if(exists('vertex.color')){
      vertex.colour <- vertex.color
    } else if (exists('node.color')){
      vertex.colour <- node.color
    } else if (exists('node.colour')){
      vertex.colour <- node.colour
    } else {
      vertex.colour <- vertex.color <-
        node.colour <- node.color <- NA
    }

    if(!exists('vertex.colour.palette')){
      vertex.colour.palette <-
        if(is.null.na(vertex.colour)){
          palette()
        } else {
          if(length(methods::slot(x, vertex.color)|>
                    unique()) > length(palette())){
            message('The palette was ramped-up')
          }

          methods::slot(x, vertex.color)|>
            unique()|> length()|> palette()
        }

    }
  }

  if(is.null.na(edge.width)){
    if(exists('tie.width')){
      edge.width <- tie.width
    } else{
      edge.width <- tie.width <- FALSE
    }
  }

  if(is.numeric(edge.width)){
    warning('edge.width should indicate the attribute to represent,
              not the width Correcting...')
    edge.width.max <- edge.width
    edge.width <- NA
  } else if(!exists('edge.width.max')){
    edge.width.max <- 5
  }

  if(!exists('edge.width.min'))edge.width.min <- 1

  if(is.null.na(edge.greyscale)){

    if(exists('tie.grayscale')){
      edge.greyscale <- tie.grayscale
    } else if(exists('tie.greyscale')){
      edge.greyscale <- tie.greyscale
    } else if (exists('edge.grayscale')){
      edge.greyscale <- edge.grayscale
    } else{
      edge.greyscale <- tie.grayscale <-
        tie.greyscale <- edge.grayscale <-
        FALSE
    }
  }

  if(!exists('edge.greyscale.fairest'))edge.greyscale.fairest <- '#bfbfbf'
  if(!exists('edge.greyscale.darkest'))edge.greyscale.darkest <- '#000000'

  if(!all(is(M)%in%c('matrix', 'sparseMatrix'))){
    M <- as.matrix(M)
  }

  g <- switch(format,
              igraph = igraph::graph.adjacency(
                adjmatrix = M, weighted = if(weighted){TRUE}else{NULL}, diag = loops,
                mode = ifelse(directed, 'directed', 'undirected')
              ),
              network = network::network(
                x = M, directed = directed, hyper = FALSE,
                loops = loops, multiple = FALSE, bipartite = FALSE,
                ignore.eval = !weighted, names.eval = if(weighted){'weight'}else{NULL}
              ))|> methods::new(
                Class = paste0(format,'_financial'),
                data = _,
                relation = x@relation,
                vertex.size = ifelse(is.null.na(vertex.size),
                                     yes = as.character(NA),
                                     no = vertex.size),
                vertex.colour = ifelse(is.null.na(vertex.colour),
                                       yes = as.character(NA),
                                       no = vertex.colour),
                edge.width = ifelse(is.null.na(edge.width)|isFALSE(edge.width),
                                    yes = as.character(NA),
                                    no = edge.width),
                edge.greyscale = ifelse(is.null.na(edge.greyscale)|
                                          isFALSE(edge.greyscale),
                                        yes = as.logical(NA),
                                        no = edge.greyscale)
              )

  g@data <- (if(!is.null.na(vertex.size)){
    methods::slot(x, vertex.size)|>
      rescale.numeric(to = c(vertex.size.min,
                             vertex.size.max))
  } else {
    vertex.size.max
  })|>
    add.attribute(g@data, 'vertex', 'size', value = _)

  g@data <- (if(!is.null.na(vertex.colour)){
    vertex.colour.palette[match(
      methods::slot(x, vertex.colour),
      methods::slot(x, vertex.colour)|> unique()|> sort()
    )]
  } else {
    '#00336FFF'
  })|>
    add.attribute(g@data, 'vertex', 'color', value = _)

  g@data <- (if(!is.null.na(edge.greyscale)){

    darkest <- check.correct.grey(edge.greyscale.darkest)

    darkest <- lapply(c(2, 4, 6), function(i){
      substr(darkest, i, i+1)|>
        base::strtoi(base = 16)
    })|> unlist()

    fairest <- check.correct.grey(edge.greyscale.fairest)

    fairest <- lapply(c(2, 4, 6), function(i){
      substr(fairest, i, i+1)|>
        base::strtoi(base = 16)
    })|> unlist()

    M_nonzero <- M[M!=0]

    greyscale <- grey.colors(
      n = unique(M_nonzero)|> length(),
      start = fairest[1]/base::strtoi('ff', base = 16),
      end = darkest[1]/base::strtoi('ff', base = 16))

    greyscale[match(M_nonzero, unique(M_nonzero)|> sort())]
  } else {
    '#a3a3a3'
  })|>
    add.attribute(g@data, 'edge', 'color', value = _)

  g@data <- (if(!is.null.na(edge.width)){
    if(!exists('M_nonzero'))M_nonzero <- M[M!=0]
    rescale.numeric(x = M_nonzero,
                    to = c(edge.width.min,
                           edge.width.max))
  } else {
    edge.width.max
  })|>
    add.attribute(g@data, 'edge', 'width', value = _)

  g
}


#' Represent a firm-firm (FF) network using the package \code{network}
#'
#' Create an object of class \code{network} from the package \code{network} using a FF matrix of class \code{financial_matrix}
#'
#' @details
#'
#' This function allows for a number of additional arguments.
#'
#' @section What can be passed to \code{vertex.colour} and \code{vertex.size}:
#'
#' The pieces of information that is possible to pass to \code{vertex.size} and \code{vertex.colour} are:
#' \itemize{
#'  \item \code{capitalisation}, will be arranged into steps (see \code{capitalisation.bins} below)
#'  \item \code{revenue}, will be arranged into steps (see \code{revenues.bins} below)
#'  \item \code{legal_form}
#'  \item \code{sector}
#'  \item \code{currency}
#' }
#'
#' @section What can be passed to \code{edge.width} and \code{edge.greyscale}:
#'
#' The pieces of information that is possible to pass to \code{edge.width} and \code{edge.greyscale} are:
#' \itemize{
#'  \item \code{capitalisation}
#'  \item \code{revenue}
#' }
#'
#' @section Additional parameters related to \code{vertex.size}:
#'
#' The effect of the additional parameters that modify the behaviour of \code{vertex.size} are:
#'
#' \code{vertex.size.max} (defaults to \code{5}):
#'  \itemize{
#'   \item if \code{vertex.size} or one of its aliases is specified, this is the size of the biggest vertex;
#'   \item if neither \code{vertex.size} nor any of its aliases is given, this is the size of ALL vertices.
#'  }
#'
#' \code{vertex.size.min} (defaults to \code{1}):
#'  \itemize{
#'   \item if \code{vertex.size} or one of its aliases is specified, this is the size of the smallest vertex;
#'   \item if neither \code{vertex.size} nor any of its aliases is given, it is ignored.
#'  }
#'
#' @section Additional parameters related to \code{vertex.colour}:
#'
#' The only additional parameter related to \code{vertex.colour} is \code{vertex.colour.palette}.
#' It supports a vector of RGB or named colours (see \code{\link[grDevices]{colours}} for all named colours in \code{R}).
#' It also accepts complete calls to functions that return a such a vector like \code{RColorBrewer::brewer.pal(n, name)} or \code{viridisLite::viridis(n, option)}.
#' If the palette is too short, it will be extended automatically using \code{\link[grDevices]{colorRampPalette}}.
#' If the palette is not declared, but this arguemnt is \code{TRUE}, it will defaulr to the following vector of colours:
#' \itemize{
#'    \item \code{#00204D}, \href{https://www.color-name.com/hex/00204d}{Oxford Blue}
#'    \item \code{#31446B}, \href{https://www.color-name.com/hex/31446B}{Police Blue}
#'    \item \code{#666970}, \href{https://www.color-name.com/hex/666970}{Dim Gray}
#'    \item \code{#958F78}, \href{https://www.color-name.com/hex/958F78}{Artichoke}
#'    \item \code{#CBBA69}, \href{https://www.color-name.com/hex/CBBA69}{Dark Khaki}
#'    \item \code{#FFEA46}, \href{https://www.color-name.com/hex/FFEA46}{Gargoyle Gas}
#' }
#' If the argument is \code{FALSE}, \code{NULL} or \code{NA}, the vertex will be coloured of \code{#081677} (\href{https://www.color-name.com/gentian-blue.color}{Gentian blue}).
#'
#' @section Additional parameters related to \code{edge.width}:
#'
#' \code{edge.width.max} (defaults to \code{5}):
#' \itemize{
#'   \item if \code{edge.width} or one of its aliases is specified, this is the thickness of the thickest edge;
#'   \item if neither \code{edge.width} nor any of its aliases is given, this is the thickness of ALL edges
#'  }
#'
#' \code{edge.width.min} (defaults to \code{1}):
#' \itemize{
#'   \item if \code{edge.width} or one of its aliases is specified, this is the thickness of the slimmest edge;
#'   \item if neither \code{edge.width} nor any of its aliases is given, it is ignored.
#' }
#'
#' @section Additional parameters related to \code{edge.greyscale}:
#'
#' \code{edge.greyscale.darkest} (defaults to \code{5}) :
#' \itemize{
#'   \item if \code{edge.greyscale} or one of its aliases is specified, this is the thickness of the thickest edge;
#'   \item if neither \code{edge.greyscale} nor any of its aliases is given, this is the thickness of ALL edges
#' }
#'
#' \code{edge.greyscale.fairest} (defaults to \code{1}):
#' \itemize{
#'   \item if \code{edge.greyscale} or one of its aliases is specified, this is the thickness of the slimmest edge;
#'   \item if neither \code{edge.greyscale} nor any of its aliases is given, it is ignored.
#' }
#'
#' Several aliases are accepted for all arguments, except \code{M}:
#' \itemize{
#'  \item for \code{vertex.size}: \code{node.size}
#'  \item for \code{vertex.colour}: \code{vertex.color}, \code{node.colour}, and \code{node.color};
#'  \item for \code{edge.width}: \code{tie.width}
#'  \item for \code{edge.greyscale}: \code{tie.grayscale}, \code{tie.greyscale}, and \code{edge.grayscale}
#' }
#'
#' @param x A matrix-like object produced by \code{\link{FF}} and related functions
#' @param vertex.size Which piece of information on the firms should be used to represent the nodes' size (see Details).
#' @param vertex.colour Which piece of  information on the firms should be used to represent the nodes' colours (see Details).
#' @param edge.width Whether to use the edges' width to represent tie strength. Defaults to \code{FALSE}.
#' @param edge.greyscale Whether to use the edges' colour to represent tie strength through a grey scale. Defaults to \code{TRUE} if the matrix is valued.
#' @param directed Whether the network should be directed. Defaults to \code{TRUE}
#' @param loops Whether the network should have loops. Defaults to \code{FALSE}
#' @param weighted Whether the ties/edges should be weighted. Defaults to \code{TRUE} if any element of the matrix equals neither 0 nor 1
#' @param ... Aliases to the other parameters and additional settings (see Details).
#'
#' @return A network in the desired format
#'
#' @examples
#' # Create the network representation of the binary FF of
#' # Berkshire Hataway's holdings based on common ownership
#' data("firms_BKB")
#' x <- FF.naive.ownership(firms_BKB)
#' FF.net.custom(x = x, node.size = 3)
#'
#' @seealso \link{FF.net} \link{FF.graph} \link{FF.graph.custom}
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @export

FF.net.custom <- function(x, vertex.size = NULL, vertex.colour = NULL,
                          edge.width = NULL, edge.greyscale = NULL,
                          directed = TRUE, loops = FALSE,
                          weighted = any(x@M%in%c(0, 1)),
                          ...){

  get.net(x, vertex.size = vertex.size, vertex.colour = vertex.colour,
          edge.width = edge.width, edge.greyscale = edge.greyscale,
          directed = directed, loops = loops,
          format = 'network',
          weighted = weighted, ...)

}

#' Easily represent a firm-firm (FF) network using the package \code{network}
#'
#' Create an object of class \code{network} from the package \code{network} using a FF matrix of class \code{financial_matrix} using all the default aesthetic options
#'
#' @details
#'
#' This function does not allow for any of the additional arguments that can be passed to \code{\link{FF.net.custom}}.
#'
#' @section Loops and values:
#' Loops will be allowed if at least one of the matrix's diagonal entries is not zero.
#' The network will be valued if at least one entry of the matrix is neither zero nor one.
#'
#' Instead, if \code{aesthetic} is set to \code{'simple'}:
#' \itemize{
#'  \item The width of the ties is \code{1};
#'  \item The colour of the ties is \code{#b4b4b4} (\href{https://www.color-name.com/hex/b4b4b4}{Philippine Silver});
#'  \item The size of the nodes is \code{5};
#'  \item The colour of the nodes is \code{#081677} (\href{https://www.color-name.com/gentian-blue.color}{Gentian blue}).
#' }
#'
#' Otherwise, if \code{aesthetic} is set to \code{'nice'}:
#' \itemize{
#'  \item The width of the ties is \code{1};
#'  \item The colour of the ties is a grey scale reflecting tie strength if the network is valued, otherwise it is \code{#b4b4b4} (\href{https://www.color-name.com/hex/b4b4b4}{Philippine Silver});
#'  \item The size of the nodes reflects their \code{capitalisation} if all firms have data on it and ranges between \code{1} and \code{5}, otherwise it is \code{5} for all nodes;
#'  \item The colour of the nodes reflects their \code{sector} if all firms have data on it is taken from a built-in palette, otherwise it is \code{#081677} (\href{https://www.color-name.com/gentian-blue.color}{Gentian blue}).
#' }
#'
#' @param x A matrix-like object produced by \code{\link{FF}} and related functions.
#' @param aesthetic Choose a pre-set for the network's look. Either \code{'simple'} or \code{'nice'} (see Details).
#'
#' @return A network in the desired format
#'
#' @examples
#' # Create a nice network representation of the binary FF of
#' # Berkshire Hataway's holdings based on common ownership
#' data("firms_BKB")
#' x <- FF.naive.ownership(firms_BKB)
#' FF.net(x = x, aesthetic = 'nice')
#'
#' @seealso \link{FF.net.custom} \link{FF.graph} \link{FF.graph.custom}
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @export

FF.net <- function(x, aesthetic = c('simple', 'nice')){

  loops <- tryCatch(any(diag(x@M)!=0), error = function(e)FALSE)

  weighted <- !all(x@M==0|x@M==1)

  if(aesthetic =='simple'){
    vertex.size <- NULL
    edge.greyscale <- FALSE
  } else {
    vertex.size <- ifelse(is.null.na(x@capitalisation)|> any(),
                          NULL, 'capitalisation')
    edge.greyscale <- weighted
  }

  get.net(x, vertex.colour = NULL, edge.width = FALSE, format = 'network',
          directed = TRUE, vertex.size = vertex.size, loops = loops,
          edge.greyscale = edge.greyscale, weighted = weighted)

}

#' Represent a firm-firm (FF) network using the package \code{igraph}
#'
#' Create an object of class \code{graph} from the package \code{igraph} using a FF matrix of class \code{financial_matrix}
#'
#' @details
#'
#' This function allows for a number of additional arguments.
#'
#' @section What can be passed to \code{vertex.colour} and \code{vertex.size}:
#'
#' The pieces of information that is possible to pass to \code{vertex.size} and \code{vertex.colour} are:
#' \itemize{
#'   \item \code{capitalisation}, will be arranged into steps (see \code{capitalisation.bins} below)
#'   \item \code{revenue}, will be arranged into steps (see \code{revenues.bins} below)
#'   \item \code{legal_form}
#'   \item \code{sector}
#'   \item \code{currency}
#' }
#'
#' @section What can be passed to \code{edge.width} and \code{edge.greyscale}:
#'
#' The pieces of information that is possible to pass to \code{edge.width} and \code{edge.greyscale} are:
#' \itemize{
#'  \item \code{capitalisation}
#'  \item \code{revenue}
#' }
#'
#' @section Additional parameters related to \code{vertex.size}:
#'
#' The effect of the additional parameters that modify the behaviour of \code{vertex.size} are:
#'
#' \code{vertex.size.max} (defaults to \code{5}) :
#'  \itemize{
#'   \item if \code{vertex.size} or one of its aliases is specified, this is the size of the biggest vertex;
#'   \item if neither \code{vertex.size} nor any of its aliases is given, this is the size of ALL vertices.
#'  }
#'
#' \code{vertex.size.min} (defaults to \code{1}):
#'  \itemize{
#'   \item if \code{vertex.size} or one of its aliases is specified, this is the size of the smallest vertex;
#'   \item if neither \code{vertex.size} nor any of its aliases is given, it is ignored.
#'  }
#'
#' @section Additional parameters related to \code{vertex.colour}:
#'
#' The only additional parameter related to \code{vertex.colour} is \code{vertex.colour.palette}.
#' It supports a vector of RGB or named colours (see \code{\link[grDevices]{colours}} for all named colours in \code{R}).
#' It also accepts complete calls to functions that return a such a vector like \code{RColorBrewer::brewer.pal(n, name)} or \code{viridisLite::viridis(n, option)}.
#' If the palette is too short, it will be extended automatically using \code{\link[grDevices]{colorRampPalette}}.
#' If the palette is not declared, but this arguemnt is \code{TRUE}, it will defaulr to the following vector of colours:
#' \itemize{
#'    \item \code{#00204D}, \href{https://www.color-name.com/hex/00204d}{Oxford Blue}
#'    \item \code{#31446B}, \href{https://www.color-name.com/hex/31446B}{Police Blue}
#'    \item \code{#666970}, \href{https://www.color-name.com/hex/666970}{Dim Grey}
#'    \item \code{#958F78}, \href{https://www.color-name.com/hex/958F78}{Artichoke}
#'    \item \code{#CBBA69}, \href{https://www.color-name.com/hex/CBBA69}{Dark Khaki}
#'    \item \code{#FFEA46}, \href{https://www.color-name.com/hex/FFEA46}{Gargoyle Gas}
#' }
#' If the argument is \code{FALSE}, \code{NULL} or \code{NA}, the vertex will be coloured of \code{#081677} (\href{https://www.color-name.com/gentian-blue.color}{Gentian blue}).
#'
#' @section Additional parameters related to \code{edge.width}:
#'
#' \code{edge.width.max} (defaults to \code{5}) :
#'  \itemize{
#'   \item if \code{edge.width} or one of its aliases is specified, this is the thickness of the thickest edge;
#'   \item if neither \code{edge.width} nor any of its aliases is given, this is the thickness of ALL edges
#'  }
#'
#' \code{edge.width.min} (defaults to \code{1}):
#'  \itemize{
#'   \item  if \code{edge.width} or one of its aliases is specified, this is the thickness of the slimmest edge;
#'   \item  if neither \code{edge.width} nor any of its aliases is given, it is ignored.
#'  }
#'
#' @section Additional parameters related to \code{edge.greyscale}:
#'
#' \code{edge.greyscale.darkest} (defaults to \code{5}) :
#'  \itemize{
#'   \item if \code{edge.greyscale} or one of its aliases is specified, this is the thickness of the thickest edge;
#'   \item if neither \code{edge.greyscale} nor any of its aliases is given, this is the thickness of ALL edges
#'  }
#' \code{edge.greyscale.fairest} (defaults to \code{1}):
#'  \itemize{
#'   \item if \code{edge.greyscale} or one of its aliases is specified, this is the thickness of the slimmest edge;
#'   \item if neither \code{edge.greyscale} nor any of its aliases is given, it is ignored.
#'  }
#'
#' Several aliases are accepted for all arguments, except \code{M}:
#' \itemize{
#'  \item for \code{vertex.size}: \code{node.size}
#'  \item for \code{vertex.colour}: \code{vertex.color}, \code{node.colour}, and \code{node.color};
#'  \item for \code{edge.width}: \code{tie.width}
#'  \item for \code{edge.greyscale}: \code{tie.grayscale}, \code{tie.greyscale}, and \code{edge.grayscale}
#' }
#'
#' @param x A matrix-like object produced by \code{\link{FF}} and related functions
#' @param vertex.size Which piece of information on the firms should be used to represent the nodes' size (see Details).
#' @param vertex.colour Which piece of  information on the firms should be used to represent the nodes' colours (see Details).
#' @param edge.width Whether to use the edges' width to represent tie strength. Defaults to \code{FALSE}.
#' @param edge.greyscale Whether to use the edges' colour to represent tie strength through a grey scale. Defaults to \code{TRUE} if the matrix is valued.
#' @param directed Whether the network should be directed. Defaults to \code{TRUE}
#' @param loops Whether the network should have loops. Defaults to \code{FALSE}
#' @param weighted Whether the ties/edges should be weighted. Defaults to \code{TRUE} if any element of the matrix equals neither 0 nor 1
#' @param ... Aliases to the other parameters and additional settings (see Details).
#'
#' @return A network in the desired format
#'
#' @examples
#' # Create the graph representation of the binary FF of
#' # Berkshire Hataway's holdings based on common ownership
#' data("firms_BKB")
#' x <- FF.naive.ownership(firms_BKB)
#' FF.graph.custom(x = x, node.size = 3)
#'
#' @seealso \link{FF.net} \link{FF.net.custom} \link{FF.graph}
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @export

FF.graph.custom <- function(x, vertex.size = NULL, vertex.colour = NULL,
                            edge.width = NULL, edge.greyscale = NULL,
                            directed = TRUE, loops = FALSE,
                            weighted = any(x@M%in%c(0, 1)),
                            ...){

  get.net(x, vertex.size = vertex.size, vertex.colour = vertex.colour,
          edge.width = edge.width, edge.greyscale = edge.greyscale,
          directed = directed, loops = loops,
          format = 'igraph',
          weighted = weighted, ...)

}

#' Easily represent a firm-firm (FF) network using the package \code{igraph}
#'
#' Create an object of class \code{igraph} from the package \code{igraph} using a FF matrix of class \code{financial_matrix} using all the default aesthetic options
#'
#' @details
#'
#' This function does not allow for any of the additional arguments that can be passed to \code{\link{FF.graph.custom}}.
#'
#' @section Loops and values:
#' Loops will be allowed if at least one of the matrix's diagonal entries is not zero.
#' The igraph will be valued if at least one entry of the matrix is neither zero nor one.
#'
#' Instead, if \code{aesthetic} is set to \code{'simple'}:
#' \itemize{
#'  \item The width of the ties is \code{1};
#'  \item The colour of the ties is \code{#b4b4b4} (\href{https://www.color-name.com/hex/b4b4b4}{Philippine Silver});
#'  \item The size of the nodes is \code{5};
#'  \item The colour of the nodes is \code{#081677} (\href{https://www.color-name.com/gentian-blue.color}{Gentian blue}).
#' }
#'
#' Otherwise, if \code{aesthetic} is set to \code{'nice'}:
#' \itemize{
#'  \item The width of the ties is \code{1};
#'  \item The colour of the ties is a grey scale reflecting tie strength if the graph is valued, otherwise it is \code{#b4b4b4} (\href{https://www.color-name.com/hex/b4b4b4}{Philippine Silver});
#'  \item The size of the nodes reflects their \code{capitalisation} if all firms have data on it and ranges between \code{1} and \code{5}, otherwise it is \code{5} for all nodes;
#'  \item The colour of the nodes reflects their \code{sector} if all firms have data on it is taken from a built-in palette, otherwise it is \code{#081677} (\href{https://www.color-name.com/gentian-blue.color}{Gentian blue}).
#' }
#'
#' @param x A matrix-like object produced by \code{\link{FF}} and related functions.
#' @param aesthetic Choose a pre-set for the graph's look. Either \code{'simple'} or \code{'nice'} (see Details).
#'
#' @return A network in the desired format
#'
#' @examples
#' # Create a nice graph representation of the binary FF of
#' # Berkshire Hataway's holdings based on common ownership
#' data("firms_BKB")
#' x <- FF.naive.ownership(firms_BKB)
#' FF.graph(x = x, aesthetic = 'nice')
#'
#' @seealso \link{FF.net} \link{FF.net.custom} \link{FF.graph.custom}
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @export

FF.graph <- function(x, aesthetic = c('simple', 'nice')){
  loops <- tryCatch(any(diag(x@M)!=0), error = function(e)FALSE)
  weighted <- !all(x@M==0|x@M==1)

  if(aesthetic =='simple'){
    vertex.size <- NULL
    edge.greyscale <- FALSE
  } else {
    vertex.size <- ifelse(is.null.na(x@capitalisation)|> any(),
                          NULL, 'capitalisation')
    edge.greyscale <- weighted
  }

  get.net(x, vertex.colour = NULL, edge.width = FALSE, format = 'igraph',
          directed = TRUE, vertex.size = vertex.size, loops = loops,
          edge.greyscale = edge.greyscale,
          weighted = if(weighted){TRUE}else{NULL})

}

