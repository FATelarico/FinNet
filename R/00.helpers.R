#' Function to return an object after assigning new names
#'
#' Combines \code{magrittr::set_colnames}, \code{magrittr::set_rownames}, \code{magrittr::set_names}
#'
#' @param x Object on which to operate
#' @param names New names
#' @param where What to change:
#' \itemize{
#'  \item{\code{col} - Column names}
#'  \item{\code{row} - Row names}
#'  \item{\code{names} - Names attribute}
#' }
#' @return The original object, with new names
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @keywords internal

.set_names <- function(x, names, where = c('col', 'row', 'attr')){
  `f<-` <- ifelse(where == 'col', `colnames<-`,
                  ifelse(where == 'row', `rownames<-`, `names<-`))
  f(x) <- names
  x
}

#' Function to check whether an object is \code{NA} or \code{NULL}
#'
#' Combines \code{base::is.na(x)}, \code{base::is.null(x)}. When \code{negating} is \code{TRUE}, it integrates also \code{f(x)|> magrittr::not()}.
#'
#' @param x Object on which to operate
#' @param negating Whether to return the negation of the result
#'
#' @return Logical, depending on \code{negating}:
#' \itemize{
#'  \item if \code{negating} is \code{FALSE}, it returns \code{TRUE} if \code{x} is \code{NA} or \code{NULL};
#'  \item if \code{negating} is \code{TRUE}, it returns \code{TRUE} if \code{x} is \strong{neither} \code{NA} \strong{nor} \code{NULL}.
#' }
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @keywords internal

is.null.na <- function(x, negating = FALSE){
  res <- any(is.null(x), is.na(x))
  ifelse(negating, !res, res)
}

#' Function to check whether an object is neither \code{NA} nor \code{NULL}
#'
#' Combines \code{base::is.na(x)|> magrittr::not()} and \code{base::is.null(x)|> magrittr::not()}.
#'
#' @param x Object on which to operate
#'
#' @return Logical: \code{TRUE} if \code{x} is neither \code{NA} or \code{NULL}, \code{FALSE} otherwise.
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @keywords internal

is.neither.null.nor.na <- function(x){
  !(is.null(x)|is.na(x))
}

#' Function to extract the symbols of the objects passed as \code{...}
#'
#' This function will not work when called inside the function to which the objects were explicitly passsed as \code{...}.
#'
#' @references \url{https://stackoverflow.com/a/11892680}
#'
#' @param ... Objects on which to operate
#'
#' @return A vector of strings matching the provided objects' symbols.
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @keywords internal
#'
naming <- function(...){
  as.character(match.call(expand.dots = TRUE))[-1]
}

#' Function to list multiple objects passed as \code{...}
#'
#' @param ... Objects on which to operate
#' @param naming Logical | Whether to name the list after the symbols of the provided objects. Defaults to \code{TRUE}
#'
#' @return A (possibly named) list of the provided objects.
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @keywords internal

listing <- function(..., naming = TRUE){
  y <- list(...)
  if(naming){
    names(y) <- naming(...)
  }
  y
}

#' Function to chose the right algorithm when querying one or more information over multiple \code{firm} objects
#'
#' @param firm List of objects on which to operate
#' @param naming Logical | Whether to name the result after \code{names(firms)}. Defaults to \code{TRUE}
#' @param unlisting Logical | Whether to un-list the result. Defaults to \code{FALSE}
#'
#' @return The queried information.
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @keywords internal

query.firms_switch <- function(firm, which, naming = TRUE, unlisting = FALSE){
  res <- if(length(which == 1)){
    query.firm(firm = firm, which = which, naming = naming)
  } else {
    lapply(which, function(this){
      query.firm(firm = firm, which = this, naming = naming)
    })
  }

  if(length(res)>0){
    if(naming)names(res) <- which
    if(unlisting)res <- unlist(res)
  }

  res
}

#' Function to compute the binary values of the FM or FO matrix for multiple \code{firm} objects
#'
#' @param firms List of objects on which to operate
#' @param which Whether to use ownership or management to construct the matrix
#' @param cols Possible values assumed by the enquired variable (determined autmotically if not provided)
#'
#' @return The values to be filled in the binary FO or FM matrix
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @keywords internal

get.binarary.values <- function(firms, which, cols = NULL){
  data <- query.firms(firms = firms, which = which)

  if(is.null.na(cols)){
    cols <- unlist(data)|> unique()|> sort()
  }

  res <- lapply(data, function(x){
    pos <- match(x, cols)
    y <- rep(0, times = length(cols))
    y[pos] <- 1
    y
  })

  unlist(res)
}

#' Function to check whether the provided colour is actually a shade of grey and correct it if necessary
#'
#' @param hex The RGB colour to check
#'
#' @return A valid grey colour in RGB format
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @keywords internal

check.correct.grey <- function(hex){
  clrs <- lapply(c(2, 4, 6), function(i){
    substr(hex, i, i+1)
  })|> unlist()

  y <- rep(clrs[1], 3)|> paste0(collapse = '')|>
    paste0('#', x = _)

  if(!all(clrs==clrs[1])){
    warning(c('Incorrect grey specified: ', hex,
              '\nReplacing with:', y))
  }

  y
}


#' Function to rescale numeric vectors
#'
#' Rescale continuous vector to have specified minimum and maximum
#'
#' From: The package \href{https://scales.r-lib.org/}{\code{scales}}
#'
#' @param x A vector to re-scale
#' @param to output range (numeric vector of length two)
#' @param from input range (vector of length two).  If not given, is
#'   calculated from the range of \code{x}
#'
#' @return Re-scaled vector
#'
#' @author \href{https://scales.r-lib.org/}{Hadley Wickham}
#'
#' @keywords internal

rescale.numeric <- function(x, to = c(0, 1), from = range(x, na.rm = TRUE, finite = TRUE), ...) {
  if (diff(from) || diff(to)) {
    return(ifelse(is.na(x), NA, mean(to)))
  }
  (x - from[1]) / diff(from) * diff(to) + to[1]
}

#' Function to create the default palette and ramp it up as needed
#'
#' @param length Length of the desired palette
#'
#' @return A vector of RGB codes containing \code{length} colours
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @keywords internal

palette <- function(length = 6){
  grDevices::colorRampPalette(c('#00204DFF', '#31446BFF', '#666970FF',
                                '#958F78FF', '#CBBA69FF', '#FFEA46FF')
  )(length)
}

#' Function to set a vertex  or edge attribute of a \code{network} or \code{graph} object
#'
#' @param x The representation of the network as a \code{network} or \code{graph} object
#' @param where What network element does the attribute refer to. Either \code{edge}/\code{tie} or \code{vertex}/\code{node}.
#' @param attr_name Name of the attribute to set
#' @param value of the attribute to set
#' @param which A subset of elements on which the attribute should be applied. Defaults to all the vertexes/nodes.
#'
#' @return A \code{network} or \code{graph} object with the desired attribute
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @keywords internal

add.attribute <- function(x, where = c('edge', 'vertex'),
                          attr_name, value, which = NULL){
  if(methods::is(x, 'igraph')){

    if(where == 'edge'){
      x <- igraph::set.edge.attribute(
        x, name = attr_name, value = value,
        index = if(is.null.na(which)){
          igraph::E(x)
        } else {
          which
        }
      )
    } else {
      x <- igraph::set.vertex.attribute(
        x, name = attr_name, value = value,
        index = if(is.null.na(which)){
          igraph::V(x)
        } else {
          which
        }
      )
    }

  } else {

    if(where == 'edge'){
      x <- network::set.edge.attribute(
        x, attrname = attr_name, value = value,
        e =  if(is.null.na(which)){
          seq_along(x$mel)
        } else {
          which
        }
      )
    } else {
      x <- network::set.vertex.attribute(
        x, attrname = attr_name, value = value,
        index =  if(is.null.na(which)){
          seq_len(network::network.size(x))
        } else {
          which
        }
      )
    }
  }

  x
}


#' Function to  load the package \code{Matrix}
#'
#' Checks if the package is required and check whether it is available
#'
#' @param x The object on which to base the decision
#' @param pos Integer specifying position to attach
#'
#' @return A \code{logical} whether to load the package \code{Matrix}
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @family Internal matrix (de)constructrs
#'
#' @keywords internal

load.Matrix <- function(x, pos = 1L){
  y <- grepl('Matrix', is(x)|> paste(collapse = ' '), ignore.case = FALSE)

  if(y){
    tryCatch(requireNamespace('Matrix', pos = pos), error = function(e){
      stop(paste0(
        'The object contains an object of class `',
        is(x)[1], '`', 'but the package ',
        '`Matrix` is not installed.\n To solve either:',
        '\n    - Provide a different object; or',
        '\n    - Run `install.packages(\'Matrix\')`'
      ))
    })
  }
  y
}

#' Function to extract the bare matrix from several complex objects
#'
#' @param x The representation of the network as an object of class:
#' \itemize{
#'  \item{\code{financial_matrix} produced by \code{FF} and family;}
#'  \item{\code{network_financial} or \code{network} if the relevant package is installed;}
#'  \item{\code{igraph_financial} or \code{igraph} if the  relevant package is installed.}
#' }
#' @return A \code{matrix} or an object from the package \code{Matrix}
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @family Internal matrix (de)constructrs
#'
#' @keywords internal

extract.matrix <- function(x, .except.igraph = FALSE){
  obj <- is(x)

  # The object is a (FF) matrix or depends on the `network` package
  if(any(c('financial_matrix',
           'network_financial', 'network')%in%obj)){

    # If it is a `network` object created with `FinNet`
    if('network_financial'%in%obj){
      # Extract the underlying network
      x <- x@data
    }

    # If it is a standard `network` object
    if('network'%in%is(x)){
      # Extract the underlying network
      network::as.sociomatrix(
        x = x,
        attrname = if('weight'%in%network::list.edge.attributes(x)){
          'weight'
        } else{
          NULL
        })
    } else { # If it is a FF matrix
      # Load `Matrix` if required and check whether it is available
      load.Matrix(x@M)
      x@M
    }
  } else if(any(c('igraph_financial', 'igraph')%in%obj)){
    # The object depends on the `igraph` package
    if('igraph_financial'%in%obj){
      # Extract the underlying graph
      x <- x@data
    }

    # If the result should be `igraph`, stop here
    if(.except.igraph)return(x)

    # Otherwise, extract the underlying matrix
    igraph::get.adjacency(
      graph = x,
      attr = if('weight'%in%names(igraph::edge.attributes(x@data))){
        'weight'
      } else{
        NULL
      })

  }
}

#' Tarjan's algorithm for finding strongly connected components
#'
#' This function performs a depth-first search (DFS) on a directed graph to identify
#' strongly connected components (SCCs) and their size
#'
#' This function is a modified version of the R implementation of Tarjan's
#' algorithm for finding strongly connected components in directed graphs by
#' Ettore Settanni at the University of Cambridge (see References).
#'
#' @param test_m A square adjacency matrix representing the directed network.
#'
#' @details The function consists of several internal steps:
#' \enumerate{
#'   \item{Node Labeling - All nodes are labeled with two-digit names for clarity in referencing.}
#'   \item{Successor List Creation - For each node, lists of direct successors are compiled.}
#'   \item{Utilization Table Setup - A table is set up for tracking exploration details such as depth and backtracking information.}
#'   \item{Main DFS Loop - The core loop where DFS occurs, including node visitation and backtracking logic to determine SCCs.}
#'   \item{Stack Management - Nodes are managed in a stack to keep track of the current path of exploration and to facilitate backtracking.}
#'   \item{SCC Identification - Upon finishing exploration of an SCC, it is identified and nodes are popped from the stack.}
#' }
#'
#' @return A list containing two elements:
#' \itemize{
#'  \item{\code{n} - number of strongly connected components}
#'  \item{\code{sizes} - size of each strongly connected component, in order of discovery}
#' }
#'
#' @references Settanni, Ettore. ‘RtarD - Find Strongly Connected Components in a Digraph Using R’. R, 15 November 2021. \url{https://github.com/Dr-Eti/RtarD-Tarjans_DFS_in_R}.
#'
#' @author \enc{Settanni,Ettore}{Ettore Settanni}
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @family SCC finders
#'
#' @keywords internal

SCC <- function(test_m){

  n_nodes <- ncol(test_m)
  node_names <- formatC(0:(ncol(test_m)-1),width = 2, flag = "0")          # thread: https://stackoverflow.com/questions/8266915/format-number-as-fixed-width-with-leading-zeros
  colnames(test_m) <- node_names
  rownames(test_m) <- node_names

  ## step 1: create list of successors for each node
  successors <- lapply(node_names, function(x){
    current_node <- x
    # search the row corresponding to the current node for successors
    all_successors_idx <- expand.grid(current_node, node_names, width = 2, flag = "0")                 # expand on current node to find successors
    all_successors_links <- apply(all_successors_idx, 1, function(y){
      test_m[y[1], y[2]]
    })
    if(length(as.character(all_successors_idx[which(all_successors_links != 0),2])) > 0){
      as.character(all_successors_idx[which(all_successors_links != 0),2])
    } else {
      NA
    }
  })
  names(successors) <- node_names


  ## step 2: lookup table (going to use this for depth descent and backtracking)
  util_table <- lapply(1:n_nodes, function(z){
    x <- successors[z]
    a <- match(names(x), node_names)
    b <- match(unlist(x), node_names)
    temp_col <- expand.grid(a,b)
    tab_head <- c("i",
                  "j",
                  "level",
                  "v_node",
                  #"v_label",
                  "v_number",
                  "v_lowlink",
                  "successors",
                  "k_successor_idx",
                  "w_k_node",
                  #"w_k_label",
                  "w_k_number",
                  "w_k_lowlink")
    temp_mat <- matrix(NA, ncol = length(tab_head), nrow = nrow(temp_col))
    colnames(temp_mat) <- tab_head
    rownames(temp_mat) <- rep(names(x),nrow(temp_mat))
    temp_mat[,"v_node"] <- temp_col[,1]
    temp_mat[, "w_k_node"] <- temp_col[,2]
    return(temp_mat)
  })
  util_table_df <- do.call(rbind, util_table)


  ## step 3: initialise stuff
  # counters
  n <- n_nodes
  i <- 0
  comp_count <- 0                                                                                                             # component counter
  # initialise condition flags
  nodes_not_numbered_yet <- n
  test0 <- TRUE
  # initialise lists
  Stack_S <- list()
  Component_list <- list()
  # initialise table of node numbers
  node_numbering <- cbind(rep(NA,length(node_names)),rep(NA,length(node_names)),rep(NA,length(node_names)))
  colnames(node_numbering) <- c("node_number","node_lowlink","node_onStack")
  rownames(node_numbering) <- node_names

  ## Step 4: Main loop - the magic happens here (if it works)
  # Loop #0: restart at each strongly connected component
  dummy4 <- TRUE
  while(dummy4){
    level <- 1                                                                                               # I AM NOT ENTIRELY SURE ABOUT THIS SYSTEM I CAME UP WITH. it's like a thread that pulls you back up once the depth-first search reached an end.
    j <- 0
    # pick the first node not numbered yet
    nodes_to_explore <- which(is.na(node_numbering[, "node_number"]))
    if(nodes_not_numbered_yet != 0){                                                                         # there are nodes not numbered yet
      v <- as.numeric(nodes_to_explore[1])                                                                   # just picking the first unnumbered node. In the first iteration this will be node 1
      v_label <- node_names[v]
    }

    # REV01
    flag_extra_round <- TRUE                                                                                 # for use later during backtracking to add an extra round and avoid some issues with updating values at the very end
    extra_round_counter <- 0

    # Loop #1: explores successors "depth first", jumping between nodes
    dummy0 <- TRUE
    while(dummy0){
      i <- i + 1                                                                                             # labels nodes as we visit them
      j <- j + 1                                                                                             # position in stack of nodes
      # update node numbering
      Stack_S[j] <- v
      node_numbering[v,"node_number"] <- i
      node_numbering[v,"node_lowlink"] <- i
      node_numbering[v,"node_onStack"] <- 1
      # successors
      w_labels <- unlist(successors[v])

      # update records were v features as successor
      back_idx <- which(util_table_df[,"w_k_node"] == v)
      if (length(back_idx) > 0){
        util_table_df[back_idx, "w_k_number"] <- node_numbering[v,"node_number"]
        util_table_df[back_idx, "w_k_lowlink"] <- node_numbering[v,"node_lowlink"]
      }
      # check if successor is a sink node
      sink_test <- which(is.na(w_labels))
      if (length(sink_test) == 0){
        w <- match(w_labels, node_names)
        n_successors <- length(w)
      } else {
        w <- NA
        n_successors <- 0
      }
      # initialise a progressive counter for incident nodes reachable from the current node
      k <- 0

      # Loop #2: explores incident nodes "sequentially" for a given node
      dummy1 <- TRUE
      while(dummy1){
        if(length(sink_test) == 0){                                                                          # the current node has a successor
          k <- k + 1
          w_k <- w[k]                                                                                        # next incident node (neighbor)
          # update main tableau (df version)
          util_table_df_subset <- which(rownames(util_table_df) == v_label)                                  # filter for the current node v
          if(length(util_table_df_subset) > 1){
            if(k == 1){
              util_table_df[util_table_df_subset,][k,"i"] <- i
              util_table_df[util_table_df_subset,][k,"j"] <- j
              util_table_df[util_table_df_subset,][k,"level"] <- level
              util_table_df[util_table_df_subset,][k,"v_node"] <- v
              util_table_df[util_table_df_subset,][k,"v_number"] <- node_numbering[v,"node_number"]
              util_table_df[util_table_df_subset,][k,"v_lowlink"] <- node_numbering[v,"node_lowlink"]
              util_table_df[util_table_df_subset,][k,"successors"] <- n_successors
            }
            util_table_df[util_table_df_subset,][k,"k_successor_idx"] <- k
            # util_table_df[util_table_df_subset,][k,"w_k_node"] <- w_k                                        # could be removed...
            util_table_df[util_table_df_subset,][k,"w_k_number"] <- node_numbering[w_k,"node_number"]
            util_table_df[util_table_df_subset,][k,"w_k_lowlink"] <- node_numbering[w_k, "node_lowlink"]
          } else {
            if(k == 1){
              util_table_df[util_table_df_subset,"i"] <- i
              util_table_df[util_table_df_subset,"j"] <- j
              util_table_df[util_table_df_subset,"level"] <- level
              util_table_df[util_table_df_subset,"v_node"] <- v
              util_table_df[util_table_df_subset,"v_number"] <- node_numbering[v,"node_number"]
              util_table_df[util_table_df_subset,"v_lowlink"] <- node_numbering[v,"node_lowlink"]
              util_table_df[util_table_df_subset,"successors"] <- n_successors
            }
            util_table_df[util_table_df_subset,"k_successor_idx"] <- k
            # util_table_df[util_table_df_subset,"w_k_node"] <- w_k                                            # could be removed...
            util_table_df[util_table_df_subset,"w_k_number"] <- node_numbering[w_k,"node_number"]
            util_table_df[util_table_df_subset,"w_k_lowlink"] <- node_numbering[w_k, "node_lowlink"]
          }
          # tests
          test0 <- is.na(node_numbering[w_k,"node_number"])                                                  # we can jump onto this node (depth first)
          test1 <- node_numbering[w_k,"node_number"] < node_numbering[v,"node_number"]                       # the next node has been visited already?
          test2 <- node_numbering[w_k,"node_onStack"] == 1                                                   # the next node is in the stack already OR has been on the stack
        } else {                                                                                             # there is no successor to the current node (sink)
          # tests
          test0 <- FALSE                                                                                     # forces to backtrack if there is no successor
          test1 <- FALSE
          test2 <- FALSE
          # update main tableau (df version)
          util_table_df_subset <- which(rownames(util_table_df) == v_label)                                  # filter for the current node v
          util_table_df[util_table_df_subset,"i"] <- i
          util_table_df[util_table_df_subset,"j"] <- j
          util_table_df[util_table_df_subset,"level"] <- level
          util_table_df[util_table_df_subset,"v_node"] <- v
          util_table_df[util_table_df_subset,"v_number"] <- node_numbering[v,"node_number"]
          util_table_df[util_table_df_subset,"v_lowlink"] <- node_numbering[v,"node_lowlink"]
          util_table_df[util_table_df_subset,"successors"] <- n_successors
          util_table_df[util_table_df_subset,"k_successor_idx"] <- k
          util_table_df[util_table_df_subset,"w_k_node"] <- (-1)
          util_table_df[util_table_df_subset,"w_k_number"] <- (-1)
          util_table_df[util_table_df_subset,"w_k_lowlink"] <- (-1)
        }

        test_POP <- FALSE

        # THIS IS WHERE WE DECIDE WHETHER TO CONTINUE DEPTH-FIRST, LOOK INTO A NEIGHBOUR, OR BACKTRACK
        if (test0){
          # continue depth-first search
          dummy1 <- FALSE                                                                                    # break Loop 2 (exit sequential exploration of successors for a given node)
          v <- w_k                                                                                           # move on to successor
          v_label <- node_names[v]
          # the below avoids having more than one line with the same level
          level_max <- max(util_table_df[,"level"], na.rm = T)
          if (level < level_max){                                                                            # probably we backtracked and re-winded the level counter.This should get us back where we were before the rewind
            level <- level_max + 1
          } else {
            level <- level + 1
          }
        } else {
          # start "back tracking"

          # Loop #3: are there neighbors left to explore?
          dummy5 <- TRUE
          while(dummy5){
            if(k != 0 & k < n_successors) {                                                                  # there are other adjacent nodes left to explore
              dummy5 <- FALSE

              # update lowlink values - FIRST TYPE (going downwards)
              if (test1 == TRUE & test2 == TRUE){                                                             # the current k successor has been visited PRIOR to v (has lower number), and it is on the stack already
                node_numbering[v,"node_lowlink"] <- min(node_numbering[v,"node_lowlink"],node_numbering[w_k,"node_number"])
                util_table_df_subset <- which(rownames(util_table_df) == v_label)                              # filter for the current node v
                if(length(util_table_df_subset) > 1){
                  util_table_df[util_table_df_subset,][1,"v_lowlink"] <- node_numbering[v,"node_lowlink"]      # always update the first line, corresponding to v
                } else {
                  util_table_df[util_table_df_subset,"v_lowlink"] <- node_numbering[v,"node_lowlink"]
                }
                # update lowlink elsewhere
                other_idx <- which(util_table_df[,"w_k_node"] == v)
                if(length(other_idx)>0){
                  util_table_df[other_idx, "w_k_lowlink"] <- node_numbering[v,"node_lowlink"]
                }
              }


            } else {
              dummy3 <- TRUE
              # Loop #4: revisit all the neighbours already visited
              while (dummy3) {
                if(n_successors !=0 & k !=0){                         # we are not at a sink node / we have not re-visited all the neighbors

                  # re-do test
                  test0 <- is.na(node_numbering[w_k,"node_number"])                                 # we can jump onto this node (depth first)
                  test1 <- node_numbering[w_k,"node_number"] < node_numbering[v,"node_number"]      # the next node has been visited already?
                  test2 <- node_numbering[w_k,"node_onStack"] == 1                                  # the next node is in the stack already


                  if (test2){                                                                       # the next node is STILL on the stack PLEASE DO NOT ADD TEST 1 OR WILL MESS UP
                    # update lowlink values - SECOND TYPE (ascending)
                    old_v_lowlink <- node_numbering[v,"node_lowlink"]
                    node_numbering[v,"node_lowlink"] <- min(node_numbering[v,"node_lowlink"],node_numbering[w_k,"node_lowlink"])
                    util_table_df_subset <- which(rownames(util_table_df) == v_label)                              # filter for the current node v
                    if(length(util_table_df_subset) > 1){
                      util_table_df[util_table_df_subset,][1,"v_lowlink"] <- node_numbering[v,"node_lowlink"]      # always update the first line, corresponding to v
                    } else {
                      util_table_df[util_table_df_subset,"v_lowlink"] <- node_numbering[v,"node_lowlink"]
                    }
                    # update lowlink elsewhere
                    other_idx <- which(util_table_df[,"w_k_node"] == v)
                    if(length(other_idx)>0){
                      util_table_df[other_idx, "w_k_lowlink"] <- node_numbering[v,"node_lowlink"]
                    }
                  }


                  # go back to the previous neighbour already visited
                  k <- k - 1
                  if (k !=0){
                    w_k <- w[k]
                    #re-do the test
                    test0 <- is.na(node_numbering[w_k,"node_number"])                                 # we can jump onto this node (depth first)
                    test1 <- node_numbering[w_k,"node_number"] < node_numbering[v,"node_number"]      # the next node has been visited already?
                    test2 <- node_numbering[w_k,"node_onStack"] == 1                                  # the next node is in the stack already
                  } else {
                    test2 <- FALSE
                    test1 <- FALSE
                    test0 <- FALSE
                    w_k <- (-1)
                  }
                } else {
                  # either we are at a sink node or we have re-visited all the neighbors

                  # NEW: WHEN WE ARE HERE, WE ARE FORCING THE BACKTRACKING - THERE WAS NO PATH LEADING US back to previously encountered nodes
                  # therefore we MUST NOT UPDATE LOWLINK when we encounter a node we visited before
                  # if v is a root node (lowlink = number), we can pop the stack at this stage
                  test_POP <- node_numbering[v,"node_number"] == node_numbering[v,"node_lowlink"]       # from Tarjan's original paper

                  # BEFORE POPPING
                  abort_POP <- (test_POP & level == 1)
                  if (abort_POP & flag_extra_round){
                    # we reached the last level (at least until we restart with level 1); WE NEED TO make sure that all nodes' lowlink values matches the lowest  lowlink of their successors.
                    # It happens that the root node gets popped out then the lowlink update occurs, and a bunch of nodes are left without root and can't pop next
                    # if that's not the case it may be that we need more rounds of update

                    extra_round_counter <- extra_round_counter + 1
                    # the below checks that we DON'T HAVE NODES WITH THE LATEST SUCCESSOR'S LOWLINK VALUE BEING lower THAN THE NODE'S LOWLINK
                    # if a node's LOWLINK is larger than the lowest among its successors' LOWLINK the we need to keep updating
                    update_needed <- lapply(Stack_S, function(x){
                      v_lowlink <- node_numbering[x,"node_lowlink"]
                      v_neigb_lowest_lowlink <- min(util_table_df[which(util_table_df[,"v_node"] == x) , "w_k_lowlink"])
                      if(v_neigb_lowest_lowlink > 0){                                                       # sink nodes by convention get "-1" in this field which might cause looping forever
                        if(v_lowlink != v_neigb_lowest_lowlink){
                          1
                        } else {0}
                      } else {0}

                    })
                    updates_test <- sum(unlist(update_needed))
                    if(updates_test > 0 & extra_round_counter < 4){
                      # reset the level to a max but add one and let the loop later pick which level is righ
                      level <- max(util_table_df[which(!is.na(util_table_df[,"level"])),"level"]) + 1          # start all over again, hoping we get all the lowlinks updated
                      test_POP <- FALSE                                                                        # ABORT POPPING FOR NOW
                    } else {
                      flag_extra_round <- FALSE                                                             # break out of this
                      abort_POP <- FALSE
                    }


                  }

                  if (test_POP){
                    # THIS IS WHERE WE POP THE STACK AND POPULATE THE COMPONENT
                    # update count of nodes not numbered yet
                    nodes_not_numbered_yet <- length(which(is.na(node_numbering[,"node_number"])))

                    # NEW: we want to pop the stack but only using v as a root (leave other nodes alone, if any) - different test to enter the stack-popping stage
                    temp_idx <- as.numeric(which(!is.na(node_numbering[,"node_number"])))
                    temp_subset_a <- which(node_numbering[temp_idx, "node_number"] >= node_numbering[v,"node_number"])
                    nodes_visited_so_far <- match(names(temp_subset_a[as.numeric(which(node_numbering[names(temp_subset_a), "node_onStack"] == 1))]), node_names)

                    Stack_S_BACKUP <- Stack_S
                    Component_list_BACKUP <- Component_list
                    # Start of "Pop nodes" sub, but only if we're done with depth-first search

                    # NEW: notice that the loop below should go from node "v" onward
                    for(r in nodes_visited_so_far){
                      x <- as.numeric(node_numbering[r,])
                      if (x[1] == x[2]){                         # the node is a root node
                        comp_count <- comp_count + 1                               # component number
                        # put nodes in component
                        temp_component_list <- lapply(1:length(Stack_S), function(s){
                          y <- Stack_S[[s]]
                          pop_test1 <- node_numbering[y, "node_number"] >= x[1]
                          pop_test2 <- node_numbering[y, "node_lowlink"] == x[2]
                          if(pop_test1 & pop_test2){
                            y
                          }
                        })
                        to_delete <- unlist(temp_component_list)
                        Component_list[[comp_count]] <- to_delete
                        # pop elements out of stack
                        #idx_delete <- which(Stack_S == to_delete)
                        temp_idx_delete <- rep(seq_along(Stack_S), sapply(Stack_S, length))         # thread: https://stackoverflow.com/questions/11002391/fast-way-of-getting-index-of-match-in-list
                        idx_delete <- temp_idx_delete[match(to_delete, unlist(Stack_S))]
                        Stack_S[idx_delete] <- NULL

                        # update j or the size of the stack will change later
                        j <- j - length(idx_delete)

                        # update table
                        node_numbering[to_delete,"node_onStack"] <- (-1)    # mark elements that have been on stack, but no longer are
                      }
                    }
                  }



                  dummy3 <- FALSE                                                                                                    # break loop 4 after moving one level up - REGARDLESS of popping the stack

                  # NEW: modified version of finding the "right level" to go back to

                  # ignore if we're forcing one last iteration to update lowling - special case
                  test_skip <- FALSE
                  while(!test_skip & level > 0){
                    level <- level - 1                                                                                             # if there is no successor OR the node at this level is not on the stack, keep going backwards
                    if(level != 0){
                      v_temp <- util_table_df[which(util_table_df[,"level"] == level),"v_node"]
                      level_on_stack <- (node_numbering[v_temp,"node_onStack"] == 1)                                               # skip levels that correspond to nodes that have been removed from stack
                      test_any_successor <- length(which(util_table_df[,"level"] == level & util_table_df[, "successors"] !=0))    # (no successor going up one level)
                      test_skip <- (level_on_stack & test_any_successor > 0)
                    }
                  }

                  # GET BACK TO WHERE YOU LEFT THINGS AT THE PREVIOUS LEVEL
                  if (level > 0){
                    back_idx <- which(util_table_df[,"level"] == level & util_table_df[, "successors"] !=0)       # there may be more than one node with the same level if E.G. ONE NEIGHBOUR IS A SINK but the other isn't. We ignore the sink
                    v <- util_table_df[back_idx, "v_node"]
                    v_label <-  node_names[v]
                    n_successors <- util_table_df[back_idx, "successors"]
                    k_back_idx_a <- as.numeric(which(util_table_df[, "v_node"] == v))
                    util_table_df_subset <- util_table_df[k_back_idx_a,]
                    if(length(k_back_idx_a) > 1){
                      k_back_idx_b <- max(which(!is.na(util_table_df_subset[, "k_successor_idx"])))               # retrieve last successor index explored for the current node
                      k <- util_table_df_subset[k_back_idx_b , "k_successor_idx"]
                      w_k <- util_table_df_subset[k_back_idx_b , "w_k_node"]
                    } else {
                      k <- as.numeric(util_table_df_subset["k_successor_idx"])
                      w_k <- as.numeric(util_table_df_subset["w_k_node"])
                    }
                    w <- as.numeric(util_table_df[k_back_idx_a, "w_k_node"])
                    w_labels <- node_names[w]
                    sink_test <- which(is.na(w_labels))                                                         # check if the successor is a sink node
                    # re-do test
                    test0 <- is.na(node_numbering[w_k,"node_number"])                                 # we can jump onto this node (depth first)
                    test1 <- node_numbering[w_k,"node_number"] < node_numbering[v,"node_number"]      # the next node has been visited already?
                    test2 <- node_numbering[w_k,"node_onStack"] == 1                                  # the next node is in the stack already
                  }
                }
              } # end of Loop 4 (dummy 3)
              if(level == 0) {
                dummy5 <- FALSE                                                                                # break loop 3
                dummy1 <- FALSE                                                                                # break loop 2 (ends up in the same place as if there were no successors)
              }
            }
          }  # end of Loop 3 (dummy 5)
        } #  end of ELSE - "back tracking" sub
      } # end of Loop 2 (dummy1)
      nodes_not_numbered_yet <- length(which(is.na(node_numbering[,"node_number"])))
      nodes_visited_so_far <- as.numeric(which(!is.na(node_numbering[,"node_number"]) & node_numbering[,"node_onStack"] == 1))
      if(!test0 & test_POP){
        if(nodes_not_numbered_yet == 0){                                                                     # break Loop 1 (depths first exploration of successors, jumping between nodes) and move up one level
          dummy4 <- FALSE                                                                                    # break Loop 5: YOU'RE DONE
        }
        dummy0 <- FALSE                                                                                      # break Loop 1
        # if we are here, we'll restart the level counter. re-label the LEVELS used so that they do not interefere with the next component
        util_table_df_BACKUP <- util_table_df
        idx_change_level <- which(!is.na(util_table_df[,"level"]) & util_table_df[,"level"] > 0)
        util_table_df[idx_change_level,"level"] <- (-1)*util_table_df[idx_change_level,"level"]
      }
    } # end of Loop 1 (dummy0)
  } # end of Loop 0 (dummy4)


  list(n = length(Component_list),
       sizes = Component_list|> lengths())

}


#' Create list of edges for SCC2
#'
#'
#' @param x A square adjacency matrix representing the directed network.
#'
#' @return If there are at least two non-zero rows:
#' \itemize{
#'  \item{A \code{list} whose elements are \code{vector}s representing the
#'        ties between units as \code{c(starting_node, end_node)} for the
#'        function #' \code{SCC2()}. Otherwise,}
#'  \item{A \code{list} like those produced by \code{SCC2()} and \code{SCC()}.}
#' }
#'
#' Note: Isolated nodes are not returned, but the object \code{x2} is modified
#' in place to store information on their number if the matrix has at least two
#' non-null rows.
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @seealso [SCC2()]
#'
#' @keywords internal

SCC2.prep <- function(x){
  # Find isolated nodes
  pos <- list(rowSums(x), colSums(x))

  # Try to remove them
  y <- x[pos[[1]]!=0, pos[[2]]!=0]

  # If only one node left
  if(is(y, 'matrix')){
    # # Modify in place the item reporting on isolated nodes
    # assign('x2', min(sum(pos[[1]]==0), sum(pos[[2]]==0)),
    #        pos = parent.frame(n = 1))

    # Locate out-going neighbours
    y <- apply(y, 1, function(j)which(j!=0))

    # Take care of nodes with multiple or no out-going neighbours
    y <- lapply(seq_along(y), function(i){ # For each starting node
      # Create a data frame
      matrix(y[[i]], nrow = 1, byrow = TRUE)|>
        rbind(rep(i, length(y[[i]])), x2 = _)|>
        as.data.frame()
    })
    # Get list of edges
    y <- do.call(cbind, y)|> as.list()
    # Add info on isolated nodes
    attr(y, 'x2') <- min(sum(pos[[1]]==0), sum(pos[[2]]==0))
    y
  } else {
    # Exit with information of the SCC
    list(n = nrow(x), sizes = rep(1, nrow(x)))
  }
}

#' Tarjan's algorithm for finding strongly connected components in C++
#'
#' This function performs a depth-first search (DFS) on a directed graph to identify
#' strongly connected components (SCCs) and their size
#'
#' This function is a modified version of the C++ implementation of Tarjan's
#' algorithm for finding strongly connected components in directed graphs made
#' available by the webiste Geeks for Geeks (see References).
#'
#'
#' @param x A square adjacency matrix representing the directed network.
#'
#' @details The function consists of several internal steps:
#' \enumerate{
#'   \item{Node Labeling - All nodes are labeled with two-digit names for clarity in referencing.}
#'   \item{Successor List Creation - For each node, lists of direct successors are compiled.}
#'   \item{Utilization Table Setup - A table is set up for tracking exploration details such as depth and backtracking information.}
#'   \item{Main DFS Loop - The core loop where DFS occurs, including node visitation and backtracking logic to determine SCCs.}
#'   \item{Stack Management - Nodes are managed in a stack to keep track of the current path of exploration and to facilitate backtracking.}
#'   \item{SCC Identification - Upon finishing exploration of an SCC, it is identified and nodes are popped from the stack.}
#' }
#'
#' @return A list containing two elements:
#' \itemize{
#'  \item{\code{n} - number of strongly connected components}
#'  \item{\code{sizes} - size of each strongly connected component, in order of discovery}
#' }
#'
#' @references Geeks for Geeks. ‘Strongly Connected Components’. C++, 17 January 2024. \url{https://www.geeksforgeeks.org/strongly-connected-components/}.
#'
#' @author \enc{Geeks for Geeks}{GeeksForGeeks}
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @useDynLib FinNet
#' @importFrom Rcpp sourceCpp
#'
#' @family SCC finders
#'
#' @keywords internal

SCC2 <- function(x){
  x <- SCC2.prep(x) # Prepare the data

  # If the matrix had less than two non-zero rows
  if(!is.null(names(x))&&names(x)[[1]]=='n'){
    x # Then, `SCC2.prep` already listed the SCCs
  } else {
    # Run the C++ routine
    y <- .Call('_FinNet_findSCC', length(unique(unlist(x))), x)
    # Return the list of SCCs
    list(n = length(y)+attr(x, 'x2'),
         sizes = c(lengths(y), rep(1, attr(x, 'x2'))))
  }
}
