#' A (virtual) class union for matrix-like objects to be included in \code{financial_matrix}
#'
#' This is the un-exported basis for the S4 classes \code{financial_matrix}
#'
#' @name Matrix&matrix
#' @importFrom methods new
#' @importClassesFrom Matrix Matrix denseMatrix sparseMatrix generalMatrix compMatrix triangularMatrix
#' @keywords internal

setClassUnion(name = 'Matrix&matrix',
              members =  c('matrix', 'Matrix'))


#' An S4 class for the adjacency matrix produced by \code{\link{FF}} and related functions to represent the relations between firms (legal person)
#'
#' @slot M Adjacency matrix
#' @slot relation Relations represented by the matrix
#' @slot legal_form Legal form of the firms
#' @slot sector Sector in which the firms operate
#' @slot revenues Yearly revenues
#' @slot capitalisation Firms' capitalisation
#' @slot currency Currency in which the capitalisation and revenues are expressed
#'
#' @name financial_matrix
#' @importFrom methods new
#' @importClassesFrom Matrix Matrix denseMatrix sparseMatrix generalMatrix compMatrix triangularMatrix
#'
#' @keywords internal

setClass('financial_matrix', slots = c(
  M = 'Matrix&matrix',
  relation = 'character',
  legal_form = 'vector',
  sector = 'vector',
  revenues = 'vector',
  capitalisation = 'vector',
  currency = 'vector'
))
