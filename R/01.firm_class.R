#' A firm
#'
#' An S4 class to representing a firm (legal person) as a financial entity
#'
#' This S4 class stores all information on a firm created using \link{register.firm} or \link{find.firm}.
#' The firm's property can be accessed using the relative function (see \link{query.firm})
#'
#' @slot name Name of the firm
#' @slot id Provide an ID code for the firm.
#' @slot legal_form Legal form of the firm
#' @slot sector Sector in which the firm operates
#' @slot sector_classif  Activity sector classification (if any)
#' @slot revenues Yearly revenues
#' @slot capitalisation Firm's capitalisation
#' @slot management Names of the members of the board
#' @slot ownership Names of the owner(s)
#' @slot shares Share owned by (each of) the owner(s)
#' @slot currency Currency in which the capitalisation and revenues are expressed
#'
#' @name firm
#' @importFrom methods new
#'
#' @keywords internal

setClass('firm', slots = c(
  name = 'character',
  id = 'character',
  legal_form = 'character',
  sector = 'character',
  revenues = 'numeric',
  capitalisation = 'numeric',
  management = 'vector',
  ownership = 'vector',
  shares = 'vector',
  currency = 'character'
))

#' Coerce into (a list of) \code{firm} object(s)
#'
#' Generic function to coerce other other classes into the S4 class \code{firm} representing a firm (legal person)
#'
#' @param x The object to coerce
#' @param ... Arguments passed to class-specific methods
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @returns
#' An object of class \code{firm} or a (usually named) list of them, depending on the available method for the object being coerced.
#'
#' @export

as.firm <- function(x, ...)UseMethod('as.firm')
