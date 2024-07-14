#' Extract all the unique people associated to at least one of the provided \code{firm} objects
#'
#' @param ... Either multiple objects of class \code{firm} or a list of such objects
#' @param who Whether to extract the 'managers' or the 'owners' (minimum unambiguous string)
#' @param sorting Whether to sort the people by alphabetical order. Defaults to \code{TRUE}
#'
#'
#' @return A vector containing the names of the individuals looked up. If
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @examples
#' # Find all the shareholders in companies that Berkshire Hathaway holds
#' data('firms_BKB')
#' shareholders <- find.people(firms_BKB, who = 'own')
#'
#' # Find all those managing the companies that Berkshire Hathaway holds
#' data('firms_BKB')
#' managers <- find.people(firms_BKB, who = 'man')
#'
#' @export


find.people <- function(...,
                        who = c('managers', 'owners', 'both', 'all'),
                        sorting = TRUE){

  if(...length()==1){
    firms <- ...elt(1)
    names(firms) <- names(...elt(1))
  } else {
    firms <- listing(...)
    names(firms) <-
      as.character(match.call(expand.dots = TRUE))[-1][1:length(firms)]
  }

  if(!who %in% c('management', 'ownership')){
    who <-
      (match.arg(arg = who, choices = c('managers', 'owners'))=='managers')|>
      ifelse(test =  _, yes = 'management', no = 'ownership')
  }
  out <- query.firms(firms, which = who)|> unlist()|> unique()
  if(sorting) out <- sort(out)

  out
}
