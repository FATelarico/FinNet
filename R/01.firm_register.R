#' Function to create a \code{firm} (legal person)
#'
#' @param name Name of the firm
#' @param id Provide an ID code for the firm. Defaults to \code{NA}
#' @param legal_form Legal form of the firm (e.g., LLP, Inc, GmbH, Private, etc.)
#' @param sector Sector in which the firm operates (its values depend on the value of \code{sector_classif})
#' @param sector_classif Which standard sector classification (if any) to be used. Possible values are
#'   - \code{NACE} for the Statistical Classification of Economic Activities in the European Community or 'Nomenclature statistique des Activités économiques dans la Communauté Européenne', revision 2;
#'   - \code{NA} for a custom classification (default if anything is provided);
#'   - \code{NULL} for no classification (default if nothing is provided).
#' @param revenues Yearly revenues
#' @param capitalisation Firm's capitalisation
#' @param management Names of the members of the board
#' @param ownership Names of the owner(s)
#' @param shares Share owned by (each of) the owner(s)
#' @param currency Currency in which the capitalisation and revenues are expressed (defaults to `USD`)
#'
#' @return An object of the S4 class \code{firm} containing several fields, only the first one of which is mandatory:
#' \item{name}{Name of the firm}
#' \item{id}{ID of the firm, usually the ticker}
#' \item{legal_form}{Legal form of the firm}
#' \item{sector}{Sector in which the firm operates}
#' \item{revenues}{Yearly revenues}
#' \item{capitalisation}{Capitalisation}
#' \item{management}{Members of the board}
#' \item{ownership}{Owner(s)}
#' \item{shares}{Share owned by (each of) the owner(s)}
#' \item{currency}{Currency}
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @examples
#'
#' # Registering Apple manually
#' AAPL <- register.firm(name = 'Apple', id = 'AAPL', legal_form = 'GmbH',
#'                       revenues = 81665400000, capitalisation = 2755039000000,
#'                       management = my_vector <- c("Timothy D. Cook",
#'                                                   "Luca Maestri",
#'                                                   "Jeffrey E. Williams",
#'                                                   "Katherine L. Adams",
#'                                                   "Deirdre O'Brien",
#'                                                   "Chris Kondo",
#'                                                   "James Wilson",
#'                                                   "Mary Demby",
#'                                                   "Nancy Paxton",
#'                                                   "Greg Joswiak"),
#'                       ownership = c('Vanguard Total Stock Market Index Fund',
#'                       'Vanguard 500 Index Fund',
#'                       'Fidelity 500 Index Fund',
#'                       'SPDR S&P 500 ETF Trust',
#'                       'iShares Core S&P 500 ETF',
#'                       'Invesco ETF Tr-Invesco QQQ Tr, Series 1 ETF',
#'                       'Vanguard Growth Index Fund',
#'                       'Vanguard Institutional Index Fund-Institutional Index Fund',
#'                       'Vanguard Information Technology Index Fund',
#'                       'Select Sector SPDR Fund-Technology'),
#'                       shares = c(0.0290, 0.0218, 0.0104, 0.0102, 0.0084,
#'                                  0.0082, 0.0081, 0.0066, 0.0043, 0.0039),
#'                       currency = 'USD')
#'
#' # Registering a coal-mining company indicating the sector using `NACE` codes, without ID
#' set.seed(123456789)
#' firm_coalmining <- register.firm(
#'   name = 'A coal-mining firm',
#'   legal_form = 'Private',
#'   sector = 'B.05',
#'   sector_classif = 'NACE'
#' )
#'
#' # Getting creative: Register a firm with coded owners and managers
#' set.seed(123456789)
#' firm_coded <- register.firm(
#'   name = 'Coded firm',
#'   revenues = sample(seq(1:100)/10, 1)*10^sample(1:5, 1),
#'   capitalisation = sample(seq(1:100)/10, 1)*10^sample(2:7, 1),
#'   management = c('Board Member', 'CEO', 'CTO', 'Activist investor'),
#'   ownership = c('State', 'Foreign investors'),
#'   shares = c(51, 49),
#'   currency = 'EUR'
#' )
#'
#' @seealso \link{find.firm}
#'
#' @export

register.firm <- function(name, id = NA, legal_form = NA,
                          sector = NA,
                          sector_classif = NULL,
                          revenues = NA, capitalisation = NA,
                          management = NA, ownership = NA, shares = NA,
                          currency = NA){

  if(!is.null(sector_classif)&&sector_classif=='NACE'){
    if(!sector%in%NACE$Code)stop('The provided sector description (`',
                                 sector, '`) is not a valid NACE rev. 2 code')
  }

  methods::new('firm', name = name,
               id = as.character(id),
               legal_form = as.character(legal_form),
               sector = as.character(sector),
               revenues = as.numeric(revenues),
               capitalisation = as.numeric(capitalisation),
               management = ifelse(is.na(management), c(''), management),
               ownership = ifelse(is.na(ownership), c(''), ownership),
               shares = ifelse(is.na(shares), c(''), shares),
               currency = as.character(currency))
}

#' Function to create a \code{firm} (legal person) using data from 'Yahoo! Finance'
#'
#' Tickers can be retrieved from [Yahoo! Finance](https://finance.yahoo.com/lookup/).
#' This function requires the package \code{yahoofinancer} to be installed. It is available from the CRAN by running \code{install.packages('yahoofinancer')}.
#'
#' @param ticker Firm's ticker.
#' @param name Provide the firm's name. If not provided, \code{NA}, or \code{NULL}, will default to the string provided as \code{ticker}.
#' @param ticker_is_id Should the ticker be used as the firm's id?
#' @param legal_form The firm's legal form of the firm. Possible values:
#' - a string (e.g., 'LLC', 'Private', 'GmbH', etc.);
#' - \code{NULL} (default), in which case the function will set \code{legal_form} to 'JSC'; or
#' - \code{NA} to specify no legal form.
#' @param sector_granularity Sector in which the firm operates. Possible values:
#' - \code{0}, \code{NULL}, or \code{NA} to omit the sector;
#' - \code{1} or \code{'generic'} (default) for a generic description (e.g., 'Consumer Technology', 'Consumer Cyclical', 'Consumer Defensive');
#' - \code{2} or \code{'specifc'} for a more granular description (e.g., 'Technology', 'Auto Manufacturers', 'Tobacco').
#' @param managers_remove_salutation_title Yahoo! Finance provide salutation titles before the names of the managers. If this is \code{TRUE} (default), they will be omitted.
#' @param managers_only_surname Yahoo! Finance provide first, middle, and last name of the managers. If this is \code{TRUE} (not recommended for large data sets), only the surname is returned.
#'
#' @return An object of the S4 class \code{firm} containing several fields, only the first one of which is mandatory:
#' \item{name}{Name of the firm (or ticker if no name was provided)}
#' \item{id}{Firm' ticker (if ticker_is_id was `TRUE`) or nothing (otherwise)}
#' \item{legal_form}{Legal form of the firm (may be null)}
#' \item{sector}{Sector in which the firm operates (may be null)}
#' \item{revenues}{Yearly revenues}
#' \item{capitalisation}{Capitalisation}
#' \item{management}{Members of the board}
#' \item{ownership}{Owner(s)}
#' \item{shares}{Share owned by (each of) the owner(s)}
#' \item{currency}{Currency}
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @examples
#' # Registering Apple automatically
#' #| Results are subject to the correct functioning of the package `yahoofinancer`
#' #| and of the Yahoo! Finance API
#  AAPL <- find.firm(ticker = 'AAPL', name = 'Apple', legal_form = 'Inc')
#'
#' @seealso \link{register.firm} \link{find.firms}
#'
#' @export


find.firm <- function(ticker, name = NULL, ticker_is_id = TRUE,
                      legal_form = NULL,
                      sector_granularity = 1,
                      managers_remove_salutation_title = TRUE,
                      managers_only_surname = FALSE){

  if(!requireNamespace("yahoofinancer", quietly = TRUE)){
    stop('This function requires `yahoofinancer`, check the \'Details\' section of its help file')
  }


  if(!sector_granularity%in%c(0, NA, NULL, 1, 'general', 2, 'specific')){
    stop('The parameter `sector_granularity` cannot be', sector_granularity)
  }

  x <- yahoofinancer::Ticker$new(ticker)
  tryCatch(register.firm(
    name = ifelse(is.null(name), ticker, name),
    id = ifelse(ticker_is_id, ticker, as.character(NA)),
    sector = ifelse(sector_granularity %in% c(0, NA, NULL), NA,
                    ifelse(sector_granularity %in% c(1, 'generic'), x$summary_profile$sector,
                           x$summary_profile$industry)),
    legal_form = ifelse(is.null(legal_form), 'JSC', legal_form),
    capitalisation = x$quote$marketCap,
    revenues = {
      rvns <- x$earnings_trend$revenue_estimate$avg_estimate
      rvns[!is.na(rvns)][1]
    },
    ownership = x$fund_ownership$organization,
    management = {
      mngmnt <- x$company_officers$name
      if(managers_only_surname){
        lapply(mngmnt, strsplit, ' ')|> lapply(function(x){
          x <- unlist(x)
          x[length(x)]
        })|> unlist(x)
      } else if(managers_remove_salutation_title){
        gsub('Mr. ', '', mngmnt)|>
          gsub('Ms. ', '', x = _)|> gsub('Mrs. ', '', x = _)
      }
    },
    shares = x$fund_ownership$percent_held,
    currency = ifelse(any(x$financial_data$financialCurrency%in%c(NULL, NA)),
                      x$price$currency,
                      x$financial_data$financialCurrency)

  ), error = function(e)NA)

}

#' Function to create mutiple \code{firms} (legal persons) using data from 'Yahoo! Finance'
#'
#' If \code{legal_form} is a vector containing:
#' - one or more \code{NULL} elements, the corresponding \code{firm}'s legal form will be \code{JSC};
#' - one or more \code{NA}s, the corresponding \code{firm}'s legal form will be \code{NA}.
#'
#' To ensure consistency, \code{ticker_is_id}, \code{sector_granularity}, \code{managers_remove_salutation_title}, and \code{managers_only_surname} cannot be vectors.
#'
#' Tickers can be retrieved from [Yahoo! Finance](https://finance.yahoo.com/lookup/).
#' This function requires the package \code{yahoofinancer} to be installed. It is available from the CRAN by running \code{install.packages('yahoofinancer')}.
#'
#'
#' @param tickers The firms' ticker.
#' @param name Provide the firms' names as a vector of the same length as tickers. If not provided, \code{NA}, or \code{NULL}, will default to the firm's \code{ticker}.
#' @param ticker_is_id Should the ticker be used as the firm's id?
#' @param legal_form The firm's legal form of the firm. Possible values:
#' - a vector of strings (e.g., 'LLC', 'Private', 'GmbH', etc.) of the same length as \code{tickers} (see 'Details' for the interpretation of \code{NA}s and \code{NULL}s);
#' - \code{NULL} (default), in which case the function will set \code{legal_form} to 'JSC' for all firms; or
#' - \code{NA} to specify no legal form.
#' @param sector_granularity Sector in which the firm operates. Possible values:
#' - \code{0}, \code{NULL}, or \code{NA} to omit the sector;
#' - \code{1} or \code{'generic'} (default) for a generic description (e.g., 'Consumer Technology', 'Consumer Cyclical', 'Consumer Defensive');
#' - \code{2} or \code{'specifc'} for a more granular description (e.g., 'Technology', 'Auto Manufacturers', 'Tobacco').
#' @param managers_remove_salutation_title Yahoo! Finance provide salutation titles before the names of the managers. If this is \code{TRUE} (default), they will be omitted.
#' @param managers_only_surname Yahoo! Finance provide first, middle, and last name of the managers. If this is \code{TRUE} (not recommended for large data sets), only the surname is returned.
#'
#' @return An object of the S4 class \code{firm} containing several fields, only the first one of which is mandatory:
#' \item{name}{Name of the firm (or ticker if no name was provided)}
#' \item{id}{Firm' ticker (if ticker_is_id was `TRUE`) or nothing (otherwise)}
#' \item{legal_form}{Legal form of the firm (may be null)}
#' \item{sector}{Sector in which the firm operates (may be null)}
#' \item{revenues}{Yearly revenues}
#' \item{capitalisation}{Capitalisation}
#' \item{management}{Members of the board}
#' \item{ownership}{Owner(s)}
#' \item{shares}{Share owned by (each of) the owner(s)}
#' \item{currency}{Currency}
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @examples
#' # Registering Apple, General Motors, and British American Tobacco automatically
#' #| Results are subject to the correct functioning of the package `yahoofinancer`
#' #| and of the Yahoo! Finance API
#  firms_US <- find.firms(tickers = c('AAPL', 'GM', 'BTI'),
#                         name = c('Apple', 'General Motors', 'British American Tobacco'),
#                         legal_form = 'Inc')
#'
#' @seealso \link{find.firm}
#'
#' @export


find.firms <- function(tickers, name = NULL, ticker_is_id = TRUE,
                       legal_form = NULL,
                       sector_granularity = 1,
                       managers_remove_salutation_title = TRUE,
                       managers_only_surname = FALSE){

  if(!requireNamespace("yahoofinancer", quietly = TRUE)){
    stop('This function requires `yahoofinancer`, check the \'Details\' section of its help file')
  }


  if(!is.null.na(sector_granularity)&&length(sector_granularity)>1){
    warning(paste('More than one value provided for `sector_granularity`',
                  'only the first element will be evaluated'))
    sector_granularity <- unlist(sector_granularity[1])[1]
  }

  if(!is.null.na(ticker_is_id)&&length(ticker_is_id)>1){
    warning(paste('More than one value provided for `ticker_is_id`',
                  'only the first element will be evaluated'))
    ticker_is_id <- unlist(ticker_is_id[1])[1]
  }

  if(!is.null.na(managers_remove_salutation_title)&&
     length(managers_remove_salutation_title)>1){
    warning(paste('More than one value provided for',
                  '`managers_remove_salutation_title`',
                  'only the first element will be evaluated'))
    managers_remove_salutation_title <-
      unlist(managers_remove_salutation_title[1])[1]
  }

  if(!is.null.na(managers_only_surname)&&length(managers_only_surname)>1){
    warning(paste('More than one value provided for `managers_only_surname`',
                  'only the first element will be evaluated'))
    managers_only_surname <-  unlist(managers_only_surname[1])[1]
  }

  if(!sector_granularity%in%c(0, NA, NULL, 1, 'general', 2, 'specific')){
    stop(paste('The parameter `sector_granularity` cannot be:',
               sector_granularity))
  }

  if(is.null.na(name)){
    name <- tickers
  } else if(length(name)!=length(tickers)){
    stop('If provided, `name` must be of the same length of `tickers`!')
  }

  if(requireNamespace("SPB", quietly = TRUE)){
    pb <- SPB::create_pb(length = length(tickers), colour = 'purple',
                         trim = 7L, print = FALSE)
  }

  out <- lapply(seq_along(tickers), function(i){
    ticker <- tickers[[i]]

    if(requireNamespace("SPB", quietly = TRUE)){
      pb <- SPB::update_pb(pb, i)
    } else {
      cat('Registering:', ticker,'\r')
      cat(rep(' ', 50), '\r')
    }


    if(yahoofinancer::validate(ticker)){
      x <- yahoofinancer::Ticker$new(ticker)
    } else {
      stop(call. = FALSE, paste('Not a valid ticker:', ticker))
    }

    tryCatch(register.firm(
      name = ifelse(is.null(name[i]), ticker, name[i]),
      id = ifelse(ticker_is_id, ticker, as.character(NA)),
      sector = ifelse(sector_granularity %in% c(0, NA, NULL), NA,
                      ifelse(sector_granularity %in% c(1, 'generic'), x$summary_profile$sector,
                             x$summary_profile$industry)),
      legal_form = ifelse(is.null(legal_form), 'JSC',
                          ifelse(is.null(legal_form[i]),
                                 'JSC',
                                 legal_form[i])),
      capitalisation = x$quote$marketCap,
      revenues = {
        rvns <- x$earnings_trend$revenue_estimate$avg_estimate
        rvns[!is.na(rvns)][1]
      },
      ownership = x$fund_ownership$organization,
      management = {
        mngmnt <- x$company_officers$name
        if(managers_only_surname){
          lapply(mngmnt, strsplit, ' ')|> lapply(function(x){
            x <- unlist(x)
            x[length(x)]
          })|> unlist(x)
        } else if(managers_remove_salutation_title){
          gsub('Mr. ', '', mngmnt)|>
            gsub('Ms. ', '', x = _)|> gsub('Mrs. ', '', x = _)
        }
      },
      shares = x$fund_ownership$percent_held,
      currency = ifelse(any(x$financial_data$financialCurrency%in%c(NULL, NA)),
                        x$price$currency,
                        x$financial_data$financialCurrency)

    ), error = function(e)NA)
  })

  if(!requireNamespace("SPB", quietly = TRUE)){
    message('For better progress bars run: `install.packages(\'SPB\')`!')
  }

  names(out) <- tickers

  out
}
