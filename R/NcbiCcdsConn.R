#' NCBI CCDS connector class.
#'
#' Connector class for NCBI CCDS database.
#'
#' This is a concrete connector class. It must never be instantiated directly,
#' but instead be instantiated through the factory \code{\link{BiodbFactory}}.
#' Only specific methods are described here. See super classes for the
#' description of inherited methods.
#'
#' @seealso \code{\link{BiodbConn}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::newInst()
#'
#' # Get a connector:
#' conn <- mybiodb$getFactory()$createConn('ncbi.ccds')
#'
#' # Get the first entry
#' e <- conn$getEntry('CCDS12227.1')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import biodb
#' @import R6
#' @export
NcbiCcdsConn <- R6::R6Class("NcbiCcdsConn",
inherit=biodb::BiodbConn,

private=list(

,doGetEntryPageUrl=function(id) {

    fct <- function(x) {
        u <- c(self$getPropValSlot('urls', 'base.url'), 'CcdsBrowse.cgi')
        p <- list(REQUEST='CCDS', GO='MainBrowse', DATA=x)
        biodb::BiodbUrl$new(url=u, params=p)$toString()
    }
    return(vapply(id, fct, FUN.VALUE=''))
}

,doGetEntryContentRequest=function(id, concatenate=TRUE) {
    return(self$getEntryPageUrl(id))
}

,doGetEntryIds=function(max.results=NA_integer_) {
    return(NULL)
}

))
