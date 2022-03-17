#' NCBI PubChem connector abstractclass.
#'
#' This is an abstract class, mother class of all NCBI PubChem connector
#' classes.
#'
#' @seealso \code{\link{NcbiEntrezConn}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::newInst()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('ncbi.pubchem.comp')
#'
#' # Get an entry
#' e <- conn$getEntry('2')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import biodb
#' @import R6
#' @import XML
#' @export
NcbiPubchemConn <- R6::R6Class("NcbiPubchemConn",
inherit=NcbiEntrezConn,

public=list(
            
#' @description
#' New instance initializer. Connector classes must not be instantiated
#' directly. Instead, you must use the createConn() method of the factory class.
#' @param db.name      PubChem database name.
#' @param id.xmltag    XML tag for ID.
#' @param entry.xmltag XML tag for entry.
#' @param id.urlfield  Database ID to use when building URL.
#' @param ... All other parameters are passed to the super class initializer.
#' @return Nothing.
initialize=function(db.name, id.xmltag, entry.xmltag, id.urlfield, ...) {

    chk::chk_string(db.name)
    chk::chk_string(id.xmltag)
    chk::chk_string(entry.xmltag)
    chk::chk_string(id.urlfield)

    # Call parent constructor
    super$initialize(...)
    abstractClass('NcbiPubchemConn', self)

    private$db.name <- db.name
    private$id.xmltag <- id.xmltag
    private$entry.xmltag <- entry.xmltag
    private$id.urlfield <- id.urlfield
}
),

private=list(
    db.name=NULL
    ,id.xmltag=NULL
    ,entry.xmltag=NULL
    ,id.urlfield=NULL

,doGetEntryPageUrl=function(id) {

    fct <- function(x) {
        u <- c(self$getPropValSlot('urls', 'base.url'), private$db.name, x)
        biodb::BiodbUrl$new(url=u)$toString()
    }

    return(vapply(id, fct, FUN.VALUE=''))
}

,doGetEntryImageUrl=function(id) {

    urls <- rep(NA_character_, length(id))

    # Loop on all IDs
    i <- 0
    for(x in id) {

        i <- i + 1

        # Set params
        params <- list()
        params[[private$id.urlfield]] <- x
        params$t <- 'l'

        # Build URL
        u <- c(self$getPropValSlot('urls', 'base.url'), 'image', 'imgsrv.fcgi')
        urls[[i]] <- biodb::BiodbUrl$new(url=u, params=params)$toString()
    }

    return(urls)
}

,doGetEntryContentFromDb=function(id) {
    re <- 'PUGREST.BadRequest|PUGREST.NotFound'
    ns <- self$getPropertyValue('xml.ns')
    return(private$retrieveContents(id=id, err.re=re, id.tag=private$id.xmltag,
        entry.tag=private$entry.xmltag, ns=ns))
}

,doGetEntryContentRequest=function(id, concatenate=TRUE) {

    if (concatenate) {
        u <- c(self$getPropValSlot('urls', 'ws2.url'), private$db.name,
            private$id.urlfield, paste(id, collapse=','), 'XML')
        url <- biodb::BiodbUrl$new(url=u)$toString()
    }
    else {
        fct <- function(x) {
            u <- c(self$getPropValSlot('urls', 'ws2.url'), private$db.name,
                private$id.urlfield, x, 'XML')
            biodb::BiodbUrl$new(url=u)$toString()
        }
        url <- vapply(id, fct, FUN.VALUE='')
    }

    return(url)
}

))
