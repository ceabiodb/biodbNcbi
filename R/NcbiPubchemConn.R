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

    biodb::logInfo("Get entry content(s) for %d id(s)...", length(id))

    URL.MAX.LENGTH <- 2048
    concatenate <- TRUE
    done <- FALSE

    while ( ! done) {

        done <- TRUE

        # Initialize return values
        content <- rep(NA_character_, length(id))

        # Get URL requests
        url.requests <- self$getEntryContentRequest(id,
            concatenate=concatenate, max.length=URL.MAX.LENGTH)

        # Loop on all URLs
        for (request in url.requests) {

            # Send request
            xmlstr <- self$getBiodb()$getRequestScheduler()$sendRequest(request)

            re <- 'PUGREST.BadRequest|PUGREST.NotFound'
            if (is.na(xmlstr) || length(grep(re, xmlstr)) > 0) {
                if (concatenate) {
                    biodb::warn("One of the IDs to retrieve is wrong.")
                    concatenate <- FALSE
                    done <- FALSE
                    break
                }
                next
            }

            # Parse XML
            xml <-  XML::xmlInternalTreeParse(xmlstr, asText=TRUE)

            # Get returned IDs
            ns <- c(pcns="http://www.ncbi.nlm.nih.gov")
            xpath <- paste0("//pcns:", private$id.xmltag)
            returned.ids <- XML::xpathSApply(xml, xpath, XML::xmlValue,
                                             namespaces=ns)

            # Store contents
            xpath <- paste0("//pcns:", private$entry.xmltag)
            nodes <- XML::getNodeSet(xml, xpath, namespaces=ns)
            c <- vapply(nodes, XML::saveXML, FUN.VALUE='')
            content[match(returned.ids, id)] <- c
        }
    }

    return(content)
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
