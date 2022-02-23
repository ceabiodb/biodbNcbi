#' NCBI Gene connector class.
#'
#' This is the connector class for a NCBI Gene database.
#'
#' @seealso \code{\link{NcbiEntrezConn}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::newInst()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('ncbi.gene')
#'
#' # Get an entry
#' e <- conn$getEntry('2833')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import biodb
#' @import R6
#' @import XML
#' @export
NcbiGeneConn <- R6::R6Class("NcbiGeneConn",
inherit=NcbiEntrezConn,

public=list(

#' @description
#' New instance initializer.
#' @param ... All other parameters are passed to the super class initializer.
#' @return Nothing.
initialize=function(...) {
    super$initialize(entrez.name='gene', entrez.tag='Entrezgene',
        entrez.id.tag='Gene-track_geneid', ...)
}
),

private=list(

doGetEntryPageUrl=function(id) {

    fct <- function(x) {
        u <- c(self$getPropValSlot('urls', 'base.url'), private$entrez.name)
        biodb::BiodbUrl$new(url=u, params=list(term=x))$toString()
    }
    return(vapply(id, fct, FUN.VALUE=''))
}

,doSearchForEntries=function(fields=NULL, max.results=NA_integer_) {

    ids <- character()

    if ( ! is.null(fields)) {

        # Search by name
        if ('name' %in% names(fields)) {
            term <- paste0('"', fields$name, '"', '[Gene Name]')

            # Set retmax
            if (is.na(max.results)) {
                xml <- self$wsEsearch(term=term, retmax=0, retfmt='parsed')
                xpath <- "/eSearchResult/Count"
                retmax <- as.integer(XML::xpathSApply(xml, xpath, XML::xmlValue))
                if (length(retmax) == 0)
                    retmax <- NA_integer_
            }
            else
                retmax <- max.results

            # Send request
            ids <- self$wsEsearch(term=term, retmax=retmax, retfmt='ids')
        }
    }

    return(ids)
}

))
