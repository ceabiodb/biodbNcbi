#' NCBI PubChem Compound connector class.
#'
#' This is the connector class for a NCBI PubChen Compound database.
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
NcbiPubchemCompConn <- R6::R6Class("NcbiPubchemCompConn",
inherit=NcbiPubchemConn,

public=list(

#' @description
#' New instance initializer. Connector classes must not be instantiated
#' directly. Instead, you must use the createConn() method of the factory class.
#' @param ... All other parameters are passed to the super class initializer.
#' @return Nothing.
initialize=function(...) {
    super$initialize(db.name='compound', id.xmltag='PC-CompoundType_id_cid',
        entry.xmltag='PC-Compound', id.urlfield='cid', entrez.name='pccompound',
        ...)
}
),

private=list(

doSearchForEntries=function(fields=NULL, max.results=0) {

    ids <- NULL

    if ( ! is.null(fields)) {

        private$checkMassField(mass=mass, mass.field=mass.field)

        term <- character()

        # Search by name
        if ('name' %in% names(fields))
            term <- paste0('"', fields$name, '"', '[IUPACName]')

        # Search by mass
        for (mass.field in c('monoisotopic.mass', 'molecular.mass'))
            if (mass.field %in% names(fields)) {

                if (mass.field == 'monoisotopic.mass')
                    pubchem.mass.field <- 'MonoisotopicMass'
                else
                    pubchem.mass.field <- 'MolecularWeight'

                if (mass.tol.unit == 'ppm') {
                    mass.min <- mass * (1 - mass.tol * 1e-6)
                    mass.max <- mass * (1 + mass.tol * 1e-6)
                } else {
                    mass.min <- mass - mass.tol
                    mass.max <- mass + mass.tol
                }

                mass.term <- paste0(mass.min, ':', mass.max, '[',
                                    pubchem.mass.field, ']')

                if (length(term) > 0)
                    term <- paste(term, 'AND', mass.term)
                else
                    term <- mass.term
            }

        # Set retmax
        if (max.results <= 0) {
            xml <- self$wsEsearch(term=term, retmax=0, retfmt='parsed')
            retmax <- as.integer(XML::xpathSApply(xml, "/eSearchResult/Count",
                                                  XML::xmlValue))
            if (length(retmax) == 0)
                retmax <- NA_integer_
        }
        else
            retmax <- max.results

        # Send request
        ids <- self$wsEsearch(term=term, retmax=retmax, retfmt='ids')
    }

    return(ids)
}

))
