#' NCBI PubChem Substance connector class.
#'
#' This is the connector class for a NCBI PubChen Substance database.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::newInst()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('ncbi.pubchem.subst')
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
NcbiPubchemSubstConn <- R6::R6Class("NcbiPubchemSubstConn",
inherit=NcbiPubchemConn,

public=list(

#' @description
#' New instance initializer. Connector classes must not be instantiated
#' directly. Instead, you must use the createConn() method of the factory class.
#' @param ... All other parameters are passed to the super class initializer.
#' @return Nothing.
initialize=function(...) {
    super$initialize(db.name='substance', id.xmltag='PC-ID_id',
        entry.xmltag='PC-Substance', id.urlfield='sid',
        entrez.name='pcsubstance', ...)
}

))
