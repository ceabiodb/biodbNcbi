#' NCBI PubChem entry class.
#'
#' This the abstract entry class for all NCBI PubChem entry classes.
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
NcbiPubchemEntry <- R6::R6Class("NcbiPubchemEntry",
inherit=biodb::BiodbXmlEntry,

public=list(

#' @description
#' New instance initializer.
#' @param ... All other parameters are passed to the super class initializer.
#' @return Nothing.
initialize=function(...) {
    super$initialize(...)
    abstractClass('NcbiPubchemEntry', self)
}
),

private=list(

doCheckContent=function(content) {
    return(length(grep('<Fault', content, fixed=TRUE)) == 0)
}

))
