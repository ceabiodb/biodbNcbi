#' NCBI PubChem Substance entry class.
#'
#' This is the entry class for a NCBI PubChen Substance database.
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
#' @export
NcbiPubchemSubstEntry <- R6::R6Class("NcbiPubchemSubstEntry",
inherit=NcbiPubchemEntry)
