#' NCBI CCDS entry class.
#'
#' Entry class for NCBI CCDS database.
#'
#' @seealso
#' \code{\link{BiodbHtmlEntry}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::newInst()
#'
#' # Get a connector that inherits from ExpasyEnzymeConn:
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
#' @import XML
#' @export
NcbiCcdsEntry <- R6::R6Class("NcbiCcdsEntry",
inherit=biodb::BiodbHtmlEntry,

private=list(

doCheckContent=function(content) {
    xpath <- "//*[starts-with(.,'No results found for CCDS ID ')]"
    return(length(XML::getNodeSet(content, xpath)) == 0)
}

))
