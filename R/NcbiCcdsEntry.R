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
#' # Get a connector for CCDS database:
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
    return(length(grep('No results found for CCDS ID ', content, fixed=TRUE))
        == 0)
}

))
