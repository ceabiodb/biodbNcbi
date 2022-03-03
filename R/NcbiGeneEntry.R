#' NCBI Gene entry class.
#'
#' This is the entry class for a NCBI Gene database.
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
NcbiGeneEntry <- R6::R6Class("NcbiGeneEntry",
inherit=biodb::BiodbXmlEntry,

private=list(

doCheckContent=function(content) {
    return(length(grep('<Error', content, fixed=TRUE)) ==0
        && length(grep('<ERROR', content, fixed=TRUE)) ==0)
}

,doParseFieldsStep2=function(parsed.content) {

    # CCDS ID
    ccdsid <- private$findCcdsId(parsed.content)
    if ( ! is.na(ccdsid))
        self$setFieldValue('ncbi.ccds.id', ccdsid)
}

,findCcdsId=function(parsed.content) {

    # 1) Get all CCDS tags.
    xpath <- "//Dbtag_db[text()='CCDS']/..//Object-id_str"
    ccds_elements <- XML::getNodeSet(parsed.content, xpath)

    # 2) If all CCDS are the same, go to point 3.
    ccds <- NA_character_
    for (e in ccds_elements) {
        current_ccds <- XML::xmlValue(e)
        if (is.na(ccds))
            ccds <- current_ccds
        else {
            if (current_ccds != ccds) {
                ccds <- NA_character_
                break
            }
        }
    }

    # 3) There are several CCDS values, we need to find the best one (i.e.: the
    # most current one).
    if (is.na(ccds)) {
        # For each CCDS, look for the parent Gene-commentary tag. Then look for
        # the text content of the Gene-commentary_label which is situed under.
        # Ignore CCDS that have no Gene-commentary_label associated. Choose the
        # CCDS that has the smallest Gene-commentary_label in alphabetical
        # order.
        version <- NA_character_
        for (e in ccds_elements) {
            xpath <- "ancestor::Gene-commentary/Gene-commentary_label"
            versions <- XML::xpathSApply(e, xpath, XML::xmlValue)
            if (length(versions) < 1) next
            current_version <- versions[[length(versions)]]
            if (is.na(version) || current_version < version) {
                version <- current_version
                ccds <- XML::xmlValue(e)
            }
        }
    }

    return(ccds)
}

))
