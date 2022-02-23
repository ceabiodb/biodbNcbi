#' NCBI PubChem Compound entry class.
#'
#' This is the entry class for a NCBI PubChen Compound database.
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
#' @export
NcbiPubchemCompEntry <- R6::R6Class("NcbiPubchemCompEntry",
inhderit=NcbiPubchemEntry,

private=list(

,doParseFieldsStep2=function(parsed.content) {

    # Set names
    names <- character()
    fields <- c('COMP.IUPAC.NAME.PREF', 'COMP.IUPAC.NAME.ALLOWED',
                'COMP.IUPAC.NAME.TRAD', 'COMP.IUPAC.NAME.SYST',
                'COMP.IUPAC.NAME.CAS')
    for (f in fields)
        if (self$hasField(f))
            names <- c(names, self$getFieldValue(f, compute=FALSE))
    if (length(names) > 0)
        self$setFieldValue('name', names)
}

))
