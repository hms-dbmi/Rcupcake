#' Obtain the data from a \code{comorbidity}, \code{molecularComorbidity}, 
#' \code{cAnalysis} or \code{molecularcAnalysis} object.
#'
#' @name extract
#' @rdname extract-methods
#' @aliases extract, comoRbidity-methods
#' @param object Object of class \code{comorbidity}, \code{molecularComorbidity}, 
#' \code{cAnalysis} or \code{molecularcAnalysis} object.
#' @return A \code{data.frame} containing the raw result
#' @examples
#' \dontrun{
#' #Being x an comoRbidity
#' qr <- extract(x) 
#' }
#' @export
# setMethod( "extract",
#    signature = "comoRbidity",
#    definition = function( object ) {
#      return( object@qresult )
#    }
# )
