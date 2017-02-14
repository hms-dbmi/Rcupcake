#' Obtain the raw query from a \code{genophenoComor} object.
#'
#' @name extract
#' @rdname extract-methods
#' @aliases extract, genophenoComor-methods
#' @param object of class \code{genophenoComor} or \code{genopheno} object
#' @return A \code{data.frame} containing the raw result from the initial data 
#' (genopheno)or comorbidity analysis (genophenoComor)
#' @examples
#' \dontrun{
#' #Being x an genophenoComor
#' qr <- extract(x) 
#' }
#' @export
setMethod( "extract",
   signature = "genopheno",
   definition = function( object ) {
     return( object@result )
   }
)

setMethod( "extract",
           signature = "genophenoComor",
           definition = function( object ) {
               return( object@result )
           }
)