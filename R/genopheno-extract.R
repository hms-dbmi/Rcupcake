#' @describeIn Extract the content of genopheno and genophenoComor objects
#' @return The data.frame
setMethod( "extract",
   signature = "genopheno",
   definition = function( object ) {
     return( object@iresult )
   }
)

setMethod( "extract",
           signature = "genophenoComor",
           definition = function( object ) {
               return( object@result )
           }
)
