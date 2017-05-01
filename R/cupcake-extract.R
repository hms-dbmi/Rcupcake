#' @describeIn Extract the content of cupcakeData and cupcakeResults objects
#' @return The data.frame
setMethod( "extract",
   signature = "cupcakeData",
   definition = function( object ) {
     return( object@iresult )
   }
)

setMethod( "extract",
           signature = "cupcakeResults",
           definition = function( object ) {
               return( object@result )
           }
)
