#' @describeIn n.variations get the number of variations
#' @return The number of unique variations
setMethod( "n.variation",
    signature = "cupcakeData",
    definition = function( object ) {
        return( object@variations$variable  ) 
    }
)
