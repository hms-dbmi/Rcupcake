#' @describeIn nmutations get the number of mutations
#' @return The number of unique mutations
setMethod( "nmutation",
    signature = "genopheno",
    definition = function( object ) {
        return( object@mutations$variable  ) 
    }
)
