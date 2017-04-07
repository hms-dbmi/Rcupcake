#' @describeIn nphenotype get the distinct phenotypes 
#' @return The number of unique phenotypes
setMethod( "nphenotype",
    signature = "genopheno",
    definition = function( object ) {
        return( object@phenotypes$variable )
    }
)
