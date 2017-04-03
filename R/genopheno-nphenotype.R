#' @describeIn nphenotype get the distinc phenotypes 
#' @return The number of unique phenotypes
setMethod( "nphenotype",
    signature = "genopheno",
    definition = function( object ) {
        return( object@phenotypes$variable )
    }
)
