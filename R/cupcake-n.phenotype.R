#' @describeIn n.phenotype get the distinct phenotypes 
#' @return The number of unique phenotypes
setMethod( "n.phenotype",
    signature = "cupcakeData",
    definition = function( object ) {
        return( object@phenotypes$variable )
    }
)
