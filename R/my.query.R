#' Query analysis to the API
#'
#' Given a vector with the fields of interest, and the paths vector generated applying
#' the getchildren function, it returns a JSON query
#'
#' @param myfields  A vector with the fields of interest
#' @param myvector  A vector with the paths of interest, generated applying the getchildren
#' function
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get an
#' on-time log from the function.
#' @return A JSON query. 
#' @examples
#' nhanesPcbs <- getchildren( 
#'               url         = "https://nhanes.hms.harvard.edu/", 
#'               apiKey      = "YOURKEY",
#'               fieldname   = "/nhanes/Demo/laboratory/laboratory/pcbs/"
#'               )
#' queryExample <- my.query( myfields = "AGE|PCB153", 
#'                          myvector = nhanesPcbs
#'               )
#' @export my.query

my.query <- function(myfields, myvector, verbose = FALSE) {


    if( verbose == TRUE){
        message(" Creating a list with the path from the vector list
                which contains all available paths for the resource")
    }

    pathList <- grep(myfields, vector, value=TRUE)


    if( verbose == TRUE){
        message(" Getting all the fields available from the pathlist")
    }

    querySELECT<- c()

    for (i in 1:length(pathList))
        {
        if( verbose == TRUE ){
            message( pathList[i] )
            message( "Get the fields for this particluar path" )
        }

        nurlstr <- paste( IRCT_PATH_RESOURCE_URL, pathList[i], sep = "" )
        nurl <- gsub( "\\#","%23", gsub("\\?", "%3F", gsub("[)]","%29", gsub("[(]","%28", URLencode(nurlstr)))))

        pathFields <- httr::content(httr::GET(nurl))

        if( verbose == TRUE ){
            message( "Generating {field} object for SELECT portion of the query" )
        }

        for (j in 1:length(pathFields)) {
            message(pathFields[j])

            myField <- list(
                list(
                    field=list(
                        pui = unbox(pathFields[[j]]$pui),
                        dataType = unbox(pathFields[[j]]$dataType$name)
                    ),
                    alias=unbox(
                        pathFields[[j]]$displayName
                    )
                )
            )
            querySELECT <- c( querySELECT, ( myField ) )
        }
    }

    if( verbose == TRUE ){
        message( "Generating the WHERE portion of the query, from the first path selected" )
    }


    queryWHERE <- c()
    # Assuming STRING variable, but not sure
    mylist      <- list( pui      = unbox( pathList[1] ),
                         datatype = unbox( "STRING" ) )

    queryWHERE  <- list( field     = mylist,
                         predicate = unbox( "CONTAINS" ),
                         fields    = list( ENCOUNTER = unbox( "NO" )))

    querySTRING <- list( select = querySELECT,
                         where  = list( queryWHERE ) )

    return( toJSON ( querySTRING, pretty=TRUE ) )
}