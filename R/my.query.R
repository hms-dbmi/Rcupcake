#' Query analysis to the API
#'
#' Given a vector with the fields of interest, and the vector generated with the paths obtained 
#' after applying the getchildren function, it returns a JSON query
#'
#' @param myfields  A vector with the fields of interest
#' @param myvector  A vector with the paths of interest, generated applying the \code{getchildren}
#' function
#' @param url  The url.
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get an on-time log from the function.
#' @return A JSON query. 
#' @examples
#' nhanesPcbs <- getchildren( 
#'               url         = "https://nhanes.hms.harvard.edu/", 
#'               apiKey      = "YOURKEY",
#'               fieldname   = "/nhanes/Demo/laboratory/laboratory/pcbs/"
#'               )
#' queryExample <- my.query( myfields = "AGE|PCB153", 
#'                          myvector  = nhanesPcbs, 
#'                          url       = "https://nhanes.hms.harvard.edu/"
#'               )
#' @export my.query

my.query <- function(myfields, myvector, url, verbose = FALSE) {


    if( verbose == TRUE){
        message(" Creating a list with the path from the vector list
                which contains all available paths for the resource")
    }

    pathList <- grep(myfields, myvector, value=TRUE)


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
        
        IRCT_REST_BASE_URL <- url
        IRCT_CL_SERVICE_URL <- paste(IRCT_REST_BASE_URL,"rest/v1/",sep="")
        IRCT_RESOURCE_BASE_URL <- paste(IRCT_CL_SERVICE_URL,"resourceService/",sep="")
        IRCT_PATH_RESOURCE_URL <- paste(IRCT_RESOURCE_BASE_URL,"path",sep="")
        
                
        nurlstr <- paste( IRCT_PATH_RESOURCE_URL, pathList[i], sep = "" )
        nurl <- gsub( "\\#","%23", gsub("\\?", "%3F", gsub("[)]","%29", gsub("[(]","%28", URLencode(nurlstr)))))

        pathFields <- httr::content(httr::GET(nurl))

        if( verbose == TRUE ){
            message( "Generating {field} object for SELECT portion of the query" )
        }
        
        if (length(pathFields)>0) {

        for (j in 1:length(pathFields)) {
            message(pathFields[j])

            myField <- list(
                list(
                    field=list(
                        pui = jsonlite::unbox(pathFields[[j]]$pui),
                        dataType = jsonlite::unbox(pathFields[[j]]$dataType$name)
                    ),
                    alias=jsonlite::unbox(
                        pathFields[[j]]$displayName
                    )
                )
            )
            querySELECT <- c( querySELECT, ( myField ) )
        }
        }
    }

    if( verbose == TRUE ){
        message( "Generating the WHERE portion of the query, from the first path selected" )
    }


    queryWHERE <- c()
    # Assuming STRING variable, but not sure
    mylist      <- list( pui      = jsonlite::unbox( pathList[1] ),
                         datatype = jsonlite::unbox( "STRING" ) )

    queryWHERE  <- list( field     = mylist,
                         predicate = jsonlite::unbox( "CONTAINS" ),
                         fields    = list( ENCOUNTER = jsonlite::unbox( "NO" )))

    querySTRING <- list( select = querySELECT,
                         where  = list( queryWHERE ) )

    return( toJSON ( querySTRING, pretty=TRUE ) )
}
