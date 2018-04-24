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
#' queryExample <- my.query( myfields = "AGE|PCB153",
#                          myvector  = c(nhanesPcbs, nhanesDemog),
#                          url       = "https://nhanes.hms.harvard.edu/"
#               )
#' @export my.query

my.query <- function(myfields, myvector, url, verbose = FALSE) {


    if( verbose == TRUE){
        message(" Creating a list with the path from the vector list
                which contains all available paths for the resource")
    }

    # Filter fields by values in vector using grep

    if( substr( myfields, nchar(myfields), nchar(myfields)) == "|" ){
        myfields <- substr( myfields, 1, nchar(myfields)-1)
    }
    
    myfields <- gsub("([.()\\^{}+$*?]|\\[|\\])", "\\\\\\1", myfields)
    pathList <- grep(myfields, myvector, value=TRUE)


    if(verbose) message(" Getting all the fields available from the pathlist")
    
    # Create vector for query
    querySELECT<- c()

    # for each entry in filtered path list:
    #     create a field entry in the query
    for (i in 1:length(pathList)){
        if( verbose ) message( "Get the fields for this particluar path" )
        
        
        IRCT_CL_SERVICE_URL <- "rest/v1/"
        IRCT_RESOURCE_BASE_URL <- paste(IRCT_CL_SERVICE_URL,"resourceService/",sep="")
        IRCT_PATH_RESOURCE_URL <- paste(IRCT_RESOURCE_BASE_URL,"path",sep="")
        
        # format URL for current path entry to retrieve fields       
        nurlstr <- paste( IRCT_PATH_RESOURCE_URL, pathList[i], sep = "" )
        # nurl <- gsub( "\\#","%23", gsub("\\?", "%3F", gsub("[)]","%29", gsub("\\*","%2A", gsub("\\^","%5E", gsub("[(]","%28", URLencode(nurlstr)))))))

        # httr::content(httr::GET(nurl))
        
        # get children of current path entry
        # pathFields <- httr::content(httr::GET(nurl))
        pathFields <- send.request(url = url, path = nurlstr)
        # message(pathFields)
        
        if( verbose == TRUE ){
            message( "Generating {field} object for SELECT portion of the query" )
        }
        
        # if there were children, add a field entry to the query for each child path
        if (length(pathFields)>0) {
            for (j in 1:length(pathFields)) {
                # message("pathFields[j] : ", pathFields[j])
    
                myField <- list(
                    list(
                        field=list(
                            pui = jsonlite::unbox(pathFields[[j]]$pui),
                            dataType = jsonlite::unbox(pathFields[[j]]$dataType$name)
                        ),
                        alias=jsonlite::unbox(
                            #pathFields[[j]]$displayName
                            pathList[i]
                        )
                    )
                )
                querySELECT <- c( querySELECT, ( myField ) )
            }
        } else {
            
            pathSegs <- unlist(strsplit(pathList[[i]],"/"))
            leafnurlstr <- stringr::str_c( pathSegs[1:length(pathSegs)-1] , collapse = "/" )
            # message("leafnurlstr")
            # message(leafnurlstr)
            # leafnurl <- gsub( "\\#","%23", gsub("\\?", "%3F", gsub("[)]","%29", gsub("\\*","%2A", gsub(" ","%20", gsub("\\^","%5E", gsub("[(]","%28", URLencode(leafnurlstr))))))))

            leafPathUrl <- concatPath(c(IRCT_PATH_RESOURCE_URL, leafnurlstr))

            # leafPathFields <- httr::content(httr::GET(leafPathUrl))
            leafPathFields <- send.request(url = url,
                                           path = leafPathUrl,
                                           verbose = verbose)
            print("leafPathFields")
            print(leafPathFields)
            
            leafPath <- grep(pathList[[i]], leafPathFields, fixed = TRUE, value=TRUE)
            ## message("leafPath")
            ## message(leafPath)

            
            # get children of current path entry
            
            # pathFields <- httr::content(httr::GET(nurl))
            pathFields <- send.request(url = url,
                                       path = nurlstr)
            ## message("pathFields")
            ## message(pathFields)
            
            entry <- NULL
            for(index in 1:length(leafPathFields)){
                ## print(paste("is", leafPathFields[[index]]$pui, "equal to", pathList[[i]]))
                if(comparePath(leafPathFields[[index]]$pui,
                               pathList[[i]])){
                    entry <- leafPathFields[[index]]
                }
            }
            print("entry")
            print(entry)
            # message(entry)
            myField <- list(
                list(
                    field=list(
                        pui = jsonlite::unbox(entry$pui),
                        dataType = jsonlite::unbox(entry$dataType$name)
                    ),
                    alias=jsonlite::unbox(
                        entry$pui
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
    field <- unlist(strsplit(myfields, "[|]"))[1]
    field <- gsub("([.()\\^{}+$*?]|\\[|\\])", "\\\\\\1", field)
    
    whereClause <- grep( field, pathList, value=TRUE)[[1]]
    
    
    # Assuming STRING variable, but not sure
    mylist      <- list( pui      = jsonlite::unbox( whereClause ),
                         dataType = jsonlite::unbox( "STRING" ) )

    queryWHERE  <- list( field     = mylist,
                         predicate = jsonlite::unbox( "CONTAINS" ),
                         fields    = list( ENOUNTER = jsonlite::unbox( "NO" )))

    querySTRING <- list( select = querySELECT,
                         where  = list( queryWHERE ) )

    return( jsonlite::toJSON ( querySTRING, pretty=TRUE ) )
}
