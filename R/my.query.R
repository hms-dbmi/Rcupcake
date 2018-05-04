#' Query analysis to the API
#'
#' Given a vector with the fields of interest, and the vector generated with the paths obtained 
#' after applying the getchildren function, it returns a JSON query
#'
#' @param myfields  A vector with the fields of interest
#' @param myvector  A vector with the paths of interest, generated applying the \code{getchildren}
#' function
#' @param url  The url.
#' @param enounter.misspell By default TRUE. If user retrieve the next error: "{"errorType":
#' "application_error","message":"Could not convert JSON. Field 'By Encounter' is mandatory."}" 
#' in the \code{my.data} function, change the argument to FALSE. 
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get an on-time log from the function.
#' @return A JSON query. 
#' @examples
#' queryExample <- my.query( myfields = "AGE|PCB153",
#                          myvector  = c(nhanesPcbs, nhanesDemog),
#                          url       = "https://nhanes.hms.harvard.edu/"
#               )
#' @export my.query

my.query <- function(myfields, myvector, url, verbose = FALSE, myfields.vector = NULL, enounter.misspell = TRUE) {
    myfields.vector <- if(!is.null(myfields.vector)){
        myfields.vector
    }else{
        v <- unlist(strsplit(myfields, "[|]"))
        v[ nchar(v) > 0 ]
    }
       
    if( verbose == TRUE){
        message(" Creating a list with the path from the vector list
                which contains all available paths for the resource")
    }

    
    # myfields <- gsub("([.()\\^{}+$*?]|\\[|\\])", "\\\\\\1", myfields)
    # pathList <- grep(myfields, myvector, value=TRUE)
    
    ## keep only the paths containing the desired fields
    pathList <- myvector[
        sapply(myvector, function(v){
            any(sapply(myfields.vector, function(f){
                length(grep(f,v, fixed = T)) > 0
            }))
        })
    ]


    if(verbose) message("Getting all the fields available from the pathlist")
    
    # Create vector for query
    querySELECT<- c()

    # for each entry in filtered path list:
    #     create a field entry in the query
    for (i in 1:length(pathList)){
        if( verbose ) message( paste("Get the fields for", pathList[i] ))
        
        
        IRCT_CL_SERVICE_URL <- "rest/v1/"
        IRCT_RESOURCE_BASE_URL <- paste(IRCT_CL_SERVICE_URL,"resourceService/",sep="")
        IRCT_PATH_RESOURCE_URL <- paste(IRCT_RESOURCE_BASE_URL,"path",sep="")
        
        # format URL for current path entry to retrieve fields       
        nurlstr <- paste( IRCT_PATH_RESOURCE_URL, pathList[i], sep = "" )
        # nurl <- gsub( "\\#","%23", gsub("\\?", "%3F", gsub("[)]","%29", gsub("\\*","%2A", gsub("\\^","%5E", gsub("[(]","%28", URLencode(nurlstr)))))))

        # httr::content(httr::GET(nurl))
        
        # get children of current path entry
        # pathFields <- httr::content(httr::GET(nurl))
        pathFields <- send.request(url = url, path = nurlstr,
                                   verbose = verbose)

        # pathFields <- get.children.updated(pathList[i], url = url, verbose = verbose)
        
        # message(pathFields)
        
        if( verbose == TRUE ){
            message( "Generating {field} object for SELECT portion of the query" )
        }
        
        # if there were children, add a field entry to the query for each child path
        if (length(pathFields)>0) {
            for (j in 1:length(pathFields)) {
                if(verbose) print(list("pathFields[j]" =  pathFields[j]))
    
                myField <- list(
                    list(
                        field=list(
                            pui = jsonlite::unbox(pathFields[[j]]$pui),
                            dataType = jsonlite::unbox(pathFields[[j]]$name)
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
            
            pathSegs <- splitPath(pathList[[i]])
            leafnurlstr <- concatPath( head(pathSegs, -1) )

            # leafnurl <- gsub( "\\#","%23", gsub("\\?", "%3F", gsub("[)]","%29", gsub("\\*","%2A", gsub(" ","%20", gsub("\\^","%5E", gsub("[(]","%28", URLencode(leafnurlstr))))))))

            leafPathUrl <- concatPath(c(IRCT_PATH_RESOURCE_URL, leafnurlstr))

            # leafPathFields <- httr::content(httr::GET(leafPathUrl))
            leafPathFields <- send.request(url = url,
                                           path = leafPathUrl,
                                           verbose = verbose)
            # leafPathFields <- get.children.updated(leafnurlstr, url = url, verbose = verbose)
            
            leafPath <- grep(pathList[[i]], leafPathFields, fixed = TRUE, value=TRUE)
            ## message("leafPath")
            ## message(leafPath)

            
            # get children of current path entry
            
            # pathFields <- httr::content(httr::GET(nurl))
            ## pathFields <- send.request(url = url,
            ##                            path = nurlstr,
            ##                            verbose = verbose)
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
            # message(entry)
            myField <- list(
                list(
                    field=list(
                        pui = jsonlite::unbox(entry$pui),
                        dataType = jsonlite::unbox(entry$name)
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
    # field <- myfields.vector[1] # unlist(strsplit(myfields, "[|]"))[1]
    # field <- gsub("([.()\\^{}+$*?]|\\[|\\])", "\\\\\\1", field)

    ## if(verbose){
    ##     print(paste0("searching for <", myfields.vector[1], "> in :"))
    ##     print(paste(pathList, collapse = "\n"))
    ## }
    whereClause <- grep(myfields.vector[1], pathList, fixed = T, value=TRUE)[[1]]
    
    
    # Assuming STRING variable, but not sure
    mylist      <- list( pui      = jsonlite::unbox( whereClause ),
                         dataType = jsonlite::unbox( "STRING" ) )

    
    
    queryWHERE  <- list( field     = mylist,
                         predicate = jsonlite::unbox( "CONTAINS" ))
    
    queryWHERE$fields = list()
    # enounter.misspell = send.request( url = url, path = "rest/v1/resourceService/resources")
    # enounter.misspell <- unlist(enounter.misspell)
    # enounter.misspell <- enounter.misspell[["predicates.fields.path"]]
    # queryWHERE$fields[[enounter.misspell]] = jsonlite::unbox("YES")

    queryWHERE$fields = list()
    queryWHERE$fields[[if(enounter.misspell) "ENOUNTER" else "ENCOUNTER"]] = jsonlite::unbox("YES")
    #print(queryWHERE)
    
    querySTRING <- list( select = querySELECT,
                         where  = list( queryWHERE ) )

    return( jsonlite::toJSON ( querySTRING, pretty=TRUE ) )
}
