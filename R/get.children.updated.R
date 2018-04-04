#' Get children paths
#'
#' Given a url, a key and a path it returns all the children paths under the given one.
#'
#' @param url  The url.
#' @param fieldname  The path in which the user is interested.
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get an
#' on-time log from the function.
#' @return A vector with all the fields that are under the path that has been given as input. 
#' @examples
#' nhanesPcbs <- get.children.updated( 
#'                  fieldname   = "/nhanes/Demo/laboratory/laboratory/pcbs/", 
#'                  url         = "https://nhanes.hms.harvard.edu/"
#'               )
#' @export get.children.updated


get.children.updated <- function( fieldname, url, verbose = FALSE ) {
    # this triggers the search.path function if needed (= if the path asked for is not absolute)
    if(! (fieldname %in% c("","/"))){
        resources <- sapply(get.children.updated("", url=url, verbose = verbose), function(r)concatPath(c("/", r)))
        if(! any( startsWith(concatPath(c("/",fieldname)), resources) )){ # we check if the provided path starts with any of the DB resources
            if(verbose) print(paste("not an absolute path; searching", fieldname))
            fieldname <- search.path(fieldname, url=url, verbose = verbose)
            if(verbose) print(paste("resolved to", fieldname))
        }
    }
    ## this inner function lists only the specified path given as argument
    lsPath <- function(path){
        if(verbose) print(list(lspath=path))
        ## try to fetch the children from the cache
        fromcache <- get.from.cache(cache, path)
        ## if it succeeds we should get back a character vector
        if(class(fromcache) == "character"){
            ## print("using cached data")
            ## do not send a request, return the cached data
            return(fromcache)
        }else{
            
            IRCT_REST_BASE_URL <- url
            IRCT_CL_SERVICE_URL <-        concatPath(c(IRCT_REST_BASE_URL,"rest/v1/"))
            IRCT_RESOURCE_BASE_URL <-     concatPath(c(IRCT_CL_SERVICE_URL,"resourceService/"))
            IRCT_PATH_RESOURCE_URL <-     concatPath(c(IRCT_RESOURCE_BASE_URL,"path"))
            IRCT_RESOURCE_RESOURCE_URL <- concatPath(c(IRCT_RESOURCE_BASE_URL,"resources"))
            
            if(verbose) print(path)
            
            if(path %in% c("","/")){
                if(verbose) print("listing resources")
                r <- httr::GET(IRCT_RESOURCE_RESOURCE_URL)
                newchildren <-  httr::content(r)
                if(verbose) print(httr::http_status(r))
                
                children <- sapply(newchildren, function(e){e$name})
            }else{
                if(verbose) print("standard get children request")
                ## split the field name on "/", and apply URLencode to each part
                ## it is necessary to split first so that the "/" delimiting the path are not URLencoded
                encodedPath <- concatPath( sapply(splitPath(path), function(s)URLencode(s, reserved = TRUE) ))
                nexturl <- concatPath(c( IRCT_PATH_RESOURCE_URL,
                                        encodedPath))
                                        ## gsub("\\#","%23", gsub("\\?", "%3F", gsub("[)]","%29", gsub(" ","%20", gsub("[(]","%28", path)))))))
                if(verbose) print(nexturl)
                ## perform the request
                r <- httr::GET(nexturl)
                if(verbose) print(httr::http_status(r))
                children <- httr::content(r)
                ## keep only the puis (= the absolute path)
                children <- sapply(children, function(c) c$pui )
                if(verbose) print(children)
            }
            
            ## add the newly discovered children to the cache
            cache <<- add.to.cache( cache, children)
            return( children )
        }
    }
    splitFieldName <- splitPath(fieldname)
    # print(splitFieldName)
    ## in order for the cache to be populated correctly, we get the children for every subpath
    ## ie for "/ab/cd/ef/" we list "/ab" , then "ab/cd", and finally "ab/cd/ef"
    ## with "unlist(tail(...., n=1))" we return only the last element, corresponding to the path asked by the user
    v <- unlist(tail(lapply(1:length(splitFieldName), function(n){
        lsPath( paste( splitFieldName[1:n], collapse = "/" ) )
    }), n=1))
    names(v) <- c()  # remove the names for readability
    return(v)
}

