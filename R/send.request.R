#' Send a Request to the PIC-SURE API
#'
#'
#' @param url  The domain name of the PIC-SURE API.
#' @param path The path of the url coming after the domain name
#' @param params The GET parameters to send
#' @param body The body to send with a POST request
#' 
#' @return the returned value by Rcurl
#' @export send.request setToken setApiKey

setToken <- function(t){
    token <<- t
    if(!exists(cache)) cache.creation()
}

setApiKey <- function(k){
    apiKey <<- k
    if(!exists(cache)) cache.creation()
}

send.request <- function(url, path, params = NULL, body = NULL, as = NULL, verbose = FALSE){

    # authentication
    if(!(exists("token")) || is.null(token)){
        if(!exists("session") || is.null(session)){
            print("session does not exist")
            if(exists("apiKey") && !is.null(apiKey)){
                if(verbose) cat("starting session with apiKey...\n")
                session <<- startSession()
            }else{
                cat("No authentication method is set.\nPlease set the Token or apiKey with setToken() or setApiKey() function.\n")
                return()
            }
        }
    }
    
    ## percent-escape the path special characters
    path <- concatPath(sapply(splitPath(path), function(e){
        URLencode(e, reserved = T)
    }))

    fullUrl <- concatPath(c(url, path))
    if(verbose) cat(paste0("sending a request to: [", fullUrl,"]\n"))

    method.function = if(is.null(body)) httr::GET else httr::POST

    
    if(!exists("token")){
        r <- method.function(fullUrl, body = body)
    }else{
        # print(list(url = fullUrl, body = body, method = method.function))
        r <- method.function(fullUrl, body = body, httr::add_headers(Authorization=paste("bearer", token)))
    }
    if(verbose) print(httr::status_code(r))
    return(httr::content(r, as = as))
}

