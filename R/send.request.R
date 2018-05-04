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
    if(!exists("cache")) cache.creation()
    token <<- t
}

setApiKey <- function(k){
    if(!exists("cache")) cache.creation()
    apiKey <<- k
}

send.request <- function(url, path, params = NULL, body = NULL, as = NULL, verbose = FALSE){

    # authentication
    if(!(exists("token")) || is.null(token)){
        if(!exists("session") || is.null(session)){
            print("session does not exist")
            if(exists("apiKey") && !is.null(apiKey)){
                if(verbose) cat("starting session with apiKey...\n")
                session <<- start.session(apiKey = apiKey, url = url)
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

    method.function = if(is.null(body)) httr::GET else httr::POST

    if(verbose) {
        cat(paste0("sending a ",
                   if(is.null(body)) "GET" else "POST"
                  ," request to: [", fullUrl,"]\n"))
        if(!is.null(body)){
            cat(paste("\n──────────────────── request body ───────────────────\n",
                      body,
                      "\n───────────────────────────────────────\n\n"))
        }
        
    }
    
    if(!exists("token")){
        r <- method.function(fullUrl, body = body)
    }else{
        # print(list(url = fullUrl, body = body, method = method.function))
        r <- method.function(fullUrl, body = body, httr::add_headers(Authorization=paste("bearer", token)))
    }
    if(verbose || httr::http_error(r)){
        cat(paste0("status code: ", httr::http_status(r), "\n"))
        cat(httr::content(r, as = "text", encoding = "UTF-8"), "\n") 
    }
    return(httr::content(r, as = as, encoding = "UTF-8"))
}

