#' End the connection to the database
#'
#' @export end.session

end.session <- function(url, verbose = F){
    send.request(url, "/rest/v1/securityService/endSession")
        
    session <<- NULL
    if(verbose) cat(paste0("session to ", url, " ended.\n"))
}
