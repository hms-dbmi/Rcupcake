irctquery <- function( url, token ){
    
    tokenQ <- paste0(url, "rest/v1/securityService/startSession?key=", token)
    
    rr <- GET( url = tokenQ)
    

    
}