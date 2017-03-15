irctquery <- function( url, token ){
    
    #urlQ <- paste0( "IRCT_REST_BASE_URL <- ", url )
    #system( urlQ )
    
    tokenQ <- paste0(
        "content(GET('", url, "rest/v1/securityService/startSession?key=", token, "'))")
    
    system(tokenQ)

    
}