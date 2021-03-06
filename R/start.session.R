#' Start the connection to the database
#'
#' Given a URL and a key access it starts the connection to the database 
#'
#' @param url  The url.
#' @param apiKey The key to access to the data. 
#' @return A message showing if the connection has been done or not. 
#' @examples
#' 
#' sessionEx <- start.session( 
#'               url         = "https://nhanes2.hms.harvard.edu/", 
#'               apiKey      = "YOURKEY"
#'               )
#' @export start.session

start.session <- function( url, apiKey){

    Key <- apiKey
    IRCT_REST_BASE_URL <- url
    IRCT_CL_SERVICE_URL <- paste(IRCT_REST_BASE_URL,"rest/v1/",sep="")
        #Service URLS
    IRCT_RESOURCE_BASE_URL <- paste(IRCT_CL_SERVICE_URL,"resourceService/",sep="")
    IRCT_QUERY_BASE_URL <- paste(IRCT_CL_SERVICE_URL,"queryService/",sep="")
    IRCT_RESULTS_BASE_URL <- paste(IRCT_CL_SERVICE_URL,"resultService/",sep="")
    IRCT_PROCESS_BASE_URL <- paste(IRCT_CL_SERVICE_URL,"processService/",sep="")
    
    #List resources
    IRCT_LIST_RESOURCE_URL <- paste(IRCT_RESOURCE_BASE_URL,"resources",sep="")
    IRCT_PATH_RESOURCE_URL <- paste(IRCT_RESOURCE_BASE_URL,"path",sep="")
    
    #Query
    IRCT_START_QUERY_URL <- paste(IRCT_QUERY_BASE_URL,"startQuery",sep="")
    IRCT_CLAUSE_URL <- paste(IRCT_QUERY_BASE_URL,"clause",sep="")
    IRCT_RUN_QUERY_URL <- paste(IRCT_QUERY_BASE_URL,"runQuery",sep="")
    
    #Process
    IRCT_START_PROCESS_URL <- paste(IRCT_PROCESS_BASE_URL,"startProcess",sep="")
    IRCT_UPDATE_PROCESS_URL <- paste(IRCT_PROCESS_BASE_URL,"updateProcess",sep="")
    IRCT_RUN_PROCESS_URL <- paste(IRCT_PROCESS_BASE_URL,"runProcess",sep="")
    
    #Result
    IRCT_GET_RESULTS_STATUS_URL <- paste(IRCT_RESULTS_BASE_URL,"resultStatus",sep="")
    IRCT_GET_RESULTS_FORMATS_URL <- paste(IRCT_RESULTS_BASE_URL,"availableFormats",sep="")
    IRCT_GET_RESULTS_URL <- paste(IRCT_RESULTS_BASE_URL,"result",sep="")
    
    startSession <-
        httr::content(httr::GET(
            paste0(
                IRCT_REST_BASE_URL,
                "/rest/v1/securityService/startSession?key=",
                Key
            )
        ))
    
    session <<- startSession
    
    cache.creation()
    
    
    if( names(startSession)[1] == "node"){
        return( "Start Session: failed. Please revise your url and apiKey. Check that your apiKey has not expired.")
    }else if( names(startSession)[1] == "status"){
        
        if( startSession[[1]] == "success" | startSession[[1]] == "ok"){
            return( "Start Session: success" )
        }else{
            return( "Start Session: failed. Please revise your url and apiKey" )
        }
    }
    
    
    
}
