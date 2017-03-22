#' Query analysis to the API
#'
#' Given an url, a key and a JSON object, it generates a \code{data.frame} object.
#'
#' @param url  The url.
#' @param apiKey The personal key to access to the data. 
#' @param query A text file containing the JSON query body. 
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get a
#' on-time log from the function.
#' @return An object of class \code{data.frame}
#' @examples
#' 
#' query <- irctquery( 
#'               url         = "https://nhanes.hms.harvard.edu/", 
#'               apiKey      = "a77l42anvcgdtkfcvbl7hnp2v9"
#'               query  = system.file("extdata", "jsonQueryNhanes", package="genophenoR"))
#'               )
#' @export irctquery

irctquery <- function( url, apiKey, query, verbose = FALSE ){
    
    IRCT_REST_BASE_URL <- url
    
    #REST URL
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
        
    startSession <- content(GET(paste0(url, "/rest/v1/securityService/startSession?key=", apiKey)))    

    body <- paste(readLines(query), collapse="")
    
    resultId <- content(POST(IRCT_RUN_QUERY_URL, body = body))$resultId
    
    if( verbose == TRUE ){
        message( "Your request is being processed")
    }
    
    Sys.sleep( 60 )
    
    response <- content(GET(paste(IRCT_GET_RESULTS_URL, resultId, "CSV", sep="/")), as="text")
    results <- read.csv(text = response)

    colnames(results)[1] <- "patient_id"
    return( results )
    
}
