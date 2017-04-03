#' Query analysis to the API
#'
#' Given an url, a key and a JSON object, it generates a \code{data.frame} object with 
#' the output of the query. 
#'
#' @param url  The url.
#' @param apiKey The key to access to the data. 
#' @param query A text file containing the JSON query body. 
#' @param outputPath Path where the output file will be saved.By default it will be 
#' saved in your working directory
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get a
#' on-time log from the function.
#' @return An object of class \code{data.frame} with the query output. 
#' @examples
#' 
#' query <- irctquery( 
#'               url         = "https://nhanes.hms.harvard.edu/", 
#'               apiKey      = "a77l42anvcgdtkfcvbl7hnp2v9", 
#'               query  = system.file("extdata", "jsonQueryNhanes", package="genophenoR"))
#'               )
#' @export irctquery

irctquery <- function( url, apiKey, query, outputPath = getwd(), verbose = FALSE ){
    
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
        
    startSession <- httr::content(GET(paste0(url, "/rest/v1/securityService/startSession?key=", apiKey)))    

    if( startSession[[1]] != "success"){
        message( "Please check the url and the key access. It is not possible to start a session 
                 with the url and key provided.")
        stop()
    }
    
    body <- paste(readLines(query), collapse="")
    
    resultId <- content(POST(IRCT_RUN_QUERY_URL, body = body))$resultId
    
    if( resultId == "null"){
        message("Please, revise your query object. Query cannot be run.")
        stop()
    }
    
    if( verbose == TRUE ){
        message( "Your request is being processed")
    }
    
    
    response <- httr::content(GET(paste(IRCT_GET_RESULTS_FORMATS_URL, resultId, sep="/")))

    while( response[[1]] == "Unable to get available formats for that id" ){
        Sys.sleep( 3 )
        response <- httr::content(GET(paste(IRCT_GET_RESULTS_FORMATS_URL, resultId, sep="/")))

    }

    
    response <-  httr::content(GET(paste(IRCT_GET_RESULTS_URL, resultId, "CSV", sep="/")), as="text")
    results <- read.csv(text = response)
    
    if( nrow( results ) == 0){
        message( "There are not results for your query")
        stop()
    }

    colnames(results)[1] <- "patient_id"
    
    write.table( results, 
                 file = paste0(outputPath , "/queryOutput.txt"), 
                 col.names = TRUE, 
                 row.names = FALSE, 
                 sep="\t", 
                 quote = FALSE)
    
    return( results )
    
}
