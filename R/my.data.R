#' Query analysis to the API
#'
#' Given an url and a JSON object, it generates a \code{data.frame} object with 
#' the output of the query. 
#'
#' @param query A text file containing the JSON query. 
#' @param url  The url.
#' @param outputPath Path and the file name where the output file will be saved. By default it
#' will be saved in the working directory with the name queryData.
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get an
#' on-time log from the function.
#' @return An object of class \code{data.frame} with the query output. 
#' @examples
#' 
#' query <- my.data( 
#'               query  = system.file("extdata", "jsonQueryNhanes", package="Rcupcake"), 
#'               url    = "https://nhanes.hms.harvard.edu/"
#'               )
#' @export my.data

my.data <- function( query, url, responseFormat = "CSV", outputPath = paste0(getwd(), "/queryData.txt") , verbose = FALSE ){
    
    IRCT_REST_BASE_URL <- url
    IRCT_CL_SERVICE_URL <- paste(IRCT_REST_BASE_URL,"rest/v1/",sep="")
    
    IRCT_QUERY_BASE_URL <- paste(IRCT_CL_SERVICE_URL,"queryService/",sep="")
    IRCT_RESULTS_BASE_URL <- paste(IRCT_CL_SERVICE_URL,"resultService/",sep="")
    
    IRCT_RUN_QUERY_URL <- paste(IRCT_QUERY_BASE_URL,"runQuery",sep="")
    IRCT_GET_RESULTS_STATUS_URL <- paste(IRCT_RESULTS_BASE_URL,"resultStatus",sep="")
    IRCT_GET_RESULTS_FORMATS_URL <- paste(IRCT_RESULTS_BASE_URL,"availableFormats",sep="")
    IRCT_GET_RESULTS_URL <- paste(IRCT_RESULTS_BASE_URL,"result",sep="")
    
    
    
    body <- paste(readLines(query), collapse = "")
    result <- httr::content(httr::POST(IRCT_RUN_QUERY_URL, 
                                         body = body))
    if( class(result) != "list" ){
        message("Please revise the connection to the url of interest")
        stop()
    }
    
    if (result$resultId == "null") {
        message("Please, revise your query object. Query cannot be run.")
        stop()
    }
    if (verbose == TRUE) {
        message("Your request is being processed")
    }
    
    status <- httr::content(httr::GET(paste(IRCT_GET_RESULTS_STATUS_URL, 
                                            result$resultId, sep = "/")))$status
    
    while ( status == "RUNNING" ) {
        Sys.sleep(3)
        status <- httr::content(httr::GET(paste(IRCT_GET_RESULTS_STATUS_URL, 
                                                result$resultId, sep = "/")))$status

    }
    
    response <- httr::content(httr::GET(paste(IRCT_GET_RESULTS_FORMATS_URL, 
                                              result$resultId, sep = "/")))
    
    if( ! responseFormat %in% response ){
        message( "Sorry, the", "is not available for this query.")
    }
    
    response <- httr::content(httr::GET(paste(IRCT_GET_RESULTS_URL, 
                                              result$resultId, responseFormat, sep = "/")), as = "text")
    results <- read.csv(text = response)
    
    if (nrow(results) == 0) {
        message("There are not results for your query")
        stop()
    }
    colnames(results)[1] <- "patient_id"
    write.table(results, file = outputPath, 
                col.names = TRUE, row.names = FALSE, sep = "\t", quote = FALSE)
    return(results)
    
}
