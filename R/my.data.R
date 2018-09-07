#' Query analysis to the API
#'
#' Given an url and a JSON object, it generates a \code{data.frame} object with 
#' the output of the query. 
#'
#' @param query A JSON query, created with my.query function or contained in a
#' text file. 
#' @param url  The url.
#' @param outputPath Path and the file name where the output file will be saved. By default it
#' will be saved in the working directory with the name queryData.
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get an
#' on-time log from the function.
#' @return An object of class \code{data.frame} with the query output. 
#' @examples
#' 
#' #query <- my.data( 
#' #              query  = system.file("extdata", "jsonQueryNhanes", package="Rcupcake"), 
#' #              url    = "https://nhanes.hms.harvard.edu/"
#' #              )
#' @export my.data

my.data <- function( query, url, responseFormat = "CSV", outputPath = paste0(getwd(), "/queryData.txt") , verbose = FALSE ){
    
    # IRCT_REST_BASE_URL <- url
    IRCT_CL_SERVICE_URL <- "rest/v1/"
    
    IRCT_QUERY_BASE_URL <- paste(IRCT_CL_SERVICE_URL,"queryService/",sep="")
    IRCT_RESULTS_BASE_URL <- paste(IRCT_CL_SERVICE_URL,"resultService/",sep="")
    
    IRCT_RUN_QUERY_URL <- paste(IRCT_QUERY_BASE_URL,"runQuery",sep="")
    IRCT_GET_RESULTS_STATUS_URL <- paste(IRCT_RESULTS_BASE_URL,"resultStatus",sep="")
    IRCT_GET_RESULTS_FORMATS_URL <- paste(IRCT_RESULTS_BASE_URL,"availableFormats",sep="")
    IRCT_GET_RESULTS_URL <- paste(IRCT_RESULTS_BASE_URL,"result",sep="")
    
    if( class(query) == "json"){
        body <- query
    }else{
        body <- paste(readLines(query), collapse = "")
    }

    result <- send.request(url = url,
                           path = IRCT_RUN_QUERY_URL,
                           body = body,
                           verbose = verbose)
    ## result <- httr::content(httr::POST(IRCT_RUN_QUERY_URL, 
    ##                                      body = body))
    if( class(result) != "list" ){
        message("Please revise the connection to the url of interest")
        stop()
    }
    
    if (result$resultId == "null") {
        message("Please, revise your query object. Query cannot be run.")
        stop()
    }
    if (verbose) {
        message("Your request is being processed")
    }
    
    status = "RUNNING"
    while ( status == "RUNNING" | status == "CREATED" ) {
        ## status <- httr::content(httr::GET(paste(IRCT_GET_RESULTS_STATUS_URL, 
        ##                                         result$resultId, sep = "/")))$status
        status <- send.request(url = url,
                               path = paste(IRCT_GET_RESULTS_STATUS_URL, 
                                            result$resultId, sep = "/"),
                               verbose = verbose)$status
        
        Sys.sleep(3)
    }
    
    ## response <- httr::content(httr::GET(paste(IRCT_GET_RESULTS_FORMATS_URL, 
    ##                                           result$resultId, sep = "/")))
    
    response <- send.request(
        url = url,
        path = paste(IRCT_GET_RESULTS_FORMATS_URL, 
                     result$resultId, sep = "/"),
        verbose = verbose)

    
    if( ! responseFormat %in% response ){
        message( "Sorry, the ", responseFormat ," format is not available for this query.")
        message( paste("response available formats: ", paste(response, collapse = ',')))
    }
    
    ## response <- httr::content(httr::GET(paste(IRCT_GET_RESULTS_URL, 
    ##                                           result$resultId, responseFormat, sep = "/")), as = "text")

    response <- send.request(url = url,
                             path = paste(IRCT_GET_RESULTS_URL, 
                                          result$resultId, responseFormat, sep = "/"), as = "text",
                             verbose = verbose)
    if(verbose) cat("response obtained\n")
    results <- read.csv(text = response)
    if(verbose) cat("response parsed\n")
    
    if (nrow(results) == 0) {
        message("There are not results for your query")
        stop()
    }
    colnames(results)[1] <- "patient_id"
    
    write.table(results, file = outputPath, 
                col.names = TRUE, row.names = FALSE, sep = "\t", quote = FALSE)
    return(results)
    
}
