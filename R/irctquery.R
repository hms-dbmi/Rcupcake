#' Query analysis to the API
#'
#' Given an url, a key and a JSON object, it generates a \code{data.frame} object with 
#' the output of the query. 
#'
#' @param query A text file containing the JSON query body. 
#' @param outputPath Path where the output file will be saved. By default it will be 
#' saved in your working directory
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get an
#' on-time log from the function.
#' @return An object of class \code{data.frame} with the query output. 
#' @examples
#' 
#' query <- irctquery( 
#'               query  = system.file("extdata", "jsonQueryNhanes", package="genophenoR"))
#'               )
#' @export irctquery

irctquery <- function( query, outputPath = getwd(), verbose = FALSE ){
    

    body <- paste(readLines(query), collapse="")
    
    resultId <- httr::content(httr::POST(IRCT_RUN_QUERY_URL, body = body))$resultId
    
    if( resultId == "null"){
        message("Please, revise your query object. Query cannot be run.")
        stop()
    }
    
    if( verbose == TRUE ){
        message( "Your request is being processed")
    }
    
    
    response <- httr::content(httr::GET(paste(IRCT_GET_RESULTS_FORMATS_URL, resultId, sep="/")))

    while( response[[1]] == "Unable to get available formats for that id" ){
        Sys.sleep( 3 )
        response <- httr::content(httr::GET(paste(IRCT_GET_RESULTS_FORMATS_URL, resultId, sep="/")))

    }

    
    response <-  httr::content(httr::GET(paste(IRCT_GET_RESULTS_URL, resultId, "CSV", sep="/")), as="text")
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
