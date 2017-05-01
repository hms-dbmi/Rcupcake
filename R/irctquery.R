irctquery <- function (query, outputPath = getwd(), verbose = FALSE) 
{
    body <- paste(readLines(query), collapse = "")
    resultId <- httr::content(httr::POST(IRCT_RUN_QUERY_URL, 
                                         body = body))$resultId
    if (resultId == "null") {
        message("Please, revise your query object. Query cannot be run.")
        stop()
    }
    if (verbose == TRUE) {
        message("Your request is being processed")
    }
    response <- httr::content(httr::GET(paste(IRCT_GET_RESULTS_FORMATS_URL, 
                                              resultId, sep = "/")))
    while (response[[1]] == "Unable to get available formats for that id") {
        Sys.sleep(3)
        response <- httr::content(httr::GET(paste(IRCT_GET_RESULTS_FORMATS_URL, 
                                                  resultId, sep = "/")))
    }
    response <- httr::content(httr::GET(paste(IRCT_GET_RESULTS_URL, 
                                              resultId, "CSV", sep = "/")), as = "text")
    results <- read.csv(text = response)
    if (nrow(results) == 0) {
        message("There are not results for your query")
        stop()
    }
    colnames(results)[1] <- "patient_id"
    write.table(results, file = paste0(outputPath, "/queryOutput.txt"), 
                col.names = TRUE, row.names = FALSE, sep = "\t", quote = FALSE)
    return(results)
}