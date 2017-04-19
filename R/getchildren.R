#' Get children paths
#'
#' Given a url, a key and a path it returns all the children paths under the given one.
#'
#' @param url  The url.
#' @param apiKey The key to access to the data. 
#' @param fieldname  The path in which the urser is interested.
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get an
#' on-time log from the function.
#' @return A vector with all the fields that are under the path that has been given as input. 
#' @examples
#' 
#' nhanesPcbs <- getchildren( 
#'                  fieldname   = "/nhanes/Demo/laboratory/laboratory/pcbs/"
#'               )
#' @export getchildren


getchildren <- function( fieldname, verbose = FALSE) {
    
    children <- c( )
    nexturl <- paste( IRCT_PATH_RESOURCE_URL, fieldname, sep = "" )
    newchildren <-  httr::content(httr::GET(nexturl))
    
    if (length(newchildren) > 0) {
        for (i in 1:length(newchildren)) {
            res = tryCatch({
                newchild <- newchildren[[i]]$pui
                children <- c(children, newchild)
                getchildren(children, 
                            gsub("\\#","%23", gsub("\\?", "%3F", gsub("[)]","%29", gsub("[(]","%28", URLencode(newchild)))))
                )
            }, error = function(errorCondition) {
                if( verbose == TRUE){
                    message("ERROR: There is something wrong with the children")
                    message(newchildren)
                }
            })
        }
    } else {
        if( verbose == TRUE ){
            message("Ending loop. No more children.")
        }

    }
    
    return( children )
}
