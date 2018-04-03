#' Search for a path
#'
#' Given a url and a folder name it performs a breadth-first search through all the
#' available paths, and stops on the first occurence corresponding to the asked path
#' 
#' @param url  The url.
#' @param fieldname  The path in which the user is interested.
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get an
#' on-time log from the function.
#' @return The path for the folder of interest. 
#' @examples
#' 
#' pathExample <- search.path( 
#'                  fieldname   = "demographics", 
#'                  url         = "https://nhanes.hms.harvard.edu/"
#'               )
#' @export search.path


search.path <- function(fieldname, url, verbose = FALSE){
    
    if(verbose == TRUE){
        print("Be patient, your request is being processed.")
    }
    result <- sapply(fieldname, function(path){
        
        goal = splitPath(path)
        result = NULL
        
        toSearch = get.children.updated("", url, verbose = verbose)
        # print( toSearch )
        
        while( length(toSearch) > 0 ){
            current  = toSearch[1]
            toSearch = toSearch[-1]
            # print(paste("searching", current))
            children <- get.children.updated(current, url, verbose = verbose )
            
            results <- sapply(c(current, children), function(e){
                if(goal == tail( splitPath(e), n=1)){
                    return(concatPath(c(e,
                                        concatPath(splitPath(path))[-1])))
                }else{
                    return(NA)
                }
            })
            result = results[ ! is.na(results) ]
            if(length(result) > 0){
                break
            }
            
            toSearch <- c(toSearch, children)
        }
        # print(list(result=result))
        return(result)
    }, USE.NAMES = FALSE)
    
    names( result ) <- c()
    return( result )
    
}