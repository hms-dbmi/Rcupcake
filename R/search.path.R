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
    ## print(paste("$$ search path", fieldname))
    if(verbose == TRUE){
        print("Be patient, your request is being processed.")
    }
    result <- sapply(fieldname, function(path){
        ## split the path, remove empty elements
        path.vector = Filter(function(e)nchar(e)>0, splitPath(path))
        ## our goal is the first node of this path
        goal = path.vector[1]
        
        print(paste("Searchpath GOAL is ", goal))
        result = NULL
        toSearch = get.children.updated("", url, verbose = verbose)
        # print( toSearch )
        
        while( length(toSearch) > 0 ){
            current  = toSearch[1]
            ## print(paste("$$ search path: current = ", current))
            toSearch = toSearch[-1]
            
            if(verbose) print(paste("listing the children of", current))
            
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
        ## the return value is the concatenation of result (= the path to the first node of the <path> argument)
        ## and the rest of the path (without it's first node because it is already in result)
        ## eg search.path "ghi/jkl/mno" will search for "ghi", and find "abc/def/ghi"
        ##    and at the end, we want the return to be "abc/def/ghi/jkl/mno"
        ## TODO: add a check to verify that the full path really exists
        ##       (= check if the user did no spelling mistake)
        return(concatPath(c(result, path.vector[-1])))
    }, USE.NAMES = FALSE)
    
    names( result ) <- c()
    return( result )
    
}
