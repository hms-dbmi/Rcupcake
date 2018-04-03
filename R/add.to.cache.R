## add to the specified cache object, the paths designed by mybranches
add.to.cache <- function(awesomeTree, mybranches){
    
    ## recursive traversal
    go <- function(element, pathv){
        ## check that we still have something to add
        if( length(pathv) > 0 ){
            ## for this step, we want to add the first element of the path
            node <- pathv[1]
            
            ## if it doesn't exist yet (NULL) or hasn't been visited (NA),
            if( is.null(element[[ node ]]) || (class(element[[ node ]]) != "list") ){
                ## if we still have something to add,
                if( length(pathv[-1]) > 0 )
                    ## create an empty list for that node
                    element[[ node ]] = list()
                else
                    ## otherwise set NA as it's value (= children were not queried yet)
                    element[[ node ]] = NA
            }
            ## traverse the rest of that branch with the rest of the path
            element[[node]] <- go( element[[ node ]] , pathv[-1] )
        }
        ## return the node, which now contains the old content and the new path
        return(element)
    }
    
    ## return (go(awesomeTree, splitPath(v)))
    for(path in mybranches){
        awesomeTree <- go(awesomeTree, splitPath(path)[-1])
    }
    return(awesomeTree)
}
