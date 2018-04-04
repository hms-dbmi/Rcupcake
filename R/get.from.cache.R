## retrieve a path (string) from the cache
get.from.cache <- function(awesomeTree, s){
    if(s %in% c("", "/")){
        return(sapply(names(awesomeTree), function(e)concatPath(c("/", e))))
    }
    
    ## recursive traversal, takes as parameter a subtree of the cache
    ## and a vector v representing the path split on "/"
    go <- function(subTree, v){
        ## from the current subtree of the cache, try to get the first element of the path
        next.element = subTree[[v[1]]]
        
        ## if we are at the last part of the desired path, try to return the children
        if(length(v)==1){
            ## it will return null if next.element is NULL (== does not exist),
            ## or if it is <NA> (== not yet queried) which is the desired behaviour
            return(names(next.element))
        }else if(length(v) > 1){
            if(is.null(next.element))
                ## if the requested subpath is not in the cache, return NULL
                return(NULL)
            else
                ## otherwise, keep going by traversing the corresponding subtree with the rest of the path
                return(go(next.element, v[-1]))
        }
    }
    ## we do [-1] because splitpath "/ab/cd/ef" == c("", "ab", "cd", "ef") (because of the leading slash)
    splitPathv = splitPath(s)[-1]
    
    ans = go(awesomeTree, splitPathv)
    if( ! is.null(ans) ){
        if(tail(splitPathv, n=1) == "Demo"){
            
            ans <- sapply(ans, function(e)concatPath(c(e,e)))
        }
        
        ## since <ans> contains only the names of the nodes,
        ## we apply concatPath to each one of them in order to return the absolute path
        return( sapply(ans, function(r){concatPath(c(s, r))}))
    }else{
        return(NULL)
    }
    
}
