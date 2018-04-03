## > concatPath(c("a","/b","/c/","/d","e"))
## [1] "a/b/c/d/e"
concatPath <- function(v){
    f <- function(a, b){
        ## count nb of slashes already in place
        n = (if(substring(a, nchar(a))=="/") 1 else 0) + (if(substring(b,1,1)=="/") 1 else 0)
        
        if     (n==0) paste (a,b, sep="/")
        else if(n==1) paste0(a, b)
        else          paste0(a, substring(b,2))
    }
    if(length(v)<=1) v
    else f(v[1], concatPath(v[-1]))
    
}