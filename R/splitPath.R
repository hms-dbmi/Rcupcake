splitPath <- function(s){
    ans <- unlist(strsplit(s, "/", fixed = T))
    if( length( ans ) == 0 ){
        return( "")
    }else{
        return( ans )
    }
    
}