comparePath <- function(a, b){
    return( concatPath(c("", a, "")) ==
            concatPath(c("", b, "")))
}
