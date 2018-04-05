
## structure for each test:
## list(
##     url = "",
##     apiKey = "/path/to/key/file", ## a file containing the apiKey
##     tests = [{
##         title = ""
##         request = function(){...}, ## this shall be a function that performs some request and return a result
##         result = Object  ## this is the expected output of the function, it will be compared to the actual output
##     }]
## )



## library(tidyverse)

path = "/ab/cd/efg/hij"

## path.vector <- splitPath( concatPath(c("/", path)) )[-1]

## path.vector.bis <- path %>%
##     c("/", .)  %>%
##     concatPath %>%
##     splitPath  %>%
##     `[`(-1)



## path.vector <- Filter(function(e){ nchar(e)>0 }, splitPath(path))

## path.vector.bis <- Filter(. %>% nchar %>% `>`(0) , splitPath(path))

sink.reset <- function(){
    for(i in seq_len(sink.number())){
        sink(NULL)
    }
}

## performs all the tests specified as argument
test <- function(domainsToTest){
    
    sapply(names(domainsToTest), function(url){
        domain <- domainsToTest[[url]]
        cat(paste("-------- ", url ," --------\n"))

        key <- readChar(domain$apiKey, file.info(domain$apiKey)$size)
        cat(paste(start.session(url, key),"\n"))

        sapply(names(domain$tests), function(title){
            t <- domain$tests[[title]]

            cat(paste0("Testing ", title, "..."))
            
            tryCatch({
                sink("/dev/null")
                r <- t$request( url = url , verbose = T)
            }, error = function(e){
                sink.reset()
                print(e)
            })
            sink.reset()
            ok = identical(t$result, r)
            cat( paste0(if(ok) "OK" else "FAILED"), "\n")
            
            if(!ok){
                cat("### Test failed ###\nExpected:\n")
                print(t$result)
                cat("Got:\n")
                print(r)
            }
                

        })

        cat("-------------------------------------\n\n")

    })
    cat("finished testing.\n")
}

f <- function(f, ...) function(url, verbose) f(..., url=url, verbose=verbose)

tests <- list(
    "https://nhanes.hms.harvard.edu/" = list(
        apiKey = "./nhanes.apikey",
        tests = list(
            "Listing the resources" = list(
                request = f(get.children.updated, ""),
                result =  c("i2b2-nhanes", "nhanes")
            ),
            "Searching for demographics" = list(
                request = f(search.path, "demographics"),
                result = "/i2b2-nhanes/Demo/demographics/demographics/"
            )
        )
    ),
    "https://pmsdn-dev.hms.harvard.edu/" = list(
        apiKey = "./pmsdn.apikey",
        tests = list(
            "Listing the resources" = list(
                request = f(get.children.updated, ""),
                result =  c("PMSDN-dev")
            ),
            "Searching for demographics" = list(
                request = f(search.path, "Demographics"),
                result = "/PMSDN-dev/Demo/01 PMS Registry (Patient Reported Outcomes)/01 PMS Registry (Patient Reported Outcomes)/Demographics/"
            )
        )
    )
)
