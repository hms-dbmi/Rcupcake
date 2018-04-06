#' automated testing script
#'
#' structure for the tests object:
#' 
#'  list(
#'     "https://domain/url/" = list(
#'         apiKey = "./path/to/file/containing/apiKey",
#'         tests = list(
#'             "Title of the test" = list(
#'                 request = function(url, verbose){ .... return(result)},
#'                 result =  an R object corresponding to the expected result of the request
#'             )
#'         )
#'     )
#'
#' @export test
#' @export sampletest


## helper functions for colored output in the console:

escape.colors = list(
    title = "34;4",
    info  = "32",
    warn  = "33",
    error = "31"
)

print.color = function(color) {
    function(str){
        cat(paste0("\033[", escape.colors[color], "m", str,"\033[0m\n"))
    }
}

print.title = print.color("title")
print.info = print.color("info")
print.warn = print.color("warn")
print.error = print.color("error")

## end of helper functions for colored output

## helper for resetting sinks

sink.reset <- function(){
    for(i in seq_len(sink.number())){
        sink(NULL)
    }
}

## performs all the tests specified as argument
test <- function(domainsToTest){
    ## for each domain to test:
    sapply(names(domainsToTest), function(url){
        domain <- domainsToTest[[url]]
        print.title(paste("──────── ", url ," ────────"))

        ## read the apiKey from the specified filepath
        key <- readChar(domain$apiKey, file.info(domain$apiKey)$size)
        ## start the session
        cat(paste(start.session(url, key),"\n"))

        ## for each test to be performed for that url:
        sapply(names(domain$tests), function(title){
            t <- domain$tests[[title]]

            cat(paste0("Testing ", title, "..."))

            tryCatch({
                ## suppress any of it's output (we just want the result)
                sink("/dev/null")
                ## get the request's result
                r <- t$request( url = url , verbose = T)
            }, error = function(e){
                r <- e   ## if there is an error, assign it to the result of the test
            })
            ## disable the "output suppressing"
            sink.reset()

            ## check if the test yielded the same result as what was expected
            ok = identical(t$result, r)

            ## log the result
            if(ok){
                print.info("ok")
            }else{
                print.error("failed")
                cat("### Test failed ###\nExpected:\n")
                print(t$result)
                cat("Got:\n")
                print(r)
            }
        })

        print.title("────────────────────────────────")

    })
    cat("finished testing.\n")
}

## helper function to automatically add url and verbose as parameter to a function,
## and to clear the cache so that the tests always start with a clean cache
f <- function(f, ...) function(url, verbose){
    cache = list()
    f(..., url=url, verbose=verbose)
}


tests <- list(
    "https://nhanes.hms.harvard.edu/" = list(
        apiKey = "./nhanes.apikey",
        tests = list(
            "Listing the resources" = list(
                request = f(get.children.updated, ""),
                result =  c("/i2b2-nhanes", "/nhanes")
            ),
            "Searching for demographics" = list(
                request = f(search.path, "demographics"),
                result = "/i2b2-nhanes/Demo/demographics/demographics/"
            )
        )
    )
)


sampleTest <- function() test(tests)
