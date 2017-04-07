#' Obtain the raw query from a \code{genopheno} and \code{genophenoComor} object.
#'
#' @name extract
#' @rdname extract-methods
#' @param object of class \code{genophenoComor} or \code{genopheno} object
#' @return A \code{data.frame} containing the raw result from the initial data 
#' (genopheno)or comorbidity analysis (genophenoComor)
#' @examples
#' \dontrun{
#' #Being x an genophenoComor
#' qr <- extract(x) 
#' }
#' @export
setGeneric ("extract",
            function(object, ...){standardGeneric("extract")}
)


#' Getter from \code{genopheno}.
#' 
#' Obtain the mutations in a \code{genopheno}.
#'
#' @name nmutation
# @rdname nmutation-methods
#' @aliases nmutation
#' @param object Object of class \code{genopheno}.
#' @examples
#' data(qr)
#' nmutation(qr)
#' @export
setGeneric ("nmutation",
            function(object){standardGeneric("nmutation")}
)

#' Getter from \code{genopheno}.
#' 
#' Obtain the phenotypes in a \code{genopheno}.
#'
#' @name nphenotype
# @rdname nphenotype-methods
#' @aliases nphenotype
#' @param object Object of class \code{genopheno}.
#' @examples
#' data(qr)
#' nphenotype(qr)
#' @export
setGeneric ("nphenotype",
            function(object){standardGeneric("nphenotype")}
)
