#' Raw data from \code{genopheno} and \code{genophenoComor}.
#' 
#' Obtain the raw data from a query  stored in a 
#' \code{genopheno} object or the raw data with all the phenotype
#' comorbidity measures of a \code{genophenoComor} object.
#'
#' @name extract
# @rdname extract-methods
#' @aliases extract
#' @param object Object of class \code{genopheno} or \code{genophenoComor}
#' @param ... NO USED
#' @return A \code{data.frame} containing \code{data.frame}  with the results
#' from a query or a \code{data.frame} with the phenotype comorbidity result.
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
