#' Obtain the raw data from a \code{cupcakeData} and \code{cupcakeResults} object.
#'
#' @name extract
#' @rdname extract-methods
#' @param object of class \code{cupcakeData} or \code{cupcakeResults} object
#' @return A \code{data.frame} containing the raw data 
#' (cupcakeData)or co-occurrence analysis results (cupcakeResults)
#' @examples
#' \dontrun{
#' #Being x an cupcakeResults
#' qr <- extract(x) 
#' }
#' @export
setGeneric ("extract",
            function(object, ...){standardGeneric("extract")}
)


#' Get the alteration variables from \code{cupcakeData}.
#' 
#' Obtain the alteration variables in a \code{cupcakeData}.
#'
#' @name n.variation
#' @rdname n.variation-methods
#' @param object Object of class \code{cupcakeData}.
#' @examples
#' data(qr)
#' n.variation(qr)
#' @export
setGeneric ("n.variation",
            function(object){standardGeneric("n.variation")}
)

#' Get the phenotypic variables from \code{cupcakeData}.
#' 
#' Obtain the phenotypes in a \code{cupcakeData}.
#'
#' @name n.phenotype
#' @rdname n.phenotype-methods
#' @aliases n.phenotype
#' @param object Object of class \code{cupcakeData}.
#' @examples
#' data(qr)
#' n.phenotype(qr)
#' @export
setGeneric ("n.phenotype",
            function(object){standardGeneric("n.phenotype")}
)
