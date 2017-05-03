#' \code{cupcakeData} obtained from quering NHANES with an example query.
#'
#' A dataset obtained from NHANES after being queried with 
#' \code{my.data} on \code{"NHANES"} database.
#'
#' @usage data("qr")
#' @format   The format is:
#' Formal class 'cupcakeData' [package "Rcupcake"] with 6 slots
#'   .. nVariations   : int 0
#'   .. nPhenotype    : int 3
#'   .. nPatient      : int 41474
#'   .. iresult       : 'data.frame'
#'   .. variations    : 'data.frame'
#'   .. phenotypes    : 'data.frame' 
#' @return A \code{cupcakeData} object.
#' @examples
#' n.variations(qr)
#' n.phenotype(qr)
#' @export qr