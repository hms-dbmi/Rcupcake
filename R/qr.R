#' \code{genopheno} obtained from quering NHANES with an example query.
#'
#' A dataset obtained from NHANES after being queried with 
#' \code{irctquery} on \code{"NHANES"} database.
#'
#' @usage data("qr")
#' @format   The format is:
#' Formal class 'genopheno' [package "genophenoR"] with 6 slots
#'   .. nMutations    : int 0
#'   .. nPhenotype    : int 3
#'   .. nPatient      : int 41474
#'   .. iresult       : 'data.frame'
#'   .. mutations     : 'data.frame'
#'   .. phenotypes    : 'data.frame' 
#' @return A \code{genopheno} object.
#' @examples
#' nmutation(qr)
#' nphenotype(qr)
#' @export qr