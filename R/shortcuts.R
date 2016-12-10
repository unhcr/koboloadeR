#' @import httr
NULL

#' @import data.table
NULL

#' @import bit64
NULL

#' @import readr
NULL


#' @import RCurl
NULL


#' @import DT
NULL

f_csv <- function(x) setDT(read_csv(content(x, "raw")))[]

