#' @import httr
NULL


#' @import RCurl
NULL

#' @import plyr
NULL

#' @import readxl
NULL

f_csv <- function(x) data.table::setDT(readr::read_csv(content(x, "raw")))[]
