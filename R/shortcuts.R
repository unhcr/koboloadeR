#' @import httr
NULL


#' @import RCurl
NULL

#' @import plyr
NULL

#' @import readxl
NULL

f_csv <- function(x) data.table::setDT(utils::read.csv(content(x, "raw")))[]
