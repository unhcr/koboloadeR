#' @import httr
NULL


#' @import RCurl
NULL

#' @import plyr
NULL

#' @import readxl
NULL

f_csv <- function(x) setDT(read_csv(content(x, "raw")))[]
