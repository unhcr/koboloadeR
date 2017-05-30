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

#' @import plyr
NULL

#' @import dplyr
NULL

#' @import tidyr
NULL

#' @import readxl
NULL

f_csv <- function(x) setDT(read_csv(content(x, "raw")))[]
