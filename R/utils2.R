#' @name psum
#' @rdname psum
#' @title Sum with NA
#' @description Helper function that will sum values even if we have NA
#'
#' @param \dots List of integer or numeric
#' @param na.rm Bolean indicating if NA shall be removed
#'
#' @return Integer or numeric.
#'
#' @author Someone
#'
#' @examples
#' \dontrun{
#' psum()
#' }
#'
#' @export psum
#'
psum <- function(..., na.rm = FALSE) {
  x <- list(...)
  rowSums(matrix(unlist(x), ncol = length(x)), na.rm = na.rm)
}



#' @name round2
#' @rdname round2
#' @title Create roundup function
#'
#'
#' @param x List of integer or numeric to be rounded
#' @param n Rounding level
#'
#' @return rounded figure.
#'
#' @author Someone
#'
#' @examples
#' \dontrun{
#' round2(x, n)
#' }
#'
#' @export round2
#'
round2 = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

###
#' @name round_preserve_sum
#' @rdname round_preserve_sum
#' @title   Helper function to round and preserve sum
#'
#' @param  x = value
#' @param  digits = digits
#'
#' @return value
#'
#' @author Someone http://biostatmatt.com/page/5
#'
#' @examples
#' \dontrun{
#' round_preserve_sum(x,digits)
#' }
#'
#' @export round_preserve_sum
#'

round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x - y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}



#' @name ltbl
#' @rdname ltbl
#' @title   Helper function to extract the last part of question headings
#'
#' @param  x = database name
#' @param  y = column index group
#' @param  z = column index
#'
#' @return last part of question headings
#'
#' @author Someone
#'
#' @examples
#' \dontrun{
#' ltbl(x,y,z)
#' }
#'
#' @export ltbl
#'
ltbl <- function(x,y,z){  gsub("\"","",tail(strsplit(names(x)[y][z],split = "/")[[1]],1))}


#' @name multresponse
#' @rdname multresponse
#' @title  Helper function to concatenate multiple choices (select_mutiple type question) formatted TRUE / FALSE
#'
#'
#' @param x String
#' @return last part of question headings
#'
#' @author Someone
#'
#' @examples
#' \dontrun{
#' multresponse(x)
#' }
#'
#' @export multresponse
#'
multresponse <- function(x) {
  y <- which(x == "TRUE")
  if (length(y) != 0) {
    for (tu in 1:length(y)) {
      if (tu == 1) {
           value <- ltbl(x,y[tu],1)
        } else {
           value <- c(value,ltbl(x,y[tu],1))
        }
    }
  }else{value <- NA}
  paste(value, collapse = " / ")
}
