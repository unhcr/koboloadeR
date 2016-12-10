
#' @name format_si
#' @rdname format_si
#' @title Helper function to format a vector of strings using
#'   SI prefix notation
#'
#'
#' Format a vector of numeric values according
#' to the International System of Units.
#' http://en.wikipedia.org/wiki/SI_prefix
#'
#' Based on code by Ben Tupper
#' https://stat.ethz.ch/pipermail/r-help/2012-January/299804.html
#' Args:
#'
#'
#' @param \dots List of integer or numeric
#'   ...: Args passed to format()
#'
#' @return Formatted number.
#'
#' @author Someone
#'
#' @examples
#' format_si()
#'
#' @export format_si
#'
format_si <- function(...) {
  function(x) {
    limits <- c(1e-24, 1e-21, 1e-18, 1e-15, 1e-12,
                1e-9,  1e-6,  1e-3,  1e0,   1e3,
                1e6,   1e9,   1e12,  1e15,  1e18,
                1e21,  1e24)
    prefix <- c("y",   "z",   "a",   "f",   "p",
                "n",   "",   "m",   " ",   "k",
                "M",   "G",   "T",   "P",   "E",
                "Z",   "Y")

    # Vector with array indices according to position in intervals
    i <- findInterval(abs(x), limits)

    # Set prefix to " " for very small values < 1e-24
    i <- ifelse(i==0, which(limits == 1e0), i)

    paste(format(round(x/limits[i], 1),
                 trim=TRUE, scientific=FALSE, ...),
          prefix[i])
  }
}



#' @name psum
#' @rdname psum
#' @title Helper function that will sum values even if we have NA
#'
#' @param \dots List of integer or numeric
#'
#' @return Integer or numeric.
#'
#' @author Someone
#'
#' @examples
#' psum()
#'
#' @export psum
#'
psum <- function(..., na.rm=FALSE) {
  x <- list(...)
  rowSums(matrix(unlist(x), ncol=length(x)), na.rm=na.rm)
}



#' @name round2
#' @rdname round2
#' @title Create roundup function
#'
#'
#' @param List of integer or numeric to be rounded
#' @param Rounding level
#'
#' @return rounded figure.
#'
#' @author Someone
#'
#' @examples
#' round2(x, n)
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
#' ltbl(x,y,z)
#'
#' @export ltbl
#'
ltbl <-function(x,y,z){  gsub("\"","",tail(strsplit(names(x)[y][z],split="/")[[1]],1))}


#' @name multrespons
#' @rdname multrespons
#' @title  Helper function to concatenate multiple choices (select_mutiple type question) formatted TRUE / FALSE
#'
#'
#' @param  x = String
#' @return last part of question headings
#'
#' @author Someone
#'
#' @examples
#' multresponse(x)
#'
#' @export multresponse
#'
multresponse <-function (x){
  y<-which(x=="TRUE")
  if(length(y)!=0){
    for (tu in 1:length(y)){
      if(tu==1){value<-ltbl(x,y[tu],1)}else {value<-c(value,ltbl(x,y[tu],1))}
    }
  }else{value<-NA}
  paste(value,collapse=" / ")
}


