#' @name kobo_left_align
#' @rdname kobo_left_align
#' @title UNHCR ggplot2 theme
#'
#' @description Left align chart title and subtitle on a ggplot2
#'
#' @return Return better chart
#'
#' @author Edouard Legoupil - with inspiration from bbc
#'
#' @examples
#' kobo_left_align()
#'
#' @export kobo_left_align
#'

kobo_left_align <- function(plot_name, pieces){
  grob <- ggplot2::ggplotGrob(plot_name)
  n <- length(pieces)
  grob$layout$l[grob$layout$name %in% pieces] <- 2
  return(grob)
}
