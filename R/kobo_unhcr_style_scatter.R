#' @name kobo_unhcr_style_scatter
#' @rdname kobo_unhcr_style_scatter
#' @title UNHCR ggplot2 theme
#'
#' @description Return ggplot2 styling for scatter plot
#'
#' @return Return UNHCR Style
#'
#' @author Edouard Legoupil - with inspiration from bbc
#'
#' @examples
#' kobo_unhcr_style_scatter()
#'
#' @export kobo_unhcr_style_scatter
#'

kobo_unhcr_style_scatter <- function() {
    font <- "Lato"
    ggplot2::theme(

      #This sets the font, size, type and colour of text for the chart's title
      plot.title = ggplot2::element_text(family = font, size = 12, face = "bold", color = "#222222"),

      #This sets the font, size, type and colour of text for the chart's subtitle,  as well as setting a margin between the title and the subtitle
      plot.subtitle = ggplot2::element_text(family = font, size = 11, margin = ggplot2::margin(9,0,9,0)),
      plot.caption = ggplot2::element_blank(),

      #This sets the position and alignment of the legend, removes a title and backround for it and sets the requirements for any text within the legend. The legend may often need some more manual tweaking when it comes to its exact position based on the plot coordinates.
      legend.position = "top",
      legend.text.align = 0,
      legend.background = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(family = font, size = 9, color = "#222222"),

      #This sets the text font, size and colour for the axis test, as well as setting the margins and removes lines and ticks. In some cases, axis lines and axis ticks are things we would want to have in the chart
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(family = font, size = 11, color = "#222222"),
      axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 9)),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),

      #This removes all minor gridlines and adds major y gridlines. In many cases you will want to change this to remove y gridlines and add x gridlines.
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "#cbcbcb"),
      panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"),

      #This sets the panel background as blank, removing the standard grey ggplot background colour from the plot
      panel.background = ggplot2::element_blank(),

      #This sets the panel background for facet-wrapped plots to white, removing the standard grey ggplot background colour and sets the title size of the facet-wrap title to font size 22
      strip.background = ggplot2::element_rect(fill = "white"),
      strip.text = ggplot2::element_text(size  = 11,  hjust = 0)
    )
  }
