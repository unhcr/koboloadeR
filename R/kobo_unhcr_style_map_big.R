#' @name kobo_unhcr_style_map_big
#' @rdname kobo_unhcr_style_map_big
#' @title UNHCR ggplot2 theme
#'
#' @description Return ggplot2 styling for maps
#'
#' @return Return UNHCR Style
#'
#' @author Edouard Legoupil -
#'
#' @examples
#' \dontrun{
#' kobo_unhcr_style_map_big()
#' }
#'
#' @export kobo_unhcr_style_map
#'

kobo_unhcr_style_map_big <- function() {
    font <- "Lato"
    ggplot2::theme_minimal() +
    ggplot2::theme(
          #------------
          ## Plot
          # plot.background = element_rect(fill = "transparent",colour = NA),
          # plot.background = element_rect(fill = "#f5f5f2", color = NA),
          plot.title = ggplot2::element_text(family = font, 
                                             face = "bold", 
                                             size = 28, 
                                             hjust = 0, 
                                             color = "#4e4d47"),
          plot.subtitle = ggplot2::element_text(family = font, 
                                                size = 18, 
                                                hjust = 0, 
                                                color = "#4e4d47", 
                                                margin = ggplot2::margin(b = -0.1, t = -0.1, l = 2, unit = "cm"), 
                                                debug = F),
          plot.caption = ggplot2::element_text(family = font, 
                                               size = 6, 
                                               hjust = 0.92, 
                                               margin = ggplot2::margin(t = 0.2, b = 0, unit = "cm"), 
                                               color = "#939184"),
          plot.margin = unit(c(.5,.5,.2,.5), "cm"),
          
          #------------
          ## Panel
          panel.border = element_blank(),
          # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
          panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
          panel.grid.minor = element_blank(),
          # panel.background = element_rect(fill = "#f5f5f2", color = NA),
         # panel.spacing = unit(c(-.1,0.7,.2,1.7), "cm"),
          panel.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),

          #------------
          ## legend
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 7, hjust = 0, color = "#4e4d47"),
          legend.text.align = 0, 
          #legend.position = "bottom",
          legend.position = c(0.5, 0.03), 
          legend.box = "horizontal",
          # legend.position = c(0.8, 0.03),
          #legend.background = element_rect(fill = "transparent",colour = NA),
          # legend.background = element_rect(fill = "#f5f5f2", color = NA),
          legend.background = element_rect(fill = alpha('white', 0.0), color = NA),
          
          #------------
          ## Axis
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()


    )
}




