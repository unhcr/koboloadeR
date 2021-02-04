#' @name kobo_chart_mosaic
#' @rdname kobo_chart_mosaic
#' @title  Generate an association chart
#'
#'
#'
#' @param data dataframe
#' @param  x variable to use for association
#' @param  y variable to use for association
#'
#' @return ggplot2 object
#'
#' @author Edouard Legoupil
#'
#'
#' @export kobo_chart_mosaic
#'
#' @examples
#' \dontrun{
#' kobo_chart_mosaic(form, mapref)
#' }
#'

kobo_chart_mosaic <- function(data, .x, .y) {
  
  library(tidyverse)
  library(rlang)
  
    xdata <- 
      data  %>% 
      count(x := {{.x}}) %>% 
      transmute(x, xmax = cumsum(n/sum(n)), xmin = lag(xmax, default = 0))
    
    ydata <- 
      data %>% 
      count(x := {{.x}}, y := {{.y}}) %>% 
      group_by(x) %>% 
      transmute(x, y, ymax = cumsum(n/sum(n)), ymin = lag(ymax, default = 0)) %>% 
      ungroup()
    
    mdata <- left_join(ydata, xdata, by = "x")
    
    csq <- chisq.test(data %>% 
                        pull({{.x}}), data %>% 
                        pull({{.y}}), 
                      simulate.p.value = TRUE)
    
    mdata <- 
      mdata %>% 
      left_join(csq$stdres %>% 
                  as_tibble(.name_repair = "universal") %>% 
                  set_names("x", "y", "stdres"), by = c("x", "y")) %>% 
      mutate(dir = sign(stdres), 
             sig = case_when(abs(stdres) < 2 ~ 0,
                             abs(stdres) > 3 ~ 99,
                             TRUE            ~ 95))
    
    xaxis <- xdata %>% 
             transmute(labels = x, breaks = (xmin+xmax)/2)
    yaxis <- ydata %>% 
             filter(x == first(x)) %>% 
             transmute(labels = y, breaks = (ymin+ymax)/2)
    
    mdata %>% 
      ggplot() +
      geom_rect(aes(xmin = xmin+.01, xmax = xmax-.01, ymin = ymin+.01, ymax = ymax-.01, 
                    fill = as_factor(dir), alpha = as_factor(sig)), 
                color = "grey50", size = 1, linetype = 2) +
      scale_x_continuous(breaks = xaxis$breaks, labels = xaxis$labels,
                         sec.axis = sec_axis(~., breaks = xaxis$breaks, labels = scales::label_percent()(xdata$xmax-xdata$xmin))) +
      scale_y_continuous(breaks = yaxis$breaks, labels = yaxis$labels,
                         sec.axis = sec_axis(~., breaks = seq(0, 100, 25)/100, labels = scales::label_percent())) +
      scale_fill_manual(values = c("0" = "white", "1" = "#0072BC", "-1" = "#EF4A60"),
                        labels = c("0" = " ", "1" = "Positive", "-1" = "Negative"),
                        drop = FALSE) +
      scale_alpha_manual(values = c("0" = 0, "95" = .5, "99" = 1),
                         labels = c("0" = "Insignificant", "95" = "95% level", "99" = "99% level"),
                         drop = FALSE) +
      guides(fill = guide_legend(title.position = "top", order = 0), alpha = guide_legend(title.position = "top", order = 1)) +
      labs(x = NULL, y = NULL, fill = "Direction", alpha = "Significance", 
           caption = scales::label_pvalue(add_p = TRUE)(csq$p.value)) +
      theme_minimal() +
      theme(panel.grid = element_blank(), legend.position = "bottom")
  }
NULL


