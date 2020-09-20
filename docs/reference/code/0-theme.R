#=========================================
# CREATE THEMES
# We'll create two themes:
#
# 1. theme.porttheme
#    - this will be a general theme that
#      we'll apply to most of our charts
#      to format the text, titles, etc
#
# 2. theme.smallmult
#    - we'll apply this exclusively to
#      "small multiple" charts
#      (AKA, trellis charts).  We need this
#      because the axis text needs to be
#      smaller for the small multiples
#=========================================


#----------------------------------------
# GENERAL THEME
# - we'll use this for most of our charts
#   and build on it when we need to
#----------------------------------------

theme.porttheme <-
  theme(text = element_text(family = "Gill Sans", color = "#444444")) +
  theme(plot.title = element_text(size = 24)) +
  theme(plot.subtitle = element_text(size = 18)) +
  theme(axis.title = element_text(size = 14)) +
  theme(axis.title.y = element_text(angle = 0, vjust = .5, margin = margin(r = 15))) +
  theme(axis.text = element_text(size = 10)) +
  theme(axis.title.x = element_text(margin = margin(t = 20))) +
  theme(legend.title = element_blank())



#------------------------------------
# THEME FOR 'WIDE' BAR CHARTS
# - there are several bar charts that
#   are very wide, and need some
#   special formatting
#------------------------------------

theme.widebar <-
  theme.porttheme +
  theme(plot.title = element_text(size = 30)) +
  theme(plot.subtitle = element_text(size = 20)) +
  theme(legend.title = element_blank(), legend.background = element_blank()) +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.position = c(.9,.55)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4))



#------------------------------------
# THEME FOR 'WIDE' BAR CHARTS
#  - we'll use this for small multiple
#    charts.  these also have some
#    special formatting requirements
#------------------------------------

theme.smallmult <-
  theme.porttheme +
  theme(axis.text = element_text(size = 6)) +
  theme(axis.text.x = element_text(angle = 90))


#------------------------------------
# THEME FOR Maps
# It's based on `theme_minimal` and basically resets all the axes.
# It also defined a very subtle grid and a warmgrey background, which gives it some sort of paper map feeling..
# The font used here is `Ubuntu Regular` – adapt to your liking, but the font must be installed on your OS.
#------------------------------------

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Ubuntu Regular", color = "#22211d"),

      #------------
      ## Plot
      # plot.background = element_rect(fill = "transparent",colour = NA),
      # plot.background = element_rect(fill = "#f5f5f2", color = NA),
      plot.title = element_text(face = "bold", size = 12, hjust = 0, color = "#4e4d47"),
      plot.subtitle = element_text(size = 8, hjust = 0, color = "#4e4d47", margin = margin(b = -0.1, t = -0.1, l = 2, unit = "cm"), debug = F),
      plot.margin = unit(c(.5,.5,.2,.5), "cm"),
      plot.caption = element_text(size = 6, hjust = 0.92, margin = margin(t = 0.2, b = 0, unit = "cm"), color = "#939184"),

      #------------
      ## Panel
      panel.border = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      # panel.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.spacing = unit(c(-.1,0.7,.2,1.7), "cm"),

      #------------
      ## legend
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7, hjust = 0, color = "#4e4d47"),
      legend.position = "bottom",
      legend.box = "horizontal",
      # legend.position = c(0.8, 0.03),
      legend.text.align = 0,
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
      axis.title.y = element_blank(),
      ...
    )
}
