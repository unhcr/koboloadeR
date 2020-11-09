.onLoad <- function(libname = find.package("koboloadeR"), pkgname = "koboloadeR") {


  # CRAN Note avoidance
  if (getRversion() >= "2.15.1")
    utils::globalVariables(
      # used to remove note when doing devtools::check(document = FALSE, args = c('--as-cran'))
      c("..density..", "CellStyle", "Var1", "addDataFrame", "aes", "alpha", "analysis_plan", "aggregVar1",
        "MainDataFrame", "report.name",
        "as.charater", "as.formula", "brewer.pal", "chisq.test", "complete.cases",
        "coord_equal", "coord_flip", "createSheet", "createWorkbook", "data", "design", "dico",
        "element_blank", "element_line", "element_rect", "element_text", "exportDDI", "facet",
        "form", "formula", "freqper", "fullname", "geom_bar", "geom_density", "geom_histogram",
        "geom_text", "getSheets", "get_map", "ggmap", "ggplot", "ggsave", "ggtitle", "guide_legend",
        "household", "installed.packages", "is.labelled", "kable", "labs", "lat", "loadWorkbook",
        "long", "margin", "na.omit", "odbcConnect", "packageDescription", "path.to.data",
        "path.to.form", "percent", "position_dodge", "qnorm", "randomSentences", "read.csv",
        "removeSheet", "render", "reorder.factor", "rtruncnorm", "saveWorkbook",
        "scale_fill_brewer", "scale_fill_gradient", "scale_fill_manual",
        "scale_x_continuous", "scale_y_continuous", "setInternet2", "setNames", "sheet",
        "spsample", "sqlFetch", "sqlQuery", "stat_summary_hex", "str", "str_length", "str_locate",
        "str_replace", "str_replace_all", "str_wrap", "stri_rand_strings", "svydesign",
        "svytable", "tail", "theme", "theme.porttheme", "theme_gray", "theme_map",
        "theme_minimal", "theme_set", "trim", "type", "unit", "usedsampling", "usedweight",
        "variable", "write.csv", "xlab", "ylab", "content", "progress", "setDT", "%>%",
        "field_name", "form_placeholder", "help_text", "label", "list_name", "name required", "value"
      )
    )


}


