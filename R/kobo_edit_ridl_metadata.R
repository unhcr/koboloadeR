#' @name kobo_edit_ridl_metadata
#' @rdname kobo_edit_ridl_metadata
#' @title  Edit RIDL / CKAN metadata
#'
#' @description  Edit RIDL / CKAN metadata
#'
#' @param form The full filename of the form to be accessed (xls file).
#' It is assumed that the form is stored in the data folder.
#'
#'
#' @author Hisham Galal
#'
#'
#' @examples
#' \dontrun{
#' kobo_edit_ridl_metadata(form = "form.xlsx")
#' }
#'
#' @export kobo_edit_ridl_metadata
#'


kobo_edit_ridl_metadata <- function(form = "data/form.xlsx") {
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar(
      "RIDL Metadata",
      left = miniUI::miniTitleBarCancelButton(),
      right = miniUI::miniTitleBarButton("done", "Done", primary = TRUE)),
    miniUI::miniContentPanel(shiny::div(id = "form"))
  )
  
  server <- function(input, output, session) {
    if (! "ridl-metadata" %in% readxl::excel_sheets(form) |
        ! "ridl-choices" %in% readxl::excel_sheets(form))
      stop("Please run kobo_prepare_form() first. Aborting...")
    
    ridl_metadata <- readxl::read_excel(form, sheet = "ridl-metadata")
    ridl_choices <- readxl::read_excel(form, sheet = "ridl-choices")
    
    as.input <- function(...) {
      if (list(...)$type == "text") {
        f <- 
          if (stringr::str_detect(list(...)$name, "notes")) 
            shiny::textAreaInput
        else 
          shiny::textInput
        
        do.call(f, list(inputId = list(...)$name, 
                        label = 
                          shiny::span(list(...)$label, 
                                      shiny::span(dplyr::if_else(!is.na(list(...)$required), "*", ""),
                                                  style = "color: red;",
                                                  .noWS = "before"),
                                      .noWS = "inside"),
                        value = if (!is.na(list(...)$value)) list(...)$value else "",
                        placeholder = list(...)$hint))
      } else {
        shiny::selectizeInput(
          inputId = list(...)$name,
          label = 
            shiny::span(list(...)$label, 
                        shiny::span(dplyr::if_else(!is.na(list(...)$required), "*", ""),
                                    style = "color: red;",
                                    .noWS = "before"),
                        .noWS = "inside"),
          choices = 
            ridl_choices %>% 
            dplyr::filter(list_name == list(...)$name) %>% 
            { purrr::set_names(.$name, .$label) } %>% 
            { c("Blank" = "", .) },
          selected = 
            if(is.na(list(...)$value))
              NULL
          else 
            stringr::str_split(list(...)$value, pattern = "\\s*,\\s*")[[1]],
          multiple = stringr::str_detect(list(...)$type, "multiple")
        )
      }
    }
    
    purrr::pmap(ridl_metadata, as.input) %>% 
      purrr::walk(~shiny::insertUI("#form", "beforeEnd", .))
    
    shiny::observeEvent(input$done, {
      inputs <- 
        input %>% 
        shiny::reactiveValuesToList() %>% 
        tibble::enframe() %>% 
        dplyr::filter(name != "cancel", name != "done") %>% 
        dplyr::mutate(
          value = 
            purrr::map_chr(value,
                           ~dplyr::if_else(is.null(.), '', stringr::str_c(., collapse = ", "))))
                           # ~stringr::str_c(dplyr::if_else(is.null(.), '', .), sep = ", ")))
      
      ridl_metadata <- 
        ridl_metadata %>% 
        dplyr::select(-value) %>% 
        dplyr::left_join(inputs, by = "name") %>% 
        as.data.frame()
      
      wb <- openopenxlsx::loadWorkbook(form)
      sheetname <- "ridl-metadata"
      if (!is.null(openxlsx::names(wb)[[sheetname]]))
        openxlsx::removeWorksheet(wb, sheetname)
      
      openxlsx::addWorksheet(wb, sheetname) 
      openxlsx::writeData(wb, sheetname, ridl_metadata, withFilter = TRUE)
      
      openxlsx::setColWidths(wb, sheetname, cols = 1:ncol(ridl_metadata), widths = "auto")
      
      headerSt <- 
        openxlsx::createStyle(
          textDecoration = "bold", fontColour = "white", fontSize = 13, 
          fgFill = "grey50",
          border = "TopBottom", borderColour = "grey80", borderStyle = "thin")
      
      openxlsx::addStyle(wb, sheetname, headerSt, hdr.rows, 1:ncol(ridl_metadata), gridExpand = TRUE)
      
      if (file.exists(form)) file.remove(form)
      openxlsx::saveWorkbook(wb, form)
      
      shiny::stopApp(TRUE)
    })
  }
  
  shiny::runGadget(shiny::shinyApp(ui, server), viewer = shiny::paneViewer())
}
