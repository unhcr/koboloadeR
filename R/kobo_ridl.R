


ridl <- function(action, ...) {
  httr::POST(glue::glue("https://ridl.unhcr.org/api/action/{action}"),
             httr::add_headers("Authorization" = Sys.getenv("RIDL_API_KEY")),
             body = rlang::list2(...)) %>% 
    httr::content() %>% 
    purrr::pluck("result")
}

#' @export
#' 
#' 
kobo_submit_ridl_resource <- function(pkg, ...) {
  metadata <- list(...)
  
  rid <- NULL
  if (!purrr::is_empty(pkg$resources)) {
    rid <- 
      purrr::transpose(pkg$resources) %>% 
      tibble::as_tibble() %>% 
      dplyr::filter(name == metadata$name) %>% 
      purrr::pluck("id", 1)
  }
  
  action <- if(purrr::is_empty(rid)) "resource_create" else "resource_update"
  
  r <- ridl(action, 
            !!!purrr::list_modify(metadata,
                                  package_id = pkg$name,
                                  clear_upload = "",
                                  url = fs::path_file(metadata$url),
                                  url_type = "upload"))
  
  rid <- r$id
  
  r <- ridl("cloudstorage_initiate_multipart",
            id = rid, name = fs::path_file(metadata$url), size = as.numeric(fs::file_size(metadata$url)))
  
  uid <- r$id
  
  r <- ridl("cloudstorage_upload_multipart",
            id = rid, uploadId = uid, partNumber = 1, upload = httr::upload_file(metadata$url))
  
  r <- ridl("cloudstorage_finish_multipart",
            id = rid, uploadId = uid, save_action = "go-dataset-complete")
}



#' @export
kobo_submit_ridl_package <- function(metadata, resources) {
  metadata <- metadata %>% dplyr::filter(!is.na(value))
  metadata <- 
    purrr::set_names(metadata$value, metadata$name) %>% 
    purrr::map2(metadata$type, ~if (stringr::str_detect(.y, "multi")) {stringr::str_split(.x, ", ")[[1]]} else {.x})
  
  pkg <- 
    ridl("package_search", q = glue::glue("name:{metadata$name}")) %>% 
    purrr::pluck("results", 1)
  
  action <- if (purrr::is_empty(pkg)) "package_create" else "package_update"
  
  pkg <- ridl(action, !!!metadata)
  
  purrr::pwalk(resources, kobo_submit_ridl_resource, pkg = pkg)
}



#' Quick explainer on how this works:
#'   
#'   You'll want to start by loading the form and data from KoBo:
#' 
#' formf <- 
#'   hcrdata::hcrfetch(
#'     src = "kobo",
#'     dataset = "My kobo project",
#'     file = "form.xlsx") 
#' 
#' dataf <- 
#'   hcrdata::hcrfetch(
#'     src = "kobo",
#'     dataset = "My kobo project",
#'     file = "data.json") 
#' 
#' Call kobo_prepare_form() to add the ridl-metadata sheets before launching the RIDL Metadata Editor RStudio Add-in (also accessible thru kobo_edit_ridl_metadata()).
#' 
#' While editing the sheet you'll have access to two variables ridl_container and ridl_dataset -exported from the data- which can be accessed from the ridl-metadata sheet by enclosing them in braces. The content of the braces is actually evaluated as an R expression with the two named variables in scope, so any necessary manipulations can be done there as well. The presence of the both variables in the data is optional - in which case the relevant information should be hard-coded in the ridl-metadata sheet and everything will end up in the same dataset/container.
#' 
#' Finally, when all is said and done, you can call kobo_submit_to_ridl(formf, dataf) to push the data to RIDL. The function takes care of massaging the submission into the format expected by RIDL. In case you'd like to submit treated data -anything other than the JSON KoBo export- you'll have to prepare the data in the right format manually. See resources.data and resources.meta in the kobo_ridl.R for details on the data structure.
#' @export kobo_submit_to_ridl

kobo_submit_to_ridl <- function(formf = "data/form.xlsx", dataf = "data/data.json") {
  metadata <- formf %>% readxl::read_excel(sheet = "ridl-metadata")
  
  data <- 
    jsonlite::fromJSON(dataf)$results %>% 
    tibble::as_tibble(.name_repair = ~stringr::str_replace_all(., "(\\/)", "."))
  
  prep_submission <- function(ridl_container = NULL, ridl_dataset = NULL, data) {
    # FIXME: add support for repeating groups
    dataf <- fs::path(tempdir(), "data.csv")
    data %>% 
      dplyr::select(tidyselect::vars_select_helpers$where(is.atomic)) %>% 
      readr::write_csv(dataf)
    
    resources.data <- 
      tibble::tibble(
        type = "data",
        url = dataf,
        name = "data.csv",
        description = "Raw extract from KoBo",
        format = "csv",
        file_type = "microdata",
        date_range_start = as.Date(data$start) %>% min() %>% as.character(),
        date_range_end = as.Date(data$end) %>% max() %>% as.character(),
        version = Sys.Date() %>% as.character(),
        `hxl-ated` = "False",
        process_status = "raw",
        identifiability = "personally_identifiable")
    
    resources.meta <- 
      tibble::tibble(
        type = "attachment",
        url = formf,
        name = "form.xlsx",
        description = "XLSform",
        format = "xls",
        file_type = "questionnaire")
    
    tibble::tibble(
      metadata = list(dplyr::mutate(dplyr::rowwise(metadata), value = glue::glue(value, .na = NULL))),
      resources = list(dplyr::bind_rows(resources.data, resources.meta)))
  }
  
  submissions <-
    data %>%
    tidyr::nest(data = -any_of(c("ridl_container", "ridl_dataset"))) %>% 
    # dplyr::filter(!is.na(ridl_container), !is.na(ridl_dataset)) %>% 
    purrr::pmap_dfr(prep_submission)
  
  submissions %>% purrr::pwalk(kobo_submit_ridl_package)
}
