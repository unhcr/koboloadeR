#' @name kobo_aggregate
#' @rdname kobo_aggregate
#' @title  Generate an aggregaation of the variable of a dataset for instance by admin level
#'
#'
#'
#' @param form The full filename of the form to be accessed (xls or xlsx file).
#' It is assumed that the form is stored in the data folder.
#' @param  aggregVar variable to use for aggregation
#'
#' @return No return, All results will be saved in the data folder
#'
#' @author Edouard Legoupil
#'
#'
#' @export kobo_aggregate
#'
#' @examples
#' \dontrun{
#' kobo_aggregate(form, mapref)
#' }
#'

kobo_aggregate <- function( form = "form.xlsx", 
                            aggregVar = "admin1") {
  
  # aggregVar = mapref
  require(tidyverse)
  ## Load all required packages ####
  kobo_load_packages()
  configInfo <- kobo_get_config(form)
  configInfo <- configInfo[!is.na(configInfo$name),]
  mainDir <- kobo_getMainDirectory()
  form_tmp <- paste(mainDir, "data", form, sep = "/", collapse = "/")

  
  ### Load the data ####
  cat("\n\n Loading data. It is assumed that the cleaning, weighting & re-encoding has been done previously \n")
  
  MainDataFrame <- utils::read.csv(paste(mainDir,"/data/MainDataFrame_edited.csv",sep = ""), encoding = "UTF-8", na.strings = "")
  
  ## Load form ###########
  cat("\n\n Loading dictionnary from the xlsform \n")
  dico <- utils::read.csv(paste0(mainDir,"/data/dico_",form,".csv"), encoding = "UTF-8", na.strings = "")
  #rm(form)
  
  ## Check that all those selectedVars are in the frame
  check <- as.data.frame(names(MainDataFrame))
  names(check)[1] <- "fullname"
  check$id <- row.names(check)
  check <- dplyr::left_join(x = check, y = dico, by = "fullname")
  
  ## Find index of variable to be used for aggregation
  
  MainDataFrame$aggregVar1 <- MainDataFrame[ , c(aggregVar)]
  
  # levels(as.factor(MainDataFrame$aggregVar1))
  
  ## 1. Numeric variables #####
  selected.mappoly.num <- dico[ which(!(is.na(dico$mappoly)) & dico$type %in% c("integer", "numeric" )), ]
  selected.mappoly.num <- dplyr::left_join(x = selected.mappoly.num, y = check, by = "fullname")
  selected.mappoly.num <- selected.mappoly.num[!is.na(selected.mappoly.num$id), ]
  selected.mappoly.numVars <- as.character(selected.mappoly.num[ , c("fullname")])
  
  if( length(selected.mappoly.numVars) > 0 ) {
  
  ## Let's do some top coding to eliminate data errors... in numeric variable
  # outlier values based on boxplot.stats with coef 3 are set to NA
  # for (i in 1:length(selected.mappoly.numVars)) {
  #   MainDataFrame[ , selected.mappoly.numVars[i]][ MainDataFrame[ , selected.mappoly.numVars[i]] > min(grDevices::boxplot.stats(MainDataFrame[ , selected.mappoly.numVars[i]], coef = 3)$out)] <- NA
  #   #cat(min(grDevices::boxplot.stats(MainDataFrame[ , selected.mappoly.numVars[i]], coef = 3)$out))
  # }
  
  #grDevices::boxplot.stats(MainDataFrame$MainDataFrame_information.Family_Size, coef = 3)$out
  #grDevices::boxplot.stats(MainDataFrame$Housing.rooms_at_the_residence, coef = 3)$out

  ##### Sub setting numeric frame
  # Create subset of file with observation and selected variables & remove duplicated rows based on IDH
  datamappoly1.num <- MainDataFrame[ , c( "aggregVar1", selected.mappoly.numVars)]
  
  ## Convert to numeric variable in case they would be factor...
  #datamappoly.num[,c(selected.mappoly.numVars)] <- lapply(datamappoly.num[,c(selected.mappoly.numVars)], numeric)
  
  ## Aggregate numeric value  based on mean ####
  datamappoly1.num2 <- datamappoly1.num %>%
    dplyr::group_by(aggregVar1) %>%
    dplyr::summarise_all(dplyr::funs(mean(., na.rm = TRUE))) %>%
    dplyr::mutate_if(is.numeric, dplyr::funs(round(., 2)))
  
  datamappoly1.num2 <- as.data.frame(datamappoly1.num2)
  
  }
  
  
  ## 2. Counting observation per aggregation variable ####
  # datamappoly1.n <-  MainDataFrame %>% 
  #                     dplyr::group_by(aggregVar1) %>% 
  #                     dplyr::tally()
  #                     #plyr::mutate(., count = n())
  
  datamappoly1.n <-  stats::aggregate( . ~ aggregVar1, MainDataFrame, length)
  #datamappoly1.n <- count(MainDataFrame, c("aggregVar1"))
  names(datamappoly1.n)[1] <- "aggregVar1"
  names(datamappoly1.n)[2] <- "count"
  
  datamappoly1.n <- as.data.frame(unique(datamappoly1.n[ ,c("aggregVar1", "count")]))
  
  
  
  ## 3. Categoric variables #####
  selected.mappoly.cat <- dico[ which(!(is.na(dico$mappoly)) & dico$type %in% c("select_one", "select_multiple" )), ]
  selected.mappoly.cat <- dplyr::left_join(x = selected.mappoly.cat, y = check, by = "fullname")
  selected.mappoly.cat <- selected.mappoly.cat[!is.na(selected.mappoly.cat$id), ]
  selected.mappoly.catVars <- as.character(selected.mappoly.cat[ , c("fullname")])
  
  #selected.mappoly.cat.multi <- dico[ which(!(is.na(dico$mappoly)) & dico$type %in% c( "select_multiple" )), ]
  #selected.mappoly.cat.multi$fullname2 <- paste0(selected.mappoly.cat.multi$fullname,".1")

  # Create subset of file with observation and selected variables & remove duplicated rows based on IDH
  datamappoly1.cat <- MainDataFrame[ , c( "aggregVar1", selected.mappoly.catVars)]
  
  ## Convert to factor variable as they are categoric
  datamappoly1.cat[,c(selected.mappoly.catVars)] <- lapply(datamappoly1.cat[,c(selected.mappoly.catVars)], factor)
  ## Hot coding variable
  datamappoly1.cat <- dummies::dummy.data.frame(datamappoly1.cat, names = selected.mappoly.catVars, sep = ".")
  
  ### Aggregate categoric value  based on sum ####
  datamappoly1.cat2 <- datamappoly1.cat %>%
    dplyr::group_by( aggregVar1 ) %>%
    dplyr::summarise_all(dplyr::funs(sum)) %>%
    dplyr::mutate_if(is.numeric, dplyr::funs(round(., 2)))
  
  
  
  
  ### Renaming select multiple variable
  #for (i in 1:ncol(datamappoly1.cat2)) {
  #  if (names(datamappoly1.cat2)[i] %in% as.character(selected.mappoly.cat.multi$fullname2)) {
  #    names(datamappoly1.cat2)[i] = as.character(selected.mappoly.cat.multi$fullname[match(names(datamappoly1.cat2)[i], selected.mappoly.cat.multi$fullname2)])
  #  }
  #}
  
  datamappoly1.cat2 <- as.data.frame(datamappoly1.cat2)
  
  ## Add count
  datamappoly1 <- dplyr::left_join(x = datamappoly1.n, y = datamappoly1.cat2, by = "aggregVar1")
  
  ## Create Rounded ratio 
  for (i in 3:ncol(datamappoly1)) { datamappoly1[i] <- round(datamappoly1[i]/datamappoly1$count,2) }
  
  ## Bind categoric and numeric aggregation #####
  if ( length(selected.mappoly.numVars) > 0) {
  datamappoly1 <- dplyr::left_join(x = datamappoly1, y = datamappoly1.num2, by = "aggregVar1")
  }
  
  ## Remove variable corresponding to NA - not in dico
  check2 <- as.data.frame(names(datamappoly1))
  names(check2)[1] <- "fullname"
  check2$id <- row.names(check2)
  check2 <- dplyr::left_join(x = check2, y = dico, by = "fullname")
  check3 <- c("aggregVar1","count" ,as.character(check2[ !(is.na(check2$name)), c("fullname")]))
  
  datamappoly1 <- datamappoly1[ , c( check3)]
  
  ## Trying to get shorter name..
  #names(datamappoly2) <- str_sub(names(datamappoly2), start = -8)
  
  ## Label to check
  datamappoly1L <- kobo_label(datamappoly1, dico)
  
  ## We now save a back up in the data folder to be used for the Rmd
  write.csv(datamappoly1L,"data/MainDataFrame_edited_map.csv", row.names = FALSE, na = "")
  #rm(datamappoly1.cat, datamappoly1.cat2, datamappoly1.n, datamappoly1.num, datamappoly1.num2, datamappoly1)
  
  
  
  
  # ## getting correct district and gov from coordinates
  # campdistrict <- readOGR("data/geo/campdistrict3.geojson")
  # 
  # ## Fortify
  # campdistrict.fort <- fortify(campdistrict, region = aggregVar)
  # datamappoly1L <- datamappoly1L[!(is.na(datamappoly1L$aggregVar1)), ]
  # datamappoly1L$id <- datamappoly1L$aggregVar1
  # campdistrict.map.fort <- merge( x = campdistrict.fort, y = datamappoly1L, by = "id")
  

}
NULL


