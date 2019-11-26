#' @name kobo_weight
#' @rdname kobo_weight
#' @title  Weight a datset

#' @description  Automatically weight the data according to the information of 0-config.R
#' @param mainDir Path to the project's working directory: mainly for shiny app
#' @author Elliott Messeiller
#'
#'
#' @export kobo_weight
#'
#' @examples
#' \dontrun{
#' kobo_weight()
#' }
#'
#'

kobo_weight <- function(mainDir = '') {
  if (mainDir == '') {
    mainDir <- getwd()
  }

  source(paste0(mainDir, "/code/0-config.R"), local = TRUE)

  sampling <- readxl::read_excel(path.to.form, sheet = "sampling_frame")


  data$weight <- ""

  if (usedweight == 'sampling_frame') {
    if (usedsampling == '2_stages') {
      stratas <- sampling

      stratified <- unique(sampling$strata)

      name_col_sample <- names(sampling)
      normal_col <- c("id_sampl", "Survey","pop", "psu","SumDist","proba","survey_buffer")

      if (sum(normal_col %in% name_col_sample) == length(normal_col)) {
        if (length(stratified) > 1) {
          normal_col <- c("X__1", "id_sampl","Survey","pop","psu","SumDist","proba","survey_buffer")
          stratas <- unique(stratas[setdiff(names(stratas), normal_col)])
        }

        if (length(stratified) == 1) {
          normal_col <- c("X__1","id_sampl","Survey","strata","pop","psu","SumDist","proba","survey_buffer")
          stratas <- unique(stratas[setdiff(names(stratas), normal_col)])

        }

        if (length(unique(stratas$strata %in% dico$name)) == 1) {
          strat_row_n <- match(stratas[1, "strata"], dico$name)

          fullname_strata <- as.character(dico[strat_row_n, "fullname"])
          fullname_strata <- data.frame(strsplit(fullname_strata, "\\."))
          fullname_strata <- data.frame(fullname_strata[-nrow(fullname_strata), ])
          fullname_strata <- as.character(fullname_strata[nrow(fullname_strata), ])

          names(stratas)[names(stratas) == "strata"] <- fullname_strata
          names(sampling)[names(sampling) == "strata"] <- fullname_strata

          fullname_strata <- as.character(dico[strat_row_n,"qlevel"])
        }

        col_stratas <- data.frame(colnames(stratas), stringsAsFactors = FALSE)
        nrow_su <- data.frame(Strata=character(), nsu = numeric(), stringsAsFactors = FALSE)

        for (j in 1:nrow(col_stratas)) {
          split_temp <- as.character(col_stratas[j,1])
          nrow_su[j,"Strata"] <- split_temp
          nrow_su[j, "nsu"] <- nrow(unique(sampling[split_temp]))
          names(stratas)[names(stratas) == split_temp] <- as.character(dico[dico$name == split_temp, c("fullname"), ])
          names(sampling)[names(sampling) == split_temp] <- as.character(dico[dico$name == split_temp, c("fullname"), ])
        }
        psu_name <- nrow_su[which.max(nrow_su$nsu),"Strata"]
        psu_fullname <- as.character(dico[dico$name==psu_name, c("fullname"),])

        n_col_stratas <- ncol(stratas)
        names_stratas <- names(stratas)
        sampling$actual_sample <- ""
        sampling$weight <- ""
        tot_pop <- sum(sampling$pop)

        for (j in 1:nrow(sampling)) {
          v_psu <- as.character(sampling[j,psu_fullname])
          sampling[j, "actual_sample"] <- as.numeric(sum(data[, psu_fullname] == v_psu))
        }

        sampling$actual_sample <- as.numeric(sampling$actual_sample)
        tot_sample <- sum(sampling$actual_sample)

        strata_pop <- data.frame(strata=character(),pop=integer(),sample=integer(), weight=integer(),stringsAsFactors = F)
        for (k in 1:length(stratified)){
          strata_pop[k,"strata"] <- stratified[k]
          strata_pop[k, "pop"] <-  sum(sampling[which(sampling[,fullname_strata]==stratified[k]),"pop"])
          strata_pop[k, "sample"] <-  sum(sampling[which(sampling[,fullname_strata]==stratified[k]),"actual_sample"])
          strata_pop[k, "weight"] <-  (strata_pop[k, "pop"] / tot_pop) / (strata_pop[k, "sample"] / tot_sample)

        }

        for (i in 1:nrow(data)) {
          stratum <- as.character(data[i,fullname_strata])
          result <- as.numeric(strata_pop[strata_pop$strata == stratum, c("weight"), ])
          data[i, "weight"] <- result
        }


        data$weight <- as.numeric(data$weight)
        surveydesign <- survey::svydesign(
          ids =  ~ 1,
          strata = data[[fullname_strata]],
          weights = ~ weight,
          data = data
        )
        pastedesign <- paste0("survey::svydesign(ids=~1,
                                strata= data[[strata1]],
                                weights= ~weight,
                                data=data)")


        # weight2dico <- data.frame(matrix("weight", ncol = 13))
        # names(weight2dico) <-
        #   c(
        #     "type",
        #     "name",
        #     "fullname",
        #     "label",
        #     "disaggregation",
        #     "correlate",
        #     "listname",
        #     "qlevel",
        #     "qgroup",
        #     "labelchoice",
        #     "ordinal",
        #     "weight",
        #     "formpart"
        #   )
        # dico <- rbind(dico, weight2dico)
        #
        # # Rewritting dico file
        # write.csv(
        #   dico,
        #   paste0(mainDir,"/data/dico_", form, ".csv"),
        #   row.names = FALSE,
        #   na = ""
        # )
        #
        # Coherce data to a clean dataframe
        data <- data.frame(data)

        #Write data with weights
        write.csv(data, file = paste0(mainDir,"/data/data.csv"))

        path.to.data <- paste0(mainDir, "/data/data.csv")

        #Fetching the directory
        #Path to file
        configfile <- paste(mainDir, "/code/0-config.R", sep = "")
        #Writting file
        sink(configfile, append = TRUE)

        cat("\n")
        cat(paste0('data <- read.csv("',path.to.data,'")'))

        cat("\n")
        cat(paste('strata1 <- "', fullname_strata, '"', sep = ""))
        cat("\n")


        cat(paste('design <- ', pastedesign, sep = ""))
        cat("\n")


        sink()
      }
      else{
        cat("You didn't use the R Sampling tool or your sampling frame is not valid. ")
        cat(
          "Copy paste the input of https://oliviercecchi.shinyapps.io/R_sampling_tool_v2/ in the sampling_frame sheet "
        )

      }
    }

    if (usedsampling == "cluster_sampling") {

    }
    if (usedsampling == "simple_random") {

    }


  }

  if (usedweight == "custom") {

  }
}
NULL
