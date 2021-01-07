#' @name kobo_ddi
#' @rdname kobo_ddi
#' @title DDI generation
#'
#' @description This function creates a DDI version 2.5, XML file structure for microdata library
#' @param form The full filename of the form to be accessed (xls or xlsx file).
#' It is assumed that the form is stored in the data folder.
#'
#' @param app The place where the function has been executed, the default is the console and the second option is the shiny app
#'
#' @return DDI version 2.5, XML file structure will saved under out/ddi
#'
#' @author Maher Daoud
#'
#'
#' @examples
#' \dontrun{
#' kobo_ddi("myform.xlsx")
#' }
#'
#' @export kobo_ddi
#'

kobo_ddi <- function(form = "form.xlsx", app="console") {
  tryCatch({
     if (app == "shiny") {
      progress <- shiny::Progress$new()
      progress$set(message = "Generating DDI XML file in progress...", value = 0)
      on.exit(progress$close())
      updateProgress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value) / 5
        }
        progress$set(value = value, detail = detail)
      }
      updateProgress()
    }

    mainDir <- kobo_getMainDirectory()
    form_tmp <- paste(mainDir, "data", form, sep = "/", collapse = "/")
    dico <- read.csv(paste0(mainDir,"/data/dico_",form,".csv"), encoding = "UTF-8", na.strings = "")

    survey <- tryCatch({
      as.data.frame(read_excel(form_tmp, sheet = "survey"),
                    stringsAsFactors = FALSE) #read survey sheet from the form
    },
    error = function(err) {
      data.frame( #if it doesn't exist, we need to create empty dataframe with those fields
        type = character(),
        name = character(),
        label = character(),
        "labelReport" = character(),
        variable = character(),
        disaggregation = character(),
        chapter = character(),
        structuralequation.risk = character(),
        structuralequation.coping = character(),
        structuralequation.resilience = character(),
        anonymise = character(),
        correlate = character(),
        clean = character(),
        cluster = character(),
        predict = character(),
        mappoint = character(),
        mappoly = character(),
        stringsAsFactors = FALSE,
        check.names = F
      )
    })
     if (app == "shiny") {
      updateProgress()
    }
    survey <- survey[!is.na(survey$chapter),]
    survey <- survey[c("type","name","labelReport","variable")]
    survey <- survey[!startsWith(tolower(survey$type), "select_multiple"),]
    survey <- survey[tolower(survey$type) != "begin_repeat" & tolower(survey$type) != "begin repeat" & tolower(survey$type) != "begin-repeat" &
                       tolower(survey$type) != "end_repeat" & tolower(survey$type) != "end repeat" & tolower(survey$type) != "end-repeat"
                     ,]

    survey$listname <- sapply(survey[,"type"], function(x) {
      strsplit(x," ")[[1]][2]
    }, simplify = TRUE, USE.NAMES = FALSE)
     if (app == "shiny") {
      updateProgress()
    }

    indicator <- tryCatch({
      as.data.frame(read_excel(form_tmp, sheet = "indicator"),stringsAsFactors = FALSE)
    },
    error = function(err) {
      data.frame(
        type = character(),
        fullname = character(),
        label = character(),
        chapter = character(),
        disaggregation = character(),
        correlate = character(),
        sensitive = character(),
        anonymise = character(),
        cluster = character(),
        predict = character(),
        variable = character(),
        mappoint = character(),
        mappoly = character(),
        structuralequation = character(),
        frame = character(),
        listname = character(),
        calculation = character(),
        stringsAsFactors = FALSE
      )
    })
    if (app == "shiny") {
      updateProgress()
    }


    indicator <- indicator[c("type","fullname","label","variable","listname")]
    names(indicator) <- c("type","name","labelReport","variable","listname")


    choices <- tryCatch({
      as.data.frame(read_excel(form_tmp, sheet = "choices"),
                    stringsAsFactors = FALSE) #read survey sheet from the form
    },
    error = function(err) {
      data.frame( #if it doesn't exist, we need to create empty dataframe with those fields
        list_name = character(),
        name = character(),
        label = character(),
        labelReport = character(),
        order = character(),
        stringsAsFactors = FALSE,
        check.names = F
      )
    })
    if ( app == "shiny" ) {
      updateProgress()
    }


    dicoSelectMultiple <- dico[dico$type == "select_multiple",c("type","fullname","label","variable","listname")]
    names(dicoSelectMultiple) <- c("type","name","labelReport","variable","listname")


    var_ind <- rbind(survey, indicator ,dicoSelectMultiple)

    dataDscr <- list()
    dataDscrNames <- c()

    for (i in 1:nrow(var_ind)) {
      if (startsWith(tolower(var_ind[i,"type"]), "select_one")) {
        ln <- var_ind[i,"listname"]
        val <- choices[choices$list_name == ln, c("name", "label")]
        val <- val[!is.na(val$name),]
        namesVal <- c()
        labelsVal <- c()
        for (j in 1:nrow(val)) {
          namesVal <- c(namesVal,val[j,"name"])
          labelsVal <- c(labelsVal,val[j,"label"])
        }
        names(namesVal) <- labelsVal
        dataDscr[[var_ind[i,"name"]]] = list(
          label = var_ind[i,"labelReport"],
          type = "char",
          measurement = var_ind[i,"variable"],
          values = namesVal
        )
        dataDscrNames <- c(dataDscrNames, var_ind[i,"name"])
      } else if (startsWith(tolower(var_ind[i,"type"]), "select_multiple")) {
        ln <- var_ind[i,"listname"]
        val <- choices[choices$list_name == ln, c("name", "label")]
        val <- val[!is.na(val$name),]
        namesVal <- c()
        labelsVal <- c()
        for (j in 1:nrow(val)) {
          namesVal <- c(namesVal,val[j,"name"])
          labelsVal <- c(labelsVal,val[j,"label"])
        }
        names(namesVal) <- labelsVal
        dataDscr[[var_ind[i,"name"]]] = list(
          label = var_ind[i,"labelReport"],
          type = "char",
          measurement = var_ind[i,"variable"],
          values = namesVal
        )
        dataDscrNames <- c(dataDscrNames, var_ind[i,"name"])
      }else{
        if (var_ind[i,"type"] %in% c("integer","numeric")) {
          tp <- "num"
        } else {
          tp <- "char"
        }
        dataDscr[[var_ind[i,"name"]]] = list(
          label = var_ind[i,"labelReport"],
          type = tp,
          measurement = var_ind[i,"variable"]
        )
        dataDscrNames <- c(dataDscrNames, var_ind[i,"name"])
      }
    }
    if (app == "shiny") {
      updateProgress()
    }

    ####################### MAIN DATAFRAME #####################
    configInfo <- kobo_get_config(form)
    configInfo <- configInfo[startsWith(tolower(configInfo$name), "instanceid"),]
    configInfo <- configInfo[!is.na(configInfo$name),]
    levelsOfDF <- kobo_get_dataframes_levels(form)
    levelsOfDF <- levelsOfDF[levelsOfDF$name != "MainDataFrame",]
    levelsOfDF[levelsOfDF$parent == "MainDataFrame","parent"] <- "household"
    levelsOfDF$flag <- T
    levelsOfDF <- levelsOfDF[order(desc(levelsOfDF$level), desc(levelsOfDF$parent)),]

    count <- 0
    for (dbr in levelsOfDF$name) {
      if (levelsOfDF[levelsOfDF$name == dbr, "flag"] == F) {
        next
      }
      count  <- count + 1
      dataFrame <- read.csv(paste(mainDir,"/data/",dbr,"-edited.csv",sep = ""),stringsAsFactors = F)
      dataFrame <- kobo_split_multiple(dataFrame, dico)
      #dataFrame <- kobo_clean(dataFrame, dico)
      dataFrame <- kobo_label(dataFrame, dico)

      child <- levelsOfDF[levelsOfDF$name == dbr, "name"]
      parent <- levelsOfDF[levelsOfDF$name == dbr, "parent"]
      while (T) {
        cat("Join child dataframe", child, "with the parent", parent)
        instanceIDChild  <- configInfo[tolower(configInfo$name) == tolower(paste0("instanceid_",child,"_",
                                                                                  ifelse(parent == "household","MainDataFrame",parent))), "value"]
        instanceIDParent <- configInfo[tolower(configInfo$name) == tolower(paste0("instanceid_",
                                                                                  ifelse(parent == "household","MainDataFrame",parent),"_",child)), "value"]
        parentDf <- read.csv(paste(mainDir,"/data/",parent,".csv",sep = ""),stringsAsFactors = F)
        unColChild <- dataFrame[,instanceIDChild]
        dataFrame <- dataFrame[,colnames(dataFrame) != instanceIDChild]
        unCN <- colnames(dataFrame)[!colnames(dataFrame) %in% colnames(parentDf)]
        unCN <- c(instanceIDChild, unCN)
        dataFrame[instanceIDChild] <- unColChild
        dataFrame <- dataFrame[ unCN ]
        dataFrame <- plyr::join(x = parentDf, y = dataFrame, by = setNames(instanceIDParent, instanceIDChild), type = "left")
        if (parent == "household") {
          break
        }else{
          child <- levelsOfDF[levelsOfDF$name == parent, "name"]
          parent <- levelsOfDF[levelsOfDF$name == parent, "parent"]
          levelsOfDF[levelsOfDF$name == parent, "flag"] <- F
        }

      }
      write.csv(dataFrame,paste(mainDir,"/data/group",count,".csv",sep = ""), row.names = FALSE, na = "")
    }

    allframes <- read.csv(paste(mainDir,"/data/group",count,".csv",sep = ""), stringsAsFactors = F)
    count <- count - 1
    while (count) {
      temp <- read.csv(paste(mainDir,"/data/group",count,".csv",sep = ""), stringsAsFactors = F)

      unColChild <- temp[,"responseID"]
      temp <- temp[,colnames(temp) != instanceIDChild]
      unCN <- colnames(temp)[!colnames(temp) %in% colnames(allframes)]
      unCN <- c("responseID", unCN)
      temp[instanceIDChild] <- unColChild
      temp <- temp[ unCN ]

      allframes <- plyr::join(x = allframes, y = temp, by = "responseID", type = "full")
      count <- count - 1
    }

    ############################################################

    dataDscrNames <- dataDscrNames[dataDscrNames %in% colnames(allframes)]
    dataDscr <- dataDscr[dataDscrNames]
    allframes <- allframes[dataDscrNames]

    codeBook <- list(
      fileDscr = list(
        datafile = allframes
      ),
      dataDscr = dataDscr
    )
    DDIwR::exportDDI(codeBook, file = paste0(mainDir,paste0("/out/ddi/ddi_output-",Sys.Date(),".xml")))

  }, error = function(err) {
    print("kobo_ddi_ERROR")
    return(structure(err, class = "try-error"))
  })
}
