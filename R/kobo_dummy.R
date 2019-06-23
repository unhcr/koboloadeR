#' @name kobo_dummy
#' @rdname kobo_dummy
#' @title  Create a dummy dataset
#'
#' @description  Automatically produce an dummy dataset in line with the structure of an xlsform.
#'
#' Making decisions about research design and analysis strategies is often difficult before data is collected,
#' because it is hard to imagine the exact form data will take.
#' This function helps imagine what data will look like before they collect it.
#' samplesize is set per defautl at 500 records
#'
#'  Supported Features
#'
#' - Gnerate a data set with an output similar to the one needed in koboloader
#' - respects ODK structure "`relevant`" skip logic (Some advanced functionality such as "coalesce()" not covered)
#'   "`constraint`" and "`repeat`"
#' - adds InstandID column to link hierearchical data based on "`repeat_count`"
#'
#' @param  dico file representing the xlsform data dictionnary - generated from kobo_dico()
#'
#' @author Edouard Legoupil
#'
#' @examples
#' kobo_dummy()
#'
#' @export kobo_dummy
#'
#' @examples
#' \dontrun{
#' kobo_dummy(form)
#' }
#'

kobo_dummy <- function(form = "form.xls") {

  samplesize <- 381

  ### Write dummy dataset
  #kobodevtools::install_github("ropensci/charlatan")
  #devtools::install_github("ThinkR-open/fakir")
  # install.packages("truncnorm")
  # install.packages("stringi")
  # install.packages("OpenRepGrid")
  # install.packages("sp")
  # library(charlatan)
  # library(fakir)
  # library(tidyverse)
  # library(truncnorm)
  # library(stringi)
  # library(OpenRepGrid)
  # library(sp)

  mainDir <- kobo_getMainDirectory()

  #form_tmp <- paste(mainDir, "data", form, sep = "/", collapse = "/")
  #form <- "form.xls"
  #library(koboloadeR)
  kobo_dico(form)
  # dico <- read.csv("data/dico_form.xls.csv")
  #dico <- paste(mainDir, "data", dico, sep = "/", collapse = "/")
  dico <- read.csv(paste0(mainDir, "/data/dico_", form, ".csv"))

  ## Extract constraint on data ###########
  ## From constraint  lower bounds &  upper bound
  ## pattern on relevant

  dico$lowerbound <- ""
  dico$upperbound <- ""
  dico$relevantifvar <- ""
  dico$relevantifvalue <- ""

  for (i in 1:nrow(dico)) {
    # i <- 39
    # i <- 11
    var1 <- as.character(dico[i, c("name")])
    bound <- as.character(dico[i, c("constraint")])
    # levels(as.factor(dico$constraint))
    relevant <- as.character(dico[i, c("relevant")])
    # levels(as.factor(dico$relevant))
    cat( paste0(var1 ," / bound = ", bound, " / relevant = ",  relevant, "\n"))

    if (!(is.na(bound)) ) {
      detectlow <- as.data.frame(rbind( str_locate(bound, ".>"),
                                        str_locate(bound, ".>="),
                                        str_locate(bound, ".> "),
                                        str_locate(bound, ".>= ")))
      detectlow <- as.numeric(max(detectlow$end, na.rm = TRUE))
      detectlowzero <- as.data.frame(str_locate(substr(bound, detectlow + 1,nchar(bound)), " "))
      detectlowzero <- ifelse( is.na(detectlowzero$start),
                               nchar(bound),
                               as.numeric(min(detectlowzero$start, na.rm = TRUE)))

      dico[i, c("lowerbound")] <- substr(bound, detectlow + 1, detectlow + detectlowzero )

      detecthigh <- as.data.frame(rbind( str_locate(bound, ".<"),
                                         str_locate(bound, ".<="),
                                         str_locate(bound, ".< "),
                                         str_locate(bound, ".<= ")))
      detecthigh <- as.numeric(max(detecthigh$end, na.rm = TRUE))
      detecthighzero <- as.data.frame(str_locate(substr(bound, detecthigh + 1,nchar(bound)), " "))
      #detecthighzero <- as.numeric(min(detecthighzero$end, na.rm = TRUE))

      detecthighzero <- ifelse( is.na(detecthighzero$start),
                                nchar(bound),
                                as.numeric(min(detecthighzero$start, na.rm = TRUE)))

      dico[i, c("upperbound")] <- substr(bound, detecthigh + 1, detecthigh + detecthighzero )
    }

    if ( !(is.na(relevant)) & relevant != "" ) {
      # selected(${
      detectrelevant1 <- as.data.frame(str_locate(relevant, "\\{"))
      detectrelevant1 <- as.numeric(max(detectrelevant1$end, na.rm = TRUE))
      # },'
      detectrelevant2 <- as.data.frame(str_locate(relevant, "\\}"))
      detectrelevant2 <- as.numeric(max(detectrelevant2$end, na.rm = TRUE))
      # ')
      detectrelevant3 <- as.data.frame(str_locate(relevant, "\\)"))
      detectrelevant3 <- as.numeric(max(detectrelevant3$end, na.rm = TRUE))
      dico[i, c("relevantifvar")] <- substr(relevant, detectrelevant1 + 1, detectrelevant2 - 1 )
      dico[i, c("relevantifvalue")] <- substr(relevant, detectrelevant2 + 3, detectrelevant3 - 2 )
    }
  }

  ## Remove relevant when relevant value is null
  for (i in 1:nrow(dico)) {
    dico[ i, c("relevantifvar")] <- ifelse( is.na(dico[ i, c("relevantifvalue")]), "",
                                            paste(dico[ i, c("relevantifvar")]) )
  }


  rm(bound, detecthigh, detecthighzero, detectlow,  detectlowzero, detectrelevant1, detectrelevant2 , detectrelevant3, i, relevant, var1  )


  ## Setting up pattern for UNHCR cases ####################

  # http://buildregex.com/
  # https://spannbaueradam.shinyapps.io/r_regex_tester/
  # https://r4ds.had.co.nz/strings.html
  # https://stringr.tidyverse.org/articles/regular-expressions.html
  # https://stat545.com/block022_regular-expression.html

  # dummydata$UNHCRCaseNo <- stri_rand_strings(n = samplesize,
  #                                            length = 4,
  #                                            #  pattern = "(LEB)|(leb)|(0-9)]{3}-[0-9]{2}[c|C][0-9]{5}")
  #                                            pattern = "^LEB|leb[0-9])$")




  ## From relevant - remove dummy data when required


  ## Generate sampling universe - dummy registration data ####


  ## First get all variables at household level #######

  dico.household <- dico[(dico$qrepeatlabel == "MainDataFrame" &
                            dico$formpart == "questions" &
                            !(dico$type %in% c("note","end",
                                               "begin_group", "end_group",
                                               "begin group", "end group",
                                               "begin_repeat", "end_repeat",
                                               "begin repeat", "end repeat"))), ]

  #levels(as.factor(as.character(dico.household$type)))



  ## Create a polygon to sample GPS coordinate from ########

  # Make a set of coordinates that represent vertices with longitude and latitude
  x_coords <- c(34.93,34.93,39.2,39.2,34.93)
  y_coords <- c(29.2,33.4,33.4,29.2,29.2)

  box1 <- sp::Polygon(cbind(x_coords,y_coords))
  box2 <- sp::Polygons(list(box1), ID = "A")
  BoxSpatialPoly <- sp::SpatialPolygons(list(box2))
  rm(x_coords, y_coords, box1 , box2)

  ## Create corresponding dummy data ########



  ## generate the unique ID for each observation
  dummydata <- data.frame(stri_rand_strings(samplesize, 8))
  names(dummydata)[1] <- "instanceID"

  cat("Generating household table")
  for (i in 1:nrow(dico.household) ) {
    # i <- 1
    # i <- 106
    fullname <- as.character(dico.household[i, c("fullname")])
    typedata <- as.character(dico.household[dico.household$fullname == fullname, c("type")])
    relevantifvar <- as.character(dico.household[dico.household$fullname == fullname, c("relevantifvar")])
    relevantifvar2 <- as.character(dico.household[dico.household$name == relevantifvar, c("fullname")])
    relevantifvalue <- as.character(dico.household[dico.household$fullname == fullname, c("relevantifvalue")])
    cat(paste0("Entering summy data for variable ", i, "- ", fullname, " / ", typedata, " / ", relevantifvar,"\n"))

    ### case to handle
    #  "imei"   "deviceid"       "phonenumber"
    if (typedata %in% c("imei", "deviceid",  "phonenumber") ) {
      dummydata[ , i + 1] <- stri_rand_strings(n = samplesize, 8)
    }
    #  "date"  "today"   "start"
    if (typedata %in% c("date", "today", "start") ) {
      # dummydata[ , i + 1] <- as.Date( dummydata[ , i + 1])
      dummydata[ , i + 1] <- sample(seq(as.Date('1919/01/01'), as.Date('2019/01/01'), by = "day"),
                                    replace = TRUE,
                                    size = samplesize)
    }

    #  "select_one"
    if (typedata == "select_one") {
      listname <- as.character(dico[dico$fullname == fullname &
                                      dico$type == "select_one", c("listname")])
      categ_level <- as.character( unique(dico[dico$listname == listname &
                                                 dico$type == "select_one_d", c("name")]))
      dummydata[ , i + 1] <- factor(sample(categ_level,
                                           size = samplesize,
                                           replace = TRUE))
    }

    #  "select_multiple_d"
    if (typedata == "select_multiple_d") {
      listname <- as.character(dico[dico$fullname == fullname &
                                      dico$type == "select_multiple_d", c("listname")])
      categ_level <- as.character( unique(dico[dico$listname == listname &
                                                 dico$type == "select_multiple", c("name")]))
      dummydata[ , i + 1] <- factor(sample(categ_level,
                                           size = samplesize,
                                           replace = TRUE))
    }

    #  "decimal" "integer" "calculate"
    if (typedata == "integer") {
      lowerbound <-  ifelse( is.na(dico.household[ i,  c("lowerbound")]), 0,   as.numeric(dico.household[ i, c("lowerbound" )]))
      upperbound <- ifelse( is.na(dico.household[ i, c("upperbound")]), 100, as.numeric(dico.household[ i, c("upperbound")]))
      dummydata[ , i + 1] <- round(rtruncnorm(n = samplesize,
                                              a = lowerbound, #lowerbound, # vector of lower bounds. These may be -Inf
                                              b = upperbound, # vector of upper bounds. These may be Inf
                                              mean = ((upperbound - lowerbound ) / 2), # vector of means.
                                              sd = ((upperbound - lowerbound ) / 4) # vector of standard deviations.
      ))
    }
    if (typedata == "calculate") {
      lowerbound <- ifelse( is.na(as.numeric(dico.household[ i, c("lowerbound")])), 0,  as.numeric(dico.household[ i, c("lowerbound")]))
      upperbound <- ifelse(is.na(as.numeric(dico.household[ i, c("upperbound")])), 100, as.numeric(dico.household[ i, c("upperbound")]))
      dummydata[ , i + 1] <- round(rtruncnorm(n = samplesize,
                                              a = lowerbound, #lowerbound, # vector of lower bounds. These may be -Inf
                                              b = upperbound, # vector of upper bounds. These may be Inf
                                              mean = ((upperbound - lowerbound ) / 2), # vector of means.
                                              sd = ((upperbound - lowerbound ) / 4) # vector of standard deviations.
      ))
    }
    if (typedata == "decimal") {
      lowerbound <- ifelse( is.na(as.numeric(dico.household[ i, c("lowerbound")])), 0,  as.numeric(dico.household[ i, c("lowerbound")]))
      upperbound <- ifelse(is.na(as.numeric(dico.household[ i, c("upperbound")])), 100, as.numeric(dico.household[ i, c("upperbound")]))
      dummydata[ , i + 1] <- rtruncnorm(n = samplesize,
                                        a = lowerbound, #lowerbound, # vector of lower bounds. These may be -Inf
                                        b = upperbound, # vector of upper bounds. These may be Inf
                                        mean = ((upperbound - lowerbound ) / 2), # vector of means.
                                        sd = ((upperbound - lowerbound ) / 4) # vector of standard deviations.
      )
    }

    #  "text"
    if (typedata == "text") {
      #dummydata[ , i + 1] <- "this is a dummy text"
      dummydata[ , i + 1] <- randomSentences(n = samplesize, 3:10)
    }

    #  "geopoint"
    if (typedata == "geopoint") {
      #dummydata[ , i + 1] <- "this is a dummy text"
      dummydata[ , i + 1] <-  paste( round(spsample(BoxSpatialPoly, n = samplesize, "random")@coords[ ,1], 6),
                                     round(spsample(BoxSpatialPoly, n = samplesize, "random")@coords[ ,2], 6),
                                     sep = ",")
    }

    cat(paste0(" Rename  variable",fullname,"\n"))
    names(dummydata)[i + 1 ] <- fullname
    #cat(summary(dummydata[i]))
    #str(dummydata)

    ## Put to NA if relevance condition is set and not respected
    if ( !(is.na(relevantifvar)) & relevantifvar != "" ) {

      cat(paste0(" Apply relevance on ",relevantifvar2," \n"))
      datacheck <- as.data.frame(dummydata[ , c(relevantifvar2) ])
      for (l in 1:nrow(dummydata)) {
        # l <- 2
        #cat(paste(dummydata[l , i + 1 ] ,"\n"))
        value <-  ifelse(is.na(datacheck[l,]),"",
                         ifelse(datacheck[l,] == relevantifvalue , paste(dummydata[l ,i + 1 ]), ""))
        if (value == "") {
          dummydata[l ,i + 1 ] <- NA } else {
            dummydata[l ,i + 1 ] <- value
          }
      }
    }
  }
  write.csv(dummydata, "data/MainDataFrame.csv", row.names = FALSE)

  rm(categ_level, fullname, i , l, listname, lowerbound, upperbound, value, datacheck, dico.household,
     relevantifvalue, relevantifvar, relevantifvar2, samplesize, typedata)

  ## Now getting repeat questions ################################

  cat("Now getting repeat questions")
  dico.repeat <- dico[(dico$qrepeatlabel != "household" &
                         dico$formpart == "questions" &
                         !(dico$type %in% c("note","end",
                                            "begin_group", "end_group",
                                            "begin group", "end group",
                                            "begin_repeat", "end_repeat",
                                            "begin repeat", "end repeat"))), ]

  repeat_name <- as.factor(levels(as.factor(as.character(dico.repeat$qrepeatlabel))))
  #levels(as.factor(as.character(dico.repeat$type)))

  for (h in  1:length(repeat_name)) {
    # h <- 2
    repeat_table <- as.character(repeat_name[h])
    ## Build corresponding repeat frame
    dico.repeat1 <- dico.repeat[dico.repeat$qrepeatlabel == repeat_table, ]

    cat("Getting records to be generated for each ID \n\n\n")
    maxvariable <- as.character(dico[dico$qrepeatlabel == repeat_table &
                                       dico$type %in% c("begin_repeat", "begin repeat")
                                     , c("repeat_count") ])
    maxvariable <- gsub('[${}]', '', maxvariable)
    maxvariablefullname <- dico[ (dico$name == maxvariable & !(is.na(dico$fullname))), ]
    maxvariablefullname <- maxvariablefullname[!(is.na(maxvariablefullname$fullname)), c("fullname")]
    maxvariablefullname <- as.character(maxvariablefullname)
    #str(maxvariablefullname)
    rm(dummydatamaxvariable)
    dummydatamaxvariable <- dummydata[ ,c("instanceID",maxvariablefullname )]

    #str(dummydatamaxvariable)
    ## Account for NA - relevant nested table
    dummydatamaxvariable <- dummydatamaxvariable[ !(is.na(dummydatamaxvariable[ ,2])), ]

   # names(dummydata)
    #dummydatarepeat <- data.frame("instanceID" )
    #names(dummydatarepeat)[1] <- "instanceID"

    dummydatarepeatall <- as.data.frame(matrix(0, ncol = 1 + nrow(dico.repeat1), nrow = 0))
    names(dummydatarepeatall)[1] <- "instanceID"
    names(dummydatarepeatall)[2:(nrow(dico.repeat1) + 1)] <- as.character(dico.repeat1[ ,c("fullname")])

    # if ( nrow(dico.repeat1) == 1) {
    #   ## only one variable linked on the second table
    #
    #
    #  } else {


      ## Loop around IDs for each case
      for (j in 1:nrow(dummydatamaxvariable) ) {
        # j <- 1
        samplesize <- as.numeric(dummydatamaxvariable[ j, 2])

        if (samplesize !=0 ) {
          this.id <- as.character(dummydatamaxvariable[ j, 1])

          dummydatarepeat <- as.data.frame(matrix(0, ncol = 1, nrow = samplesize))
          dummydatarepeat[1] <-  this.id
          names(dummydatarepeat)[1] <- "instanceID"



          ## Loop around variables
          for (i in 1:nrow(dico.repeat1) ) {
            # i <- 1
            fullname <- as.character(dico.repeat1[i, c("fullname")])
            typedata <- as.character(dico.repeat1[dico.repeat1$fullname == fullname, c("type")])


            relevantifvar <- as.character(dico.repeat1[dico.repeat1$fullname == fullname, c("relevantifvar")])
            relevantifvar2 <- as.character(dico.repeat1[dico.repeat1$name == relevantifvar, c("fullname")])
            relevantifvalue <- as.character(dico.repeat1[dico.repeat1$fullname == fullname, c("relevantifvalue")])

            cat(paste0("Entering dummy data for nested table ", h, " - ", repeat_table,
                       "for case ", j,
                       " for variable ", i, "- ", fullname, " / ", typedata,"\n"))
            if (typedata %in% c("date") ) {
              dummydatarepeat[ , i + 1] <- sample(seq(as.Date('1919/01/01'), as.Date('2019/01/01'), by = "day"),
                                                  replace = TRUE,
                                                  size = samplesize)
            }

            if (typedata == "select_one") {
              listname <- as.character(dico[dico$fullname == fullname &
                                              dico$type == "select_one", c("listname")])
              categ_level <- as.character( unique(dico[dico$listname == listname &
                                                         dico$type == "select_one_d", c("name")]))
              dummydatarepeat[ , i + 1] <- factor(sample(categ_level,
                                                         size = samplesize,
                                                         replace = TRUE))
            }
            if (typedata == "select_multiple_d") {
              listname <- as.character(dico[dico$fullname == fullname &
                                              dico$type == "select_multiple_d", c("listname")])
              categ_level <- as.character( unique(dico[dico$listname == listname &
                                                         dico$type == "select_multiple", c("name")]))
              dummydatarepeat[ , i + 1] <- factor(sample(categ_level,
                                                         size = samplesize,
                                                         replace = TRUE))
            }
            if (typedata == "integer") {
              lowerbound <- ifelse( is.na(as.numeric(dico.repeat1[ i, c("lowerbound")])), 0,  as.numeric(dico.repeat1[ i, c("lowerbound")]))
              upperbound <- ifelse(is.na(as.numeric(dico.repeat1[ i, c("upperbound")])), 100, as.numeric(dico.repeat1[ i, c("upperbound")]))
              dummydatarepeat[ , i + 1] <- round(rtruncnorm(n = samplesize,
                                                            a = lowerbound, #lowerbound, # vector of lower bounds. These may be -Inf
                                                            b = upperbound, # vector of upper bounds. These may be Inf
                                                            mean = ((upperbound - lowerbound ) / 2), # vector of means.
                                                            sd = ((upperbound - lowerbound ) / 4) # vector of standard deviations.
              ))
            }
            if (typedata == "calculate") {
              lowerbound <- ifelse( is.na(as.numeric(dico.repeat1[ i, c("lowerbound")])), 0,  as.numeric(dico.repeat1[ i, c("lowerbound")]))
              upperbound <- ifelse(is.na(as.numeric(dico.repeat1[ i, c("upperbound")])), 100, as.numeric(dico.repeat1[ i, c("upperbound")]))
              dummydatarepeat[ , i + 1] <- round(rtruncnorm(n = samplesize,
                                                            a = lowerbound, #lowerbound, # vector of lower bounds. These may be -Inf
                                                            b = upperbound, # vector of upper bounds. These may be Inf
                                                            mean = ((upperbound - lowerbound ) / 2), # vector of means.
                                                            sd = ((upperbound - lowerbound ) / 4) # vector of standard deviations.
              ))
            }
            if (typedata == "decimal") {
              lowerbound <- ifelse( is.na(as.numeric(dico.repeat1[ i, c("lowerbound")])), 0,  as.numeric(dico.repeat1[ i, c("lowerbound")]))
              upperbound <- ifelse(is.na(as.numeric(dico.repeat1[ i, c("upperbound")])), 100, as.numeric(dico.repeat1[ i, c("upperbound")]))
              dummydatarepeat[ , i + 1] <- rtruncnorm(n = samplesize,
                                                      a = lowerbound, #lowerbound, # vector of lower bounds. These may be -Inf
                                                      b = upperbound, # vector of upper bounds. These may be Inf
                                                      mean = ((upperbound - lowerbound ) / 2), # vector of means.
                                                      sd = ((upperbound - lowerbound ) / 4) # vector of standard deviations.
              )
            }

            if (typedata == "text") {
              #dummydatarepeat[ , i + 1] <- "this is a dummy text"
              dummydatarepeat[ , i + 1] <- randomSentences(n = samplesize, 3:10)
            }

            ## Then rename correctly
            names(dummydatarepeat)[i + 1 ] <- fullname
            #cat(summary(dummydatarepeat[i]))


            ## Put to NA if relevance condition is set and not respected
            if ( !(is.na(relevantifvar)) & relevantifvar != "" ) {
              datacheck <- as.data.frame(dummydatarepeat[ , c(relevantifvar2) ])
              cat(paste0(" Apply relevance on ",relevantifvar2," \n"))
              for (l in 1:nrow(dummydatarepeat)) {
                # l <- 3

                value <-  ifelse(is.na(datacheck[l,]),"",
                                 ifelse(datacheck[l,] == relevantifvalue , paste(dummydatarepeat[l ,i + 1 ]), ""))
                if (value == "") {
                  dummydatarepeat[l ,i + 1 ] <- NA } else {
                    dummydatarepeat[l ,i + 1 ] <- value
                  }
                #dummydatarepeat[l ,i + 1 ] <- ifelse(datacheck[l,] == relevantifvalue , paste(dummydatarepeat[l ,i + 1 ]), "")
              }
            }


          }
          cat("Appending this record \n\n")
          dummydatarepeatall <- rbind(dummydatarepeatall, dummydatarepeat)
          rm(dummydatarepeat)
          }
        }
   # }
    write.csv(dummydatarepeatall, paste0("data/",repeat_table,".csv"), row.names = FALSE)
    cat(paste0("\n\n\n Finished generation of nested table ", h, " - ", repeat_table, "\n"))
    rm(dummydatarepeatall)


  }

  }
NULL
