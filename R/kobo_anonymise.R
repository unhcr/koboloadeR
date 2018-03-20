#' @name kobo_anonymise
#' @rdname kobo_anonymise
#' @title  Generate anonymised dataset
#'
#' @description  Automatically produce an anonymised dataset in line with the anonymisation plan set up in the xlsform.
#'
#'  This method should be used whenever Kobo or ODK forms are used as data collection tools and personal data is being collected.
#'  Even when personal data is not being collected it still may be appropriate to apply the methodology since quasi-identifiable data
#'   or other sensitive data could lead to personal identification or should not be shared.
#'   https://jangorecki.github.io/blog/2014-11-07/Data-Anonymization-in-R.html
#'
#' \tabular{rrrrrr}{
#'   \strong{Type}    \tab \strong{Description}  \cr
#'   ----------------\tab----------- \cr
#'   \strong{Direct identifiers}     \tab	Can be directly used to identify an individual. E.g. Name, Address, Date of birth, Telephone number, GPS location \cr
#'   \strong{Quasi- identifiers}     \tab	Can be used to identify individuals when it is joined with other information. E.g. Age, Salary, Next of kin, School name, Place of work \cr
#'   \strong{Sensitive information}  \tab & Community identifiable information	Might not identify an individual but could put an individual or group at risk. E.g. Gender, Ethnicity, Religious belief \cr
#'   \strong{Meta data}              \tab 	Data about who, where and how the data is collected is often stored separately to the main data and can be used identify individuals
#' }
#'
#'The following are different anonymisation actions that can be performed on sensitive fields. The type of anonymisation should be dictated by the desired use of the data. A good approach to follow is to start from the minimum data required, and then to identify if any of those fields should be obscured.
#'
#'The methods above can be reference in the column
#'
#' \tabular{rrrrrr}{
#' \strong{Method}          \tab \strong{Description} \cr
#' ----------------\tab-------------- \cr
#' \strong{Remove}     \tab	Data is removed entirely from the data set. The data is preserved in the original file. \cr
#' \strong{Reference}   \tab	Data is removed entirely from the data set and is copied into a reference file. A random unique identifier field is added to the reference file and the data set so that they can be joined together in future.  The reference file is never shared and the data is also preserved in the original file. \cr
#' \strong{Mask}      \tab	The data values are replaced with meaningless values but the categories are preserved. A reference file is created to link the original value with the meaningless value. Typically applied to categorical data. For example, Town names could be masked with random combinations of letters. It would still be possible to perform statisitical analysis on the data but the person running the analysis would not be able to identify the original values, they would only become meaningful when replaced with the original values. The reference file is never shared and the data is also preserved in the original file. \cr
#' \strong{Generalise}	\tab Continuous data is turned into categorical or ordinal data by summarising it into ranges. For example, Age could be turned into age ranges, Weight could be turned into ranges. It can also apply to categorical data where parent groups are created. For example, illness is grouped into illness type. Generalised data can also be masked for extra anonymisation. The data is preserved in the original file.
#' }
#'
#'
#' @param  kobo or odk dataset to use
#' @param  dico Generated from kobo_dico function
#'
#' @author Edouard Legoupil
#'
#' @examples
#' kobo_anonymise()
#'
#' @export kobo_anonymise
#'
#' @examples
#' \dontrun{
#' kobo_anonymise(frame, dico)
#' }
#'

kobo_anonymise <- function(frame, dico) {

  # frame <- household
  # framename <- "household"
  framename <- deparse(substitute(frame))


  ## Get the anonymisation type defined within the xlsform / dictionnary ######

  if (levels(dico$anonymise) == "default-non-anonymised") {
    cat(paste0("You have not defined variables to anonymise within your xlsform. \n"))
    cat(paste0(" Insert a column named anonymise to insert your anonymisation plan\n")) }
  else{


    #### Check we have anonymisation instructions ###############
    dico.ano <- dico[ !(is.na(dico$anonymise)) & dico$qrepeatlabel == framename,  ]
    if (nrow(dico.ano) > 0) {
      cat(paste0(nrow(dico.ano), " variables to anonymise\n"))




  #### Remove ###############
  anotype.remove  <- dico[ which(dico$anonymise == "remove" ),  ]
  if (nrow(anotype.remove) > 0) {
    cat(paste0(nrow(anotype.remove), " potential variables to remove \n\n"))

    if (file.exists("code/temp-remove.R")) file.remove("code/temp-remove.R")
    cat("cat(\"Now Running removal Script \n \")", file = "code/temp-remove.R" , sep = "\n", append = TRUE)


    ### Specific cases  ###
    cat(paste0("colname <- grep(\"geopoint_latitude\", colnames(",framename,"))"), file = "code/temp-remove.R" , sep = "\n", append = TRUE)
    cat("if (length(colname) > 0) { cat(\"Removing Latitude column \n\")", file = "code/temp-remove.R" , sep = "\n", append = TRUE)
    cat(paste0(framename,"[ ,colname] <- \"remove\"} else {}"), file = "code/temp-remove.R" , sep = "\n", append = TRUE)
    cat(paste0("colname <- grep(\"Latitude\", colnames(",framename,"))"), file = "code/temp-remove.R" , sep = "\n", append = TRUE)
    cat("if (length(colname) > 0) { cat(\"Removing Latitude column \n\")", file = "code/temp-remove.R" , sep = "\n", append = TRUE)
    cat(paste0(framename,"[ ,colname] <- \"remove\"} else {}"), file = "code/temp-remove.R" , sep = "\n", append = TRUE)

    cat(paste0("colname <- grep(\"geopoint_longitude\", colnames(",framename,"))"), file = "code/temp-remove.R" , sep = "\n", append = TRUE)
    cat("if (length(colname) > 0) { cat(\"Removing Longitude column \n\")", file = "code/temp-remove.R" , sep = "\n", append = TRUE)
    cat(paste0(framename,"[ ,colname] <- \"remove\"} else {}"), file = "code/temp-remove.R" , sep = "\n", append = TRUE)
    cat(paste0("colname <- grep(\"Longitude\", colnames(",framename,"))"), file = "code/temp-remove.R" , sep = "\n", append = TRUE)
    cat("if (length(colname) > 0) { cat(\"Removing Longitude column \n\")", file = "code/temp-remove.R" , sep = "\n", append = TRUE)
    cat(paste0(framename,"[ ,colname] <- \"remove\"} else {}"), file = "code/temp-remove.R" , sep = "\n", append = TRUE)

    cat(paste0("colname <- grep(\"geopoint_altitude\", colnames(",framename,"))"), file = "code/temp-remove.R" , sep = "\n", append = TRUE)
    cat("if (length(colname) > 0) { cat(\"Removing Altitude column \n\")", file = "code/temp-remove.R" , sep = "\n", append = TRUE)
    cat(paste0(framename,"[ ,colname] <- \"remove\"} else {}"), file = "code/temp-remove.R" , sep = "\n", append = TRUE)
    cat(paste0("colname <- grep(\"Altitude\", colnames(",framename,"))"), file = "code/temp-remove.R" , sep = "\n", append = TRUE)
    cat("if (length(colname) > 0) { cat(\"Removing Altitude column \n\")", file = "code/temp-remove.R" , sep = "\n", append = TRUE)
    cat(paste0(framename,"[ ,colname] <- \"remove\"} else {}"), file = "code/temp-remove.R" , sep = "\n", append = TRUE)

    cat(paste0("colname <- grep(\"geopoint_precision\", colnames(",framename,"))"), file = "code/temp-remove.R" , sep = "\n", append = TRUE)
    cat("if (length(colname) > 0) { cat(\"Removing accuracy column \n\")", file = "code/temp-remove.R" , sep = "\n", append = TRUE)
    cat(paste0(framename,"[ ,colname] <- \"remove\"} else {}"), file = "code/temp-remove.R" , sep = "\n", append = TRUE)
    cat(paste0("colname <- grep(\"Accuracy\", colnames(",framename,"))"), file = "code/temp-remove.R" , sep = "\n", append = TRUE)
    cat("if (length(colname) > 0 ) { cat(\"Removing accuracy column \n\")", file = "code/temp-remove.R" , sep = "\n", append = TRUE)
    cat(paste0(framename,"[ ,colname] <- \"remove\"} else {}"), file = "code/temp-remove.R" , sep = "\n", append = TRUE)

    cat(paste0("colname <- grep(\"SubmissionDate\", colnames(",framename,"))"), file = "code/temp-remove.R" , sep = "\n", append = TRUE)
    cat("if (length(colname) > 0 ) { cat(\"Removing SubmissionDate column \n\")", file = "code/temp-remove.R" , sep = "\n", append = TRUE)
    cat(paste0(framename,"[ ,colname] <- \"remove\"} else {}"), file = "code/temp-remove.R" , sep = "\n", append = TRUE)



      for (i in 1:nrow(anotype.remove)) {
       # i <- 1
        cat(paste0(i, "- Remove, if exists, the value of: ", as.character(anotype.remove[ i, c("label")]),"\n"))
        varia <- paste0(framename,"$",as.character(anotype.remove[ i, c("fullname")]))
        cat(paste0("if (\"", as.character(anotype.remove[ i, c("fullname")]) , "\" %in% names(", framename, ")) {" ), file = "code/temp-remove.R" , sep = "\n", append = TRUE)
        cat(paste0(framename,"$",as.character(anotype.remove[ i, c("fullname")])," <- \"removed\" } else" ), file = "code/temp-remove.R" , sep = "\n", append = TRUE)
        cat("{}", file = "code/temp-remove.R" , sep = "\n", append = TRUE)
      }

    cat(" Source removal script \n\n")
    #mainDir <- getwd()
    #source(paste0(mainDir,"/code/temp-remove.R"))
    source("code/temp-remove.R")
    #if (file.exists("code/temp-remove.R")) file.remove("code/temp-remove.R")

    } else{}



  #### Reference ###############
  anotype.reference <- dico[ which(dico$anonymise == "reference" ),  ]
  # & dico$qrepeatlabel == framename
  if (nrow(anotype.reference) > 0) {
    cat(paste0(nrow(anotype.reference), " variables to encrypt \n\n"))

        if (file.exists("code/temp-reference.R")) file.remove("code/temp-reference.R")
        cat("cat(\"Now Running reference Script \n \")", file = "code/temp-reference.R" , sep = "\n", append = TRUE)

        ## Initiate reference table
        formula0 <- paste0(framename,".anom.reference <- as.data.frame(row.names(", framename,"))" )
        cat(paste0(formula0, ""), file = "code/temp-reference.R" , sep = "\n", append = TRUE)

      for (i in 1:nrow(anotype.reference)) {
        # i <- 1
        cat(paste0(i, "- Replace by row id and create a reference table, the value of: ", as.character(anotype.reference [ i, c("label")]),"\n"))

        formula1 <-  paste0(framename,".anom.reference1 <- as.data.frame(", framename,"$",as.character(anotype.reference [ i, c("fullname")]),")" )
        formula11 <- paste0("names(",framename,".anom.reference1) <- \"",anotype.reference [ i, c("fullname")],"\"" )
        formula12 <- paste0(framename,".anom.reference <- cbind(",framename,".anom.reference, ", framename,".anom.reference1)")
        formula13 <- paste0("rm(",framename,".anom.reference1) ")
        formula2 <-  paste0(framename, "$", as.character(anotype.reference [ i, c("fullname")]), " <- row.names(", framename,")" )


        cat(paste0("if (\"", as.character(anotype.reference [ i, c("fullname")]) , "\" %in% names(", framename, ")) {" ), file = "code/temp-reference.R" , sep = "\n", append = TRUE)
        cat(paste0(formula1, ""), file = "code/temp-reference.R" , sep = "\n", append = TRUE)
        cat(paste0(formula11, ""), file = "code/temp-reference.R" , sep = "\n", append = TRUE)
        cat(paste0(formula12, ""), file = "code/temp-reference.R" , sep = "\n", append = TRUE)
        cat(paste0(formula13, ""), file = "code/temp-reference.R" , sep = "\n", append = TRUE)
        cat(paste0(formula2, "} else"), file = "code/temp-reference.R" , sep = "\n", append = TRUE)
        cat("{}", file = "code/temp-reference.R" , sep = "\n", append = TRUE)
      }
    formula3 <- paste0( "write.csv(",framename,".anom.reference, \"data/anom_reference_",framename,".csv\", row.names = FALSE, na = \"\")")
    cat(formula3, file = "code/temp-reference.R" , sep = "\n", append = TRUE)

  #  mainDir <- getwd()
  #  source(paste0(mainDir,"/code/temp-reference.R"))
    source("code/temp-reference.R")
    if (file.exists("code/temp-reference-reference.R")) file.remove("code/temp-reference.R")

    } else{}

  #### Scramble ###############
  anotype.scramble <- dico[ which(dico$anonymise=="scramble" & dico$qrepeatlabel == framename),  ]

  if (nrow(anotype.scramble ) > 0) {
    cat(paste0(nrow(anotype.scramble), " variables to scramble \n\n"))

        if (file.exists("code/temp-scramble.R")) file.remove("code/temp-scramble.R")
       cat("cat(\"Now Running scramble Script \n \")", file = "code/temp-scramble.R" , sep = "\n", append = TRUE)

       for (i in 1:nrow(anotype.scramble )) {
        cat(paste0(i, "- Scramble through cryptographical hash function, if exists, the value of: ", as.character(anotype.scramble[ i, c("label")]),"\n"))

        indic.formula <- paste0(framename,"$",as.character(anotype.scramble[ i, c("fullname")]),
                                "<- digest(" ,framename,"$",as.character(anotype.scramble[ i, c("fullname")]),
                                ", algo= \"crc32\")" )
        cat(paste0("if (\"", as.character(anotype.scramble [ i, c("fullname")]) , "\" %in% names(", framename, ")) {" ), file = "code/temp-remove.R" , sep = "\n", append = TRUE)
        cat(paste0(formula, "} else"), file = "code/temp-scramble.R" , sep = "\n", append = TRUE)
        cat("{}", file = "code/temp-scramble.R" , sep = "\n", append = TRUE)
      }
        source("code/temp-scramble.R")
       # source(paste0(getwd(),"/code/temp-scramble.R"))
       # if (file.exists("code/temp-scramble.R")) file.remove("code/temp-scramble.R")
  } else{}  }
  else { cat("Sorry, it looks like there's nothing to anonymise based on the anonymisation plan within the xlsform dictionnary... \n") }
  }

 # return(frame)

}
NULL
