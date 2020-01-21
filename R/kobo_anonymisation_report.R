#' @name kobo_anonymisation_report
#' @rdname kobo_anonymisation_report
#' @title  Generate a report displaying disclosure risk for Statistical Disclosure Control
#'
#' @description  Automatically produce a disclosure risk measurement report.
#'
#'  The report is generated from functions released within sdcmicro package from the worldbank.
#'   https://cran.r-project.org/web/packages/sdcMicro/sdcMicro.pdf
#'
#'
#' @param  frame kobo or odk dataset to use
#' @param form The full filename of the form to be accessed (xls or xlsx file).
#' @param app The place where the function has been executed, the default is the console and the second option is the shiny app
#'
#' @author Edouard Legoupil
#'
#' @examples
#' \dontrun{
#' kobo_anonymisation_report(frame, form = "form.xls")
#' }
#'
#' @export kobo_anonymisation_report
#'
#'

kobo_anonymisation_report <- function(frame, form = "form.xls", app = "console") {
  tryCatch({
    if (app == "shiny") {
      progress <- shiny::Progress$new()
      progress$set(message = "Generating crunching report in progress...", value = 0)
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
    configInfo <- kobo_get_config()
    mainDir <- kobo_getMainDirectory()
    form_tmp <- paste(mainDir, "data", form, sep = "/", collapse = "/")

    dico <- utils::read.csv(paste0(mainDir,"/data/dico_",form,".csv"), encoding = "UTF-8", na.strings = "")
    framename <- deparse(substitute(frame))
    utils::write.csv(frame, paste0(mainDir,"/data/anomreport-",framename,".csv"), row.names = FALSE, na = "")

    ## Check that all those selectedVars are in the frame ####
    check <- as.data.frame(names(frame))
    names(check)[1] <- "fullname"
    check$id <- row.names(check)


    #### Check presence of variable for anom plan...
    if (app == "shiny") {
      progress$set(message = "Check presence of variable for anom plan...")
      updateProgress()
    }
    selected.key <- dico[ which(dico$anonymise == "key" & dico$type == "select_one" ) , ]
    selected.key <- plyr::join(x = selected.key, y = check, by = "fullname", type = "left")
    selected.key <- selected.key[!is.na(selected.key$id),  ]

    if ( nrow(selected.key) == 0) {
      cat("You have not selected key variables for your dataset! \n")
      return(structure("You have not selected key variables for your dataset!", class = "try-error"))
    } else {
          selected.sensible <- dico[ which(dico$anonymise == "sensitive" & dico$type == "select_one" ), ]
          selected.sensible <- plyr::join(x = selected.sensible, y = check, by = "fullname", type = "left")
          selected.sensible <- selected.sensible[!is.na(selected.sensible$id), ]

          selected.num <- dico[ which(dico$anonymise == "outlier" ), ]
          selected.num <- plyr::join(x = selected.num, y = check, by = "fullname", type = "left")
          selected.num <- selected.num[!is.na(selected.num$id),  ]


          reportanom  <- paste0(mainDir,"/code/anonymisation-report-",framename,".Rmd")

          ## TO DO : CHECK IF FILE EXIST - AND REQUEST USER TO DELETE BEFORE REGENERATING - SUGGESTING TO SAVE PREVIOUS UNDER NEW NAME
          if (file.exists(reportanom)) file.remove(reportanom)

          #form <- "form.xls"
          #kobo_dico(form)

          cat("---", file = reportanom , sep = "\n", append = TRUE)
          cat("title: \"Data Anonymization & Statistical Disclosure Risk Analysis Report\"", file = reportanom , sep = "\n", append = TRUE)
          cat("author: \"Generated with [Koboloader](https://github.com/unhcr/koboloadeR) and [sdcMicro](https://cran.r-project.org/web/packages/sdcMicro/sdcMicro.pdf)\"", file = reportanom , sep = "\n", append = TRUE)
          cat("date: \" `r format(Sys.Date(), '%d %B %Y')`\"", file = reportanom , sep = "\n", append = TRUE)
          cat("always_allow_html: yes", file = reportanom , sep = "\n", append = TRUE)
          cat("output:", file = reportanom , sep = "\n", append = TRUE)
          cat("  word_document:", file = reportanom , sep = "\n", append = TRUE)
          cat("    fig_caption: yes", file = reportanom , sep = "\n", append = TRUE)
          cat("    fig_height: 5", file = reportanom , sep = "\n", append = TRUE)
          cat("    fig_width: 8", file = reportanom , sep = "\n", append = TRUE)
          cat("    toc: yes", file = reportanom , sep = "\n", append = TRUE)
          cat("    toc_depth: 2", file = reportanom , sep = "\n", append = TRUE)
          cat("    reference_docx: style-unhcr-portrait.docx", file = reportanom , sep = "\n", append = TRUE)
          cat("---", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)

          ## r setup ################

          cat("```{r setup, include = FALSE, echo = FALSE, warning = FALSE, message = FALSE, cache=FALSE}", file = reportanom , sep = "\n", append = TRUE)
          cat("mainDir <- getwd()", file = reportanom , sep = "\n", append = TRUE)
          cat("mainDirroot <- substring(mainDir, 0 , nchar(mainDir) - 5)", file = reportanom , sep = "\n", append = TRUE)
          cat("## Load all required packages", file = reportanom , sep = "\n", append = TRUE)
          cat("# sdcMicro,classInt and ggrepel package", file = reportanom , sep = "\n", append = TRUE)
          cat("library(sdcMicro)", file = reportanom , sep = "\n", append = TRUE)
          cat("library(classInt)", file = reportanom , sep = "\n", append = TRUE)
          cat("library(ggrepel)", file = reportanom , sep = "\n", append = TRUE)
          cat("library(koboloadeR)", file = reportanom , sep = "\n", append = TRUE)
          cat("kobo_load_data()", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("## Provide below the name of the form in xsl form - format should be xls not xlsx", file = reportanom , sep = "\n", append = TRUE)
          cat("form <- \"form.xls\"", file = reportanom , sep = "\n", append = TRUE)
          #cat("kobo_dico(form)", file = reportanom , sep = "\n", append = TRUE)
          cat("dico <- utils::read.csv(paste0(mainDirroot,\"/data/dico_\",form,\".csv\"), encoding = \"UTF-8\", na.strings = \"\")", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)

          cat(paste0("dataanom <- utils::read.csv(paste0(mainDirroot,\"/data/anomreport-",framename,".csv\"), sep = \",\", encoding = \"UTF-8\", na.strings = \"\")"), file = reportanom , sep = "\n", append = TRUE)


        #  cat(paste0("dataanom <- read.csv(paste0(mainDirroot,\"/data/anomreport-",framename,".csv\")    , sep = \";\", encoding = \"UTF-8\", na.strings = \"\")", file = reportanom , sep = "\n", append = TRUE))
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("numrow <- nrow(dataanom) ", file = reportanom , sep = "\n", append = TRUE)
          cat("numvar <- ncol(dataanom)", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("#############################################################################", file = reportanom , sep = "\n", append = TRUE)
          cat("##### Step 1 - Determine variables key uses and select utility measures ####", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("## Check that all those selectedVars are in the frame...", file = reportanom , sep = "\n", append = TRUE)
          cat("check <- as.data.frame(names(dataanom))", file = reportanom , sep = "\n", append = TRUE)
          cat("names(check)[1] <- \"fullname\"", file = reportanom , sep = "\n", append = TRUE)
          cat("check$id <- row.names(check)", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat(" #### Remove ###############", file = reportanom , sep = "\n", append = TRUE)
          cat("selected.key <- dico[ which(dico$anonymise == \"key\" & dico$type == \"select_one\" ) , ]", file = reportanom , sep = "\n", append = TRUE)
          cat("selected.key <- plyr::join(x = selected.key, y = check, by = \"fullname\", type = \"left\")", file = reportanom , sep = "\n", append = TRUE)
          cat("selected.key <- selected.key[!is.na(selected.key$id),  ]", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("selected.sensible <- dico[ which(dico$anonymise == \"sensitive\" & dico$type == \"select_one\" ), ]", file = reportanom , sep = "\n", append = TRUE)
          cat("selected.sensible <- plyr::join(x = selected.sensible, y = check, by = \"fullname\", type = \"left\")", file = reportanom , sep = "\n", append = TRUE)
          cat("selected.sensible <- selected.sensible[!is.na(selected.sensible$id), ]", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("selected.num <- dico[ which(dico$anonymise == \"outlier\" ), ]", file = reportanom , sep = "\n", append = TRUE)
          cat("selected.num <- plyr::join(x = selected.num, y = check, by = \"fullname\", type = \"left\")", file = reportanom , sep = "\n", append = TRUE)
          cat("selected.num <- selected.num[!is.na(selected.num$id),  ]", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("# Key variables - To be converted to factors", file = reportanom , sep = "\n", append = TRUE)
          cat("selected.keyVars <- as.character(selected.key[ , c(\"fullname\")])", file = reportanom , sep = "\n", append = TRUE)
          cat("selected.sensibleVars <- as.character(selected.sensible[ , c(\"fullname\")])", file = reportanom , sep = "\n", append = TRUE)
          cat("selected.numVars <- as.character(selected.num[ , c(\"fullname\")])", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("## Sample weight", file = reportanom , sep = "\n", append = TRUE)
          cat("dataanom$weight <- 1", file = reportanom , sep = "\n", append = TRUE)
          cat("selected.weightVar = c('weight')", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("# All variables (id, key variables, sensible variables, numerical variables, weights)", file = reportanom , sep = "\n", append = TRUE)
          cat("dataanom$id <- row.names(dataanom)", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("#############################################################################", file = reportanom , sep = "\n", append = TRUE)
          cat("##### Step 2: Assessing disclosure risk  ####", file = reportanom , sep = "\n", append = TRUE)
          cat("# Create subset of file with observation and selected variables & remove duplicated rows based on IDH", file = reportanom , sep = "\n", append = TRUE)
          cat("dataanom.anom <- dataanom[ which(!duplicated(dataanom$id)), c('id',", file = reportanom , sep = "\n", append = TRUE)
          cat(" selected.keyVars,", file = reportanom , sep = "\n", append = TRUE)

          if ( nrow(selected.sensible) == 0) { cat("\n") } else {
              cat(" selected.sensibleVars,", file = reportanom , sep = "\n", append = TRUE) }

          if ( nrow(selected.num) == 0) { cat("\n") } else {
          cat(" selected.numVars,", file = reportanom , sep = "\n", append = TRUE) }


          cat(" selected.weightVar)]", file = reportanom , sep = "\n", append = TRUE)
          cat("#dataanom.anom <- kobo_label(dataanom.anom , dico)", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("# Create initial sdc object for selected variables", file = reportanom , sep = "\n", append = TRUE)
          cat("sdc.dataanom <- createSdcObj(dat = dataanom.anom,", file = reportanom , sep = "\n", append = TRUE)

          cat(" keyVars = selected.keyVars,", file = reportanom , sep = "\n", append = TRUE)

         # if ( nrow(selected.sensible) == 0) {
             # cat(" sensibleVars = NULL,", file = reportanom , sep = "\n", append = TRUE) } else {
             # cat(" sensibleVars = selected.sensibleVars,", file = reportanom , sep = "\n", append = TRUE)}

          if ( nrow(selected.num) == 0) {
              cat(" numVars = NULL,", file = reportanom , sep = "\n", append = TRUE)  } else {
              cat(" numVars = selected.numVars,", file = reportanom , sep = "\n", append = TRUE) }

          cat(" pramVars = NULL,", file = reportanom , sep = "\n", append = TRUE)

          cat(" weightVar = selected.weightVar)", file = reportanom , sep = "\n", append = TRUE)

          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("```", file = reportanom , sep = "\n", append = TRUE)


          # Introduction #######

          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("# Introduction", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("The dissemination of microdata (i.e. survey data) is important as it is: ", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat(" * Reducing duplication in data collection; ", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat(" * Improving the reliability and relevance of data; ", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat(" * Supporting research and promoting development of new tools for using data; ", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat(" * Enhancing the credibility of UNHCR as an authoritative source of Information on refugees. ", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("Proper and secure microdata dissemination requires to apply Statistical Disclosure Control (SDC) methods to data before release. Data anonymisation is always a trade-off between disclosure risks and information loss. The objective is to modify data in such a way that both the disclosure risk and the information loss caused are acceptably low. ", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("## Anonymisation concepts", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("The present report is based on the World Bank sponsored [disclosure Control Toolbox](http://www.ihsn.org/software/disclosure-control-toolbox) for the R language and built on the recommendations from the [International Household Survey Network](http://ihsn.org/sites/default/files/resources/ihsn-working-paper-007-Oct27.pdf).", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("Per default, it is assumed that __direct identifiers__ (such as name, progres ID, telephone, GPS locations) values are removed.", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("Suppose a hypothetical intruder has access to some released microdata and attempts to identify or find out more information about a particular respondent. Disclosure, also known as re-identification, occurs when the intruder reveals previously unknown information about a respondent by using the released data. Three types of disclosure can be distinguished: ", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat(" * __Identity disclosure__ occurs if the intruder associates a known individual with a released data record. For example, the intruder links a released data record with external information, or identifies a respondent with extreme data values. In this case, an intruder can exploit a small subset of variables to make the linkage, and once the linkage is successful, the intruder has access to all other information in the released data related to the specific respondent.", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat(" * __Attribute disclosure__ occurs if the intruder is able to determine some new characteristics of an individual based on the information available in the released data. For example, if a hospital publishes data showing that all female patients aged 56 to 60 have cancer, an intruder then knows the medical condition of any female patient aged 56 to 60 without having to identify the specific individual. ", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat(" * __Inferential disclosure__ occurs if the intruder is able to determine the value of some characteristic of an individual more accurately with the released data than otherwise would have been possible. For example, with a highly predictive regression model, an intruder may be able to infer a respondent's sensitive income information using attributes recorded in the data, leading to inferential disclosure.", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)

          ## Report Content##################
          cat("## Report Content", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("The objective of this report is to allow to quickly identify potential statistical disclosure risks so that the organization can make informed decisions when disclosing data. A series of measurement can be performed to assess those risks:", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat(" * Risk linked to each records in the dataset: __Global disclosure risk__ &  __Record-level disclosure risk__; ", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat(" * Risk linked to combination of categoric variables in the dataset: __k-anonymity__ &  __l-diversity__: ; ", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat(" * Risk linked to specific values for numeric variables in the dataset: various index based on __Robust Mahalanobis distances__ are calculated; ", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("The report then suggest a series of perturbative and non-pertubative approaches to decrease the risks detected above: ", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat(" * for __categoric__ variable: Recoding, suppressing, post randomization , ", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat(" * for __continuous__ variables: Adding noise, micro-aggregation, swapping ", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("Once those additional treatments are applied, this report can be then regenerated till the ratio risk/loss is acceptable.", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("# Variables for risk scenarios", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("To assess disclosure risk, one must make realistic assumptions about the information data users might have at hand to match against the micro dataset; these assumptions are called disclosure risk scenarios. This goes hand in hand with the selection of categorical key variables because the choice these identifying variables defines a specific disclosure risk scenario. The specific set of chosen key variables has direct influence on the risk assessment.", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("The dataset includes __`r numrow`__ records and  __`r numvar`__ variables. ", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("The tables below present the variables that were considered in the disclosure risk scenario assesed within this report.", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("It can also help to define some potential recoding for those variables. Global recoding is a non-perturbative method that can be applied to both categorical and continuous key variables. The basic idea of recoding a categorical variable is to combine several categories into a new, less informative category. A frequent use case is the recoding of age given in years into age-groups. If the method is applied to a continuous variable, it means to discretize the variable. ", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)

          ## Categoric Key variables ###############

          cat("## Categoric Key variables ", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("Also called _implicit identifiers_ or _quasi-identifiers_: Set of variables that, in combination, can be linked to external information to re-identify respondents in the released dataset (gender, age, occupation, specific needs, region..)", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("```{r configkey, echo = FALSE, warning = FALSE, message = FALSE, eval= TRUE}", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("selected.key1 <- selected.key[ , c(\"name\",\"label\")]", file = reportanom , sep = "\n", append = TRUE)
          cat("#rownames(selected.key1) <- NULL", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("kable(as.data.frame(selected.key1), caption = \"__Table__:Key Variables\") %>% kable_styling(position = \"center\")", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("```", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)

          ## ## Numeric Key variables ######
          cat("## Numeric Key variables ", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("Key variables that are numeric", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("```{r confignum, echo = FALSE, warning = FALSE, message = FALSE, eval= TRUE}", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("selected.num1 <- selected.num[ , c(\"name\",\"label\")]", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("kable(as.data.frame(selected.num1), caption = \"__Table__: Numeric variables\") %>% kable_styling(position = \"center\")", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("```", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)

          ## Sensitive variables #############
          cat("## Sensitive variables", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("Variables whose values must not be discovered for any respondent. Determination is often subject to legal and ethical concerns (Protection risk, Vulnerabilities..)", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)

          if ( nrow(selected.sensible) == 0) {

            cat(" You have not defined sensitive variables in your disclosure scenerios. \n", file = reportanom , sep = "\n", append = TRUE) } else {

          cat("```{r configsensi, echo = FALSE, warning = FALSE, message = FALSE, eval= TRUE}", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("selected.sensible1 <- selected.sensible[ , c(\"name\",\"label\")]", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("kable(as.data.frame(selected.sensible1), caption = \"__Table__: Sensitive variables\") %>% kable_styling(position = \"center\")", file = reportanom , sep = "\n", append = TRUE)
          cat("```", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)}

          # Measuring Disclosure Risk ##########

          cat("# Measuring Disclosure Risk", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("```{r summary, echo = FALSE, warning = FALSE, message = FALSE, eval= TRUE, comment = \"\"}", file = reportanom , sep = "\n", append = TRUE)
          cat("## Data summary", file = reportanom , sep = "\n", append = TRUE)
          cat("## basic information on the input obj such as the number of observations and variables.", file = reportanom , sep = "\n", append = TRUE)
          cat("#print(sdc.dataanom, type = \"general\")", file = reportanom , sep = "\n", append = TRUE)
          cat("```", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("## Risks linked to observations", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("### Population Frequencies and Global re-identification risks", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("Risk evaluation is based on the concept of uniqueness in the sample and/or in the population. The focus is on individual units that possess rare combinations of selected key variables.  The assumption is that units having rare combinations of key variables can be more easily identified and thus have a higher risk of re-identification/disclosure. It is possible to cross-tabulate all identifying variables and view their cast.  Keys possessed by only very few individuals are considered risky, especially if these observations also have small sampling weights. This means that the expected number of individuals with these patterns is expected to be low in the population as well.", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("Two approaches can be used to determine the global risk for a dataset using individual risks: ", file = reportanom , sep = "\n", append = TRUE)

          # cat("\n", file = reportanom , sep = "\n", append = TRUE)

          #cat("Benchmark: This approach counts the number of observations that can be considered risky and also have higher risk as the main part of the data. For example, we consider units with individual risks being both >= 0 : 1 and twice as large as the median of all individual risks + 2 times the median absolute deviation (MAD) of all unit risks. ", file = reportanom , sep = "\n", append = TRUE)

          #cat("\n", file = reportanom , sep = "\n", append = TRUE)


          cat(" * Global risk: The sum of the individual risks in the dataset gives the expected number of re-identifications.", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("The expected __number of re-identification__ indicator is estimated by the sum of record-level disclosure risks. The __number of records with higher risks than the main part of the dataset__ are the number of records whose individual risk is greater than 0.1 and greater than 2 X [median(r) + 2 X MAD(r)], where r is a vector of record-level risks, and MAD is the median absolute deviation of all record-level risks. ", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("```{r reid, echo = FALSE, warning = FALSE, message = FALSE, eval= TRUE, comment = \"\"}", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("## displays information on re-identification risks", file = reportanom , sep = "\n", append = TRUE)
          cat("##print(sdc.dataanom, type = \"risk\")", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("cat(\"\n Global risk percentage\n\")", file = reportanom , sep = "\n", append = TRUE)
          cat("sdc.dataanom@risk$global$risk", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("cat(\"\n Number of expected re-identifications\n\") ", file = reportanom , sep = "\n", append = TRUE)
          cat("sdc.dataanom@risk$global$risk_ER", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("# Hierarchical risk percentage", file = reportanom , sep = "\n", append = TRUE)
          cat("#sdc.dataanom@risk$global$hier_risk", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("# Hierarchical risk: number of expected identifications", file = reportanom , sep = "\n", append = TRUE)
          cat("#sdc.dataanom@risk$global$hier_risk_ER", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("risksum <- as.data.frame(sdc.dataanom@risk$individual[, \"risk\"])", file = reportanom , sep = "\n", append = TRUE)
          cat("names(risksum)[1] <- \"indrisk\"", file = reportanom , sep = "\n", append = TRUE)
          cat("risksum$class.fixed <- as.factor(findCols(classIntervals(risksum$indrisk, n = 8, style = \"fixed\", fixedBreaks = c(0, 0.001, 0.01, 0.5, 0.1, 0.15, 0.30, 0.5, 1))))", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("risksum$class.fixed.name <- recode_factor(risksum$class.fixed, `1` = \"Individual Risk is lower than 0.1%\",", file = reportanom , sep = "\n", append = TRUE)
          cat("                                          `2` = \"Individual Risk is higher than 0.1% and lower than 1%\",", file = reportanom , sep = "\n", append = TRUE)
          cat("                                     `3` = \"Individual Risk is higher than 1% and lower than 5%\",", file = reportanom , sep = "\n", append = TRUE)
          cat("                                      `4` = \"Individual Risk is higher than 5% and lower than 10%\",", file = reportanom , sep = "\n", append = TRUE)
          cat("                                     `5` = \"Individual Risk is higher than 10% and lower than 15%\",", file = reportanom , sep = "\n", append = TRUE)
          cat("                                     `6` = \"Individual Risk is higher than 15% and lower than 30%\",", file = reportanom , sep = "\n", append = TRUE)
          cat("                                     `7` = \"Individual Risk is higher than 30% and lower than 50%\",", file = reportanom , sep = "\n", append = TRUE)
          cat("                                     `8` = \"Individual Risk is higher than 50% and lower than 100%\")", file = reportanom , sep = "\n", append = TRUE)
          cat("risksum$class.fixed.name <- factor(risksum$class.fixed.name, levels = c(\"Individual Risk is lower than 0.1%\",", file = reportanom , sep = "\n", append = TRUE)
          cat("                                                                   \"Individual Risk is higher than 0.1% and lower than 1%\",", file = reportanom , sep = "\n", append = TRUE)
          cat("                                                                   \"Individual Risk is higher than 1% and lower than 5%\",", file = reportanom , sep = "\n", append = TRUE)
          cat("                                                                   \"Individual Risk is higher than 5% and lower than 10%\",", file = reportanom , sep = "\n", append = TRUE)
          cat("                                                                    \"Individual Risk is higher than 10% and lower than 15%\",", file = reportanom , sep = "\n", append = TRUE)
          cat("                                                                   \"Individual Risk is higher than 15% and lower than 30%\",", file = reportanom , sep = "\n", append = TRUE)
          cat("                                                                   \"Individual Risk is higher than 30% and lower than 50%\",", file = reportanom , sep = "\n", append = TRUE)
          cat("                                                                   \"Individual Risk is higher than 50% and lower than 100%\"))", file = reportanom , sep = "\n", append = TRUE)

          cat("risksum2 <- as.data.frame(cbind(table(risksum$class.fixed.name ), prop.table(table(risksum$class.fixed.name ))))", file = reportanom , sep = "\n", append = TRUE)
          cat("risksum2$class.fixed.name <- row.names(risksum2) ", file = reportanom , sep = "\n", append = TRUE)
          cat("risksum2$class.fixed.name <- factor(risksum2$class.fixed.name, levels = c(", file = reportanom , sep = "\n", append = TRUE)
          cat("  \"Individual Risk is higher than 50% and lower than 100%\",", file = reportanom , sep = "\n", append = TRUE)
          cat("  \"Individual Risk is higher than 30% and lower than 50%\",", file = reportanom , sep = "\n", append = TRUE)
          cat("  \"Individual Risk is higher than 15% and lower than 30%\",", file = reportanom , sep = "\n", append = TRUE)
          cat("  \"Individual Risk is higher than 10% and lower than 15%\",", file = reportanom , sep = "\n", append = TRUE)
          cat("  \"Individual Risk is higher than 5% and lower than 10%\",", file = reportanom , sep = "\n", append = TRUE)
          cat("  \"Individual Risk is higher than 1% and lower than 5%\",", file = reportanom , sep = "\n", append = TRUE)
          cat("  \"Individual Risk is higher than 0.1% and lower than 1%\",", file = reportanom , sep = "\n", append = TRUE)
          cat("  \"Individual Risk is lower than 0.1%\"))", file = reportanom , sep = "\n", append = TRUE)
          cat("risksum2$percentreponse <- paste0(round(risksum2$V2*100,digits = 1),\"%\")", file = reportanom , sep = "\n", append = TRUE)


          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("## and now the graph", file = reportanom , sep = "\n", append = TRUE)
          cat("ggplot(risksum2, aes(x = class.fixed.name, y = V1)) +", file = reportanom , sep = "\n", append = TRUE)
          cat("geom_bar(fill = \"#2a87c8\",colour = \"#2a87c8\", stat = \"identity\", width = .8) +", file = reportanom , sep = "\n", append = TRUE)
          cat("guides(fill = FALSE) +", file = reportanom , sep = "\n", append = TRUE)
          cat("geom_label_repel(aes(y = V1, label = percentreponse), fill = \"#2a87c8\", color = 'white') +", file = reportanom , sep = "\n", append = TRUE)
          cat("ylab(\"\") +", file = reportanom , sep = "\n", append = TRUE)
          cat("xlab(\"\") +", file = reportanom , sep = "\n", append = TRUE)
          cat("coord_flip() +", file = reportanom , sep = "\n", append = TRUE)
          cat("ggtitle(\"Number of Observation broken down per individual disclosure risk level\") +", file = reportanom , sep = "\n", append = TRUE)
          cat("theme(plot.title = element_text(face = \"bold\", size = 9 ),", file = reportanom , sep = "\n", append = TRUE)
          cat("plot.background = element_rect(fill = \"transparent\",colour = NA))", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("# Look at high risk records", file = reportanom , sep = "\n", append = TRUE)
          cat("#high.risk.dataanoms <- dataanom.anom[sdc.dataanom@risk$individual[, \"risk\"] > 0.2, selected.keyVars ]", file = reportanom , sep = "\n", append = TRUE)
          cat("#high.risk.dataanoms", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("```", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)

          ## ## Risk for categoric variables #########
          cat("## Risk for categoric variables", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)


          ## k-anonymity ########
          cat("### Observations violating k-anonymity", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("A dataset is said to satisfy k-anonymity for k > 1 if, for each combination of values of quasi-identifiers (e.g. age, gender, occupation, etc.), at least k records exist in the dataset sharing that combination;", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("```{r kanom, echo = FALSE, warning = FALSE, message = FALSE, eval= TRUE, comment = \"\"}", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("## displays information about 2- and 3-anonymity", file = reportanom , sep = "\n", append = TRUE)
          cat("#print(sdc.dataanom, type = \"kAnon\")", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)

          cat("cat(\" # of observations that violate 2-anonymity \n\")", file = reportanom , sep = "\n", append = TRUE)
          cat("nrow(dataanom.anom[sdc.dataanom@risk$individual[,2] < 2,])", file = reportanom , sep = "\n", append = TRUE)

          cat("cat(\" # of observations that violate 3-anonymity \n\")", file = reportanom , sep = "\n", append = TRUE)
          cat("nrow(dataanom.anom[sdc.dataanom@risk$individual[,2] < 3,])", file = reportanom , sep = "\n", append = TRUE)

          cat("cat(\" # of observations that violate 5-anonymity \n\")", file = reportanom , sep = "\n", append = TRUE)
          cat("nrow(dataanom.anom[sdc.dataanom@risk$individual[,2] < 5,])", file = reportanom , sep = "\n", append = TRUE)


          cat("# Show lines with variables that violate k-anonymity", file = reportanom , sep = "\n", append = TRUE)
          cat("#dataanom.anom[sdc.dataanom@risk$individual[,2] < 3,]", file = reportanom , sep = "\n", append = TRUE)
          cat("#dataanom.anom[sdc.dataanom@risk$individual[,2] < 5,]", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("```", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)

          ## l-diversity score #######
          cat("### Distribution of the distinct l-diversity score", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("The objective is to avoid that records sharing a combination of key attributes in a k-anonymous data set also share the values for one or more confidential attributes. There's a need to ensure that the sensitive variable has at least l-distinct values for each group of observations with the same pattern of key variables.", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)

          if ( nrow(selected.sensible) == 0) {

            cat(" You have not defined sensitive variables in your disclosure scenerios. \n", file = reportanom , sep = "\n", append = TRUE) } else {


          cat("```{r ldivrisk, echo = FALSE, warning = FALSE, message = FALSE, eval= TRUE, comment = \"\"}", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("for (h in 1:length(selected.sensibleVars))", file = reportanom , sep = "\n", append = TRUE)
          cat(" {", file = reportanom , sep = "\n", append = TRUE)
          cat("  # h <-1", file = reportanom , sep = "\n", append = TRUE)
          cat(" res1 <- ldiversity(dataanom.anom, keyVars = selected.keyVars, ldiv_index = selected.sensibleVars[h])", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat(" cat(paste0(\"Displaying l-didersity risk for: \", selected.sensible[ h, c(\"label\")], \"\n\"))", file = reportanom , sep = "\n", append = TRUE)
          cat(" # res1", file = reportanom , sep = "\n", append = TRUE)
          cat(" print(res1)", file = reportanom , sep = "\n", append = TRUE)
          cat(" }", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("```", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE) }

          ## Local suppression to achieve basic anonymisation target #####
          cat("# Local suppression to achieve basic anonymisation target", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("Based on a given threshold for disclosure risk, the best method to protect a microdata set is hard to  determine  in  general.  For  a  particular microdata  set  the  best SDC  method  depends  on  the  intended uses of the data by the users, the willingness of the statistical agency to disseminate this data set, the legal aspects  of  releasing  these  data,  and  on  the  structure  of  the  data.", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("Local suppression is a non-perturbative method that is typically applied to categorical variables to suppress certain values in at least one variable. Individual values are suppressed in a way that the set of variables with a specific pattern are increased. ", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("Local suppression can be configured either to achieve k-Anonymity with minimum suppression of values or to comply with a record-level risk threshold and suppress values only for records with higher risks than the threshold.", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("## Local suppression for k-anonymity", file = reportanom , sep = "\n", append = TRUE)
          cat("Even after recoding, some combinations of key variable values may still violate k-anonymity, or some records may still have relatively high disclosure risks. Further recoding, however, may not be possible because the data utility would be too low. At this stage, local suppression can be applied.", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("```{r supprk, echo = FALSE, warning = FALSE, message = FALSE, eval= TRUE, comment = \"\"}", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("## displays various information if local suppression has been applied", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("sdc.dataanomk2 <- localSuppression(sdc.dataanom, k = 2, importance = NULL) ", file = reportanom , sep = "\n", append = TRUE)
          cat("sdc.dataanom.localsuppressionk2 <- t(as.data.frame(sdc.dataanomk2@localSuppression$supps))", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("kable(as.data.frame(sdc.dataanom.localsuppressionk2), caption = \"__Table__: Needed suppression to achieve 2-anonymity\") %>% kable_styling(position = \"center\")", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("sdc.dataanomk3 <- localSuppression(sdc.dataanom, k = 3, importance = NULL) ", file = reportanom , sep = "\n", append = TRUE)
          cat("sdc.dataanom.localsuppressionk3 <- t(as.data.frame(sdc.dataanomk3@localSuppression$supps))", file = reportanom , sep = "\n", append = TRUE)
          cat("kable(as.data.frame(sdc.dataanom.localsuppressionk3), caption = \"__Table__: Needed suppression to achieve 3-anonymity\") %>% kable_styling(position = \"center\")", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("```", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)

          ## ## Local suppression for individual risk containment ####
          cat("## Local suppression for individual risk containment", file = reportanom , sep = "\n", append = TRUE)

          cat("When using sample based dataset, k-Anonymity can be difficult to achieve. In this case, using Individual Risk threshold is an option to consider. A 15% threshold can be used as a reference.", file = reportanom , sep = "\n", append = TRUE)

          cat("```{r supprtre, echo = FALSE, warning = FALSE, message = FALSE, eval= TRUE, comment = \"\"}", file = reportanom , sep = "\n", append = TRUE)

          cat("for (h in 1:length(selected.keyVars))", file = reportanom , sep = "\n", append = TRUE)
          cat("{", file = reportanom , sep = "\n", append = TRUE)
            cat("# h <-1  ", file = reportanom , sep = "\n", append = TRUE)

          cat("sdc.dataanom15 <- localSupp(sdc.dataanom, keyVar = selected.keyVars[h], threshold = 0.15)  ", file = reportanom , sep = "\n", append = TRUE)
          cat("sdc.dataanom.localsuppressiont15  <- t(as.data.frame(sdc.dataanom15@localSuppression$supps))", file = reportanom , sep = "\n", append = TRUE)
          cat("sdc.dataanom.localsuppressiont15  <- as.data.frame(sdc.dataanom15@localSuppression$supps)", file = reportanom , sep = "\n", append = TRUE)
          cat("sdc.dataanom.localsuppressiont <- as.data.frame(t(sdc.dataanom.localsuppressiont15))", file = reportanom , sep = "\n", append = TRUE)
          cat("names(sdc.dataanom.localsuppressiont)[1] <- \"Suppression\"", file = reportanom , sep = "\n", append = TRUE)
          cat("cat(\"\n\n\")", file = reportanom , sep = "\n", append = TRUE)
          cat("cat(paste0(\"Needed suppressions to achieve a 15% individual risk threshold for variable: \", selected.key1[h, c(\"label\")]))", file = reportanom , sep = "\n", append = TRUE)
          cat("cat(\"\n\n\")", file = reportanom , sep = "\n", append = TRUE)
          cat("print(sdc.dataanom.localsuppressiont)", file = reportanom , sep = "\n", append = TRUE)

          #  cat("kable(as.data.frame(sdc.dataanom.localsuppressiont15), caption = \"__Table__: Needed suppression to achieve 15% threshold for individual risk\") %>% kable_styling(position = \"center\")", file = reportanom , sep = "\n", append = TRUE)
          cat("}", file = reportanom , sep = "\n", append = TRUE)

          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("#print(sdc.dataanom, type = \"ls\")", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("```", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)

          ## ## Recoding categorical key variables ##########
          #cat("## Recoding categorical key variables", file = reportanom , sep = "\n", append = TRUE)
          #cat("\n", file = reportanom , sep = "\n", append = TRUE)
          #cat("\n", file = reportanom , sep = "\n", append = TRUE)
          #cat("```{r recod, echo = FALSE, warning = FALSE, message = FALSE, eval= TRUE, comment = \"\"}", file = reportanom , sep = "\n", append = TRUE)
          #cat("\n", file = reportanom , sep = "\n", append = TRUE)


          #cat("## shows information about categorical key variables before and after recoding", file = reportanom , sep = "\n", append = TRUE)
          #cat("print(sdc.dataanom, type = \"recode\")", file = reportanom , sep = "\n", append = TRUE)
          #cat("\n", file = reportanom , sep = "\n", append = TRUE)
          #cat("#print(sdc.dataanom, type = \"comp_numvars\")", file = reportanom , sep = "\n", append = TRUE)
          #cat("\n", file = reportanom , sep = "\n", append = TRUE)
          #cat("```", file = reportanom , sep = "\n", append = TRUE)

          ## Annex : Disclosure Risk Control (SDC) treatment #####
          cat("# Annex: Disclosure Risk Control (SDC) treatment", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("Based on a given threshold for disclosure risk, the best method to protect a microdata set is hard to  determine  in  general.  For  a  particular microdata  set  the  best SDC  method  depends  on  the  intended uses of the data by the users, the willingness of the statistical agency to disseminate this data set, the legal aspects  of  releasing  these  data,  and  on  the  structure  of  the  data.", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)

          cat("## For categoric variables", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)

          ### Post-randomization (PRAM) ###
          cat("### Post-randomization (PRAM)", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("Post-randomization is a perturbation, probabilistic method that can be applied to categorical variables. The idea is that the values of a categorical variable in the original microdata file are changed into other categories, taking into account pre-defined transition probabilities.", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("PRAM can not only be applied to key variables, but also to __sensitive variables violating l-diversity__.", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("```{r rando, echo = FALSE, warning = FALSE, message = FALSE, eval= TRUE, comment = \"\"}", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("## displays various information if post-randomization has been applied.", file = reportanom , sep = "\n", append = TRUE)
          cat("#print(sdc.dataanom, type = \"pram\")", file = reportanom , sep = "\n", append = TRUE)
          cat("```", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("## For numeric variables", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("### Microaggregation ", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("Micro-aggregation is a perturbative method that is typically applied to continuous variables. The idea is that records are partitioned into groups; within each group, the values of each variable are aggregated. Typically, the arithmetic mean is used to aggregate the values, but other robust methods are also possible. Individual values of the records for each variable are replaced by the group aggregation value, which is often the mean.", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("### Adding Noise ", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("Adding noise is a perturbative protection method for microdata, which is typically applied to continuous variables. This approach protects data against exact matching with external files if, for example, information on specific variables is available from registers.", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("### Shuffling ", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("Various techniques can be used for masking, such as multiple imputation, general additive data perturbation and the information preserving statistical obfuscation synthetic data generators . These methods are capable of maintaining linear relationships between variables but fail to maintain marginal distributions or non-linear relationships between variables.", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("### Resulting Information Loss", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("Estimates are computed for both the original and perturbed data and then compared. Following", file = reportanom , sep = "\n", append = TRUE)
          cat("are three important information loss measures: ", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat(" * __IL1s__ is a measure that can be interpreted as scaled distances between original and perturbed values for all p continuous key variables. ", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat(" * __eig__ is a measure calculating relative absolute differences between eigenvalues", file = reportanom , sep = "\n", append = TRUE)
          cat("of the co-variances from standardized continuous key variables of the original", file = reportanom , sep = "\n", append = TRUE)
          cat("and perturbed variables. ", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat(" * __lm__ is a measure based on regression models. It is fitted values from a pre-specified model obtained from the original and the modified data. ", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("```{r summarynum, echo = FALSE, warning = FALSE, message = FALSE, eval= TRUE, comment = \"\"}", file = reportanom , sep = "\n", append = TRUE)
          cat("## displays risk- and utility measures for numerical key variables", file = reportanom , sep = "\n", append = TRUE)
          cat("# print(sdc.dataanom, type = \"numrisk\")", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("```", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("```{r calcrisk, echo = FALSE, warning = FALSE, message = FALSE, eval= TRUE, comment = \"\"}", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("#calcRisks(sdc.dataanom)", file = reportanom , sep = "\n", append = TRUE)
          cat("\n", file = reportanom , sep = "\n", append = TRUE)
          cat("```", file = reportanom , sep = "\n", append = TRUE)

          if(app=="shiny"){
            progress$set(message = "Rendering Anonymisation report Now in progress...")
            updateProgress()
          }
        cat("Render Anonymisation report Now. It may take some time... \n ")
        rmarkdown::render(reportanom, clean = TRUE, envir = new.env() )
        ## Put the report in the out folder
        file.rename(paste0(mainDir,"/code/anonymisation-report-",framename,".docx"), paste0(mainDir,"/out/anonymisation_reports/Anonymisation-report-", framename ,"-",Sys.Date(), "-chapter.docx"))
        cat(" Done!! Reports are in the folder OUT - Review the report- furter anonymise and regenerate as needed...\n")

    }
  }, error = function(err) {
    print("kobo_anonymisation_report_ERROR")
    return(structure(err, class = "try-error"))
  })

}
NULL
