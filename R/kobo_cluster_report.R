#' @name kobo_cluster_report
#' @rdname kobo_cluster_report
#' @title  Generate reports with various clusterisation techniques
#'
#' @description  Automatically produce a report exploring potential clusters within survey records.
#'
#'  The report is generated from functions released within FactoMiner & FactoMineR
#'
#'
#' @param  frame kobo or odk exported dataset to use
#' @param output The output format html or aspx if you need to upload on sharepoint), docx (to quickly cut non interesting vz and take note during data interpretation session), pptx (to quickly cut non interesting vz and persent during data interpretation session), Default is html
#' @param form The full filename of the form to be accessed (has to be an xls file).
#' @param app The place where the function has been executed, the default is the console and the second option is the shiny app
#'
#' @author Edouard Legoupil
#'
#'
#' @export kobo_cluster_report
#'
#' @examples
#' \dontrun{
#' kobo_cluster_report(frame =  MainDataFrame , form = "form.xlsx")
#' }
#'

kobo_cluster_report <- function(frame =  MainDataFrame , 
                                form = "form.xlsx", 
                                output ="html", 
                                app = "console") {
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


    # frame <- MainDataFrame
    # framename <- "MainDataFrame"
    framename <- deparse(substitute(frame))

    ## Check that all those selectedVars are in the frame ####
    if (app == "shiny") {
      progress$set(message = "Check that all those selectedVars are in the frame...")
      updateProgress()
    }
    check <- as.data.frame(names(frame))
    names(check)[1] <- "fullname"
    check$id <- row.names(check)

    dico <- utils::read.csv(paste0(mainDir,"/data/dico_",form,".csv"), encoding = "UTF-8", na.strings = "")

    #### Check presence of variable for anom plan...
    if (app == "shiny") {
      progress$set(message = "Check presence of variable for anom plan...")
      updateProgress()
    }
    selected.cluster <- dico[ which(dico$cluster == "yes" & dico$type %in% c("select_one", "calculate") 
                                    ), ]
    selected.cluster <- plyr::join(x = selected.cluster, y = check, by = "fullname", type = "left")
   # selected.cluster <- selected.cluster[!is.na(selected.cluster$id), ]
    selected.clusterVars <- as.character(selected.cluster[ , c("fullname")])
    #selected.clusterVars2 <- as.character(selected.cluster[ , c("name")])
    selected.clusterVars2 <- stringr::str_replace_all(as.character(selected.cluster[ , c("name")]), "_", ".")

    ## getting records unique identifiers
    selected.id <- dico[ which(dico$cluster == "id"), ]
    selected.id <- plyr::join(x = selected.id, y = check, by = "fullname", type = "left")
    selected.id <- selected.id[!is.na(selected.id$id),  ]
    
    ## if no id defined - instanceID used default
    if (nrow(selected.id) != 1) {
      cat("you have not selected a correct unique id, we use instanceID per default")
      selected.idVars <- "instanceID"
    } else {
      selected.idVars <- as.character(selected.id[ , c("fullname")])
      cat(paste("you have configure ",selected.idVars , " as unique id" ))
    }
   
    
    #frame1 <- frame[ , c( selected.idVars)]
    #frame2 <- frame[ , c( selected.clusterVars)]
    
    frame <- frame[ , c( selected.idVars, selected.clusterVars)]
    
    framecluster <- paste0(mainDir,"/data/clustering-report-",framename,".csv")
    if (file.exists(framecluster)) file.remove(framecluster)
    
    utils::write.csv(frame, framecluster,  row.names = FALSE)

    if (nrow(selected.cluster) == 0) {
      cat("You have not selected variables to cluster for your dataset! \n")
      return(structure("You have not selected variables to cluster for your dataset!", class = "try-error"))
    } else {
      if (app == "shiny") {
        progress$set(message = "Generating Multivariate Analysis in progress...")
        updateProgress()
      }
        reportcluster  <- paste0(mainDir,"/code/clustering-report-",framename,".Rmd")
          ## TO DO : CHECK IF FILE EXIST - AND REQUEST USER TO DELETE BEFORE REGENERATING - SUGGESTING TO SAVE PREVIOUS UNDER NEW NAME
          if (file.exists(reportcluster)) file.remove(reportcluster)
        
        ## Start Building the report ##########
        
          cat("---", file = reportcluster , sep = "\n", append = TRUE)
          cat("title: \"Multivariate analysis\"", file = reportcluster , sep = "\n", append = TRUE)
          cat("author: \"Generated with [Koboloader](https://github.com/unhcr/koboloadeR)\"", file = reportcluster , sep = "\n", append = TRUE)
          cat("date: \" `r format(Sys.Date(), '%d %B %Y')`\"", file = reportcluster, sep = "\n", append = TRUE)
          
          
          
          if (output == "docx") {
            
            cat("always_allow_html: yes", file = reportcluster , sep = "\n", append = TRUE)
            cat("output:",file = reportcluster , sep = "\n", append = TRUE)
            cat("  word_document:", file = reportcluster , sep = "\n", append = TRUE)
            cat("    fig_caption: yes", file = reportcluster , sep = "\n", append = TRUE)
            cat("    fig_height: 5", file = reportcluster , sep = "\n", append = TRUE)
            cat("    fig_width: 8", file = reportcluster , sep = "\n", append = TRUE)
            cat("    toc: yes", file = reportcluster , sep = "\n", append = TRUE)
            cat("    toc_depth: 2", file = reportcluster , sep = "\n", append = TRUE)
            cat("    reference_docx: style-unhcr-portrait.docx", file = reportcluster , sep = "\n", append = TRUE)
            cat("---", file = reportcluster , sep = "\n", append = TRUE)
            cat("\n\n", file = reportcluster , sep = "\n", append = TRUE)
            
          } else if (output == "html") {
            
            cat("always_allow_html: yes", file = reportcluster , sep = "\n", append = TRUE)
            cat("output:",file = reportcluster , sep = "\n", append = TRUE)
            cat("  html_document:", file = reportcluster , sep = "\n", append = TRUE)
            cat("    fig_caption: yes", file = reportcluster , sep = "\n", append = TRUE)
            cat("    fig_height: 5", file = reportcluster , sep = "\n", append = TRUE)
            cat("    fig_width: 8", file = reportcluster , sep = "\n", append = TRUE)
            cat("    toc: yes", file = reportcluster , sep = "\n", append = TRUE)
            cat("    toc_depth: 2", file = reportcluster , sep = "\n", append = TRUE)
            cat("    toc_float: yes", file = reportcluster , sep = "\n", append = TRUE)
            cat("    includes:", file = reportcluster , sep = "\n", append = TRUE)
            cat("       in_header: css/header.html", file = reportcluster , sep = "\n", append = TRUE)
            cat("---", file = reportcluster , sep = "\n", append = TRUE)
            cat("\n\n", file = reportcluster , sep = "\n", append = TRUE)
            cat("<link rel=\"stylesheet\" href=\"css/unhcr-bootstrap.css\">", file = reportcluster , sep = "\n", append = TRUE)
            cat("<link rel=\"stylesheet\" href=\"css/style.css\">", file = reportcluster , sep = "\n", append = TRUE)
            cat("<link rel=\"stylesheet\" href=\"css/unhcr-header.css\">", file = reportcluster , sep = "\n", append = TRUE)
            cat("\n\n", file = reportcluster , sep = "\n", append = TRUE)
            
          }else if (output == "aspx") {
            
            cat("always_allow_html: yes", file = reportcluster , sep = "\n", append = TRUE)
            cat("output:",file = reportcluster , sep = "\n", append = TRUE)
            cat("  html_document:", file = reportcluster , sep = "\n", append = TRUE)
            cat("    fig_caption: yes", file = reportcluster , sep = "\n", append = TRUE)
            cat("    fig_height: 5", file = reportcluster , sep = "\n", append = TRUE)
            cat("    fig_width: 8", file = reportcluster , sep = "\n", append = TRUE)
            cat("    toc: yes", file = reportcluster , sep = "\n", append = TRUE)
            cat("    toc_depth: 2", file = reportcluster , sep = "\n", append = TRUE)
            cat("    toc_float: yes", file = reportcluster , sep = "\n", append = TRUE)
            cat("    includes:", file = reportcluster , sep = "\n", append = TRUE)
            cat("       in_header: css/header.html", file = reportcluster , sep = "\n", append = TRUE)
            cat("---", file = reportcluster , sep = "\n", append = TRUE)
            cat("\n\n", file = reportcluster , sep = "\n", append = TRUE)
            cat("<link rel=\"stylesheet\" href=\"css/unhcr-bootstrap.css\">", file = reportcluster , sep = "\n", append = TRUE)
            cat("<link rel=\"stylesheet\" href=\"css/style.css\">", file = reportcluster , sep = "\n", append = TRUE)
            cat("<link rel=\"stylesheet\" href=\"css/unhcr-header.css\">", file = reportcluster , sep = "\n", append = TRUE)
            cat("\n\n", file = reportcluster , sep = "\n", append = TRUE)
            
          } else if (output == "pptx") {
            
            cat("always_allow_html: yes", file = reportcluster , sep = "\n", append = TRUE)
            cat("output:",file = reportcluster , sep = "\n", append = TRUE)
            cat("  powerpoint_presentation:", file = reportcluster , sep = "\n", append = TRUE)
            cat("    fig_caption: yes", file = reportcluster , sep = "\n", append = TRUE)
            cat("    fig_height: 9", file = reportcluster , sep = "\n", append = TRUE)
            cat("    fig_width: 18", file = reportcluster , sep = "\n", append = TRUE)
            cat("    reference_doc: templateUNHCR.pptx", file = reportcluster , sep = "\n", append = TRUE)
            cat("    slide_level: 2", file = reportcluster , sep = "\n", append = TRUE)
            cat("---", file = reportcluster , sep = "\n", append = TRUE)
            cat("\n\n", file = reportcluster , sep = "\n", append = TRUE)
          }
          
          
          
          

         
          
          
          
          # , [FactoMineR](http://factominer.free.fr/factomethods/multiple-correspondence-analysis.html) & [FactoExtra](http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/114-mca-multiple-correspondence-analysis-in-r-essentials/)
          
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat("```{r setup, include=FALSE}", file = reportcluster , sep = "\n", append = TRUE)
          cat("knitr::opts_chunk$set(echo = TRUE)", file = reportcluster , sep = "\n", append = TRUE)
          
          
          cat("using <- function(...) {", file = reportcluster , sep = "\n", append = TRUE)
          cat("libs <- unlist(list(...))", file = reportcluster , sep = "\n", append = TRUE)
          cat("req <- unlist(lapply(libs,require,character.only = TRUE))", file = reportcluster , sep = "\n", append = TRUE)
          cat("      need <- libs[req == FALSE]", file = reportcluster , sep = "\n", append = TRUE)
          cat("        if (length(need) > 0) { ", file = reportcluster , sep = "\n", append = TRUE)
          cat("        install.packages(need, repos = 'http://cran.us.r-project.org')", file = reportcluster , sep = "\n", append = TRUE)
          cat("         lapply(need,require,character.only = TRUE)", file = reportcluster , sep = "\n", append = TRUE)
          cat("    }", file = reportcluster , sep = "\n", append = TRUE)
          cat("  }", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n\n", file = reportcluster , sep = "\n", append = TRUE)
          
          cat("## Load all required packages", file = reportcluster , sep = "\n", append = TRUE)
          cat("using('knitr', 'FactoMineR', 'factoextra', 'ggplot2', 'reshape2', 'plyr', 'stringr', 'koboloadeR')", file = reportcluster , sep = "\n", append = TRUE)
          cat("options(scipen = 999) # turn-off scientific notation like 1e+48", file = reportcluster , sep = "\n", append = TRUE)
        
          cat("```", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          
          
          
          cat("```{r , echo=FALSE, warning=FALSE, message=FALSE, cache=TRUE}", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat("mainDir <- getwd()", file = reportcluster , sep = "\n", append = TRUE)
          cat("mainDirroot <- substring(mainDir, 0 , nchar(mainDir) - 5)", file = reportcluster , sep = "\n", append = TRUE)
          cat("## Load all required packages", file = reportcluster , sep = "\n", append = TRUE)
          cat("library(koboloadeR)", file = reportcluster , sep = "\n", append = TRUE)
          #cat("kobo_load_data()", file = reportcluster , sep = "\n", append = TRUE)
          cat("## Provide below the name of the form in xsl form - format should be xls not xlsx", file = reportcluster , sep = "\n", append = TRUE)
          cat("form <- \"form.xls\"", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat("dico <- utils::read.csv(paste0(mainDirroot,\"/data/dico_\",form,\".csv\"), encoding = \"UTF-8\", na.strings = \"\")", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)

          cat(paste0("datacluster <-  utils::read.csv(paste0(mainDirroot,\"/data/clustering-report-",framename,".csv\"), sep = \",\", encoding = \"UTF-8\", na.strings = \"\")"), file = reportcluster , sep = "\n", append = TRUE)
         
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat("```", file = reportcluster , sep = "\n", append = TRUE)
          #cat("# Executive Summary", file = reportcluster , sep = "\n", append = TRUE)
          #cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          #cat("***", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat("# Methodology: an introduction to statistical clustering", file = reportcluster , sep = "\n", append = TRUE)
          cat("An important challenge to understand the profile of a population is to discover how categories interact together. 
              To describe profiles within a population, it is necessary to interlace defined by multiple categories. 
              Univariate analysis does not allow to get a synthetic vision from a large set of variables that can describe a population. 
              Because of inherent brain & cognitive limitations, it is challenging to process together more than 7 categories and to make
              sense out of too many graphs.", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat("Since the 70's, Social scientist have developed advanced __exploratory__ techniques that allow to discover statistical profiles
              among a specific population. Clustering is an exploratory data analysis tool wich aims to group a set of records in such a way 
              that records in the same group are more similar to each other than to those in other groups. 
              [Multiple Correspondence Analysis (MCA)](https://en.wikipedia.org/wiki/Multiple_correspondence_analysis) together with
              [Hierarchical Classification on Principle Components](http://factominer.free.fr/classical-methods/hierarchical-clustering-on-principal-components.html) allow to 
              process nominal categorical data (as it is the case for Refugee biodata) in order to detect and represent the underlying structures in a data set. 
              This approach is based on looking at description to generate induction rather than testing an hypothesis according to model.", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        #  cat("![Correspondence Analysis Handbook : J.-P. Benzecri (1992).](bencrezi.jpg)", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat("This approach implies 5 simple steps:", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat(" 1.  __Dimensionnality reduction__: Reduce the numbers of dimensions to two main composite dimensions in order to represent each observation in a 2D space.", file = reportcluster , sep = "\n", append = TRUE)
          cat(" 2.  __Composition of the 2 axis__: Describe the components of each axis", file = reportcluster , sep = "\n", append = TRUE)
          cat(" 3.  __Categories Representation__: This allow to visualise how close different variables are", file = reportcluster , sep = "\n", append = TRUE)
          cat(" 4.  __Clustering__: Records are grouped according to profile using the underlying proximity between variables", file = reportcluster , sep = "\n", append = TRUE)
          cat(" 5.  __Overview of groups__: Display the frequency of each group as well the breakdown of specific needs within each group.", file = reportcluster , sep = "\n", append = TRUE)
          cat(" 6.  __Modalities within each group__: Describe the main variable modalities that describe the profile.", file = reportcluster , sep = "\n", append = TRUE)
         cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          
          
          cat("# 1. Dimensionnality reduction  ", file = reportcluster , sep = "\n", append = TRUE)
          cat("The initial step is to first select the variable to use for the analysis. \n", file = reportcluster , sep = "\n", append = TRUE)

          ## First chunk ########
          cat("```{r, echo=FALSE, warning=FALSE}", file = reportcluster , sep = "\n", append = TRUE)
          cat("## Check that all those selectedVars are in the frame...", file = reportcluster , sep = "\n", append = TRUE)
          cat("check <- as.data.frame(names(datacluster))", file = reportcluster , sep = "\n", append = TRUE)
          cat("names(check)[1] <- \"fullname\"", file = reportcluster , sep = "\n", append = TRUE)
          cat("check$id <- row.names(check)", file = reportcluster , sep = "\n", append = TRUE)
          cat("check <- plyr::join(x = check, y = dico, by = \"fullname\", type = \"left\")", file = reportcluster , sep = "\n", append = TRUE)

          
          cat("selected.cluster <- dico[ which(dico$cluster == \"yes\" & dico$type == \"select_one\" ), ]", file = reportcluster , sep = "\n", append = TRUE)
          cat("selected.cluster <- plyr::join(x = selected.cluster, y = check, by = \"fullname\", type = \"left\")", file = reportcluster , sep = "\n", append = TRUE)
         # cat("selected.cluster <- selected.cluster[!is.na(selected.cluster$id), ]", file = reportcluster , sep = "\n", append = TRUE)
          cat("selected.clusterVars <- as.character(selected.cluster[ , c(\"fullname\")])", file = reportcluster , sep = "\n", append = TRUE)
          cat("#selected.clusterVars2 <- as.character(selected.cluster[ , c(\"name\")])", file = reportcluster , sep = "\n", append = TRUE)
          cat("selected.clusterVars2 <- stringr::str_replace_all(as.character(selected.cluster[ , c(\"name\")]), \"_\", \".\")", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          
          cat("selected.id <- dico[ which(dico$cluster == \"id\"), ]", file = reportcluster , sep = "\n", append = TRUE)
          cat("selected.id <- plyr::join(x = selected.id, y = check, by = \"fullname\", type = \"left\")", file = reportcluster , sep = "\n", append = TRUE)
          cat("selected.id <- selected.id[!is.na(selected.id$id),  ]", file = reportcluster , sep = "\n", append = TRUE)
          cat("if (nrow(selected.id) != 1) {\n", file = reportcluster , sep = "\n", append = TRUE)
          cat("selected.idVars <- \"instanceID\"", file = reportcluster , sep = "\n", append = TRUE)
          cat("} else {\n", file = reportcluster , sep = "\n", append = TRUE)
          cat("selected.idVars <- as.character(selected.id[ , c(\"fullname\")])", file = reportcluster , sep = "\n", append = TRUE)
          cat("}\n", file = reportcluster , sep = "\n", append = TRUE)


          cat("selected.idVars <- as.character(selected.id[ , c(\"fullname\")])", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat("#############################################################################", file = reportcluster , sep = "\n", append = TRUE)
          cat("##### Step : subsetting data frame", file = reportcluster , sep = "\n", append = TRUE)
          cat("# Create subset of file with observation and selected variables & remove duplicated rows based on IDH", file = reportcluster , sep = "\n", append = TRUE)
          cat("datacluster2 <- datacluster[ , c( selected.idVars, selected.clusterVars)]", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          #cat("row.names(datacluster2) <- datacluster2[ , c( selected.idVars)]", file = reportcluster , sep = "\n", append = TRUE)
          cat("datacluster2[ , c( selected.idVars)] <- NULL", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat("## Convert to factor variable as they are categoric", file = reportcluster , sep = "\n", append = TRUE)
          cat("datacluster2[,c(selected.clusterVars)] <- lapply(datacluster2[,c(selected.clusterVars)], factor)", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat("## Rename Dataframe with short name", file = reportcluster , sep = "\n", append = TRUE)
          cat("check2 <- as.data.frame(names(datacluster2))", file = reportcluster , sep = "\n", append = TRUE)
          cat("names(check2)[1] <- \"fullname\"", file = reportcluster , sep = "\n", append = TRUE)
          cat("check2$id <- row.names(check2)", file = reportcluster , sep = "\n", append = TRUE)
          cat("check2 <- plyr::join(x = check2, y = dico, by = \"fullname\", type = \"left\")", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat("## Take out special characters from those name", file = reportcluster , sep = "\n", append = TRUE)
          cat("check2$name2 <- stringr::str_replace_all(check2$name, \"_\", \".\")", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat("names(datacluster2) <- check2[, c(\"name2\")]", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat("datacluster3 <- datacluster2[complete.cases(datacluster2), ]", file = reportcluster , sep = "\n", append = TRUE)
          cat("if(nrow(datacluster3) > 10000) { samplesize <- 10000 } else { samplesize <- nrow(datacluster3)} ", file = reportcluster , sep = "\n", append = TRUE)
          
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          
          cat("data.sample <- datacluster2[sample(1:nrow(datacluster2), samplesize, replace = FALSE), ]", file = reportcluster , sep = "\n", append = TRUE)
          
          cat("selected <- dico[ dico$fullname %in% selected.clusterVars, c(\"fullname\", \"labelReport\") ]", file = reportcluster , sep = "\n", append = TRUE)
          cat("row.names(selected) <- NULL", file = reportcluster , sep = "\n", append = TRUE)
          cat("knitr::kable (selected , caption = \"Selected variable for the analysis\")", file = reportcluster , sep = "\n", append = TRUE)
          cat("```", file = reportcluster , sep = "\n", append = TRUE)
          
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat("The algorithm is then used on those variable in order to assemble them within 2 dimensions. \n", file = reportcluster , sep = "\n", append = TRUE)
          
          cat("```{r, echo=FALSE, warning=FALSE}", file = reportcluster , sep = "\n", append = TRUE)
          
          
          
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat("### Checking sample modalities frequency", file = reportcluster , sep = "\n", append = TRUE)
          cat("#prop.table(table(data.sample$cool1Cat, useNA = \"ifany\"))", file = reportcluster , sep = "\n", append = TRUE)
          cat("#prop.table(table(data2$cool1Cat, useNA = \"ifany\"))", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          
          ## 1. Multiple correspondence analysis #####
          cat("# Dimensionality reduction: Multiple correspondance analysis", file = reportcluster , sep = "\n", append = TRUE)
          cat("data.mca <- FactoMineR::MCA(data.sample, graph = FALSE)", file = reportcluster , sep = "\n", append = TRUE)
          cat("factoextra::fviz_mca_ind(data.mca, col.ind = \"blue\", label = \"none\", addEllipses = TRUE, ellipse.level = 0.95, jitter = list(what = \"point\")) +", file = reportcluster , sep = "\n", append = TRUE)
          cat("  theme_minimal() +", file = reportcluster , sep = "\n", append = TRUE)
          cat("  labs(title = \"Geometric representation of Individuals in a cloud of points\" )", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat("```", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          
          # 2. Composition of the 2 axis ####
          cat("# 2. Composition of the 2 axis ", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat("The graphs below describe the componnents within each of the dimnensions from the previous representation. \n", file = reportcluster , sep = "\n", append = TRUE)
          cat("```{r, echo=FALSE, warning=FALSE}", file = reportcluster , sep = "\n", append = TRUE)
          cat("## Contribution of top 10 variables", file = reportcluster , sep = "\n", append = TRUE)
          cat("factoextra::fviz_contrib(data.mca, choice = \"var\", axes = 1, top = 15, sort.val = \"asc\") + theme_minimal() +", file = reportcluster , sep = "\n", append = TRUE)
          cat("  labs(title = \"Contribution of the top 15  variables to the first axis (Dim 1)\", x = \"\", ", file = reportcluster , sep = "\n", append = TRUE)
          cat( "caption = \"The reference dashed line corresponds to the expected value if the contribution where uniform\")  +", file = reportcluster , sep = "\n", append = TRUE)
          cat("  coord_flip()", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          
          cat("factoextra::fviz_contrib(data.mca, choice = \"var\", axes = 2, top = 15, sort.val = \"asc\") + theme_minimal() +", file = reportcluster , sep = "\n", append = TRUE)
          cat("  labs(title = \"Contribution of the top 15  variables to the second axis (Dim 2)\", x = \"\", ", file = reportcluster , sep = "\n", append = TRUE)
          cat( "caption = \"The reference dashed line corresponds to the expected value if the contribution where uniform\")  +", file = reportcluster , sep = "\n", append = TRUE)
          cat("  coord_flip()", file = reportcluster , sep = "\n", append = TRUE)
          cat("```", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
         
          ## 3. Categories Representation #### 
           cat("# 3. Categories Representation  ", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat(" It is also possible to map variable modalities on that bidimensional space. \n", file = reportcluster , sep = "\n", append = TRUE)
          cat("```{r, echo=FALSE, warning=FALSE}", file = reportcluster , sep = "\n", append = TRUE)
          cat("factoextra::fviz_mca_var(data.mca, labelsize = 2, geom = \"text\", repel = TRUE, pointsize = 0, alpha.var = \"contrib\", palette = \"RdBu\", select.var = list(contrib = 20)) +", file = reportcluster , sep = "\n", append = TRUE)
          cat("  labs(title = \"Variable categories representation for the top 15 contributions\" )  +", file = reportcluster , sep = "\n", append = TRUE)
          cat("  theme_minimal()", file = reportcluster , sep = "\n", append = TRUE)
          cat("```", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          
          
          ### 4. Hierarchical Classification ####
          cat("# 4. Hierarchical Classification  ", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat(" We can now group together the observation based on their proximities. \n", file = reportcluster , sep = "\n", append = TRUE)
          cat("```{r, echo=FALSE, warning=FALSE}", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat("## Predict projection for new rows with Multiple Correspondence Analysis", file = reportcluster , sep = "\n", append = TRUE)
          cat("#data.predict <- predict(data.mca, data2)", file = reportcluster , sep = "\n", append = TRUE)
          cat("#data.mca.hcpc <- FactoMineR::HCPC(data.mca, nb.clust = -1, min = 3, max = 4, graph = FALSE, order = FALSE, consol = TRUE, kk = 1000)", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          
          
          cat("data.mca.hcpc <- HCPC(data.mca, graph = FALSE)", file = reportcluster , sep = "\n", append = TRUE)
          cat("# Visualize dendrogram", file = reportcluster , sep = "\n", append = TRUE)
          cat("#factoextra::fviz_dend(data.mca.hcpc, show_labels = FALSE, rect = TRUE)", file = reportcluster , sep = "\n", append = TRUE)
          cat("#factoextra::fviz_dend(data.mca.hcpc, cex = 0.5, k = 4,  color_labels_by_k = TRUE,  title=\"Hierarchical classification\",  horiz=TRUE, rect = TRUE)", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat("plot(data.mca.hcpc, choice = \"tree\", title = \"Hierarchical classification\",  horiz = TRUE, tree.barplot = FALSE, cex = 0.6)", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat("plot(data.mca.hcpc, title = \"3D view within the cloud of point of the profiles\")", file = reportcluster , sep = "\n", append = TRUE)
          cat("plot(data.mca.hcpc, choice = \"map\", title = \"profiles within the cloud of points\")", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          
          cat("# Visualize cluster", file = reportcluster , sep = "\n", append = TRUE)
          cat("#fviz_cluster(data.mca.hcpc, ellipse.type = \"convex\")", file = reportcluster , sep = "\n", append = TRUE)

          cat("kable(data.mca.hcpc$desc.var$test.chi2, digits = 4, caption = \"Ordered importance of variable contributions to profiles\")", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat("```", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          
          ## 5. Overview of groups  #####
          cat("# 5. Overview of groups  ", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          
          cat(" Once defined, we can now have an idea of the frequency of each profile. \n", file = reportcluster , sep = "\n", append = TRUE)
          cat("```{r, echo=FALSE, warning=FALSE}", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat("cluster <- data.mca.hcpc$data.clust", file = reportcluster , sep = "\n", append = TRUE)
          cat("cluster$clust <- as.character(cluster$clust)", file = reportcluster , sep = "\n", append = TRUE)
          cat("cluster$clust[cluster$clust == 1] <- \"Profile 1\"", file = reportcluster , sep = "\n", append = TRUE)
          cat("cluster$clust[cluster$clust == 2] <- \"Profile 2\"", file = reportcluster , sep = "\n", append = TRUE)
          cat("cluster$clust[cluster$clust == 3] <- \"Profile 3\"", file = reportcluster , sep = "\n", append = TRUE)
          cat("cluster$clust[cluster$clust == 4] <- \"Profile 4\"", file = reportcluster , sep = "\n", append = TRUE)
          cat("cluster$clust[cluster$clust == 5] <- \"Profile 5\"", file = reportcluster , sep = "\n", append = TRUE)
          cat("cluster$clust[cluster$clust == 6] <- \"Profile 6\"", file = reportcluster , sep = "\n", append = TRUE)
          cat("cluster$clust <- as.factor(cluster$clust)", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat("#names(cluster)", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat("ggplot(cluster, aes(x = clust)) +", file = reportcluster , sep = "\n", append = TRUE)
          cat("      geom_bar(aes(y = ..count.. / sapply(PANEL, FUN = function(x) sum(count[PANEL == x]))),", file = reportcluster , sep = "\n", append = TRUE)
          cat("               fill = \"#2a87c8\", colour = \"#2a87c8\") +", file = reportcluster , sep = "\n", append = TRUE)
          cat("      #facet_wrap(~subgov, ncol = 4) +", file = reportcluster , sep = "\n", append = TRUE)
          cat("      guides(fill = FALSE) +", file = reportcluster , sep = "\n", append = TRUE)
          cat("      ylab(\"Frequency\") +", file = reportcluster , sep = "\n", append = TRUE)
          cat("      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) +", file = reportcluster , sep = "\n", append = TRUE)
          cat("      xlab(\"\") +", file = reportcluster , sep = "\n", append = TRUE)
          cat("      coord_flip() +", file = reportcluster , sep = "\n", append = TRUE)
          cat("      ggtitle(\"Percentage of case per profile within the sample\") +", file = reportcluster , sep = "\n", append = TRUE)
          cat("      theme(plot.title = element_text(face = \"bold\", size = 9),", file = reportcluster , sep = "\n", append = TRUE)
          cat("            plot.background = element_rect(fill = \"transparent\",colour = NA)) +", file = reportcluster , sep = "\n", append = TRUE)
          cat("            kobo_unhcr_style_bar()", file = reportcluster , sep = "\n", append = TRUE)
          
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat("```", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          
          # 6. Modalities within each group #####
          cat("# 6. Modalities within each group  ", file = reportcluster , sep = "\n", append = TRUE)
          
          
          cat("The variable modalities in each of the profiles can be __interpreted__ using the following information: ", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat(" * _Cla/Mod_ is the pourcentage of records who have that modality and who are in that the profile (for instance: 50.3% of the cases who have a head of family who marrital status is single also belongs to profile 1)", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat(" * _Mod/Cla_ is the pourcentage of records in that profile who have that modality (for instance: 81.5% of the cases of profile  1 have a head of family who marrital status is single )", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat(" * _Global_ is the global pourcentage of all records who have that modality (for instance: in all population, 51.67% of of the cases who have a head of family who marrital status is single.)", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat(" * _p-value_ provides an idea of the significant of the difference between Mod/Cla proportation & Global. If p-value is less than 0.05, then one category is significantly linked to another categories ", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat(" * _v-test_ If the v-test is positive, it means that the category is over-expressed for the category and if the v-test is negative it means that the category is under-expressed for the category.What is interesting in the v-test is only the sign of the v-test.", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat("Only variable modalities for which critical probability is below 0.02 are used.", file = reportcluster , sep = "\n", append = TRUE)
          
          
          cat(" The tables below allows to describe each profile in narrative terms. \n", file = reportcluster , sep = "\n", append = TRUE)
          
          cat("```{r, echo=FALSE, warning=FALSE}", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat("kable(data.mca.hcpc$desc.var$category$`1`, digits = 2, caption = \"Description of modalities contribution to Profile 1\")", file = reportcluster , sep = "\n", append = TRUE)
          cat("kable(data.mca.hcpc$desc.var$category$`2`, digits = 2, caption = \"Description of modalities contribution to Profile 2\")", file = reportcluster , sep = "\n", append = TRUE)
          cat("kable(data.mca.hcpc$desc.var$category$`3`, digits = 2, caption = \"Description of modalities contribution to Profile 3\")", file = reportcluster , sep = "\n", append = TRUE)
          cat("kable(data.mca.hcpc$desc.var$category$`4`, digits = 2, caption = \"Description of modalities contribution to Profile 4\")", file = reportcluster , sep = "\n", append = TRUE)
          cat("kable(data.mca.hcpc$desc.var$category$`5`, digits = 2, caption = \"Description of modalities contribution to Profile 5\")", file = reportcluster , sep = "\n", append = TRUE)
          cat("```", file = reportcluster , sep = "\n", append = TRUE)
          cat("\n", file = reportcluster , sep = "\n", append = TRUE)
          cat("__Reference__: [hierarchical-clustering-on-principal-components](http://www.sthda.com/english/wiki/hcpc-hierarchical-clustering-on-principal-components-hybrid-approach-2-2-unsupervised-machine-learning).", file = reportcluster , sep = "\n", append = TRUE)


          if(app=="shiny"){
            progress$set(message = "Rendering Clustering report Now in progress...")
            updateProgress()
          }
        
          
        cat("Render Clustering report Now. It may take some time... \n ")
        
        
        if (output == "docx") {
          
          cat(paste( " - Render word output report for ",reportcluster))
          mainDir <- kobo_getMainDirectory()
          rmarkdown::render(reportcluster, clean = TRUE, envir = new.env() )
          ## Put the report in the out folder
          mainDir <- kobo_getMainDirectory()
          file.rename(paste(mainDir,"/code/clustering-report-",framename,".docx", sep = ""), paste0(mainDir,"/out/cluster_reports/Clustering-report-", framename,"-" ,Sys.Date(), "-report.docx"))
          ## Clean  memory
          gc()
          
        } else if (output == "html") {
          
          cat(paste( " - Render html output report for ",reportcluster))
          mainDir <- kobo_getMainDirectory()
          rmarkdown::render(reportcluster, clean = TRUE, envir = new.env() )
          ## Put the report in the out folder
          mainDir <- kobo_getMainDirectory()
          file.rename(paste(mainDir,"/code/clustering-report-",framename,".html", sep = ""), paste0(mainDir,"/out/cluster_reports/Clustering-report-", framename,"-" ,Sys.Date(), "-report.html"))
          ## Clean  memory
          gc()
          
        } else if (output == "aspx") {
          
          cat(paste( " - Render aspx output - for sharepoint hosting - report for ",reportcluster))
          mainDir <- kobo_getMainDirectory()
          rmarkdown::render(reportcluster, clean = TRUE, envir = new.env() )
          ## Put the report in the out folder
          mainDir <- kobo_getMainDirectory()
          file.rename(paste(mainDir,"/code/clustering-report-",framename,".html", sep = ""), paste0(mainDir,"/out/cluster_reports/Clustering-report-", framename,"-" ,Sys.Date(), "-report.aspx"))
          ## Clean  memory
          gc()
          
        } else if (output == "pptx") {
          
          cat(paste(" - Render PowerPoint output report for ",reportcluster))
          mainDir <- kobo_getMainDirectory()
          rmarkdown::render(reportcluster, clean = TRUE, envir = new.env() )
          ## Put the report in the out folder
          mainDir <- kobo_getMainDirectory()
          file.rename(paste(mainDir,"/code/clustering-report-",framename,".pptx", sep = ""), paste0(mainDir,"/out/cluster_reports/Clustering-report-", framename,"-" ,Sys.Date(), "-report.pptx"))
          ## Clean  memory
          gc()
        }
        
        cat(" Done!! Reports are in the folder OUT - Review the report- furter review your clustering assumptions and regenerate as needed...\n")

    }
  }, error = function(err) {
    print("kobo_cluster_report_ERROR")
    return(structure(err, class = "try-error"))
  })
}
NULL
