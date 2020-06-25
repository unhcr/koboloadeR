#' @name kobo_load_packages
#' @rdname kobo_load_packages
#' @title Load Packages
#'
#' @description Load all necessary packages for koboloadeR
#'
#' @return No return
#'
#' @author Edouard Legoupil, Maher Daoud
#'
#' @examples
#' \dontrun{
#' kobo_load_packages()
#' }
#'
#'
#' @export kobo_load_packages
#'

kobo_load_packages <- function() {
  ## A curated list of packages
  ## @edouard_lgp
  ## additional inspiration form https://github.com/ThinkR-open/installR/blob/master/install-mini.R

  percent <- function(x, digits = 2, format = "f", ...) {
    paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
  }

  mon_print <- function(synth){
    N <- nrow(synth)
    if (is.null(N)) {  N <- 1
    synth <- matrix(synth, nrow = 1) }
    for (i in 1:N) {
      tp <- synth[i,]
      tp[1]  <- stringr::str_pad(tp[1], 17, side = "right")
      cat(tp,"\n")
    }
  }


  ## Few options to set up
  try(setInternet2(use = TRUE),silent = TRUE)
  packs <- installed.packages()
  exc <- names(packs[,'Package'])
  local({r <- getOption("repos"); r["CRAN"] <- "https://cran.r-project.org/"; options(repos = r)})


  ## Fixing automatically the "Unable to move temporary installation" R issue
  ## https://stackoverflow.com/questions/47760372/non-interactive-editing-fixing-of-invisible-and-locked-function
  #trace(utils:::unpackPkgZip, edit=TRUE) # your edits
  #unpackPkg <- utils:::unpackPkgZip # copy function

  # save edited function to file Into C:\Users\myself\Documents\R\R-3.4.4\etc\Rprofile.SITE:
  #saveRDS(unpackPkg, file = "unpack.rds")
  #unpack <- readRDS(file = "C:/Users/myself/Documents/R/R-3.4.4/etc/unpack.rds")
  #utils::assignInNamespace("unpackPkgZip", unpack, ns = "utils") # overwrite default



  ##This should detect and install missing packages before loading them

  packages <- unique(sort(c(

    ## Tydiverse #########
    # https://www.tidyverse.org

    #"dplyr",  "doBy","tidyr", ## Data manipulation
    #"ggplot2", ## advanced graphics
    # "stringr",
    "tidyverse",
    "plyr",
    "stringr",
    "qrmtools",

    "ggrepel", ## getting nice labels in ggplot2
    "ggthemes", ## Customised themes for ggplot2: excel, stata, economist, tufte, wall street journal...
    #"ggseas", ## seasonal adjustemnt with GGplot2
    #"ggvis", ## interactive grammar of graphics

    "extrafont", ##" load additional font

    ## Data Manipulation ####
    "reshape2", # package to easily melt data to long form
    "lubridate", "date","gdata",## playing with date
    "zoo", ## Manage reformatting of date
    "data.table", ## Fast aggregation of large data (e.g. 100GB in RAM), fast ordered joins, fast add/modify/delete of columns
    "DT", # A Wrapper of the JavaScript Library 'DataTables
    "stringdist", "stringi", ## string manipulation
    "forcats", ## Recoding cat

    ##### Visualisation ########
    #"lattice",
    #"graphics",
    # Visualiation of correlation Matrix  ##
    "corrplot",
    ## drawing ellipses and ellipse-like confidence regions
    "ellipse",
    #"xkcd", ## Style from the xkcd comics
    #"scatterplot3d",
    #"igraph", #network analysis and visualisation
    # Assemble graphics together
    "grid", "gridExtra",
    "gtable", # Arrange 'Grobs' in Tables
    "ggfortify",
    "ggtern",   ##ternary plot with ggplot2
    "vcd",  # Visualisation of categorical data
    #Scale Functions for Visualization
    "scales",

    # Color palette ####
    "RColorBrewer",
    "viridis",

    #####  Mapping ########
    "rgdal","rgeos","sp","maptools", ## standard Geo manipulation packages
    "ggmap", ## get background from webmapping API
    "raster","cartography", ## packages used for the maps --
    "classInt",  ## used for univariate classification
    "hexbin", ## Hexagrid viz
    #"deldir", # delaunay triangulation & Voronoi
    #"fields", ## Tools for Spatial Data

    #### Pre-modelling  ########
    ## Missing value imputation ###
    # "missForest",  "missMDA", "Amelia",
    ## Outlier Detection ####
    # "outliers",  "evir",

    ### Feature Selection ########
    #"features",
    # "RRF",
    # "Boruta", # wrapper for feature selection algorythm

    ### Dimension Reduction ########
    #"stats",  # Dimensionality Reduction Algorithms princomp
    # "CCP",

    # "htmlTable", # generate a detailled describtion of a given dataset

    ## Multivariate analysis - MCA ########
    "FactoMineR",
    "ade4",
    "factoextra",
    ##"FactoInvestigate",

    ### Packages for Modeling Stage ########
    "Hmisc", #  Contains many functions useful for data analysis, high-level graphics, utility operations, functions for computing
    "car", ## ## Companion to Applied Regression
    #  "gbm", # Generalized Boosted Regression Models
    #  "rminer", "CORElearn",  # ordinal Regression
    "caret", # Gradient Boosting & AdaBoost
    #  "bigRR",  ## Classification


    ###  Machine Learning ########
    "e1071", #SVM (Support Vector Machine)
   ## "knncat", # KNN (K- Nearest Neighbors)
    "randomForest", # randomForest
    "quantreg", #Quantile regression
    "quantregForest", #Regression quantile forest
    "xgboost",
    "GGally",
    "ROCR",

    ###  Recursive Partitioning and Regression Trees
    "rpart",
    "rpart.plot",


    ###  Time Series & survival analysis ########
    #"forecast", "ltsa",
    #"survival", "BaSTA",
    #"pastecs", #Analysis of Space-Time Ecological Series

    ###  Lasso and Elastic-Net Regularized Generalized Linear Models  ########
    #"glmnet",
    "lme4", # Linear Mixed-Effects Models  #
    "MASS",
    #"VGAM", #Vector Generalized Linear and Additive Models
    #"aod", ## Analysis of Overdispersed Data
    "questionr",

    ### Cluster analysis #####
    #"cluster", "cba", "Rankcluster",


    ### Post Modeling Stage ########
    ## Testing Linear Regression Models
    #  "lmtest",
    ## Global Validation of Linear Models Assumptions
    #  "gvlma",
    ## General Model Validation
    #  "lsmeans", "comparison",
    ## Regression validation
    #  "regtest", "ACD",
    ## Classification validation
    #  "binomTools","Daim",
    ## Clustering valisation
    # "clusteval","sigclust",

    ### ROC Analysis ########
    "pROC",
    # "timeROC",

    ### Packages for Survey data management ########
    "sampling", ## Survey Sampling
    #  "simFrame",
    "survey",  ##Analysis of Complex Survey Samples



    ## Benchmark and Frontier Analysis Using Data Envelopmenbt Aanalysis ########
    #  "Benchmarking",
    #  "pwr", # Power Analysis allows  to determine the sample size required to detect an effect of a given size with a given degree of confidence.

    ## Text mining ########
    #  "tm", "twitteR" ,
    ## Word Clouds
    #  "wordcloud",
    ## Interactive Visualization of Topic Models
    #  "LDAvis",

    # Applied economtrics with R ########
    # "AER",

    ## Get data from SPSS, SAS or Stata ########
    # "foreign", "haven",

    ## Direct connection with databases ########
    #  "sqldf",
    #  "RODBC",
    #  "RMongo",
    #  "RSQLite"

    ## Get data from Excel ########
    #"rJava", "XLConnect", ## Read and write excel files
    "readxl", ## Read Excel files
    # "xlsx",

    ## Get data or files from API ########
    #"httr", "rjson","jsonlite",
    #"XML", "xml2", ## Manipulation of xml
    #"gmailr", # Access gmail api
    ##-install CURL before and separately
    "RCurl",


    ### Extend R  ########
    "formatR", #  used to format the code
    "Rcpp", ## used to compile some pacckages
    "devtools", # package used to load packages hosted in github --
    # "parallel", ## Improve performance


    ### Scoring & Composition Indicators
    "polycor", # Polychoric and Polyserial Correlations
    "psych",  #A package for personality, psychometric, and psychological research
    "nFactors", ## Manipulate results of factor modeling
    "moments", ## Moments, cumulants, skewness, kurtosis and related tests
    "clusterSim", ## Searching for Optimal Clustering Procedure for a Data Set
    "mice", # Multivariate Imputation by Chained Equations
    "VIM", # Visualization and Imputation of Missing Values
    "REdaS", # Companion Package to the Book 'R: Einfhrung durch angewandte Statistik'
    "mirt", #multidimenisonal Item Response Theory

    ### GUI ########
    #"RGtk2",
    #"rattle",

    ### Anonymisation #######
    "bit64",  "sdcMicro",

    ## Generate reports  ########
    "knitr",
    "pander",
    "xtable",
    "rmarkdown",
    "kableExtra",
    "koRpus",
    "tables"
  )))

  vrai_liste <- packages
  try(vrai_liste <- unique(unlist(tools::package_dependencies(packages,recursive = TRUE))))


  for (elpa in packages) {
    exc <- names(installed.packages()[,'Package'])
    cat("Package Installation progress so far.. : ",
        percent(sum(is.element(packages,exc))/length(packages), digits = 0)
        ," - ",
        percent(sum(is.element(vrai_liste,exc)) / length(vrai_liste), digits = 0)
        ,"\n\n")
    if (is.element(elpa,exc))  { cat(elpa," is already installed on your computer \n")  }
    if (!is.element(elpa,exc)) { cat(elpa," will be installed on your computer  \n")
      try(utils::install.packages(elpa,dependencies = TRUE))
      packs <- installed.packages()
      exc <- names(packs[,'Package'])
      if (is.element(elpa,exc)) { cat("   The installation went well for the package: ",elpa,"\n")
      }
    }
  }

  cat("\n\n\n ***Installation completed*** \n\n\n")


  packs <- installed.packages()
  exc <- names(packs[,'Package'])

  ## adding the short description of the package
  packsdescp <- as.data.frame(packs)
  for (i in 1:nrow(packsdescp)) {
    # i <- 1
    packsname <- as.character(packsdescp[ i, c("Package") ])
    #cat(packsname)
    packsdescp[ i, c("Title") ] <- as.character(packageDescription(packsname, fields = c("Title")))
  }

  packsdescp <- unique(packsdescp[ , c("Package", "Title")])
  packsdescp$Title <- substr(packsdescp$Title,0, 60)

  synth <- as.data.frame(cbind(unique(packages),is.element(packages,exc)))
  names(synth)[1] <- "packages"
  synth <- merge( x = synth, y = packsdescp[ ,c("Package","Title")],by.x = "packages", by.y = "Package", all.x = TRUE)
  synth$packages <- as.character(synth$packages)
  manque <- as.matrix(synth[synth[,2] == "FALSE", c("packages", "Title")])
  synth <- as.matrix(synth[synth[,2] == "TRUE", c("packages", "Title")])

  cat("\n\n\n Below is the list of packages that have been now installed on your computer : \n\n")
  #cat(synth)
  try(mon_print(synth),silent = TRUE)

  cat("\n\n\nThe list below indicates the missing packages: \n\n")
  #cat(manque)
  try(mon_print(manque),silent = TRUE)

  if (nrow(manque) == 0) {cat("Perfect, there's no missing package \n\n\n")} else
  {cat(" You have missing package -> run again this script or try to install them one by one using the 'install' button under the 'packages' tab \n\n")}

  try(silent = TRUE,rm(packages,elpa,exc,packs,percent,mon_print,synth,manque,vrai_liste))



  # Now load packages into memory ####
  # Sys.getenv("R_LIBS_USER")
  # .libPaths()
  # gpclibPermit()

  # #library("tidyverse")
  # library("ggthemes")
  # library("plyr")
  # #library("ggrepel")
  # library("viridis")
  # library("RColorBrewer")
  # #library("classInt")
  # library("hexbin")
  #
  # library("DT")
  # #library("extrafont")
  # library("corrplot")
  # library("graphics")
  # library("vcd")
  # library("stringi")
  # library("forcats")
  #
  #
  # library("zoo")
  # library("reshape2")
  # library("lubridate")
  # #library("date")
  # library("gdata")
  # library("gridExtra")
  # library("scales")
  #
  # #library("survey")
  #
  # library("knitr")
  # library("kableExtra")
  # library("koRpus")
  # library("tables")
  # library("rmarkdown")
  #
  # library("readxl")
  # #library("xlsx")
  #
  # library("car")
}
