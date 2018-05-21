## A curated list of packages
## @edouard_lgp

##This should detect and install missing packages before loading them –
packages <- c(


  ## Tydiverse #########
  # https://www.tidyverse.org

  #"dplyr",  "doBy","tidyr", ## Data manipulation
  #"ggplot2", ## advanced graphics
  # "stringr",
  "tidyverse",

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
  #  "caret", # Gradient Boosting & AdaBoost
  #  "bigRR",  ## Classification


  ###  Machine Learning ########
  "e1071", #SVM (Support Vector Machine)
  "knncat", # KNN (K- Nearest Neighbors)
  "randomForest", # randomForest

  ###  Recursive Partitioning and Regression Trees
  "rpart",
  "rpart.plot",


  ###  Time Series & survival analysis ########
  #"forecast", "ltsa",
  #"survival", "BaSTA",
  #"pastecs", #Analysis of Space-Time Ecological Series

  ###  Lasso and Elastic-Net Regularized Generalized Linear Models  ########
  #"glmnet",
    "lme4", # Linear Mixed-Effects Models  #  "MASS",
  #"VGAM", #Vector Generalized Linear and Additive Models
  #"aod", ## Analysis of Overdispersed Data

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

  ## Procedures for Psychological, Psychometric, and Personality Research ########
  #  "psych",

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
)

## identify packages not installed yet
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

rm(packages)

# Now load packages into memory ####
# Sys.getenv("R_LIBS_USER")
# .libPaths()
# gpclibPermit()

library("tidyverse")
library("ggthemes")
library("plyr")
library("ggrepel")
library("viridis")
library("RColorBrewer")
library("classInt")
library("hexbin")

library("DT")
library("extrafont")
library("corrplot")
library("graphics")
library("vcd")
library("stringi")


library("zoo")
library("reshape2")
library("lubridate")
library("date")
library("gdata")
library("gridExtra")
library("scales")

library("survey")

library("knitr")
library("kableExtra")
library("koRpus")
library("tables")
library("rmarkdown")

library("readxl")
#library("xlsx")

library("car")

