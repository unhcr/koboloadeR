##This should detect and install missing packages before loading them –
packages <- c(
  "dplyr",  "data.table", "doBy", ## Data manipulation
  "reshape2", # package to easily melt data to long form
  "Hmisc", # generate a detailled describtion of a given dataset
  "formatR", #,  used to format the code
  "ggplot2", ## advanced graphics
  "ggthemes", ## load different cusotmised themes for ggplot2: excel, stata, economist, tufte, wall street journal...
  "grid", "gridExtra","scales", # package for elegant data visualization using the Grammar of Graphics
  "vcd", # Visualisation of categorical data
  "RColorBrewer", # a package offering color palette from
  "extrafont", ##" load additional font
  "sp","maptools","rgdal","rgeos", ## standard Geo manipulation packages
  "ggmap", ## get background from webmapping API
  "hexbin", ## Hexagrid viz
  "raster","cartography", ## packages used for the maps --
  "classInt",  ## used for univariate classification
  "lubridate","date","gdata", ## playing with date
  "lme4", "lmtest", "car", "caret",  ## used for regressions
  "AER",  # interesting datasets
  "lattice", # Visualisation
  "FactoMineR", "ade4",  ## multivariate analysis
  "survival", # survival analysis
  "sqldf", "RODBC",  ## Direct connection with databases
  "stringr", # manipulation of string data
  "XML",  ## Manipulation of xml
  "tm", ## text mining
  "rJava", "XLConnect", ## Read and write excel files
  "cluster", ## Cluster analysis
  "foreign", ## read data from SPSS, SAS or Stata
  "parallel", "httr", "rjson",
  "MASS", "gvlma", "VGAM", "aod", "fields",
  "scatterplot3d",  "psych",  "ellipse",   "pastecs",
  "FactoMineR", ## Multiple Correspondance analysis
  "rattle", ## GUI for data mining
  "stringr","stringdist","stringi", ## string manipulation
  "RCurl", ##used to download files from API
  "Rcpp", ## used to compile some pacjckages
  "plyr","tidyR",
  "readxl", ## Read Excel files
  "devtools", # package used to load packages hosted in github -- install CURL before and separately
  "xkcd" ## Style from the xkcd comics
)

## identify packages not installed yet
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

rm(packages)

# loads packages into memory
library(stringr)
library(stringi)
library(lattice)
library(rattle)
library(car)
library(plyr)
library(ggplot2) ## The grammar of graphics!
library(extrafont) ## Additional fonts
library(ggthemes) ## Additional themes for gplot2
library(zoo) ## Manage reformatting of date
library(reshape2) ## Restructure data between wide and long format before plotting them - melt and cast
library(maptools) ## Create maps
library(rgdal) ## Open geographic files
#library(rgeos)
library(ggmap) ## get background map from google map
library(sp) ## Spatial library
#library(raster) ## Managing raster dataset
library(RColorBrewer) ## Color palette
library(classInt) ## Classififcation
library(hexbin) ## Hexa binning
gpclibPermit()
library(lubridate)
library(date)
library(gdata)
library(gridExtra)
library(scales)
#library(formatR)
#library(RGtk2)
#library(gWidgetsRGtk2)
library(readxl)
library(plyr)
#library(xlsx)
library(FactoMineR)
