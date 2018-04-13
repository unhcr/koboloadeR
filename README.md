# Introduction
KoboAnalyser is a R package to conduct data discovery and analysis for data collected through  KoboToolbox.
The package is largely based on KoboloadeR (https://github.com/Edouard-Legoupil/koboloadeR) by Edouard Legoupil

# KoboAnalyser quick setup

## Prerequisite
To be able to use KoboAnalyser you will need:

* R: download here: https://cran.rstudio.com/). For Windows, choose "install R for the first time".

* R Studio  (https://www.rstudio.com/products/rstudio/download/#download)


## Installation
* Install R: follow instruction from the installer.
* Install R Studio: follow instruction from the installer
* Launch R Studio

### Install KoboAnalyser from Github (up to date version):

* In the R console, install 'devtool' package:
```
install.packages("devtools")
```
* Install KoboAnalyser:
```
library(devtools)
install_github("ElliottMess/KoboAnalyser")

```
* You are all set! You can know use KoboAnalyser

### Install KoboAnalyser from Zip package:

* In Tools (top of R Studio): click Install Packages
* Choose Install from: Package Archive File (.zip; .tar.gz)
* In Package archive, select the file KoboAnalyser.tar.gz that you downloaded/received
* You are all set! You can know use KoboAnalyser

## Quick start
First, create a project in R Studio:

* In R Studio, select File, click New project. A box opens
* Choose New Directory
* Choose Empty project
* Type the name of the folder where you want to put your data
* Select where you want to put this folder
* Click Create project

Then setup a few things: run those two lines:

```
library (KoboAnalyser) # This loads KoboAnalyser package
kobo_projectinit() # Creats folders necessary and transfer files needed
```
It might take a while as a few other packages have to be installed or loaded. Once the see the " >" again at the beginning of the line, you can run:

```
kobo_shiny("app_koboanalyser.R")
```

This will launch a graphic interface with other instructions and options.
For better performances, select "Open in Browser" on the top of the window.

## Troubleshooting

Before anything else, try to restart the R session:
* In R studio, on top go to "Session"
* "Restart R"

### The application crashed
If the application (graphic interface) crashes, make sure that all packages necessary are loaded with:
```
source("code/0-packages.R")
```
Also make sure that you downloaded your data in the right format:
* Export as XLS
* XML values and headers
* Include groups in headers
* 'Group separator' as dot ('.')

### Error when building the dictionnary or configuration file

If you see this message (or similar error):
```
Error in file(file, ifelse(append, "a", "w")) : 
  cannot open the connection
```
It is most likely because you have the form, dictionary or data set open on your computer. If you close it and start over, the problem should be fixed.
  
### Error when loading packages
 If you get this error:
```
Error: package or namespace load failed for 'rJava'
```

It could be because you have a 64bit version of R, but have a 32bit version of Java. Check this thread for guidance: https://stackoverflow.com/questions/37735108/r-error-onload-failed-in-loadnamespace-for-rjava

# Core functions
DOCUMENTATION TO BE ADDED

# Resources
Here are a few introductions and tutorials for  R:

* An introduction to R: complete but not very user-friendly: https://cran.r-project.org/doc/manuals/r-release/R-intro.html
*	Try R: http://tryr.codeschool.com/
*	Data Camp's Introduction to R: https://www.datacamp.com/courses/free-introduction-to-r

To go further:
*	https://www.rstudio.com/online-learning/
*	https://www.r-bloggers.com/how-to-learn-r-2/
