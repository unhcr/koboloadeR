# koboloadeR: Survey Data Crunching <img src="man/figures/koboloadeR.png" width="200" align="right" /> 

<!-- badges: start -->
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)

[![packageversion](https://img.shields.io/badge/package%20version-0.0.1-orange.svg)](https://github.com/unhcr/koboloadeR/blob/master/DESCRIPTION)

[![CRAN status](https://www.r-pkg.org/badges/version/koboloadeR)](https://cran.r-project.org/package=koboloadeR)
[![CRAN](https://img.shields.io/cran/v/koboloadeR.svg)](https://cran.r-project.org/package=koboloadeR)
[![CRAN](https://img.shields.io/cran/l/koboloadeR.svg)](https://CRAN.R-project.org/package=koboloadeR)

[![CRAN](http://cranlogs.r-pkg.org/badges/koboloadeR)](https://CRAN.R-project.org/package=koboloadeR)
[![CRAN](http://cranlogs.r-pkg.org/badges/grand-total/koboloadeR)](https://CRAN.R-project.org/package=koboloadeR)

[![Travis build status](https://travis-ci.org/unhcr/koboloadeR.svg?branch=gh-pages)](https://travis-ci.org/unhcr/koboloadeR)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/unhcr/koboloadeR?branch=gh-pages&svg=true)](https://ci.appveyor.com/project/unhcr/koboloadeR)
[![codecov](https://codecov.io/gh/unhcr/koboloadeR/branch/gh-pages/graph/badge.svg)](https://codecov.io/gh/unhcr/koboloadeR)

[![Codacy Badge](https://api.codacy.com/project/badge/Grade/2adb516e959b4c1599ca4367b8480196)](https://www.codacy.com/app/Edouard-Legoupil/koboloadeR?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=unhcr/koboloadeR&amp;utm_campaign=Badge_Grade)
[![Last-changedate](https://img.shields.io/github/last-commit/unhcr/koboloader.svg)](https://github.com/unhcr/koboloadeR/commits/master)

<!-- badges: end -->

koboloadeR is a metapackage, that brings together a series of specialised packages in an organised data analysis workflow, to conduct data discovery and analysis for data collected through  [KoboToolbox](https://www.kobotoolbox.org/), [ODK](https://opendatakit.org/), [ONA](https://ona.io/home/) or any __[xlsform](http://xlsform.org)__ compliant data collection platform.

This package first builds on the capacity of [UNHCR Kobo server](http://kobo.unhcr.org) but it can also be used from any structured dataset. It comes as a companion tool to the [Integrated Framework for Household Survey](https://unhcr.github.io/Integrated-framework-household-survey).

koboloadeR aims at helping [humanitarian data analysts](https://humanitarian-user-group.github.io/) to focus in data interpretation by saving the time needed to quickly generate the graphs and charts required to discover insights from a dataset. It also ensure analysis __reproducibility__ through a separation of the analysis configuration and the analysis process. The package allows to account for sample weights and hierachical dataset structure (both capacities that are not available through the default [reporting engine](http://support.kobotoolbox.org/articles/2847676-viewing-and-creating-custom-reports) or the [excel-analyzer](http://support.kobotoolbox.org/articles/592387-using-the-excel-analyzer)). 

## Approach
 
The main concept behind the package is to implement a survey data analysis plan and configuration directly within the same [xlsform](http://xlsform.org) excel file that has been used to develop the questionnaire. A few additional column are created in this excel document, the package read those column to generate a series of predefined report.

![alt text](https://raw.githubusercontent.com/unhcr/koboloadeR/gh-pages/inst/script/workflow.png)

You can have a look at [some examples of output reports here](https://github.com/unhcr/koboloadeR/tree/gh-pages/out).

The approach offered through the package has the following advantages: 

  *  End users __do not need to code__ in R and to master the language in order to use the package;  
 
  *  The data __analysis plan__ is de facto fully documented and described;  
 
  *  The resulting data crunching reports are fully __reproducible__;  
 
  *  Analysis __iterations__ are facilitated;
 
  *  Good __practices__ are enforced through the package.

A more detailed introduction to the concepts used in the package is presented in the [Data Crunching article](articles/Crunching.html). 

## Environment setup 

### Software installation  

  1. Install either [Java JRE](https://www.oracle.com/technetwork/java/javase/downloads/jre8-downloads-2133155.html) or [Java JDK](http://jdk.java.net/12/):  JAVA is required to manipulate excel files. 
 
Under Windows, You will need to have JRE or JDK register under the `Path` system variable (Right click on “This PC/Computer”, Go to `->Properties->Advanced system settings->Advanced->Environment Variables`, then under System variables select `Path` => Click `Edit`.
 
If you install, the `JDK` (Java Development kit), Please make sure that `JAVA_HOME` is actually recorded as an [Environment Variable](https://java.com/en/download/help/path.xml). 

If you install, the `JRE` (Java Runtime Environment), Please make sure that `JRE_HOME` is actually recorded as an [Environment Variable](https://confluence.atlassian.com/doc/setting-the-java_home-variable-in-windows-8895.html). 

Once in R, you may double-check that Environement variable are correctly, i.e. JAVA_HOME or JRE_HOME, set by

```{r}
Sys.getenv()
```

If JAVA is not correctly set, you will see an installatin error at a latter stage when loading the package `RJava`.

Note in some case, you may need to reboot your computer to ensure that this environement variable is properly accounted for.

  2. [Install R](https://cran.rstudio.com/): follow instruction from the installer.

  3. __Only for windows user__ [Install RTools](https://cran.r-project.org/bin/windows/Rtools/): This executable is needed to install the package from github. Follow instruction from the installer.

  4. [Install R Studio](https://www.rstudio.com/products/rstudio/download/#download) : follow instruction from the installer

You can now Launch __R Studio__ . You may check the list of common issues in the [Common Troubleshooting](articles/Troubleshooting.html) article.

## Package installation from Github

Note that the package is still in beta-version. We hope to have soon a release available on CRAN.

  *  Open R studio interface and within the R console, install `devtools` package: 

```{r}
install.packages("devtools")
```

  *  Install koboloadeR: 

```{r}
library(devtools)
install_github("unhcr/koboloadeR", ref = "gh-pages") 
```  

  *  You are all set! You can know use koboloadeR. If you have a problem consult the common troubleshooting part at the end of this page.

### Project Walk Through 

  *  In R Studio, select File, click New project. A box opens
  
  *  Choose New Directory
  
  *  Choose Empty project
  
  *  Type the name of the folder where you want to put your data
  
  *  Select where you want to put this folder
  
  *  Click Create project

Then setup a few things: run those two lines:

```{r}
library (koboloadeR) # This loads koboloadeR package

kobo_projectinit() # Creates folders necessary and transfer files needed
```

It might take a while as a few other packages have to be installed or loaded. Once the see the " >" again at the beginning of the line, you are ready to start

#### Graphical user interface (GUI) 

All instructions and options for the project configuration and analysis plan settings can be done through a dedicated GUI.

```{r}
kobo_shiny("app_main_koboloadeR.R")
```  

For better performances, select "Open in Browser" on the top of the window.

#### Console mode

Alternatively, you can use the __console mode__ by running the file `run-analysis.R` that is automatically copied in the `code` folder after you run the `kobo_projectinit()` function . 

This will be likely the quickest options, once your are used to the package.

Note however that this implies that you configure correctly on your own the full configuration within the xlform file. 


#### Presentation of features 

  *  [Getting data from server](articles/Getting_data.html) - How to retrieve data from the server within R?
    
  *  [Data Analysis Plan within your `xlsfrom`](articles/xlsform.html) - How to extend the xlsform to include your analysis plan?
    
  *  [Using console script](articles/Console.html) - How to use the package without using the GUI in Shiny?
    
  *  [Sampling](articles/Sampling.html): how to generate a sample from a registry or to post-stratify data when having a low response rate? 
  
  *  [Data Anonymisation and disclosure risk measurement](articles/Anonymisation.html): how to create anonymised data?
  
  *  [Data Cleaning](articles/Cleaning.html): how to use the package for reproducible and documented data cleaning? 
    
  *  [Predicting and scoring](articles/Predicting_Scoring.html): how to use survey in conjunction wiht registration data to build risk prediction and vulnerability scoring?
    
  *  [Dissiminating](articles/Dissiminating.html): how to dissiminate both survey microdata using DDI and variable crosstabulation on CKAN? 

## Contributing

Contributions to the packages are welcome. Please read first the [contribution guidelines](CONTRIBUTING.html), follow the [code of conduct](CODE_OF_CONDUCT.html) and use the [issue template](ISSUE_TEMPLATE.html).

To go in more details, the suggested workflow is presented below (note that all of it is not yet fully implented - see [issue tracking for more details](https://github.com/unhcr/koboloadeR/issues)). You can read the [function documentations](reference/index.html) directly.

![alt text](https://raw.githubusercontent.com/unhcr/koboloadeR/gh-pages/inst/script/workflow2.png)

CRAN Notes
==========

Test results
------------
