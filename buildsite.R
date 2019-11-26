
## https://www.r-bloggers.com/building-a-website-with-pkgdown-a-short-guide/

#install.packages("pkgdown")

library("pkgdown")
pkgdown::build_site()


## a few other exploration of the package

devtools::document()

require(devtools)
# use_readme_rmd()
# use_news_md()
# use_vignette("koboloadeR")  #substitute with the name of your package
# use_github_links()
# use_travis()
# use_cran_badge()


check <- drake::expose_imports("koboloadeR",
                               character_only = FALSE,
                               envir = parent.frame(),
                               jobs = 1)
unlink(".RData")



# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
#
# BiocManager::install("graph")
# BiocManager::install("Rgraphviz")
library("CodeDepends")

##f = system.file( package = "koboloadeR")
#gg = makeCallGraph("package:koboloadeR")
# if(require(Rgraphviz)) {
#   gg = layoutGraph(gg, layoutType = "circo")
#   graph.par(list(nodes = list(fontsize=55)))
#   renderGraph(gg) ## could also call plot directly
# }


#devtools::install_github("datastorm-open/DependenciesGraphs")
library(DependenciesGraphs)
deps <- funDependencies("package:koboloadeR", "kobo_dico")
plot(deps)
# Prepare data
dep <- envirDependencies("package:koboloadeR")

# visualization
plot(dep)

# library(mvbutils)
# foodweb(where = e)

library(pkgnet)
report1 <- CreatePackageReport(pkg_name = "koboloadeR")


library("miniCRAN")

tags <- "koboloadeR"
pkgDep(tags, availPkgs = cranJuly2014)
dg <- makeDepGraph(tags, enhances = TRUE, availPkgs = cranJuly2014)
set.seed(1)
plot(dg, legendPosition = c(-1, 1), vertex.size = 20)




library(NCmisc)
library(stringr)
library(dplyr)

checkPacks <-function(path){

  ## get all R files in your directory
  ## by the way, extract R code from Rmd: http://felixfan.github.io/extract-r-code/
  files<-list.files(path)[str_detect(list.files(path), ".R$")]

  ## extract all functions and which package they are from
  ## using NCmisc::list.functions.in.file
  funs<-unlist(lapply(paste0(path, "/", files), list.functions.in.file))
  packs<-funs %>% names()

  ## "character" functions such as reactive objects in Shiny
  characters<-packs[str_detect(packs, "^character")]

  ## user defined functions in the global environment
  globals<-packs[str_detect(packs, "^.GlobalEnv")]

  ## functions that are in multiple packages' namespaces
  multipackages<-packs[str_detect(packs, ", ")]

  ## get just the unique package names from multipackages
  mpackages<-multipackages %>%
    str_extract_all(., "[a-zA-Z0-9]+") %>%
    unlist() %>%
    unique()
  mpackages<-mpackages[!mpackages %in% c("c", "package")]

  ## functions that are from single packages
  packages<-packs[str_detect(packs, "package:") & !packs %in% multipackages] %>%
    str_replace(., "[0-9]+$", "") %>%
    str_replace(., "package:", "")

  ## unique packages
  packages_u<-packages %>%
    unique() %>%
    union(., mpackages)

  return(list(packs=packages_u, tb=table(packages)))

}

check2 <- checkPacks("R")

devtools::install_github("dracodoc/mischelper")


### https://laderast.github.io/2019/02/12/package-building-description-namespace/
devtools::check(document = FALSE, args = c('--as-cran', '--no-examples', '--no-tests'))


  ## Create documentation for the package!



#build_vignettes()
