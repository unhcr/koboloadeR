
## https://www.r-bloggers.com/building-a-website-with-pkgdown-a-short-guide/



require(devtools)
# use_readme_rmd()
# use_news_md()
# use_vignette("koboloadeR")  #substitute with the name of your package
# use_github_links()
# use_travis()
# use_cran_badge()




## Create documentation for the package!

#install.packages("pkgdown")

library("pkgdown")
pkgdown::build_site()

#build_vignettes()
