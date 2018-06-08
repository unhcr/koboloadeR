#' @name kobo_cluster_report
#' @rdname kobo_cluster_report
#' @title  Generate anonymised dataset
#'
#' @description  Automatically produce a report exploring potential clusters within survey records.
#'
#'  The report is generated from functions released within FactoMiner & FactoMineR
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
#' @export kobo_cluster_report
#'
#' @examples
#' \dontrun{
#' kobo_cluster_report(frame, dico)
#' }
#'

kobo_cluster_report <- function(frame, dico) {

  # frame <- household
  # framename <- "household"
  framename <- deparse(substitute(frame))
  write.csv(frame, paste0("data/clustering-report-",framename,".csv"), row.names = FALSE, na = "")

  ## Check that all those selectedVars are in the frame ####
  check <- as.data.frame(names(frame))
  names(check)[1] <- "fullname"
  check$id <- row.names(check)


  #### Check presence of variable for anom plan...

  selected.cluster <- dico[ which(dico$cluster == "yes" & dico$type == "select_one" ), ]
  selected.cluster <- join(x = selected.cluster, y = check, by = "fullname", type = "left")
  selected.cluster <- selected.cluster[!is.na(selected.cluster$id), ]
  selected.clusterVars <- as.character(selected.cluster[ , c("fullname")])
  #selected.clusterVars2 <- as.character(selected.cluster[ , c("name")])
  selected.clusterVars2 <- str_replace_all(as.character(selected.cluster[ , c("name")]), "_", ".")


  selected.id <- dico[ which(dico$cluster == "id"), ]
  selected.id <- join(x = selected.id, y = check, by = "fullname", type = "left")
  selected.id <- selected.id[!is.na(selected.id$id),  ]
  selected.idVars <- as.character(selected.id[ , c("fullname")])


  if (nrow(selected.cluster) == 0) { cat ("You have not selected variables to cluster for your dataset! \n") } else {




        reportcluster  <- paste0("code/clustering-report-",framename,".Rmd")

        ## TO DO : CHECK IF FILE EXIST - AND REQUEST USER TO DELETE BEFORE REGENERATING - SUGGESTING TO SAVE PREVIOUS UNDER NEW NAME
        if (file.exists(reportcluster)) file.remove(reportcluster)



        cat("---", file = reportcluster , sep = "\n", append = TRUE)
        cat("title: \"Multivariate analysis\"", file = reportcluster , sep = "\n", append = TRUE)
        cat("author: \"Generated with [Koboloader](https://github.com/unhcr/koboloadeR), [FactoMineR](http://factominer.free.fr/factomethods/multiple-correspondence-analysis.html) & [FactoExtra](http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/114-mca-multiple-correspondence-analysis-in-r-essentials/)\"", file = reportcluster , sep = "\n", append = TRUE)
        cat("date: \" `r format(Sys.Date(), '%d %B %Y')`\"", file = reportcluster, sep = "\n", append = TRUE)
        cat("output:", file = reportcluster , sep = "\n", append = TRUE)
        cat("  word_document:", file = reportcluster , sep = "\n", append = TRUE)
        cat("    fig_caption: yes", file = reportcluster , sep = "\n", append = TRUE)
        cat("    fig_height: 5", file = reportcluster , sep = "\n", append = TRUE)
        cat("    fig_width: 8", file = reportcluster , sep = "\n", append = TRUE)
        cat("    toc: yes", file = reportcluster , sep = "\n", append = TRUE)
        cat("    toc_depth: 2", file = reportcluster , sep = "\n", append = TRUE)
        cat("    reference_docx: style-unhcr-portrait.docx", file = reportcluster , sep = "\n", append = TRUE)
        cat("---", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("```{r setup, include=FALSE}", file = reportcluster , sep = "\n", append = TRUE)
        cat("knitr::opts_chunk$set(echo = TRUE)", file = reportcluster , sep = "\n", append = TRUE)
        cat("library(knitr)", file = reportcluster , sep = "\n", append = TRUE)
        cat("library(FactoMineR) ## Multiple correspondance analysis and classification", file = reportcluster , sep = "\n", append = TRUE)
        cat("library(factoextra)", file = reportcluster , sep = "\n", append = TRUE)
        cat("library(ggplot2)", file = reportcluster , sep = "\n", append = TRUE)
        cat("library(reshape2)", file = reportcluster , sep = "\n", append = TRUE)
        cat("library(simFrame)", file = reportcluster , sep = "\n", append = TRUE)
        cat("library(sampling)", file = reportcluster , sep = "\n", append = TRUE)
        cat("```", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)


        cat("```{r , echo=FALSE, warning=FALSE, message=FALSE, cache=TRUE}", file = reportcluster , sep = "\n", append = TRUE)
        cat("samplesize <- 10000", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("mainDir <- getwd()", file = reportcluster , sep = "\n", append = TRUE)
        cat("mainDirroot <- substring(mainDir, 0 , nchar(mainDir) - 5)", file = reportcluster , sep = "\n", append = TRUE)
        cat("## Load all required packages", file = reportcluster , sep = "\n", append = TRUE)
        cat("source(paste0(mainDirroot,\"/code/0-packages.R\"))", file = reportcluster , sep = "\n", append = TRUE)
        cat("source(paste0(mainDirroot,\"/code/0-theme.R\"))", file = reportcluster , sep = "\n", append = TRUE)
        cat("library(koboloadeR)", file = reportcluster , sep = "\n", append = TRUE)
        cat("## Provide below the name of the form in xsl form - format should be xls not xlsx", file = reportcluster , sep = "\n", append = TRUE)
        cat("form <- \"form.xls\"", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("dico <- read.csv(paste0(mainDirroot,\"/data/dico_\",form,\".csv\"), encoding = \"UTF-8\", na.strings = \"\")", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)

        cat(paste0("datacluster <-  read.csv(paste0(mainDirroot,\"/data/clustering-report-",framename,".csv\"), sep = \",\", encoding = \"UTF-8\", na.strings = \"\")"), file = reportcluster , sep = "\n", append = TRUE)

        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("```", file = reportcluster , sep = "\n", append = TRUE)
        cat("# Executive Summary", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("***", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("# Introduction", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("An important challenge to understand the profile of a population is to discover how categories interact together. Refugees profiles are defined by multiple categories.  Univariate analysis does not allow to get a synthetic vision from a large set of variables that can describe a population. Because of inherent brain & cognitive limitations, it is challenging to process together more than 7 categories and to make sense out of too many graphs.", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("# Methodology", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("Since the 70's, Social scientist have developed advanced __exploratory__ techniques that allow to discover statistical profiles among a specific population. Clustering is an exploratory data analysis tool wich aims to group a set of records in such a way that records in the same group are more similar to each other than to those in other groups. [Multiple Correspondence Analysis (MCA)](https://en.wikipedia.org/wiki/Multiple_correspondence_analysis) together with [Hierarchical Classification on Principle Components](http://factominer.free.fr/classical-methods/hierarchical-clustering-on-principal-components.html) allow to process nominal categorical data (as it is the case for Refugee biodata) in order to detect and represent the underlying structures in a data set. This approach is based on looking at description to generate induction rather than testing an hypothesis according to model. Those analysis are performed with the [R statistical language](http://www.sthda.com/english/wiki/hcpc-hierarchical-clustering-on-principal-components-hybrid-approach-2-2-unsupervised-machine-learning).", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("![Correspondence Analysis Handbook : J.-P. Benzecri (1992).](bencrezi.jpg)", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("This approach implies 5 simple steps:", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat(" *  __Dimensionnality reduction__: Reduce the numbers of dimensions to two main composite dimensions in order to represent each observation in a 2D space.", file = reportcluster , sep = "\n", append = TRUE)
        cat(" *  __Composition of the 2 axis__: Describe the componnent of each axis", file = reportcluster , sep = "\n", append = TRUE)
        cat(" *  __Categories Representation__: This allow to visualise how close different variables are", file = reportcluster , sep = "\n", append = TRUE)
        cat(" *  __Clustering__: Records are grouped according to profile using the underlying proximity between variables", file = reportcluster , sep = "\n", append = TRUE)
        cat(" *  __Modalities within each group__: Describe the main variable modalities that describe the profile.", file = reportcluster , sep = "\n", append = TRUE)
        cat(" *  __Overview of groups__: Display the frequency of each group as well the breakdown of specific needs within each group.", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
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
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("# Dimensionnality reduction  ", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)

        ## First chunk ########
        cat("```{r, echo=FALSE, warning=FALSE}", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("## Check that all those selectedVars are in the frame...", file = reportcluster , sep = "\n", append = TRUE)
        cat("check <- as.data.frame(names(datacluster))", file = reportcluster , sep = "\n", append = TRUE)
        cat("names(check)[1] <- \"fullname\"", file = reportcluster , sep = "\n", append = TRUE)
        cat("check$id <- row.names(check)", file = reportcluster , sep = "\n", append = TRUE)
        cat("check <- join(x = check, y = dico, by = \"fullname\", type = \"left\")", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("selected.cluster <- dico[ which(dico$cluster == \"yes\" & dico$type == \"select_one\" ), ]", file = reportcluster , sep = "\n", append = TRUE)
        cat("selected.cluster <- join(x = selected.cluster, y = check, by = \"fullname\", type = \"left\")", file = reportcluster , sep = "\n", append = TRUE)
        cat("selected.cluster <- selected.cluster[!is.na(selected.cluster$id), ]", file = reportcluster , sep = "\n", append = TRUE)
        cat("selected.clusterVars <- as.character(selected.cluster[ , c(\"fullname\")])", file = reportcluster , sep = "\n", append = TRUE)
        cat("#selected.clusterVars2 <- as.character(selected.cluster[ , c(\"name\")])", file = reportcluster , sep = "\n", append = TRUE)
        cat("selected.clusterVars2 <- str_replace_all(as.character(selected.cluster[ , c(\"name\")]), \"_\", \".\")", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("selected.id <- dico[ which(dico$cluster == \"id\"), ]", file = reportcluster , sep = "\n", append = TRUE)
        cat("selected.id <- join(x = selected.id, y = check, by = \"fullname\", type = \"left\")", file = reportcluster , sep = "\n", append = TRUE)
        cat("selected.id <- selected.id[!is.na(selected.id$id),  ]", file = reportcluster , sep = "\n", append = TRUE)
        cat("selected.idVars <- as.character(selected.id[ , c(\"fullname\")])", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("#############################################################################", file = reportcluster , sep = "\n", append = TRUE)
        cat("##### Step : subsetting data frame", file = reportcluster , sep = "\n", append = TRUE)
        cat("# Create subset of file with observation and selected variables & remove duplicated rows based on IDH", file = reportcluster , sep = "\n", append = TRUE)
        cat("datacluster2 <- datacluster[ , c( selected.idVars, selected.clusterVars)]", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("row.names(datacluster2) <- datacluster2[ , c( selected.idVars)]", file = reportcluster , sep = "\n", append = TRUE)
        cat("datacluster2[ , c( selected.idVars)] <- NULL", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("## Convert to factor variable as they are categoric", file = reportcluster , sep = "\n", append = TRUE)
        cat("datacluster2[,c(selected.clusterVars)] <- lapply(datacluster2[,c(selected.clusterVars)], factor)", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("## Rename Dataframe with short name", file = reportcluster , sep = "\n", append = TRUE)
        cat("check2 <- as.data.frame(names(datacluster2))", file = reportcluster , sep = "\n", append = TRUE)
        cat("names(check2)[1] <- \"fullname\"", file = reportcluster , sep = "\n", append = TRUE)
        cat("check2$id <- row.names(check2)", file = reportcluster , sep = "\n", append = TRUE)
        cat("check2 <- join(x = check2, y = dico, by = \"fullname\", type = \"left\")", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("## Take out special characters from those name", file = reportcluster , sep = "\n", append = TRUE)
        cat("check2$name2 <- str_replace_all(check2$name, \"_\", \".\")", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("names(datacluster2) <- check2[, c(\"name2\")]", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("datacluster3 <- datacluster2", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("data.sample <- datacluster2[sample(1:nrow(datacluster2), samplesize, replace = FALSE), ]", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("### Checking sample modalities frequency", file = reportcluster , sep = "\n", append = TRUE)
        cat("#prop.table(table(data.sample$cool1Cat, useNA = \"ifany\"))", file = reportcluster , sep = "\n", append = TRUE)
        cat("#prop.table(table(data2$cool1Cat, useNA = \"ifany\"))", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("# Multiple correspondance analysis", file = reportcluster , sep = "\n", append = TRUE)
        cat("data.mca <- MCA(data.sample, graph = FALSE)", file = reportcluster , sep = "\n", append = TRUE)
        cat("fviz_mca_ind(data.mca, col.ind = \"blue\", label = \"none\", addEllipses = TRUE, ellipse.level = 0.95, jitter = list(what = \"point\")) +", file = reportcluster , sep = "\n", append = TRUE)
        cat("  theme_minimal() +", file = reportcluster , sep = "\n", append = TRUE)
        cat("  labs(title = \"Geometric representation of Individuals in a cloud of points\" )", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("```", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("# Composition of the 2 axis ", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("```{r, echo=FALSE, warning=FALSE}", file = reportcluster , sep = "\n", append = TRUE)
        cat("## Contribution of top 10 variables", file = reportcluster , sep = "\n", append = TRUE)
        cat("fviz_contrib(data.mca, choice = \"var\", axes = 1, top = 15) + theme_minimal() +", file = reportcluster , sep = "\n", append = TRUE)
        cat("  labs(title = \"Contribution of the top 15  variables to the first axis\" )  +", file = reportcluster , sep = "\n", append = TRUE)
        cat("  coord_flip()", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("fviz_contrib(data.mca, choice = \"var\", axes = 2, top = 15) + theme_minimal() +", file = reportcluster , sep = "\n", append = TRUE)
        cat("  labs(title = \"Contribution of the top 15 variables to the second axis\" )  +", file = reportcluster , sep = "\n", append = TRUE)
        cat("  coord_flip()", file = reportcluster , sep = "\n", append = TRUE)
        cat("```", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("# Categories Representation  ", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("```{r, echo=FALSE, warning=FALSE}", file = reportcluster , sep = "\n", append = TRUE)
        cat("fviz_mca_var(data.mca, labelsize = 3, pointsize = 0, select.var = list(contrib = 15)) +", file = reportcluster , sep = "\n", append = TRUE)
        cat("  labs(title = \"Variable categories representation for the top 15 contributions\" )  +", file = reportcluster , sep = "\n", append = TRUE)
        cat("  theme_minimal()", file = reportcluster , sep = "\n", append = TRUE)
        cat("```", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("# Hierarchical Classification  ", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("```{r, echo=FALSE, warning=FALSE}", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("## Predict projection for new rows with Multiple Correspondence Analysis", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("#data.predict <- predict(data.mca, data2)", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("#data.mca.hcpc <- HCPC(data.mca, nb.clust = -1, min = 3, max = 4, graph = FALSE, order = FALSE, consol = TRUE, kk = 1000)", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("data.mca.hcpc <- HCPC(data.mca, graph = FALSE)", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("kable(data.mca.hcpc$desc.var$test.chi2, digits = 4, caption = \"Ordered importance of variable contributions to profiles\")", file = reportcluster , sep = "\n", append = TRUE)
        cat("# Visualize dendrogram", file = reportcluster , sep = "\n", append = TRUE)
        cat("#fviz_dend(data.mca.hcpc, show_labels = FALSE, rect = TRUE)", file = reportcluster , sep = "\n", append = TRUE)
        cat("#fviz_dend(data.mca.hcpc, cex = 0.5, k = 4,  color_labels_by_k = TRUE,  title=\"Hierarchical classification\",  horiz=TRUE, rect = TRUE)", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("plot(data.mca.hcpc, choice = \"tree\", title = \"Hierarchical classification\",  horiz = TRUE, tree.barplot = FALSE, cex = 0.6)", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("plot(data.mca.hcpc, title = \"3D view within the cloud of point of the profiles\")", file = reportcluster , sep = "\n", append = TRUE)
        cat("plot(data.mca.hcpc, choice = \"map\", title = \"profiles within the cloud of points\")", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("# Visualize cluster", file = reportcluster , sep = "\n", append = TRUE)
        cat("#fviz_cluster(data.mca.hcpc, ellipse.type = \"convex\")", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("```", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("# Modalities within each group  ", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("Only variable modalities for which critical probility is below 0.02 are used.", file = reportcluster , sep = "\n", append = TRUE)
        cat("```{r, echo=FALSE, warning=FALSE}", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("kable(data.mca.hcpc$desc.var$category$`1`, digits = 2, caption = \"Description of modalities contribution to Profile 1\")", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("kable(data.mca.hcpc$desc.var$category$`2`, digits = 2, caption = \"Description of modalities contribution to Profile 2\")", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("kable(data.mca.hcpc$desc.var$category$`3`, digits = 2, caption = \"Description of modalities contribution to Profile 3\")", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("kable(data.mca.hcpc$desc.var$category$`4`, digits = 2, caption = \"Description of modalities contribution to Profile 4\")", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("```", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("# Overview of groups  ", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("```{r, echo=FALSE, warning=FALSE}", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("cluster <- data.mca.hcpc$data.clust", file = reportcluster , sep = "\n", append = TRUE)
        cat("cluster$clust <- as.character(cluster$clust)", file = reportcluster , sep = "\n", append = TRUE)
        cat("cluster$clust[cluster$clust == 1] <- \"Profile 1\"", file = reportcluster , sep = "\n", append = TRUE)
        cat("cluster$clust[cluster$clust == 2] <- \"Profile 2\"", file = reportcluster , sep = "\n", append = TRUE)
        cat("cluster$clust[cluster$clust == 3] <- \"Profile 3\"", file = reportcluster , sep = "\n", append = TRUE)
        cat("cluster$clust[cluster$clust == 4] <- \"Profile 4\"", file = reportcluster , sep = "\n", append = TRUE)
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
        cat("            plot.background = element_rect(fill = \"transparent\",colour = NA))", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)
        cat("```", file = reportcluster , sep = "\n", append = TRUE)
        cat("\n", file = reportcluster , sep = "\n", append = TRUE)




      cat("Render Clustering report Now. It may take some time... \n ")
      rmarkdown::render(reportcluster, clean = TRUE, envir = new.env() )
      ## Put the report in the out folder
      file.rename(paste0("code/clustering-report-",framename,".docx"), paste0("out/Clustering-report-", framename ,Sys.Date(), "-chapter.docx"))
      cat(" Done!! Reports are in the folder OUT - Review the report- furter review your clustering assumptions and regenerate as needed...\n")

  }

}
NULL
