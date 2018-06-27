
#### Generate Trend Report...
#### Generate Rmd files for each chapter ------
rm(list = ls())
## Load the form

#source("code/1-loaddata-aggregate-district.R")

mainDir <- getwd()
## Load all required packages
source(paste0(mainDir,"/code/0-packages.R"))
library(koboloadeR)

## Load data

form <- "form.xls"
kobo_dico(form)
dico <- read.csv(paste("data/dico_",form,".csv",sep = ""), encoding = "UTF-8", na.strings = "")
#

datamappoly1L <- read.csv("data/datamappoly1L.csv", encoding = "UTF-8", na.strings = "NA")
datamappoly1L <- kobo_label(datamappoly1L , dico)

datamappoly1Q <- read.csv("data/datamappoly1Q.csv", encoding = "UTF-8", na.strings = "NA")
datamappoly1Q <- kobo_label(datamappoly1Q , dico)


datamappolyL <- read.csv("data/datamappolyL.csv", encoding = "UTF-8", na.strings = "NA")
datamappolyL <- kobo_label(datamappolyL , dico)

datamappolyQ <- read.csv("data/datamappolyQ.csv", encoding = "UTF-8", na.strings = "NA")
datamappolyQ <- kobo_label(datamappolyQ , dico)

datamappolyM <- read.csv("data/datamappolyM.csv", encoding = "UTF-8", na.strings = "NA")
datamappolyM <- kobo_label(datamappolyM , dico)



### Build report #####

chapters <- as.data.frame(unique(dico$chapter))
names(chapters)[1] <- "Chapter"
## Default behavior if no chapter was defined in xlsform
if ((nrow(chapters) == 1) & is.na(chapters$Chapter)) {
  cat("Defaulting questions allocation to chapter")
  dico$chapter[ dico$type %in% c("select_one","select_multiple_d")] <- "report"
  chapters <- as.data.frame(unique(dico$chapter))
  names(chapters)[1] <- "Chapter"
} else {}

chapters <- as.data.frame(chapters[!is.na(chapters$Chapter), ])

names(chapters)[1] <- "Chapter"





##Loop.chapter ------------

for (i in 1:nrow(chapters)) {
  # i <- 1
  chaptersname <- as.character(chapters[ i , 1])
  cat(paste(i, " - Render chapter for ",as.character(chapters[ i , 1]),"\n" ))

  ## Start building the report... #######
  chapter.name <- paste("code/trend-report-",chaptersname ,".Rmd", sep = "")

  ## TO DO : CHECK IF FILE EXIST - AND REQUEST USER TO DELETE BEFORE REGENERATING - SUGGESTING TO SAVE PREVIOUS UNDER NEW NAME
  if (file.exists(chapter.name)) file.remove(chapter.name)

cat("---", file = chapter.name , sep = "\n", append = TRUE)
cat(paste("title: \"Data Trend Report- Draft not for distribution. \"", sep = ""), file = chapter.name , sep = "\n", append = TRUE)
cat("author: \"Generated with [Koboloader](https://github.com/unhcr/koboloadeR) \"", file = chapter.name , sep = "\n", append = TRUE)
cat("date: \" `r format(Sys.Date(),  '%d %B %Y')`\"", file = chapter.name , sep = "\n", append = TRUE)
cat("output:",file = chapter.name , sep = "\n", append = TRUE)
cat("  word_document:", file = chapter.name , sep = "\n", append = TRUE)
cat("    fig_caption: yes", file = chapter.name , sep = "\n", append = TRUE)
cat("    fig_height: 5", file = chapter.name , sep = "\n", append = TRUE)
cat("    fig_width: 8", file = chapter.name , sep = "\n", append = TRUE)
cat("    toc: yes", file = chapter.name , sep = "\n", append = TRUE)
cat("    toc_depth: 2", file = chapter.name , sep = "\n", append = TRUE)
cat("    reference_docx: style-unhcr-portrait.docx", file = chapter.name , sep = "\n", append = TRUE)
cat("---", file = chapter.name , sep = "\n", append = TRUE)


cat("\n", file = chapter.name , sep = "\n", append = TRUE)
cat("\n", file = chapter.name , sep = "\n", append = TRUE)
## First chunk to get the data in the report

cat("```{r setup, include = FALSE, echo = FALSE, warning = FALSE, message = FALSE}", file = chapter.name , sep = "\n", append = TRUE)
cat("mainDir <- getwd()", file = chapter.name , sep = "\n", append = TRUE)
cat("mainDirroot <- substring(mainDir, 0 , nchar(mainDir) - 5)", file = chapter.name , sep = "\n", append = TRUE)
cat("## Load all required packages", file = chapter.name , sep = "\n", append = TRUE)
cat("source(paste0(mainDirroot,\"/code/0-packages.R\"))", file = chapter.name , sep = "\n", append = TRUE)
cat("source(paste0(mainDirroot,\"/code/0-theme.R\"))", file = chapter.name , sep = "\n", append = TRUE)
cat("library(koboloadeR)", file = chapter.name , sep = "\n", append = TRUE)
cat("library(corrgram)", file = chapter.name , sep = "\n", append = TRUE)
cat("library(corrplot)", file = chapter.name , sep = "\n", append = TRUE)
cat("library(scales)", file = chapter.name , sep = "\n", append = TRUE)
cat("library(lsr)", file = chapter.name , sep = "\n", append = TRUE)
cat("## Provide below the name of the form in xsl form - format should be xls not xlsx", file = chapter.name , sep = "\n", append = TRUE)
cat(paste0("form <- \"",form,"\""), file = chapter.name , sep = "\n", append = TRUE)
cat("dico <- read.csv(paste0(mainDirroot,\"/data/dico_\",form,\".csv\"), encoding = \"UTF-8\", na.strings = \"\")", file = chapter.name , sep = "\n", append = TRUE)


## TO DO: Use config file to load the different frame
cat("## Data", file = chapter.name , sep = "\n", append = TRUE)

cat("datamappoly1L <- read.csv(paste0(mainDirroot,\"/data/datamappoly1L.csv\"), encoding = \"UTF-8\", na.strings = \"NA\")", file = chapter.name , sep = "\n", append = TRUE)
cat("## label Variables", file = chapter.name , sep = "\n", append = TRUE)
cat("datamappoly1L <- kobo_label(datamappoly1L , dico)", file = chapter.name , sep = "\n", append = TRUE)
cat("\n", file = chapter.name , sep = "\n", append = TRUE)

cat("datamappoly1Q <- read.csv(paste0(mainDirroot,\"/data/datamappoly1Q.csv\"), encoding = \"UTF-8\", na.strings = \"NA\")", file = chapter.name , sep = "\n", append = TRUE)
cat("## label Variables", file = chapter.name , sep = "\n", append = TRUE)
cat("datamappoly1Q <- kobo_label(datamappoly1Q , dico)", file = chapter.name , sep = "\n", append = TRUE)
cat("\n", file = chapter.name , sep = "\n", append = TRUE)


cat("datamappolyL <- read.csv(paste0(mainDirroot,\"/data/datamappolyL.csv\"), encoding = \"UTF-8\", na.strings = \"NA\")", file = chapter.name , sep = "\n", append = TRUE)
cat("## label Variables", file = chapter.name , sep = "\n", append = TRUE)
cat("datamappolyL <- kobo_label(datamappolyL , dico)", file = chapter.name , sep = "\n", append = TRUE)
cat("\n", file = chapter.name , sep = "\n", append = TRUE)

cat("datamappolyQ <- read.csv(paste0(mainDirroot,\"/data/datamappolyQ.csv\"), encoding = \"UTF-8\", na.strings = \"NA\")", file = chapter.name , sep = "\n", append = TRUE)
cat("## label Variables", file = chapter.name , sep = "\n", append = TRUE)
cat("datamappolyQ <- kobo_label(datamappolyQ , dico)", file = chapter.name , sep = "\n", append = TRUE)
cat("\n", file = chapter.name , sep = "\n", append = TRUE)

cat("datamappolyM <- read.csv(paste0(mainDirroot,\"/data/datamappolyM.csv\"), encoding = \"UTF-8\", na.strings = \"NA\")", file = chapter.name , sep = "\n", append = TRUE)
cat("## label Variables", file = chapter.name , sep = "\n", append = TRUE)
cat("datamappolyM <- kobo_label(datamappolyM , dico)", file = chapter.name , sep = "\n", append = TRUE)
cat("\n", file = chapter.name , sep = "\n", append = TRUE)

cat(paste0("\n```\n", sep = '\n'), file = chapter.name, append = TRUE)

## Remove variable corresponding to NA - not in dico
check2 <- as.data.frame(names(datamappolyL))
names(check2)[1] <- "fullname"
check2$id <- row.names(check2)
check2 <- join(x = check2, y = dico, by = "fullname", type = "left")
check3 <- c(as.character(check2[ !(is.na(check2$name)), c("fullname")]))

### To DO : Offer option to insert in the report skeleton interpretation questions
### Intro text####################################################################
cat(paste("# Introduction\n"),file = chapter.name , sep = "\n", append = TRUE)

cat(paste("## Report content  \n"),file = chapter.name , sep = "\n", append = TRUE)
cat(paste("This data trend crunching report allows to quickly and systematically explore the results of a monitoring questionnaire aggregated by location & time.\n"),file = chapter.name , sep = "\n", append = TRUE)
cat(paste("When running a monitoring excercise, the surveyed household are not a representative sample of the population. "),file = chapter.name , sep = "\n", append = TRUE)
cat(paste("While, it is not possible to extrapolate the characteristics from the monitored population to the whole population, the aggregated trends observed for this population subset are good indication of the population status at a certain geographic level. Using such approach, it becomes possible to build a composite measurement of the situation at the level of an area. \n"),file = chapter.name , sep = "\n", append = TRUE)

cat(paste("Numeric questions are aggregated using the mean value. Categoric questions are split per question modality and the aggregated using the respective % of the modality for the district and quarter. "),file = chapter.name , sep = "\n", append = TRUE)
cat(paste("The report is organised by chapter covering the main part of the monitoring questionnaires:   \n "),file = chapter.name , sep = "\n", append = TRUE)
cat(paste("  *  First, a table presents the correlation between the various questions in the chapter. The analysis of those correlation can for instance inform the review and adaption of the questionnaires by eliminating questions that have a very high level of correlations.  \n"),file = chapter.name , sep = "\n", append = TRUE)
cat(paste("  *  Second the aggregated results for each question per quarter is presented by 2 levels of geographic aggregation. Those graphs are expected to allow for a quick idnetification of potential trends.  \n"),file = chapter.name , sep = "\n", append = TRUE)

cat(paste("## Monitoring coverage   \n"),file = chapter.name , sep = "\n", append = TRUE)
cat(paste("The coverage of this monitoring dataset vy time and geographic unit is represented in the chart below.\n"),file = chapter.name , sep = "\n", append = TRUE)

## Open chunk
cat(paste0("```{r count, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=4, size=\"small\"}\n", sep = '\n'), file = chapter.name, append = TRUE)

cat(paste0("\n", sep = '\n'), file = chapter.name, append = TRUE)
cat(paste0("ggplot(datamappoly1Q, aes(y = datamappoly1Q$count, x = datamappoly1Q$DateQ)) +", sep = '\n'), file = chapter.name, append = TRUE)
cat(paste0(" geom_bar( stat = \"identity\", fill = \"#2a87c8\", colour = \"#2a87c8\") +", sep = '\n'), file = chapter.name, append = TRUE)
cat(paste0("  xlab(\"\") +", sep = '\n'), file = chapter.name, append = TRUE)
cat(paste0("  ylab(\"\") +", sep = '\n'), file = chapter.name, append = TRUE)
cat(paste0("  facet_wrap(~ datamappoly1Q$Location1, ncol = 4) + ", sep = '\n'), file = chapter.name, append = TRUE)
cat(paste0("  theme(plot.title = element_text(face = \"bold\", size = 9), ", sep = '\n'), file = chapter.name, append = TRUE)
cat(paste0("      plot.background = element_rect(fill = \"transparent\",colour = NA), ", sep = '\n'), file = chapter.name, append = TRUE)
cat(paste0("      legend.title = element_blank(), ", sep = '\n'), file = chapter.name, append = TRUE)
cat(paste0("      legend.position = \"bottom\",", sep = '\n'), file = chapter.name, append = TRUE)
cat(paste0("      axis.text.x = element_text(angle = 45, hjust = 1))", sep = '\n'), file = chapter.name, append = TRUE)
cat(paste0("\n", sep = '\n'), file = chapter.name, append = TRUE)
cat(paste0("\n```\n", sep = '\n'), file = chapter.name, append = TRUE)

cat(paste0("```{r count2, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=9, size=\"small\"}\n", sep = '\n'), file = chapter.name, append = TRUE)

cat(paste0("\n", sep = '\n'), file = chapter.name, append = TRUE)
cat(paste0("ggplot(datamappolyQ, aes(y = datamappolyQ$count, x = datamappolyQ$DateQ)) +", sep = '\n'), file = chapter.name, append = TRUE)
cat(paste0(" geom_bar( stat = \"identity\", fill = \"#2a87c8\", colour = \"#2a87c8\") +", sep = '\n'), file = chapter.name, append = TRUE)
cat(paste0("  xlab(\"\") +", sep = '\n'), file = chapter.name, append = TRUE)
cat(paste0("  ylab(\"\") +", sep = '\n'), file = chapter.name, append = TRUE)
cat(paste0("  facet_wrap(~ datamappolyQ$Location2, ncol = 4) + ", sep = '\n'), file = chapter.name, append = TRUE)
cat(paste0("  theme(plot.title = element_text(face = \"bold\", size = 9), ", sep = '\n'), file = chapter.name, append = TRUE)
cat(paste0("      plot.background = element_rect(fill = \"transparent\",colour = NA), ", sep = '\n'), file = chapter.name, append = TRUE)
cat(paste0("      legend.title = element_blank(), ", sep = '\n'), file = chapter.name, append = TRUE)
cat(paste0("      legend.position = \"bottom\",", sep = '\n'), file = chapter.name, append = TRUE)
cat(paste0("      axis.text.x = element_text(angle = 45, hjust = 1))", sep = '\n'), file = chapter.name, append = TRUE)
cat(paste0("\n", sep = '\n'), file = chapter.name, append = TRUE)
cat(paste0("\n```\n", sep = '\n'), file = chapter.name, append = TRUE)



  ## Getting chapter questions #######
  #chapterquestions <- dico[which(dico$chapter== chaptersname ), c("chapter", "name", "label", "type", "qrepeatlabel", "fullname","listname") ]
  chapterquestions <- dico[which(dico$chapter == chaptersname & dico$fullname %in% check3),
                           c("chapter", "name", "label", "type", "qrepeatlabel", "fullname","listname","variable") ]

  chapterquestions.name <- as.character(chapterquestions[ ,c("fullname")])
  chapterqu <- paste("\"",as.character(chapterquestions.name),"\"", collapse = ", ", sep = "")


  cat(paste("# Analysis of Indicators Correlation  \n"),file = chapter.name , sep = "\n", append = TRUE)

  ## Check Corregram ######
  ## Open chunk
  cat(paste0("```{r ", chaptersname , ".cor, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=9, size=\"small\"}\n", sep = '\n'), file = chapter.name, append = TRUE)

  cat(paste0("data.L <- datamappolyL[ ,c(",chapterqu  , ")]"),file = chapter.name , sep = "\n", append = TRUE)

  cat(paste0("# Matrix Correlation"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("data.corr.c <- cor(data.L)"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("## Output corrmatrix as pair"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("data.corr.pair <- melt(data.corr.c)"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("data.corr.pair <- data.corr.pair[ !(is.na(data.corr.pair$value)), ]"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("data.corr.pair <- data.frame(t(apply(data.corr.pair, 1, sort)))"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("data.corr.pair <- data.corr.pair[duplicated(data.corr.pair[, 1:2], MARGIN = 1), ]"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("data.corr.pair <- data.corr.pair[, 3:1]"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("data.corr.pair$key <- paste(data.corr.pair$X3, data.corr.pair$X2, sep = \" - \")"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("data.corr.pair$corr <- as.numeric(as.character(data.corr.pair$X1))"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("data.corr.pair$Correlation.Type <- \"\""),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("data.corr.pair$Correlation.Type[ data.corr.pair$corr  > 0 ] <- \"Positive\""),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("data.corr.pair$Correlation.Type[ data.corr.pair$corr  < 0 ] <- \"Negative\""),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("data.corr.pair$Correlation <- \"\""),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("data.corr.pair$Correlation[ data.corr.pair$corr < 0.2 & data.corr.pair$corr > -0.2 ] <- \"Very weak\""),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("data.corr.pair$Correlation[ data.corr.pair$corr > 0.2 & data.corr.pair$corr < 0.4 | data.corr.pair$corr < -0.2  & data.corr.pair$corr > -0.4 ] <- \"Weak\""),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("data.corr.pair$Correlation[ data.corr.pair$corr > 0.4 & data.corr.pair$corr < 0.6 | data.corr.pair$corr < -0.4  & data.corr.pair$corr > -0.6 ] <- \"Moderate\""),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("data.corr.pair$Correlation[ data.corr.pair$corr > 0.6 & data.corr.pair$corr < 0.8 | data.corr.pair$corr < -0.6 & data.corr.pair$corr > -0.8 ] <- \"Strong\""),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("data.corr.pair$Correlation[ data.corr.pair$corr > 0.8 | data.corr.pair$corr < -0.8 ] <- \"Very strong\""),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("data.corr.pair$Correlation <- factor(data.corr.pair$Correlation, levels = c(\"Very weak\", \"Weak\", \"Moderate\", \"Strong\", \"Very strong\"))"),file = chapter.name , sep = "\n", append = TRUE)

  cat(paste0("# adding label"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("names(data.corr.pair)[1] <- \"fullname\""),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("data.corr.pair <- join(x = data.corr.pair, y = dico[ c(\"fullname\", \"label\")], by = \"fullname\", type = \"left\")"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("names(data.corr.pair)[1] <- \"Var1\""),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("names(data.corr.pair)[2] <- \"fullname\""),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("data.corr.pair <- join(x = data.corr.pair, y = dico[ c(\"fullname\", \"label\")], by = \"fullname\", type = \"left\")"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("names(data.corr.pair)[2] <- \"Var2\""),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("names(data.corr.pair)[8] <- \"Question/modality-1\""),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("names(data.corr.pair)[9] <- \"Question/modality-2\""),file = chapter.name , sep = "\n", append = TRUE)

  cat(paste0("### Adding significance.... extract p value"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("data.corr.cs <- as.data.frame(cor.mtest(data.L)$p)"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("## ading variable name form correlation matrix"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("names(data.corr.cs) <- names(as.data.frame(data.corr.c))"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("row.names(data.corr.cs) <- row.names(as.data.frame(data.corr.c))"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("## Transform in Matrix"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("data.corr.cs <- as.matrix(data.corr.cs)"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("data.corr.pair1 <- melt(data.corr.cs)"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("data.corr.pair1 <- data.corr.pair1[ !(is.na(data.corr.pair1$value)), ]"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("data.corr.pair1 <- data.frame(t(apply(data.corr.pair1, 1, sort)))"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("data.corr.pair1 <- data.corr.pair1[duplicated(data.corr.pair1[, 1:2], MARGIN = 1), ]"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("data.corr.pair1 <- data.corr.pair1[, 3:1]"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("data.corr.pair1$sign <- as.numeric(as.character(data.corr.pair1$X1))"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("data.corr.pair1$Significant <- \"\""),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("data.corr.pair1$Significant[ data.corr.pair1$sign > 0.1  ] <- \"Not significant\""),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("data.corr.pair1$Significant[ data.corr.pair1$sign < 0.1  ] <- \"Weakly significant\""),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("data.corr.pair1$Significant[ data.corr.pair1$sign < 0.05  ] <- \"Significant\""),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("data.corr.pair1$Significant[ data.corr.pair1$sign < 0.01  ] <- \"Very Significant\""),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("data.corr.pair1$key <- paste(data.corr.pair1$X3, data.corr.pair1$X2, sep = \" - \")"),file = chapter.name , sep = "\n", append = TRUE)

  cat(paste0("data.corr.pair2 <- join(x = data.corr.pair, y = data.corr.pair1 , by = \"key\")"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("data.corr.pair3 <- data.corr.pair2[ order(data.corr.pair2$Correlation, decreasing = TRUE), c(\"Question/modality-1\", \"Question/modality-2\", \"Correlation\" , \"Correlation.Type\",  \"Significant\" )]"),file = chapter.name , sep = "\n", append = TRUE)


 # \"Very weak\", \"Weak\", \"Moderate\", \"Strong\", \"Very strong\"

  cat(paste0("\n\n", sep = '\n'), file = chapter.name, append = TRUE)
#  cat(paste("# Matrice de p-value de la corrélation"),file = chapter.name , sep = "\n", append = TRUE)
#  cat(paste("data.corr.c <- cor(data.L)"),file = chapter.name , sep = "\n", append = TRUE)
#  cat(paste("data.corr.s <- cor.mtest(data.L)"),file = chapter.name , sep = "\n", append = TRUE)

#  cat(paste("corrplot(data.corr.c, type = \"upper\", #order = \"hclust\","),file = chapter.name , sep = "\n", append = TRUE)
#  cat(paste("p.mat = data.corr.s, sig.level = 0.01,  insig = \"blank\", ## add significance"),file = chapter.name , sep = "\n", append = TRUE)
#  cat(paste("#col=c(\"black\", \"white\"),bg=\"lightblue\","),file = chapter.name , sep = "\n", append = TRUE)
#  cat(paste("col = brewer.pal(n = 8, name = \"PuOr\"),"),file = chapter.name , sep = "\n", append = TRUE)
#  cat(paste("tl.col = \"black\", tl.srt = 45, tl.cex = 0.2, ## check label..."),file = chapter.name , sep = "\n", append = TRUE)
#  cat(paste("diag = FALSE  )## do not display diagnonal"),file = chapter.name , sep = "\n", append = TRUE)

#  cat(paste("## Output corrmatrix as pair"),file = chapter.name , sep = "\n", append = TRUE)
#  cat(paste("tmp <- melt(data.corr.c)"),file = chapter.name , sep = "\n", append = TRUE)
#  cat(paste("tmp <- data.frame(t(apply(tmp, 1, sort)))"),file = chapter.name , sep = "\n", append = TRUE)
#  cat(paste("tmp <- tmp[duplicated(tmp[, 1 : 2], MARGIN = 1), ]"),file = chapter.name , sep = "\n", append = TRUE)
#  cat(paste("temp1 <- tmp[, 3 : 1]"),file = chapter.name , sep = "\n", append = TRUE)

  cat(paste0("\n```\n", sep = '\n'), file = chapter.name, append = TRUE)


  cat(paste("## Indicators/modalities with very strong correlation  \n"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("```{r ", chaptersname , "1.cor, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=9, size=\"small\"}\n", sep = '\n'), file = chapter.name, append = TRUE)
  cat(paste0("tab <- data.corr.pair3[data.corr.pair3$Correlation == \"Very strong\", c(\"Question/modality-1\", \"Question/modality-2\", \"Correlation.Type\",  \"Significant\" ) ]"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("row.names(tab) <- NULL"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("kable(tab, caption = \"__Table__ \") %>% kable_styling(position = \"center\")"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("\n```\n", sep = '\n'), file = chapter.name, append = TRUE)

  cat(paste("## Indicators/modalities with strong correlation  \n"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("```{r ", chaptersname , "2.cor, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=9, size=\"small\"}\n", sep = '\n'), file = chapter.name, append = TRUE)
  cat(paste0("tab <- data.corr.pair3[data.corr.pair3$Correlation == \"Strong\", c(\"Question/modality-1\", \"Question/modality-2\", \"Correlation.Type\",  \"Significant\" ) ]"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("row.names(tab) <- NULL"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("kable(tab, caption = \"__Table__\") %>% kable_styling(position = \"center\")"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("\n```\n", sep = '\n'), file = chapter.name, append = TRUE)

  cat(paste("## Indicators/modalities with moderate correlation  \n"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("```{r ", chaptersname , "3.cor, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=9, size=\"small\"}\n", sep = '\n'), file = chapter.name, append = TRUE)
  cat(paste0("tab <- data.corr.pair3[data.corr.pair3$Correlation == \"Moderate\", c(\"Question/modality-1\", \"Question/modality-2\", \"Correlation.Type\",  \"Significant\" ) ]"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("row.names(tab) <- NULL"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("kable(tab, caption = \"__Table__ \") %>% kable_styling(position = \"center\")"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("\n```\n", sep = '\n'), file = chapter.name, append = TRUE)

  cat(paste("## Indicators/modalities with weak correlation  \n"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("```{r ", chaptersname , "4.cor, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=9, size=\"small\"}\n", sep = '\n'), file = chapter.name, append = TRUE)
  cat(paste0("tab <- data.corr.pair3[data.corr.pair3$Correlation == \"Weak\", c(\"Question/modality-1\", \"Question/modality-2\", \"Correlation.Type\",  \"Significant\" ) ]"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("row.names(tab) <- NULL"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("kable(tab, caption = \"__Table__\") %>% kable_styling(position = \"center\")"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("\n```\n", sep = '\n'), file = chapter.name, append = TRUE)

  cat(paste("## Indicators/modalities with very weak correlation  \n"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("```{r ", chaptersname , "5.cor, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=9, size=\"small\"}\n", sep = '\n'), file = chapter.name, append = TRUE)
  cat(paste0("tab <- data.corr.pair3[data.corr.pair3$Correlation == \"Very weak\", c(\"Question/modality-1\", \"Question/modality-2\", \"Correlation.Type\",  \"Significant\" ) ]"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("row.names(tab) <- NULL"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("kable(tab, caption = \"__Table__\") %>% kable_styling(position = \"center\")"),file = chapter.name , sep = "\n", append = TRUE)
  cat(paste0("\n```\n", sep = '\n'), file = chapter.name, append = TRUE)




  cat(paste("# Result trends for each question   \n"),file = chapter.name , sep = "\n", append = TRUE)
  #cat(paste("##### Page Break"),file = chapter.name ,sep = "\n", append = TRUE)

  for (j in 1:nrow(chapterquestions))
  {
    # j <- 1
    ## Now getting level for each questions
    questions.name <- as.character(chapterquestions[ j , c("fullname")])
    questions.shortname <- as.character(chapterquestions[ j , c("name")])
    questions.label <- as.character(chapterquestions[ j , c("label")])
    questions.type <- as.character(chapterquestions[ j , c("type")])
    cat(paste("\n", i, "-", j, " - Render question: ", questions.label,  "\n" ))

    ## write question name-------
    cat("\n ",file = chapter.name , sep = "\n", append = TRUE)
    cat(paste("## ", questions.label ,"\n", sep = ""),file = chapter.name , sep = "\n", append = TRUE)


    if (questions.type == "integer") {
      cat(paste("This question was of type ", questions.type,". The graphs below present means value for this question.\n" ,sep = ""),file = chapter.name , sep = "\n", append = TRUE) }
    else if (questions.type == "select_one_d") {
      cat(paste("This question was of a modality in a unique choice question. The graphs below present the proportion for that modality aggregated at different geographic level.\n" ,sep = ""),file = chapter.name , sep = "\n", append = TRUE) }
    else if (questions.type == "select_one_d") {
      cat(paste("This question was of a modality in a multiple choice question. The graphs below present the proportion for that modality aggregated at different geographic level.\n" ,sep = ""),file = chapter.name , sep = "\n", append = TRUE) }


    ## Now create para based on question type-------


    ## Open chunk
    cat(paste0("```{r ", questions.name, ".tab, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=4, size=\"small\"}\n", sep = '\n'), file = chapter.name, append = TRUE)

    cat(paste0("\n", sep = '\n'), file = chapter.name, append = TRUE)
    cat(paste0("ggplot(datamappoly1Q, aes(y = datamappoly1Q$",questions.name  ,", x = datamappoly1Q$DateQ)) +", sep = '\n'), file = chapter.name, append = TRUE)
    cat(paste0(" geom_bar( stat = \"identity\", fill = \"#2a87c8\", colour = \"#2a87c8\") +", sep = '\n'), file = chapter.name, append = TRUE)
    cat(paste0("  xlab(\"\") +", sep = '\n'), file = chapter.name, append = TRUE)
    cat(paste0("  ylab(\"\") +", sep = '\n'), file = chapter.name, append = TRUE)

   if (questions.type == "integer") {
      cat(paste0("  scale_y_continuous(labels = percent) +", sep = '\n'), file = chapter.name, append = TRUE)  } else {
      cat(paste0("  scale_y_continuous(labels = format_si()) +", sep = '\n'), file = chapter.name, append = TRUE)}

    cat(paste0("  facet_wrap(~ datamappoly1Q$Location1, ncol = 4) + ", sep = '\n'), file = chapter.name, append = TRUE)
    cat(paste0("  theme(plot.title = element_text(face = \"bold\", size = 9), ", sep = '\n'), file = chapter.name, append = TRUE)
    cat(paste0("      plot.background = element_rect(fill = \"transparent\",colour = NA), ", sep = '\n'), file = chapter.name, append = TRUE)
    cat(paste0("      legend.title = element_blank(), ", sep = '\n'), file = chapter.name, append = TRUE)
    cat(paste0("      legend.position = \"bottom\",", sep = '\n'), file = chapter.name, append = TRUE)
    cat(paste0("      axis.text.x = element_text(angle = 45, hjust = 1))", sep = '\n'), file = chapter.name, append = TRUE)

    cat(paste0("\n", sep = '\n'), file = chapter.name, append = TRUE)
    cat(paste0("\n```\n", sep = '\n'), file = chapter.name, append = TRUE)

    cat(paste0("\n", sep = '\n'), file = chapter.name, append = TRUE)

    ## Open chunk
    cat(paste0("```{r ", questions.name, "2.tab, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=9, size=\"small\"}\n", sep = '\n'), file = chapter.name, append = TRUE)

    cat(paste0("\n", sep = '\n'), file = chapter.name, append = TRUE)
    cat(paste0("ggplot(datamappolyQ, aes(y = datamappolyQ$",questions.name  ,", x = datamappolyQ$DateQ)) +", sep = '\n'), file = chapter.name, append = TRUE)
    cat(paste0(" geom_bar( stat = \"identity\", fill = \"#2a87c8\", colour = \"#2a87c8\") +", sep = '\n'), file = chapter.name, append = TRUE)
    cat(paste0("  xlab(\"\") +", sep = '\n'), file = chapter.name, append = TRUE)
    cat(paste0("  ylab(\"\") +", sep = '\n'), file = chapter.name, append = TRUE)

    if (questions.type == "integer") {
      cat(paste0("  scale_y_continuous(labels = percent) +", sep = '\n'), file = chapter.name, append = TRUE)  } else {
        cat(paste0("  scale_y_continuous(labels = format_si()) +", sep = '\n'), file = chapter.name, append = TRUE)}

    cat(paste0("  facet_wrap(~ datamappolyQ$Location2, ncol = 4) + ", sep = '\n'), file = chapter.name, append = TRUE)
    cat(paste0("  theme(plot.title = element_text(face = \"bold\", size = 9), ", sep = '\n'), file = chapter.name, append = TRUE)
    cat(paste0("      plot.background = element_rect(fill = \"transparent\",colour = NA), ", sep = '\n'), file = chapter.name, append = TRUE)
    cat(paste0("      legend.title = element_blank(), ", sep = '\n'), file = chapter.name, append = TRUE)
    cat(paste0("      legend.position = \"bottom\",", sep = '\n'), file = chapter.name, append = TRUE)
    cat(paste0("      axis.text.x = element_text(angle = 45, hjust = 1))", sep = '\n'), file = chapter.name, append = TRUE)
    cat(paste0("\n", sep = '\n'), file = chapter.name, append = TRUE)
    cat(paste0("\n```\n", sep = '\n'), file = chapter.name, append = TRUE)

    cat(paste("##### Page Break"),file = chapter.name ,sep = "\n", append = TRUE)

  }

render(paste0(chapter.name , sep = ""))
## Put the report in the out folder
file.rename(paste0("code/trend-report-",chaptersname ,".docx"), paste0("out/Trend-report-",chaptersname ,Sys.Date(), ".docx") )
cat(paste(" Done!! Reports ",chapter.name," in the folder OUT ! \n", sep = ""))


## Clean  memory
gc()
}








#rmarkdown::render('report-tabulation.Rmd')

cat(" Done!! Reports are in the folder OUT - Review the report- Adjust your configuration files and you will be very soon ready to start the qualitative analysis and the analysis workshops...")



