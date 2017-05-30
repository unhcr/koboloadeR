#' @name kobo_trend
#' @rdname kobo_trend
#' @title  Generate histogramm plots based on dates
#'
#' @description  Automatically generate histogramm for all nominal & ordinal variables based on dates. ggplot2 is used.
#'
#'
#' @param data kobodatset to use
#' @param date field of date type used to generare trends
#' @param duration number of days in the past
#' @param dico ( generated from kobo_dico)
#'
#' @author Edouard Legoupil
#'
#' @examples
#' kobo_bar_trend()
#'
#' @export kobo_trend
#'
#' @examples
#' \dontrun{
#' kobo_trend(data, date, dico)
#' }
#'
#'

kobo_trend <- function(data, date, dico) {

  mainDir <- "out"
  subDir <- "trend"
  if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
    cat("trend directory exists in out directory and is a directory.\n")
  } else if (file.exists(paste(mainDir, subDir, sep = "/", collapse = "/"))) {
    cat("trend directory exists in your out directory.\n")
    # you will probably want to handle this separately
  } else {
    cat("trend directory does not exist in your out directory - creating now!\n ")
    dir.create(file.path(mainDir, subDir))
  }


  selectallt <- as.data.frame(dico[dico$type %in% c("select_one"), c("fullname","listname","label","name")])
  ### Verify that those variable are actually in the original dataframe
  check <- as.data.frame(names(data))
  names(check)[1] <- "fullname"
  check$id <- row.names(check)
  selectallt <- join(x=selectallt, y=check, by="fullname",  type="left")
  selectallt <- selectallt[!is.na(selectallt$id), ]

  selectall  <- as.character(selectallt[, c("fullname")])
  data.date <- as.data.frame(data [, date ])
  names(data.date)[1] <- "date"

  #str(data.date$date)
  if(!inherits(data.date$date, 'Date')) {
    cat("conversion to date format\n")
  data.date$date <- as.Date(as.character(data.date$date), format = "%d-%m-%Y %H:%M:%S")
  } else { cat("Already date format\n") }

  #data.date$month <- format(data.date$date,"%B-%Y")
  #str(data.date$month)

  data.selectall <- cbind(data.date,data [ , selectall  ])
  data.selectall  <- kobo_label(data.selectall, dico)

  ## histogramme to display event occurence over time - startting faceting
  for (i in 1:nrow(selectallt) ) {
    #i<- 1
    #rm(variablename)
    variablename <- names(data.selectall)[i+1]
    title <- attributes(data.selectall)$variable.labels[i+1]

    data.trend2 <- as.data.frame(prop.table(table(format(data.selectall$date,"%B-%Y"), data.selectall[ , i+1]), 1))
    data.trend2 <- data.trend2[data.trend2$Freq>0, ]
    data.trend2$date <-  as.Date(as.character(paste('01-', data.trend2$Var1, sep = '')), format = "%d-%B-%Y")
    data.trend2$Var1 <- factor(data.trend2$Var1, levels=data.trend2[order(data.trend2$date), c("Var1")])
    #levels(data.trend2$Var1)


    ggplot(data.trend2, aes(x=Var1, y=Freq, group=as.factor(data.trend2$Var2), fill=as.factor(data.trend2$Var2))) +
      #geom_line( aes(color=as.factor(data.trend2$Var2), size=2 ))+
      #geom_smooth(aes(color=as.factor(data.trend2$Var2), size=2 )) +
      geom_bar( stat = "identity",position = "dodge") +
      # fill="#2a87c8",colour="#2a87c8",
      xlab("") + ylab("")+
      scale_y_continuous(labels=percent)+
      ggtitle(title)+
      theme(plot.title=element_text(face="bold", size=9),
            plot.background = element_rect(fill = "transparent",colour = NA),
            legend.title = element_blank(),
            legend.position="bottom")
    ggsave(filename=paste("out/trend/trend_",variablename,".png",sep=""), width=10, height=8,units="in", dpi=300)
    cat(paste0("Generated Trend graph for question: ", title , "\n"))

  }


}
NULL
