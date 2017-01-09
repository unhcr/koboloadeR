#' @name kobo_bar_multi
#' @rdname kobo_bar_multi
#' @title  Generate bar Chart - frequency - for select_multiple questions
#'
#' @description  Automatically generate bar chart for each of the select_multiple question in the dataset. ggplot2 is used.
#'
#'
#' @param data kobodatset to use
#' @param dico ( generated from kobo_dico)
#'
#'
#' @return Save in the out folder one bar plot per select_one variable
#'
#' @author Edouard Legoupil
#'
#' @examples
#' kobo_bar_one()
#'
#' @export kobo_bar_multi
#' @examples
#' \dontrun{
#' kobo_bar_multi(data, dico)
#' }
#'
#' @export plots
#'

kobo_bar_multi <- function(data, dico) {

  selectdf <- dico[dico$type=="select_multiple", c("fullname","listname","label","name")]
  
  ### Verify that those variable are actually in the original dataframe
  check <- as.data.frame(names(data))
  names(check)[1] <- "fullname"
  check$id <- row.names(check)
  selectdf <- join(x=selectdf, y=check, by="fullname",  type="left")
  selectdf <- selectdf[!is.na(selectdf$id), ]
  
  selectmulti <- as.character(selectdf[, c("fullname")])
  data.selectmulti <- data [selectmulti ]
  data.selectmulti  <- kobo_label(data.selectmulti, dico) 
  
  
  listmulti <- dico[dico$type=="select_multiple_d", c("listname","label","name","fullname")]
  selectdf1 <- as.data.frame(unique(selectdf$listname))
  names(selectdf1)[1] <- "listname"
  listmulti <- join(x=listmulti, y=selectdf1, by="listname", type="left") 


  for (i in 1:nrow(listmulti) ) {
    # i <- 5
    listloop <- as.character(listmulti[i,1])
    listlabel <-  as.character(listmulti[i,2])
    
    
    ### select variable for a specific multiple questions
    selectmultilist <- as.character(dico[dico$type=="select_multiple" & dico$listname==listloop , c("fullname")])
    
    ## Check that those variable are in the dataset
    selectdf2 <- dico[dico$type=="select_multiple" & dico$listname==listloop , c("fullname","listname","label","name")]
    selectdf2 <- join(x=selectdf2, y=check, by="fullname",  type="left")
    selectdf2 <- selectdf[!is.na(selectdf$id), ]
    selectmultilist <- as.character(selectdf2[, c("fullname")])

    ## Reshape answers
    data.selectmultilist <- data.selectmulti[ selectmultilist ]
    data.selectmultilist$id <- rownames(data.selectmultilist)
    meltdata <- melt(data.selectmultilist,id="id")

    castdata <- as.data.frame(table(meltdata[c("value")])) #,  useNA = "ifany"
    castdata$freqper <- castdata$Freq/nrow(data.selectmultilist)
    #castdata <- dcast(meltdata, value~variable, fun.aggregate = length)
    castdata$Var1 <-factor(castdata$Var1, levels=castdata[order(castdata$freqper), "Var1"])

    ggplot(castdata, aes(x=Var1, y=freqper)) +
      geom_bar(fill="#2a87c8",colour="#2a87c8",stat = "identity") +
      xlab("") + ylab("")+
      coord_flip()+
      ggtitle(listlabel)+
      theme(plot.title=element_text(face="bold", size=9),
            plot.background = element_rect(fill = "transparent",colour = NA))
    ggsave(filename=paste("out/bar_multifreq_",listloop,".png",sep=""), width=8, height=10,units="in", dpi=300)
    
    cat(paste0("Generated graph for question: ", listlabel , "\n"))

  }



}
NULL
