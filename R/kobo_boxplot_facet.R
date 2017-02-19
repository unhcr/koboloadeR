#' @name kobo_boxplot_facet
#' @rdname kobo_boxplot_facet
#' @title  Generate histogramm plots based on dates
#'
#' @description  Automatically generate boxplot. ggplot2 is used.
#'
#'
#' @param data kobodatset to use
#' @param dico ( generated from kobo_dico)
#'
#'
#'
#' @author Edouard Legoupil
#'
#' @examples
#' kobo_boxplot_facet()
#'
#' @export kobo_boxplot_facet
#' @examples
#' \dontrun{
#' kobo_boxplot_facet(data,  dico)
#' }
#'
#'

kobo_boxplot_facet <- function(data,  dico) {

  mainDir <- "out"
  subDir <- "boxplot"
  if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
    cat("boxplot directory exists in out directory and is a directory.\n")
  } else if (file.exists(paste(mainDir, subDir, sep = "/", collapse = "/"))) {
    cat("boxplot directory exists in your out directory.\n")
    # you will probably want to handle this separately
  } else {
    cat("boxplot directory does not exist in your out directory - creating now!\n ")
    dir.create(file.path(mainDir, subDir))
  }
  
  
  ## Check that those variable are in the dataset
  selectdf <- dico[dico$type=="integer" , c("fullname","listname","label","name","variable","disaggregation")]
  check <- as.data.frame(names(data))
  names(check)[1] <- "fullname"
  check$id <- row.names(check)
  selectdf <- join(x=selectdf, y=check, by="fullname",  type="left")
  selectdf <- selectdf[!is.na(selectdf$id), ]
  
  ## now correct list of variables
  selectinteger <- as.character(selectdf[, c("fullname")])
  ## df of variable to loop around
  selectintegert <- as.data.frame(selectinteger)
  
  ## Check that those variable are in the dataset
  selectdf1 <- dico[dico$disaggregation=="facet" & dico$type=="select_one" , c("fullname","listname","label","name","variable","disaggregation")]
  selectdf1 <- join(x=selectdf1, y=check, by="fullname",  type="left")
  selectdf1 <- selectdf1[!is.na(selectdf1$id), ]
  
  ## now correct list of variables
  selectfacet <- as.character(selectdf1[, c("fullname")])
  ## df of variable to loop around
  selectfacett <- as.data.frame(selectfacet)
  
  if(length(selectfacet)==0) {
    cat("There's no variable to facet in your data analysis plan.\n")
  } else {  cat(paste0( length(selectfacet) , " variable(s) to facet in your data analysis plan. Let's proceed! \n"))
    
    ## subset data with selectone
    data.integer <- cbind(data [ selectfacet], data [ selectinteger ]) 
    ## Remove variable where we get only NA
    data.integer <- data.integer[,colSums(is.na(data.integer))<nrow(data.integer)]
    data.integer <- kobo_label(data.integer, dico)
    
    
    ## loop around the list of variables to facet
    for (i in 1:nrow(selectfacett) ) {
      # i <-1
      facetname <- as.character(selectfacett[i ,1])
      facetlabel <- as.character(dico[dico$fullname==facetname,c("label")])
      
      ### Now let's create dot plot
      for (j in 1:nrow(selectintegert) ) {
        # j <-1
        variablename <- as.character(selectintegert[j,1])
        variablelabel <- as.character(dico[dico$fullname==variablename,c("label")])
        
        ## print sutff
        cat(paste(i," facet ", facetlabel," - ",facetname," \n"  ))
        cat(paste(j," variable ",variablelabel," - ",variablename," \n"))
        ### testing that the variable to map is not the same than the variable to facetat!
        
          ## and now the graph
          data.integer[ , j+nrow(selectfacett)] <- as.integer(data.integer[ , j+nrow(selectfacett)])
          
          ggplot(data.integer, aes(x=data.integer[ , i], y=data.integer[ , j+nrow(selectfacett)]), fill=as.factor(data.integer[ , i])) +
            geom_boxplot( ) +  #notch=TRUE
            scale_size_area(max_size = 10)+
            guides(fill=FALSE) +
            xlab("") +
            ylab("") +
            coord_flip()+
            scale_y_continuous(breaks= pretty_breaks()) +
            #geom_smooth(method=lm) +  # Add a loess smoothed fit curve with confidence region
            ggtitle(paste("Boxplot for question: ", variablelabel,sep=""),
                    subtitle = paste("Facetted by question: ",facetlabel,sep=""))+
            
            theme(plot.title=element_text(face="bold", size=9),
                  plot.background = element_rect(fill = "transparent",colour = NA))
          ggsave(filename=paste("out/boxplot/boxplot_",facetname,"_correl_",variablename,".png",sep=""), width=10, height=10,units="in", dpi=300)
          
          cat(paste0("Generated bar chart for question: ",variablelabel  ," - faceted with with - ",facetlabel,"\n"))
          
          rm(variablename)
        }
      
      rm(facetname)
    }
  }
}
NULL

