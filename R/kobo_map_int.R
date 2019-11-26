#' @name kobo_map_int
#' @rdname kobo_map_int
#' @title  Generate Maps for integer variables
#'
#' @description  Automatically generate maps for all nominal & ordinal variables based on dates. ggplot2 is used.
#'
#'
#' @param data kobodatset to use
#'
#' @param xmax Bounding box for the map - max longitude - in decimal degree
#' @param xmin Bounding box for the map - min longitude - in decimal degree
#' @param ymax Bounding box for the map - max latitude - in decimal degree
#' @param ymin Bounding box for the map - min latitude - in decimal degree
#'
#' @param dico ( generated from kobo_dico)
#'
#'
#'
#' @author Edouard Legoupil
#'
#'
#' @export kobo_map_int
#'
#' @examples
#' \dontrun{
#' kobo_map_int(data,xmax,xmin,ymax,ymin, dico)
#' }
#'
#'

kobo_map_int <- function(data,xmax,xmin,ymax,ymin, dico) {

  mainDir <- "out"
  subDir <- "map"
  if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
    cat("map directory exists in out directory and is a directory.\n")
  } else if (file.exists(paste(mainDir, subDir, sep = "/", collapse = "/"))) {
    cat("map directory exists in your out directory.\n")
    # you will probably want to handle this separately
  } else {
    cat("map directory does not exist in your out directory - creating now!\n ")
    dir.create(file.path(mainDir, subDir))
  }



  ##### Selecting the variable to be mapped

  selectgeo <- as.character(dico[dico$type %in% c("geopoint"), c("fullname")])

  if(length(selectgeo)==0) {
    cat("There's no variable to be correlated in your data analysis plan.\n")
  } else if(length(selectgeo)==1){
    cat("There's one geopoint variable(s) we can map from in your data analysis plan. Let's proceed! \n")


    data$geo <- as.character(data[ , names(data)==selectgeo])
    options(digits = 15)
    data$lat <- as.numeric(substr(data$geo , 1,13))
    data$long <- as.numeric(substr( data$geo, 15,27))


    ### Eliminate record without coordinates
    datasp <-data[!rowSums(is.na(data["lat"])), ]
    datasp <-datasp[!rowSums(is.na(datasp["long"])), ]

    #ymax <- 40
    #ymin <- 34
    #xmax <- 34
    #xmin <- 28

    ### Eliminate record with obviously wrong coordinates
    datasp <-datasp[(datasp["lat"] <=  xmax), ]
    datasp <-datasp[(datasp["lat"] >= xmin), ]
    datasp <-datasp[(datasp["long"] <= ymax ), ]
    datasp <-datasp[(datasp["long"] >= ymin ), ]


    selectallt <- as.data.frame(dico[dico$type %in% c("integer"), c("fullname","listname","label","name")])
    ### Verify that those variable are actually in the original dataframe
    check <- as.data.frame(names(data))
    names(check)[1] <- "fullname"
    check$id <- row.names(check)
    selectallt <- join(x=selectallt, y=check, by="fullname",  type="left")
    selectallt <- selectallt[!is.na(selectallt$id), ]

    selectall  <- as.character(selectallt[, c("fullname")])


    datasp.selectall <- cbind(datasp[ ,c("lat","long")],datasp [ , selectall  ])
    datasp.selectall  <- kobo_label(datasp.selectall, dico)


    ## Googlemap background
    cat("Getting a black & white background map from Google \n")
    gmapback <- get_map(location = c(ymin, xmin, ymax, xmax), color = "bw", source = "google",maptype = "road")

    ## histogramme to display event occurence over time - startting faceting
    for (i in 1:nrow(selectallt) ) {
      #i<- 25
      #rm(variablename)
      variablename <- names(datasp.selectall)[i+2]
      title <- attributes(datasp.selectall)$variable.labels[i+2]

      ## names(datasp.selectall)
      ## scale(as.numeric(datasp.selectall[,i+2]), center = TRUE, scale = TRUE)
      datasp.selectall[,i+2] <- as.numeric(datasp.selectall[,i+2])
      ggmap( gmapback) +
      #ggplot(datasp.selectall, aes(long, lat)) +
        stat_summary_hex(aes(x= long, y= lat, z = datasp.selectall[,i+2]),
                         data=datasp.selectall,
                         #fun = "mean",
                         fun = "var",
                         na.rm = TRUE,
                         #fun = "quantile", fun.args = list(probs = 0.1),
                         alpha = 0.7, bins = 70) +
        coord_equal() +   theme_map() +
        scale_fill_gradient(low = "#ffffcc", high = "#ff4444",
                            name = "",
                            guide = guide_legend( direction = "horizontal", label.position = "bottom",
                                                 keyheight = unit(2, units = "mm"),
                                                 keywidth = unit(length(labels)*10, units = "mm"),
                                                  title.position = 'top',  title.hjust = 0.5, label.hjust = 1, nrow = 1, byrow = T, reverse = T )) +
        theme( legend.position = c(0.5, 0.03), legend.text.align = 0, legend.background = element_rect(fill = alpha('white', 0.0)),
               legend.text = element_text(size = 7, hjust = 0, color = "#4e4d47"), plot.title = element_text(hjust = 0.5, color = "#4e4d47"),
               plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47", margin = margin(b = -0.1, t = -0.1, l = 2, unit = "cm"), debug = F),
               legend.title = element_text(size = 8), plot.margin = unit(c(.5,.5,.2,.5), "cm"), panel.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
               panel.border = element_blank(), plot.caption = element_text(size = 6, hjust = 0.92, margin = margin(t = 0.2, b = 0, unit = "cm"), color = "#939184")) +
        labs(title = title, subtitle = "Map displaying average value in hexagrid", caption = "", x = NULL, y = NULL)

      ggsave(filename=paste("out/map/maphex_",variablename,".png",sep=""), width=10, height=8,units="in", dpi=300)
      cat(paste0("Generated Map for question: ", title , "\n"))
      }
  } else {
    cat("Strange, you have multiple geopoint in your dataset \n")
  }

}
NULL
