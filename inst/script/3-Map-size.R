### Get map Background
## source("code/3-Map-require.R")


############################################################
# function to map variable
# Now we can start plotting in a loop
# the key is at the line here
# p + aes_string(x = names(mydata)[i])
# Use aes_string instead of aes, so that when you look at summary(ggplot_obj), 
# the mapping for x-values that are changing will be the actual string and not a variable i.


for (i in 2:79 ) {
  #i <- 2
  variablename <- names(data.agreement.score.centers)[i]
  title <- attributes(data.agreement.score.centers)$variable.labels[i]
  #title <- variablename
  plot <- ggmap(googleterrainsyria)  + 
    geom_point(aes_string( x = names(data.agreement.score.centers)[80], y = names(data.agreement.score.centers)[81], colour= names(data.agreement.score.centers)[i] #, size = abs(as.numeric(names(data.agreement.score.centers)[i]))
                           ), data=data.agreement.score.centers , alpha = 0.85 ) +
    scale_colour_gradient2() +
    ggtitle(paste("Map: ", title , sep="")) +
    theme(plot.title=element_text(face="bold", size=8),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_blank(),axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA))
  #assign(paste("plot", variablename, sep=""), plot)
  ggsave(filename=paste("out/map_",i,variablename,".png",sep=""), plot=plot, width=8, height=6,units="in", dpi=300)
  rm(plot)
  
  rm(centersyriacham)
  centersyriacham <- ggmap(googleterraincham) +
    geom_point(aes_string( x = names(data.agreement.score.centers)[80], y = names(data.agreement.score.centers)[81], colour= names(data.agreement.score.centers)[i] #, size = abs(as.numeric(names(data.agreement.score.centers)[i]))
                           ), data=data.agreement.score.centers, alpha = 0.85)  +
    scale_colour_gradient2() +
    ggtitle("Community Centers in Damascus Area") +
    theme(plot.title=element_text(face="bold", size=8),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_blank(),axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA))
  
  rm(centersyrialat)
  centersyrialat <- ggmap(googleterrainlat) +
    geom_point(aes_string( x = names(data.agreement.score.centers)[80], y = names(data.agreement.score.centers)[81], colour= names(data.agreement.score.centers)[i] #, size = abs(as.numeric(names(data.agreement.score.centers)[i]))
                           ), data=data.agreement.score.centers, alpha = 0.85) +
    scale_colour_gradient2() +
    ggtitle("Community Centers near the coast ( Latakia, Tartous)") +
    theme(plot.title=element_text(face="bold", size=8),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_blank(),axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA))
  
  rm(centersyriahoms)
  centersyriahoms <- ggmap(googleterrainhoms) +
    geom_point(aes_string( x = names(data.agreement.score.centers)[80], y = names(data.agreement.score.centers)[81], colour= names(data.agreement.score.centers)[i] #, size = abs(as.numeric(names(data.agreement.score.centers)[i]))
                           ), data=data.agreement.score.centers, alpha = 0.85) +
    scale_colour_gradient2() +
    ggtitle("Community Centers near Homs") +
    theme(plot.title=element_text(face="bold", size=8),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_blank(),axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA))
  
  rm(centersyriahamas)
  centersyriahamas <- ggmap(googleterrainhamas) +
    geom_point(aes_string( x = names(data.agreement.score.centers)[80], y = names(data.agreement.score.centers)[81], colour= names(data.agreement.score.centers)[i] #, size = abs(as.numeric(names(data.agreement.score.centers)[i]))
                           ), data=data.agreement.score.centers, alpha = 0.85) +
    scale_colour_gradient2() +
    ggtitle("Community Centers near Hamas") +
    theme(plot.title=element_text(face="bold", size=8),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_blank(),axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA))
  
  rm(centersyriaqarah)
  centersyriaqarah <- ggmap(googleterrainqarah) +
    geom_point(aes_string( x = names(data.agreement.score.centers)[80], y = names(data.agreement.score.centers)[81], colour= names(data.agreement.score.centers)[i] #, size = abs(as.numeric(names(data.agreement.score.centers)[i]))
                           ), data=data.agreement.score.centers, alpha = 0.85,   color="#FFA600", fill="#FFA600") +
    scale_colour_gradient2() +
    ggtitle("Community Centers near qarah") +
    theme(plot.title=element_text(face="bold", size=8),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_blank(),axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA))
  
  rm(centersyriadaraa)
  centersyriadaraa <- ggmap(googleterraindaraa) +
    geom_point(aes_string( x = names(data.agreement.score.centers)[80], y = names(data.agreement.score.centers)[81], colour= names(data.agreement.score.centers)[i] #, size = abs(as.numeric(names(data.agreement.score.centers)[i]))
                           ), data=data.agreement.score.centers, alpha = 0.85) +
    scale_colour_gradient2() +
    ggtitle("Community Centers near Suwayda") +
    theme(plot.title=element_text(face="bold", size=8),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_blank(),axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA))
  
  rm(centersyriaaleppo)
  centersyriaaleppo <- ggmap(googleterrainaleppo) +
    geom_point(aes_string( x = names(data.agreement.score.centers)[80], y = names(data.agreement.score.centers)[81], colour= names(data.agreement.score.centers)[i] #, size = abs(as.numeric(names(data.agreement.score.centers)[i]))
                           ), data=data.agreement.score.centers, alpha = 0.85) +
    scale_colour_gradient2() +
    ggtitle("Community Centers near Aleppo") +
    theme(plot.title=element_text(face="bold", size=8),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_blank(),axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA))
  
  rm(centersyriahassake)
  centersyriahassake <- ggmap(googleterrainhassake) +
    geom_point(aes_string( x = names(data.agreement.score.centers)[80], y = names(data.agreement.score.centers)[81], colour= names(data.agreement.score.centers)[i] #, size = abs(as.numeric(names(data.agreement.score.centers)[i]))
                           ), data=data.agreement.score.centers, alpha = 0.85, color="#FFA600", fill="#FFA600") +
    scale_colour_gradient2() +
    ggtitle("Community Centers near Hassake") +
    theme(plot.title=element_text(face="bold", size=8),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_blank(),axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA))
  
  rm(allmap)
  allmap <- arrangeGrob(centersyriacham, centersyriaaleppo, centersyriahoms, centersyriahamas, centersyrialat, centersyriaqarah, centersyriadaraa, centersyriahassake, ncol=2)
  ggsave(filename=paste("out/map_",i,variablename,"_all.png",sep=""), allmap, width=8, height=16, units="in", dpi=300)
  ##
  rm(centersyriacham, centersyriaaleppo, centersyriahoms, centersyriahamas, centersyrialat, centersyriaqarah, centersyriadaraa, centersyriahassake)
  rm(variablename, title)
  }
