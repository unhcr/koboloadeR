## get online map background


googleterrainsyria <- get_map(location = c(lon =39.0560, lat = 34.8148), color = "bw", source = "google",maptype = "terrain", zoom = 7)
googleterraincham <- get_map(location = c(lon =36.33, lat =33.56 ), color = "bw", source = "google",maptype = "terrain", zoom = 10)
googleterrainlat <- get_map(location = c(lon =36.1, lat = 35.22), color = "bw", source = "google",maptype = "terrain", zoom = 10)
googleterrainhoms <- get_map(location = c(lon =36.71158, lat = 34.73767), color = "bw", source = "google",maptype = "terrain", zoom = 12)
googleterrainhamas <- get_map(location = c(lon =36.7450, lat = 35.1409), color = "bw", source = "google",maptype = "terrain", zoom = 12)
googleterrainqarah <- get_map(location = c(lon =36.7367, lat = 34.1711), color = "bw", source = "google",maptype = "terrain", zoom = 11)
googleterraindaraa <- get_map(location = c(lon =36.4, lat = 32.7721), color = "bw", source = "google",maptype = "terrain", zoom = 10)
googleterrainaleppo <- get_map(location = c(lon =37.1302, lat = 36.215), color = "bw", source = "google",maptype = "terrain", zoom = 13)
googleterrainhassake <- get_map(location = c(lon =40.7023, lat = 36.5971), color = "bw", source = "google",maptype = "terrain", zoom = 9)

#####
lay_out = function(...) {    
  x <- list(...)
  n <- max(sapply(x, function(x) max(x[[2]])))
  p <- max(sapply(x, function(x) max(x[[3]])))
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(n, p)))    
  
  for (i in seq_len(length(x))) {
    print(x[[i]][[1]], vp = grid::viewport(layout.pos.row = x[[i]][[2]], 
                                           layout.pos.col = x[[i]][[3]]))
  }
} 



## Map that displays categorise numeric value for Z-score
source('code/3-Map-size.R', echo=TRUE)
