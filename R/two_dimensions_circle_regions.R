#' @export
two_dimension_circle_regions <- function(n.sides=1000){
  # two circles
  library(sf)
  parameters <- list(c(0,0,4),c(4,0,4))
  
  circles <- lapply(parameters, function(x){
    do.call(circle,as.list(c(x,n.sides)))
  })
  
  polygons <- lapply(circles, function(x)st_polygon(list(as.matrix(x))))
  
  # regions
  A <- st_difference(polygons[[1]],polygons[[2]])
  B <- st_difference(polygons[[2]],polygons[[1]])
  AB <- st_intersection(polygons[[1]],polygons[[2]])
  
  polygon_list <- list(A=A,B=B,AB=AB)
  polygon_name <- names(polygon_list)
  polygon_dfs <- lapply(1:length(polygon_list), function(i){
    df <- unlist(polygon_list[[i]]) %>% matrix(ncol = 2) %>% data.frame()
    colnames(df) <- c("x","y")
    df$group <- polygon_name[[i]]
    return(df)
  })
  data_ploygons <- do.call(rbind,polygon_dfs)
  
  # centers
  data_centers <- lapply(polygon_list, st_centroid) %>% unlist %>% matrix(byrow = T,ncol=2) %>% data.frame()
  data_centers$group <- polygon_name
  colnames(data_centers) <- c("x","y","group")
  
  list(data_ploygons, data_centers)
}