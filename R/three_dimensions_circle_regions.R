#' @export
three_dimension_circle_regions <- function(n.sides=1000){
  # three circles
  parameters <- list(c(2,4,4),c(0,0,4),c(4,0,4))
  
  circles <- lapply(parameters, function(x){
    do.call(circle,as.list(c(x,n.sides)))
  })
  
  polygons <- lapply(circles, function(x)st_polygon(list(as.matrix(x))))
  
  # regions
  A <- st_multi_difference(l=polygons)
  B <- st_multi_difference(l=polygons[c(2,1,3)])
  C <- st_multi_difference(l=polygons[c(3,1,2)])
  AB <- st_difference(st_intersection(polygons[[1]],polygons[[2]]),polygons[[3]])
  AC <- st_difference(st_intersection(polygons[[1]],polygons[[3]]),polygons[[2]])
  BC <- st_difference(st_intersection(polygons[[3]],polygons[[2]]),polygons[[1]])
  ABC <- st_multi_intersection(l=polygons)
  
  polygon_list <- list(A=A,B=B,C=C,AB=AB,AC=AC,BC=BC,ABC=ABC)
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