#' @export
resize.shpdf<-function(shp.df=shp.df.aura2, 
                       long.lim=c(minlong, maxlong),
                       lat.lim=c(minlat, maxlat)){
  shp.df[shp.df$long<long.lim[2]&shp.df$long>long.lim[1]&shp.df$lat<lat.lim[2]&shp.df$lat>lat.lim[1] , ]->replot.df
  return(replot.df)
}
