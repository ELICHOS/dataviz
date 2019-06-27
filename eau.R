readOGR("C:/Users/elie/Downloads/Opendata Métropole/resultat/epo_eau_potable.epobornefont.shx")->eau.pot
plot(eau.pot)
proj4string(eau.pot) = CRS("+proj=longlat +datum=WGS84")

eau.pot@data
fortify(eau.pot)
eau.pot@coords
eau.pot@coords->mat
lapply(1:nrow(mat), function(i){
  as.matrix(rbind(
    matrix(data = as.numeric(unlist(mat[i , c(1, 1, 2, 2)])), ncol = 2)->a,
    a[ncol(a):1 , ]
  ))->mat.i
  #mat.i<-mat[i, c(1, 1, 2, 2)]
  p = Polygon(mat.i)
  ps = Polygons(list(p),1)
  sps = SpatialPolygons(list(ps)) 
  proj4string(sps) = proj4string(eau.pot)
  spTransform(sps, CRS("+proj=longlat +datum=WGS84"))->sps
  fortify(sps)
})->eau.list
lapply(1:length(eau.list), function(i){
  eau.list[[i]]->eau.list.i
  eau.list.i$id<-i
  eau.list.i$group<-paste(i, 1, sep=".")
  data.frame(eau.list.i)
})->eau.list2
do.call("rbind", eau.list2)->eau.df
####
sapply(1:nrow(eau.df), function(i){
  eau.df[i , ]->eau.df.i
  #lapply(1:nrow(df.corners), function(j){
  subset(df.corners, eau.df.i$lat>df.corners[, "X3"]&eau.df.i$lat<df.corners[ , "X4"]&eau.df.i$long>df.corners[ , "X1"]&eau.df.i$long<df.corners[ , "X2"])$LABEL2
  
  #{df.corners[ ,"LABEL2"]} else {}
  #})->lap
  #lap[!is.null(lap)]->lap
  #lap#data.frame(matrix(unique(lap)))
})->eau.df$SQUARE.LABEL2
####
eau.pot@proj4string
