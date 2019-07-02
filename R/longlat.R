#' @export
longlat<-function(adjust.ou.pos="adjust",
                  prop=0.8,
                  object=shp.df.aura2,
                  lim=NULL, #c(minlong, maxlong),
                  dim="long"){
  if(class(object)=="data.frame"){
    if(is.null(lim)){
      min(object[, dim])->mino
      max(object[, dim])->maxo
    } else {
      lim[1]->mino
      lim[2]->maxo}
  }
if(class(object)=="SpatialPolygonsDataFrame"|class(object)=="SpatialPolygons"){
  object@bbox->df
  if(dim=="long"){
    mino<-df["x", "min"]
    maxo<-df["x", "max"]
  } else {if(dim=="lat"){
    mino<-df["y", "min"]
    maxo<-df["y", "max"]
  }
  }
}
  if(adjust.ou.pos=="pos"){
  res<-mino+(prop*(maxo-mino))
  }
  if(adjust.ou.pos=="adjust"){
    res<-prop*(maxo-mino)
  }
  return(res)
}
