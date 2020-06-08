#' @export
MultiColors<-function(data=data, VAR="", GRUP="", GRUpalette.pack=NULL, GRUpalette.name=NULL){
  if(!is.factor(data[ , GRUP])){
    data[ , GRUP][is.na( data[ , GRUP])]<-"Non renseigné"
   GRUPS<-factor( data[ , GRUP])
  } else {  
    GRUPS<-data[ , GRUP] 
    if(NA%in%GRUPS){
      levels(GRUPS)<-c(levels(GRUPS), "Non renseigné")
    GRUPS[is.na(GRUPS)]<-"Non renseigné"
    }
  }
  if(!is.factor(data[ , VAR])){
    VARS<-factor( data[ , VAR])
  } else {  VARS<-data[ , VAR] }
  print(levels(GRUPS))
  
  length(levels(VARS))->npal.var
  length(levels(GRUPS))->npal.grup
  data.frame(table(GRUPS, VARS, exclude=NULL))->df
  subset(df, df$Freq>0)->df
  
  subset(RColorBrewer::brewer.pal.info, RColorBrewer::brewer.pal.info$category=="seq")->palettes.choix
  if(is.null(GRUpalette.name)){
    message("coucou1")
    palettes.choix[sample(x = 1:nrow(palettes.choix), size = npal.grup, replace = FALSE) , ]->chosepal
  } else {
    if(length(GRUpalette.name)==npal.grup&sum(GRUpalette.name%in%row.names(palettes.choix))==npal.grup){
      subset(palettes.choix, row.names(palettes.choix)%in%GRUpalette.name)->chosepal
      chosepal[GRUpalette.name , ]->chosepal
    } else {message("length(GRUpalette.name) incorrect ou GRUpalette.name inconnus")}
  }
  print(class(chosepal))
  row.names(chosepal)->chosepal.vec
  names(chosepal.vec)<-levels(GRUPS)
  print(df)
  
  lapply(levels(GRUPS), function(gr1){
#print(gr1)
    subset(df, df$GRUPS==gr1)->dfgr1
    #print(dfgr1)
    if(nrow(dfgr1)<3){
      
      dataviz::pickobrew(the.palette = chosepal.vec[names(chosepal.vec)==gr1], 
                         the.indexes =  c(5, 8)[1:nrow(dfgr1)]  
                         )->res
      res<-c(res)
      print(dfgr1$VARS)
      print(dfgr1)
      print(nrow(dfgr1))
      names(res)<-dfgr1$VARS
      } else {
    RColorBrewer::brewer.pal(n = nrow(dfgr1), name = chosepal.vec[names(chosepal.vec)==gr1])->res
        names(res)<-dfgr1$VARS
        
      }
    return(res)
  })->res
  #names(res)<-levels(GRUPS)
  res<-unlist(res)
  return(res)
  
}
