colors.of.factors<-function(VAR.factor=source.aggre$Var1 , col.palette="Spectral", col.manual=FALSE, col.vector ){
  if(col.manual==FALSE){
  #data.frame("levels"=levels(VAR.factor), 
  #           "colours"=brewer.pal(n = length(levels(VAR.factor)), name = col.palette))
  colvec=brewer.pal(n = length(levels(VAR.factor)), name = col.palette)
  }
  if(col.manual==TRUE){
    colvec<-col.vector
    
  }
  names(colvec)<-levels(VAR.factor)
  return(colvec)
}
#
#colvec[rev(order(colvec))]->colvec
