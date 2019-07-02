#' @export
funkyquest<-function(varlab=c("Q1NB"), dfs=DFS){
  laplo<-lapply(seq_along(dfs), FUN = function(i){
    dfs[[i]]->pani
    pani[ , grepl(pattern = varlab,  gsub(".", "", x=names(pani), fixed = TRUE), ignore.case = TRUE)==TRUE]->pano
    data.frame("times"=i, "value"=pano)->pano
    pano
  })
  names(laplo)<-paste(varlab, "e", 1:3, sep="")
  do.call("rbind", laplo)->lapla
  return(lapla)
}
