pickobrew<-function(the.palette="PuBuGn", the.indexes=c(3, 6, 9)){
  library(RColorBrewer)
  brewer.pal.info->databrew
  databrew[row.names(databrew)==the.palette , "maxcolors"]->maxo
  vec<-sapply(the.indexes, function(i){ brewer.pal(n = maxo, name = the.palette)[i] })
  if(is.list(vec)){vec<-unlist(vec)}
  res<-vec
  return(res)
}