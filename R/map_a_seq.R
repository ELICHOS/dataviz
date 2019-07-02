#' map.a.seq permet de tracer une carte avec des coloris dépendants d'une variable numérique continue.
#'
#' @param sp.object A spatialpolygons or spatialpolygonsdataframe object
#' @param var Variable à maper du type sp.object@@data$var. Pas une chaine de caractere.
#' @param alternate.df data.frame complémentaire si class(sp.object)=="SpatialPolygons"
#' @param alternate.var Variable à maper du type alternate.df$alternate.var Pas une chaine de caractere.
#' @param match.id.df Variable dans le alternate.df qui match les id de sp.object. Chaine de caracteres.
#' @param match.id.sp Variable dans le sp.object qui match les id de alternate.df. Chaine de caracteres.
#' @param compara "sup" = '>' ; "sup.egal" = '>=' ; not.impleted yet: 'inf' = '<' ; 'inf.egal' = '<='
#' @param qcut vecteur numérique des seuils
#' @param qcol vecteur des couleurs. De meme longueur que qcut-1
#' @param borders Couleur des frontières. Vecteur, valeur unique ou NULL.
#' @param title Titre de la carte (par défaut: NULL)
#' @param subtitle Sous-titre de la  carte (par défaut: NULL)
#' @param all.poly FALSE. Fond de carte: faut-il tracer tous les polygones de l'objet sp.object?
#' @param all.poly.borders Frotières du fond de carte
#' @return some value
#' @export
map.a.seq<-function(sp.object=france.base, var="POPULATION", alternate.df= NULL, alternate.var= NULL,
                    match.id= NULL,match.id.sp=NULL,
                    compara="sup",
                    qcut=qrank, qcol=col, borders=NULL,
                    title=NULL, subtitle=NULL,
                    all.poly=TRUE,
                    all.poly.borders=NA){
  library(rgeos)
  ####
  if(length(qcol)!=(length(qcut)-1)){message("Problème avec length(qcol)")}
  #
  if(length(borders)==1){
    bordo<-rep(borders[1], times=length(qcut))
  } else {bordo<-borders}
  ####
  if(all.poly==TRUE){
    plot(sp.object, main=title, sub=subtitle, border=all.poly.borders)
  }
  if(all.poly==FALSE){
    bbbox<-sp.object@bbox
    if(mapasp(sp.object)=="iso"){par(pty="s")}
    plot(0, xlim=c(bbbox["x", "min"], bbbox["x", "max"]), ylim=c(bbbox["y", "min"], bbbox["y", "max"]), xaxt="n", yaxt="n", bty="n", ylab="", xlab="")
  }
  ####
  if(compara=="sup"){
    #
    #if(class(sp.object)=="SpatialPolygonsDataFrame"){
    if(is.null(alternate.df)){
      for(i in 1:length(qcut)){
        if(i==1){
          plot(subset(sp.object, sp.object@data[, var]>qcut[i]&sp.object@data[, var]<qcut[i+1]),add=TRUE,
               col=qcol[i], border=bordo[i])
        }
        if(i < length(qcut)&i!=1){
          plot(subset(sp.object, sp.object@data[, var]>qcut[i]&sp.object@data[, var]<qcut[i+1]), add=TRUE,
               col=qcol[i], border=bordo[i])
        }
        if(i == length(qcut)){
          plot(subset(sp.object, sp.object@data[, var]>qcut[i]), add=TRUE,
               col=qcol[i], border=bordo[i])
        }
      }
    }
    #
    #if(class(sp.object)=="SpatialPolygons"){
    if(!is.null(alternate.df)){
      for(i in 1:length(qcut)){
        if(i==1){
          plot(subset(sp.object, sp.object$match.id.sp%in%alternate.df[alternate.df$alternate.var>qcut[i] , match.id.df]&sp.object$match.id.sp%in%alternate.df[alternate.df$alternate.var<qcut[i+1] , match.id.df]),col=qcol[i], border=bordo[i], add=TRUE)
        }
        if(i < length(qcut)&i!=1){
          plot(subset(sp.object, sp.object$match.id.sp%in%alternate.df[alternate.df$alternate.var>qcut[i] , match.id.df]&sp.object$match.id.sp%in%alternate.df[alternate.df$alternate.var<qcut[i+1] , match.id.df]), add=TRUE,
               col=qcol[i], border=bordo[i])
        }
        if(i == length(qcut)){
          plot(subset(sp.object, sp.object$match.id.sp%in%alternate.df[alternate.df$alternate.var>qcut[i] , match.id.df]), add=TRUE,
               col=qcol[i], border=bordo[i])
        }
      }
    }
  }
  ####
  if(compara=="sup.egal"){
    #
    if(is.null(alternate.df)){
      for(i in 1:length(qcut)){
        if(i==1){
          plot(subset(sp.object, sp.object@data[, var]>=qcut[i]&sp.object@data[, var]<qcut[i+1]),add=TRUE,
               col=qcol[i], border=bordo[i])
        }
        if(i < length(qcut)&i!=1){
          plot(subset(sp.object, sp.object@data[, var]>=qcut[i]&sp.object@data[, var]<qcut[i+1]), add=TRUE,
               col=qcol[i], border=bordo[i])
        }
        if(i == length(qcut)){
          plot(subset(sp.object, sp.object@data[, var]>=qcut[i]), add=TRUE,
               col=qcol[i], border=bordo[i])
        }
      }
    }
    #
    if(!is.null(alternate.df)){
      for(i in 1:length(qcut)){
        if(i==1){
          plot(subset(sp.object, sp.object$match.id.sp%in%alternate.df[alternate.df$alternate.var>=qcut[i] , match.id.df]&sp.object$match.id.sp%in%alternate.df[alternate.df$alternate.var<qcut[i+1] , match.id.df]), add=TRUE, col=qcol[i], border=bordo[i])
        }
        if(i < length(qcut)&i!=1){
          plot(subset(sp.object, sp.object$match.id.sp%in%alternate.df[alternate.df$alternate.var>qcut[i] , match.id.df]&sp.object$match.id.sp%in%alternate.df[alternate.df$alternate.var<qcut[i+1] , match.id.df]), add=TRUE,
               col=qcol[i], border=bordo[i])
        }
        if(i == length(qcut)){
          plot(subset(sp.object, sp.object$match.id.sp%in%alternate.df[alternate.df$alternate.var>qcut[i] , match.id.df]), add=TRUE,
               col=qcol[i], border=bordo[i])
        }
      }
    }
  }
}
