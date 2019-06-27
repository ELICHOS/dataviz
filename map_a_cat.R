#' map.a.cat permet de tracer une carte avec des coloris dépendants d'une variable qualitative (factor).
#'
#' @param sp.object A spatialpolygons or spatialpolygonsdataframe object
#' @param var Variable à maper du type sp.object@@data$var. Pas une chaine de caractere.
#' @param alternate.df data.frame complémentaire si class(sp.object)=="SpatialPolygons"
#' @param alternate.var Variable à maper du type alternate.df$alternate.var Pas une chaine de caractere.
#' @param match.id.df Variable dans le alternate.df qui match les id de sp.object. Chaine de caracteres.
#' @param match.id.sp Variable dans le sp.object qui match les id de alternate.df. Chaine de caracteres.
#' @param qcol vecteur des couleurs. De meme longueur que qcut-1
#' @param borders Couleur des frontières. Vecteur, valeur unique ou NULL.
#' @param title Titre de la carte (par défaut: NULL)
#' @param subtitle Sous-titre de la  carte (par défaut: NULL)
#' @param all.poly FALSE. Fond de carte: faut-il tracer tous les polygones de l'objet sp.object?
#' @param all.poly.borders Frotières du fond de carte
#' @return some value
#' @export
map.a.cat<-function(sp.object=france84, var="datafactor", alternate.df= NULL, alternate.var= NULL,
                    match.id= NULL,match.id.sp=NULL,
                    qcol=col, borders=NULL,
                    title=NULL, subtitle=NULL,
                    all.poly=TRUE,
                    all.poly.borders=NA){
  library(rgeos)
  ####
  length(levels(sp.object@data[, var]))->taille.lev
  lev<-levels(sp.object@data[, var])
  if(length(qcol)!=taille.lev){message("Problème avec length(qcol)")}
  #
  if(length(borders)==1){
    bordo<-rep(borders[1], times=taille.lev)
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
  if(is.null(alternate.df)){
    for(i in 1:taille.lev){
      plot(subset(sp.object, sp.object@data[, var]==lev[i]),add=TRUE,
           col=qcol[i], border=bordo[i])
    }
  }
  if(!is.null(alternate.df)){
    for(i in 1:taille.lev){
      plot(subset(sp.object, sp.object$match.id.sp%in%alternate.df[alternate.df$alternate.var==lev[i] , match.id.df]),col=qcol[i], border=bordo[i], add=TRUE)
    }
  }
}

