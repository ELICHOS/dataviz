#' squareseqlegend permet de construire une légende pour des variables séquentielles
#'
#' @param Qcut Vecteur des seuils
#' @param Couleur Vecteur des couleurs
#' @param orientation Orientation: 'h' ou 'v'
#' @param seuil.inf.x Positions x de la légende dans le plan: en \% du plan à gauche
#' @param seuil.sup.x et à droite
#' @param seuil.inf.y Idem pour y
#' @param seuil.sup.y
#' @param bbox.data Matrix des x et y max et min du plan
#' @param textlabels Vecteur des labels
#' @param text.side Position du texte: 'right'ou 'left' pour 'v' et 'bottom' ou 'top' pour 'h"
#' @param text.space Esace entre le texte et la légende
#' @param text.cex Cex du texte
#' @param text.double Deux vecteurs de texte? (un de chaque coté?)
#' @param text.haut SI text.double=TRUE
#' @param text.bas SI text.double=TRUE
#' @param text.gauche SI text.double=TRUE
#' @param text.droit SI text.double=TRUE
#' @param title.log un titre?
#' @param title.label titre
#' @param title.pos tittre alignement
#' @return some value
#' @export
squareseqlegend<-function(Qcut=Qrat,
                          Couleur=ColRat,
                          orientation="v",
                          seuil.inf.x=0.01,
                          seuil.sup.x=0.95,
                          seuil.inf.y=0.2,
                          seuil.sup.y=0.4,
                          bbox.data=france.dep@bbox,
                          textlabels=Qrat,
                          text.side="right",
                          text.space=0.02,
                          text.cex=0.8,
                          text.double=FALSE,
                          text.haut=c("0%", "25%", "50%", "75%", "100%"),
                          text.bas=Qrat,
                          text.gauche=c("0%", "25%", "50%", "75%", "100%"),
                          text.droit=Qrat,
                          title.log=TRUE,
                          title.label="Quartiles",
                          title.pos=4){
  #
  six<-bbox.data["x", "min"]+(seuil.inf.x*(bbox.data["x", "max"]-bbox.data["x", "min"]))
  ssx<-bbox.data["x", "max"]-(seuil.sup.x*(bbox.data["x", "max"]-bbox.data["x", "min"]))
  byx<-(ssx-six)/(length(Qcut)-1)
  #
  siy<-bbox.data["y", "min"]+(seuil.inf.y*(bbox.data["y", "max"]-bbox.data["y", "min"]))
  ssy<-bbox.data["y", "max"]-(seuil.sup.y*(bbox.data["y", "max"]-bbox.data["y", "min"]))
  byy<-(ssy-siy)/(length(Qcut)-1)
  #
  if(orientation=="v"){
    if(text.double==FALSE){
      rect(
        ybottom = seq(
          from=siy,
          to=ssy-byy,
          by=byy),
        ytop= seq(
          from=siy+byy,
          to=ssy,
          by=byy),
        xleft=six,
        xright=ssx,
        col=Couleur
      )
      if(text.side=="right"){
        text(x = ssx+(text.space*(bbox.data["x", "max"]-bbox.data["x", "min"])), y = seq(from=siy, to = ssy, by = byy), labels = textlabels, cex = text.cex)
      } else { if(text.side=="left"){
        text(x = six-(text.space*(bbox.data["x", "max"]-bbox.data["x", "min"])), y = seq(from=siy, to = ssy, by = byy), labels = textlabels, cex = text.cex)
      }
      }
      text(x = six, y = ssy+(text.space*(bbox.data["y", "max"]-bbox.data["y", "min"])), labels = title.label, font = 2, pos=title.pos)
    } else {if(text.double==TRUE){
      rect(
        ybottom = seq(
          from=siy,
          to=ssy-byy,
          by=byy),
        ytop= seq(
          from=siy+byy,
          to=ssy,
          by=byy),
        xleft=six,
        xright=ssx,
        col=Couleur
      )
      text(x = ssx+(text.space*(bbox.data["x", "max"]-bbox.data["x", "min"])), y = seq(from=siy, to = ssy, by = byy), labels = text.droit, cex = text.cex)
      text(x = six-(text.space*(bbox.data["x", "max"]-bbox.data["x", "min"])), y = seq(from=siy, to = ssy, by = byy), labels = text.gauche, cex = text.cex)
    }
    }
    text(x = six, y = ssy+(text.space*(bbox.data["y", "max"]-bbox.data["y", "min"])), labels = title.label, font = 2, pos=title.pos)
  } else {if(orientation=="h") {
    if(text.double==FALSE){
      rect(
        xleft = seq(
          from=six,
          to=ssx-byx,
          by=byx),
        xright= seq(
          from=six+byx,
          to=ssx,
          by=byx),
        ybottom=siy,
        ytop=ssy,
        col=Couleur
      )
      if(text.side=="bottom"){
        text(y = siy-(text.space*(bbox.data["y", "max"]-bbox.data["y", "min"])), x = seq(from=six, to = ssx, by = byx), labels = textlabels, cex = text.cex)
        text(x = six, y = ssy+(text.space*(bbox.data["y", "max"]-bbox.data["y", "min"])), labels = title.label, font = 2, pos=title.pos)

      } else {if(text.side=="top"){
        text(y = ssy+(text.space*(bbox.data["y", "max"]-bbox.data["y", "min"])), x = seq(from=six, to = ssx, by = byx), labels = textlabels, cex = text.cex)
        text(x = six, y = ssy+2.5*(text.space*(bbox.data["y", "max"]-bbox.data["y", "min"])), labels = title.label, font = 2, pos=title.pos)

      }
      }
    } else {
      if(text.double==TRUE){
        rect(
          xleft = seq(
            from=six,
            to=ssx-byx,
            by=byx),
          xright= seq(
            from=six+byx,
            to=ssx,
            by=byx),
          ybottom=siy,
          ytop=ssy,
          col=Couleur
        )
        text(y = siy-(text.space*(bbox.data["y", "max"]-bbox.data["y", "min"])), x = seq(from=six, to = ssx, by = byx), labels = text.bas, cex = text.cex)
        text(y = ssy+(text.space*(bbox.data["y", "max"]-bbox.data["y", "min"])), x = seq(from=six, to = ssx, by = byx), labels = text.haut, cex = text.cex)
        text(x = six, y = ssy+2.5*(text.space*(bbox.data["y", "max"]-bbox.data["y", "min"])), labels = title.label, font = 2, pos=title.pos)
      }
    }
  }
  }
}
