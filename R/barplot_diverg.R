#' barplot.diverg permet de repr?senter une variable factor issue d'une enqu?te de mani?re divergente
#' @title barplot.diverg
#' @author Elie
#' @param data data.v1.2 ; le data.frame
#' @param var1 data.v1.2$La.situation.d.emploi ; le data.frame ind?x?
#' @param var2 data.v1.2$dep_T ; le data.frame ind?x?. var2 repr?sente les strates, les cat?gories.
#' @param interactive.vars TRUE ; faut-il une aide pour indexer les variables?
#' @param marginvar 2 ; la marge sur laquelle sont calcul?s les pourcentages
#' @param vardroite 3 ; l'indexe (num?rique) de la (ou des) variables ? droite
#' @param vargauche 1 ; l'indexe (num?rique) de la (ou des) variables ? gauche
#' @param coldroite couleurs$bg1 ; couleurs ? droite (vecteur de character)
#' @param colgauche couleurs$rp1 ; couleurs ? gauche (vecteur de character)
#' @param varplus c(2) ; une variable en plus? Dans le cas d'une r?ponse interm?diaire (ni droite ni gauche)
#' @param colplus couleurs$grayStable ; couleur pour la ou les variable en plus
#' @param varna 4 ; l'indexe (num?rique) de la (ou des) variables d?signant les non r?ponses, NA...
#' @param colna couleurs$grayNA ; couleur pour la ou les variable NA
#' @param naou 1 ; NA sur le graph 1 ou 2 (le cas ?ch?ant)
#' @param add.na.axis TRUE ; faut-il un axe sp?cifique aux NA
#' @param BYNA 10 ; interval minimal pour l'axe des NA
#' @param colo.vec colo ; DEPRECATED
#' @param XLIM c(-100,20) ; XLIM pour le graph 1. Pensez ? une borne inf?rieure suffisante pour les NA
#' @param BYX 20 ; interval minmal pour l'axe X principal (graph 1 et 2)
#' @param YLABS colnames(dat) ; nom des cat?gories (var2)
#' @param par.left.1 8 ; mar ? gauche pour le graph 1 (laisser de la place pour YLABS)
#' @param par.left.2 2 , mar ? gauche pour le graph 2
#' @param Titre "La situation de l'emploi..."
#' @param Comment "Enqu?te r?alis?e en septembre 2017 par la MRIE Rh?ne-Alpes"
#' @param line.titre -1
#' @param line.comment -2
#' @param XLAB "%"
#' @return la fonction ne retourne rien du tout.
#' @export
barplot.diverg<-function(data=data.v1.2 , var1=data.v1.2$La.situation.d.emploi , var2=data.v1.2$dep_T ,
                         interactive.vars=TRUE,
                         marginvar=2 ,
                         vardroite=3, vargauche=1, coldroite=couleurs$bg1, colgauche=couleurs$rp1,
                         varplus=c(2), colplus=couleurs$grayStable,
                         varna=4, colna=couleurs$grayNA, naou=1, add.na.axis=TRUE, BYNA=10,
                         colo.vec=colo,
                         XLIM=c(-100,20), BYX=20, YLABS="Ensemble", #For multi: YLABS=colnames(dat)
                         par.left.1=8, par.left.2=2,
                         Titre="La situation de l'emploi...",
                         Comment="Enquète réalisée en septembre 2017 par la MRIE Rhône-Alpes",
                         line.titre=-1, line.comment=-2,
                         XLAB="%", bornmin=100, barswidth=5, YLIM=c(1, 5)){
  if(is.null(var2)==TRUE){
    dat<-prop.table(table(var1))
    dat<-round(dat*100, 2)
  }else{
  if(marginvar==2){
    dat<-cbind(prop.table(table(var1, var2), margin = marginvar),
               "Ensemble"=(prop.table(table(var1)))
    )
  } else if(marginvar==1){
    dat<-cbind(prop.table(table(var1, var2), margin = marginvar),
               "Ensemble"=(prop.table(table(var2)))
    )
  } else message("Erreur marginvar")
  dat<-round(dat*100, 2)
  }
  #
  if(interactive.vars==TRUE){
    print(dat)
    vardroite <- readline("Indicateur num?rique de la (ou des) variable(s) ? droite:")
    vargauche <- readline("Indicateur num?rique de la (ou des) variable(s) ? gauche:")
    varna <- readline("Indicateur num?rique de la (ou des) variable(s) NA ou NSP:")
    varplus <- readline("Indicateur num?rique de la (ou des) variable(s) ? repr?senter ? part:")

    vardroite <- as.numeric(unlist(strsplit(vardroite, ",")))
    vargauche <- as.numeric(unlist(strsplit(vargauche, ",")))
    varna <- as.numeric(unlist(strsplit(varna, ",")))
    varplus <- as.numeric(unlist(strsplit(varplus, ","))) }
  if(interactive.vars==FALSE){
    vardroite <- vardroite
    vargauche <- vargauche
    varna <- varna
    varplus <- varplus
  }
  #
  if(is.null(var2)==TRUE){
    rownames(dat[c(varna, vargauche, vardroite, varplus)])->leglev
    1->lencolna

  }else{
  rownames(dat[c(varna, vargauche, vardroite, varplus) ,])->leglev
    length((colnames(dat)))->lencolna
  }
  c(colna, colgauche, coldroite, colplus)->legcol
  if(length(leglev)!=length(legcol)){message="length(leglev)!=length(legcol)"}
  #
  if(is.null(var2)!=TRUE){
  if(is.numeric(varplus)==TRUE){
    if(naou==1){
      layout(mat = matrix(data = c(1,1, 2, 3, 4, 4), nrow = 3, ncol = 2, byrow = TRUE),
             widths = c(1, mean(dat[varplus ,])/100 ),
             heights = c(0.2, 4, 1))
      #
      par(mar=c(0, 0, 0, 0))
      plot.new()
      mtext(text = Titre, side = 1, line = line.titre, cex = 1.5, font = 2)

      #
      par(mar=c(2, par.left.1, 1, 2)+0.1)
      barplot(-rep(100,(length(colnames(dat)))),names.arg=YLABS,cex.names=1.1,horiz=T, las=2,
              border=par("bg"),xlim=XLIM,col=colna,axes=F)
      barplot(-(100-dat[varna,]),names.arg=NULL, horiz=T, yaxt="n",
              border=par("bg"),xlim=XLIM,col=par("bg"),axes=F,
              add=T)
      barplot(-dat[vargauche,],names.arg=NULL,horiz=T, yaxt="n",
              border=NA,xlim=XLIM,col=colgauche,axes=F,add=T)
      barplot(dat[vardroite,],names.arg=NULL,horiz=T, yaxt="n",
              border=NA,xlim=XLIM,col=coldroite,axes=F,add=T, xlab=XLAB)
      abline(v = 0, lwd=1.5)
      atg<-seq(0, -round(max(dat[vargauche ,])/100, 1)*100, by = -BYX)
      atd<-seq(0+BYX, max(XLIM), by = BYX)
      atgd<-c(rev(atg), atd)
      axis(side = 1, at = atgd, lwd = 1, lwd.ticks = 1,
           labels = abs(atgd))
      mtext(text = XLAB, side = 1, line =3, at = mean(atgd))
      for( i in atgd){
        abline(v = i, lwd=1, lty=2, col=gray(0.9))
      }
      if(add.na.axis==TRUE){
        labna<-seq(0, max(dat[varna ,]), by=BYNA)
        atna<-seq(-100, max(-(100-dat[varna,])), by=BYNA)
        axis(side=1, at=atna, lwd=1, lwd.ticks = 1, labels=labna)
      }
      for( i in atna){
        abline(v = i, lwd=1, lty=2, col=gray(0.9))
      }
      #
      par(mar=c(2, par.left.2, 1, 2)+0.1)
      barplot(dat[varplus ,],horiz=T, yaxt="n",xaxt="n",
              border=par("bg"),col=colplus,axes=F)
      atplus<-seq(0, max(dat[varplus ,]), BYX)
      axis(side=1, at = atplus, labels = atplus)
      for( i in atplus){
        abline(v = i, lwd=1, lty=2, col=gray(0.9))
      }
      #
      plot.new()
      par(mar=c(0, 0, 1, 0)+0.1)
      legend('center', legend = leglev, bty = "n",border = legcol,text.font = 3,
             fill =legcol, cex = 1.5, ncol=length(leglev))
      mtext(text = Comment, side = 1, line = line.comment, font = 3, cex=1)
    }
  }
  } else {if(is.null(var2)==TRUE){
    if(is.numeric(varplus)==TRUE){
      if(naou==1){
        #windows(height = 2, width = 4)
        #message("Hello1")
        layout(mat = matrix(data = c(1,1, 2, 3, 4, 4), nrow = 3, ncol = 2, byrow = TRUE),
               widths = c(1, mean(dat[varplus])/100 ),
               heights = c(0.2, 4, 1))
        #
        par(mar=c(0, 0, 2, 0))
        plot.new()
        mtext(text = Titre, side = 1, line = line.titre, cex = 1.5, font = 2)

        #
        par(mar=c(1, par.left.1, 10, 2)+0.1)
        #message("Hello2")

        barplot(-rep(bornmin,(lencolna)),names.arg=YLABS,cex.names=1.1,horiz=T, las=2,
                border=par("bg"),xlim=XLIM,col=colna,axes=F, width = barswidth, ylim=YLIM)
        #message("Hello3")

        barplot(-(bornmin-dat[varna]),names.arg=NULL, horiz=T, yaxt="n",
                border=par("bg"),xlim=XLIM,col=par("bg"),axes=F,
                add=T, width = barswidth)
        barplot(-dat[vargauche],names.arg=NULL,horiz=T, yaxt="n",
                border=NA,xlim=XLIM,col=colgauche,axes=F,add=T, width = barswidth)
        barplot(dat[vardroite],names.arg=NULL,horiz=T, yaxt="n",
                border=NA,xlim=XLIM,col=coldroite,axes=F,add=T, xlab=XLAB, width = barswidth)
        abline(v = 0, lwd=1.5)
        atg<-seq(0, -round(max(dat[vargauche])/100, 1)*100, by = -BYX)
        atd<-seq(0+BYX, max(XLIM), by = BYX)
        atgd<-c(rev(atg), atd)
        axis(side = 1, at = atgd, lwd = 1, lwd.ticks = 1,
             labels = abs(atgd))
        mtext(text = XLAB, side = 1, line =3, at = mean(atgd))
        for( i in atgd){
          abline(v = i, lwd=1, lty=2, col=gray(0.9))
        }
        if(add.na.axis==TRUE){
          labna<-seq(0, max(dat[varna]), by=BYNA)
          atna<-seq(-100, max(-(100-dat[varna])), by=BYNA)
          axis(side=1, at=atna, lwd=1, lwd.ticks = 1, labels=labna)
        }
        for( i in atna){
          abline(v = i, lwd=1, lty=2, col=gray(0.9))
        }
        #
        par(mar=c(1, par.left.2, 10, 2)+0.1)
        barplot(dat[varplus],horiz=T, yaxt="n",xaxt="n",
                border=par("bg"),col=colplus,axes=F, width = barswidth, ylim=YLIM)
        atplus<-seq(0, max(dat[varplus]), BYX)
        axis(side=1, at = atplus, labels = atplus)
        for( i in atplus){
          abline(v = i, lwd=1, lty=2, col=gray(0.9))
        }
        #
        plot.new()
        par(mar=c(0, 0, 0, 0)+0.1)
        legend('center', legend = leglev, bty = "n",border = legcol,text.font = 3,
               fill =legcol, cex = 1.5, ncol=length(leglev))
        mtext(text = Comment, side = 1, line = line.comment, font = 3, cex=1)
      }
    }
  }
    }
}
