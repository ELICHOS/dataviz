#' barplot.reponse.multi permet de représenter conjointement deux variables qualitatives
#'
#' @param tab.eff =tab.profil.urb, table des effectifs, peut être NULL
#' @param tab.prop =prop.tab.profil.urb,table des proportions, peut être NULL
#' @param var1 si tab.eff et tab.prop sont NULL
#' @param var2 idem
#' @param margin.prop =1, marge sur laquelle calculer les proportions
#' @param mar =c(7, 10, 4, 2),par(mar)
#' @param xlim =c(0, 0.4),
#' @param BYX=0.1 interval minimal pour l'échelle des x
#' @param XLAB ="%"
#' @param colo =c(couleurs$rur.green, couleurs$urb.blue),
#' @param wrap.len.labels =" ", paramètre len pour wrapper les labels
#' @param legend.labels =c("Rural", "Urbain"),
#' @param x.legend =0.15,
#' @param y.legend =10,
#' @param axe.title.lab = c("Type:", "Effectifs n totaux:", "Part des répondants  concernés:"),
#' @param axe.title.at =c(-0.075, -0.05, 0.2),
#' @param lecture.lab =
#' @param add.test =TRUE, y a-t-il un test d'indépendance (fisher, Khi2...)
#' @param test =fish.test.profil.urb,  variable résultat de test
#' @param box.test.y.ajust =0, décalage entre la test box et la légende sur l'axe y
#' @param y.amplitude =4 , amplitude du déclage sur y
#' @param box.test.x.ajust =0.1, décalage entre la test box et la légende sur l'axe x
#' @param x.amplitude =0.11, amplitude du déclage sur y
#' @param test.spe ="Test de Fisher (petits effectifs)"
#' @return some value
#' @export
barplot.reponse.multi<-function(tab.eff=tab.profil.urb,
                                tab.prop=prop.tab.profil.urb,
                                var1, var2,
                                margin.prop=1,
                                mar=c(7, 10, 4, 2),
                                xlim=c(0, 0.4), BYX=0.1, XLAB="%",
                                colo=c(couleurs$rur.green, couleurs$urb.blue),
                                wrap.len.labels,
                                legend.labels=c("Rural", "Urbain"), x.legend=0.15, y.legend=10,
                                axe.title.lab= c("Type:", "Effectifs \n totaux:", "Part des répondants \n concernés:"),
                                axe.title.at=c(-0.075, -0.05, 0.2),
                                lecture.lab,
                                add.test=TRUE,
                                test=fish.test.profil.urb,
                                box.test.y.ajust=0, y.amplitude=4 ,
                                box.test.x.ajust=0.1, x.amplitude=0.11,
                                test.spe="Test de Fisher (petits effectifs)",
                                print.tab=FALSE){
  if(is.null(tab.eff)==FALSE){
    if(is.null(tab.prop)==TRUE){
      tab.prop<-prop.table(x = tab.eff, margin = margin.prop)
      effectifs.nb<-rowSums(tab.eff)
    }
  }
  if(is.null(tab.eff)==TRUE&is.null(tab.prop)==TRUE){
    tab.eff<-table(var1, var2)
    prop.table(tab.eff, margin = margin.prop)->tab.prop
    effectifs.nb<-colSums(tab.eff)
  }
  if(print.tab==TRUE){
  print(tab.prop)
  }
  #
  par(mar=mar)
  #
  barplot(tab.prop, beside = TRUE, horiz=TRUE,
          xlim=xlim, plot=FALSE)->data.barplot
  apply(data.barplot, MARGIN = 2, FUN = mean)->vecx
  atvec<-seq(0, round(max(tab.prop),1), by = BYX)
  lgt<-length(data.barplot)/2
  #
  barplot(tab.prop, beside = TRUE, horiz=TRUE,
          xaxt="n", yaxt="n", las=2, xlim=xlim,
          col=colo)
  #
  for( i in atvec){
    abline(v = i, lwd=1, lty=2, col=gray(1))
  }
  axis(side = 1, at = atvec, lwd = 1, lwd.ticks = 1,
       labels = atvec*100)
  mtext(text = XLAB, side = 1, line =3, at = mean(atvec))
  axis(side = 2, at=vecx, labels = wrap.labels(x = colnames(tab.prop),len = 22),
       line = 3, tick = FALSE,
       las=2, cex.axis=0.8)
  for(i in 1:length(vecx)){
    mtext(text = effectifs.nb[i], at = vecx[i], side = 2, line = 1, las=2)
  }
  for(j in 1:2){
    for(i in 1:lgt){
      text(y = data.barplot[seq(j, length(data.barplot), by=2)][i],
           x = tab.prop[j ,][i],
           labels=as.character(round(tab.prop[j ,][i]*100, 1)),
           adj=1,
           font=3
      )
    }
  }
  par(xpd=TRUE)
  axis(side = 3, at = axe.title.at, labels = axe.title.lab,
       tick = FALSE, line = -1, font=2)
  legend(x = x.legend, y = y.legend , fill = colo,
         legend = legend.labels , bty = "n", bg = "white")
  if(add.test==TRUE){
    library(plotrix)
    library(gtools)
    textbox(cex = 0.8, font = 3, x = c( (x.legend+box.test.x.ajust), (x.legend+box.test.x.ajust)+x.amplitude),
            y = c( (y.legend+box.test.y.ajust), (y.legend+box.test.y.ajust)-y.amplitude), justify = "l", box = TRUE, margin = c(-0.05, -0.005),
            textlist =paste(test.spe, ": ", "    p.valeur = ", labels = round(test$p.value, 4),
                            stars.pval(test$p.value)[1], sep="")

    )
    mtext(line = 5, side = 1, text = lecture.lab, cex = 0.8,
          font=3
    )
  }
}

