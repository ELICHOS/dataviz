prop.en.ligne<-function(DATA=Dep.NiVi, REF="01", LIGNE=4,
                        NAMES=names(Dep.NiVi)[grepl("Décile", names(Dep.NiVi))|grepl("Ensemble", names(Dep.NiVi))],
                        cols=c(col1, col2), YLIM.VALEUR=c(250, 1000), YLIM.PROP=c(0, 100),
                        WRAP.TITLE=60, PLOT=TRUE){
  YLIM.VALEUR<-YLIM.VALEUR/12
  if(REF=="TOTAL"){
    res<-DATA[LIGNE, NAMES]/DATA[1, NAMES]
  } else {
    subset(DATA, DATA$Rang2==nchar(as.character(REF))&grepl(pattern = REF, x = DATA$Rang)==TRUE)->REF.dat
    message("OK")
    res<-DATA[LIGNE, NAMES]/REF.dat[, NAMES]
  }
  res<-round(res*100, 1)
  res$Libelle<-DATA$Libelle[LIGNE]
  res$Rang<-Dep.NiVi$Rang[LIGNE]
  res$Rang.de.ref<-REF
  res$Type<-"Prop"

  ####res2
  res2<-DATA[LIGNE, NAMES]
  res2$Libelle<-DATA$Libelle[LIGNE]
  res2$Rang<-Dep.NiVi$Rang[LIGNE]
  res2$Rang.de.ref<-REF
  res2$Type<-"Valeur"
  ####
  res0<-rbind(res, res2)
  reshape(res0, direction="long", varying = list(NAMES), times = as.numeric(c(c("0"), 1:10)))->res0
  ####
  if(PLOT==TRUE){
  titles<-wrap.it(DATA$Libelle[LIGNE], WRAP.TITLE)
  ####
  p.PROP<-ggplot(data=subset(res0,  res0$Type=="Prop"&res0$time!=0), aes(x=time, y=Ensemble))+
    geom_hline(yintercept=subset(res0,
                                 res0$Type=="Prop"&res0$time==0)$Ensemble,
               colour=cols[2], size=2, alpha=0.5)+
    geom_point(mapping = aes(x=time, y=Ensemble), size=5, colour=cols[1])+
    geom_segment(aes(x=time,
                     xend=time,
                     y=YLIM.PROP[1],
                     yend=Ensemble), colour=cols[1])+
    #geom_point(data = subset(pain.et.cer,  pain.et.cer$Type=="Valeur"&pain.et.cer$time==0),
    #           aes(x=5, y=Ensemble), colour=col2, size=2)+

    #theme_minimal()+
    theme_hc()+
    scale_x_continuous(name= "Décile de niveaux de vie",
                       breaks=1:10,
                       labels=1:10)+
    scale_y_continuous(name =  "%", limits = YLIM.PROP)+
    ggtitle(label = paste(titles, "(%)"))+
    theme(title = element_text(size = 7))
  ####
  p.VALEUR<-ggplot(data=subset(res0,  res0$Type=="Valeur"&res0$time!=0), aes(x=time, y=Ensemble))+
    geom_hline(yintercept=subset(res0,
                                 res0$Type=="Valeur"&res0$time==0)$Ensemble,
               colour=cols[2], size=2, alpha=0.5)+
    geom_point(mapping = aes(x=time, y=Ensemble), size=5, colour=cols[1])+
    geom_segment(aes(x=time,
                     xend=time,
                     y=YLIM.VALEUR[1],
                     yend=Ensemble), colour=cols[1])+
    #geom_point(data = subset(pain.et.cer,  pain.et.cer$Type=="Valeur"&pain.et.cer$time==0),
    #           aes(x=5, y=Ensemble), colour=col2, size=2)+

    #theme_minimal()+
    theme_hc()+
    scale_x_continuous(name= "Déciles de niveaux de vie",
                       breaks=1:10,
                       labels=1:10)+
    scale_y_continuous(name =  "Dépenses mensuelles moyennes", limits = YLIM.VALEUR)+
    ggtitle(label = paste(titles, "(€)"))+
    theme(title = element_text(size = 7))
  }
  ####
  if(PLOT==TRUE){
    return(list("res0"=res0, "p.VALEUR"=p.VALEUR, "p.PROP"=p.PROP))
  } else {
    return(list("res0"=res0))
  }
}
