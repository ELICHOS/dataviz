plot.knack<-function(knat = catdes.123$Q1.Nb, 
                      col.neg=pickobrew(the.palette = "YlOrRd", the.indexes = 6), 
                      col.pos=pickobrew(the.palette = "YlGn", the.indexes = 4), 
                      col.na=gray(level = 0.95), ...){
  if(class(knat)[1]=="catdes"){
    knat2<-tabacp(knat)
  } else {knat2<-knat}
  if(class(knat2)=="list"){
    p<-lapply(X = knat2, FUN = function(k){
      if(class(k)!="NULL"){
      seq.cut<-c(-100, 0, 100)
      # CLASS
      k$CLASS<-as.factor(k$CLASS)
      # VARIABLE
      k$VARIABLE<-as.factor(k$VARIABLE)
      k$VARIABLE<-factor(x = k$VARIABLE,  levels(k$VARIABLE)[order(levels(k$VARIABLE))])
      # KNACKPLOT
      knack.k<-ggplot(k, aes(CLASS, VARIABLE))+
        geom_raster(aes(fill = cut(VALUE, breaks = seq.cut)))+
        #geom_text(aes(label=round(VALUE, 1)), fontface="italic", colour=gray(0.3))+
        labs(title ="Heat Map", x = "Outlet Identifier", y = "Item Type")+
        scale_fill_manual(values = c(col.neg, col.pos), 
                          na.value=col.na, 
                          labels=wrap.it(c("Valeurs significativement inférieures (risque d'erreur max: 5%)", "Valeurs significativement supérieures (risque d'erreur max: 5%)", "Pas d'association significative"), 
                                         len=40),
                          name=wrap.it("Sens de l'association entre variables et classes        :",
                                       80))+#, guide_legend(ncol=3))+
        theme_light(
          base_family = "Calibri Light")+
        labs(title = "", x="Classes", y="Variables")+
        #coord_flip()+
        theme(axis.title = element_blank(),
              #axis.text.y = element_text(colour = NEWCOL[c(1, 2, 4, 3, 6, 5, 7)], face = "bold", size = 11),
              legend.position="top")+
        geom_vline(xintercept = c(0, seq(1.5, 6.5, 1)), color=gray(0.95))+
        geom_hline(yintercept = c(0, seq(1.5, 17.5, 1)), color=gray(0.95))
      knack.k
      }
    })
  }
  if(class(knat2)=="data.frame"){
  seq.cut<-c(-100, 0, 100)
  # CLASS
  knat2$CLASS<-as.factor(knat2$CLASS)
  # VARIABLE
  knat2$VARIABLE<-as.factor(knat2$VARIABLE)
  knat2$VARIABLE<-factor(x = knat2$VARIABLE,  levels(knat2$VARIABLE)[order(levels(knat2$VARIABLE))])
  # KNACKPLOT
  knack.i<-ggplot(knat2, aes(CLASS, VARIABLE))+
    geom_raster(aes(fill = cut(VALUE, breaks = seq.cut)))+
    #geom_text(aes(label=round(VALUE, 1)), fontface="italic", colour=gray(0.3))+
    labs(title ="Heat Map", x = "Outlet Identifier", y = "Item Type")+
    scale_fill_manual(values = c(col.neg, col.pos), na.value=col.na)+#, guide_legend(ncol=3))+
    theme_light(
      base_family = "Calibri Light")+
    labs(title = "", x="Classes", y="Variables")+
    #coord_flip()+
    theme(axis.title = element_blank(),
          #axis.text.y = element_text(colour = NEWCOL[c(1, 2, 4, 3, 6, 5, 7)], face = "bold", size = 11),
          legend.position="top")+
    geom_vline(xintercept = c(0, seq(1.5, 6.5, 1)), color=gray(0.95))+
    geom_hline(yintercept = c(0, seq(1.5, 17.5, 1)), color=gray(0.95))
  p<-knack.i
  }
  return(p)
}