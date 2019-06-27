allu.funk2<-function(var.names=c("Q2.Profil.e1"),
                     ordering=c("Non", "Oui","Ne sait pas"),
                     leg.size=15,
                     QUESTION="Par rapport à ce qu'il se passait il y a 6 mois, avez-vous l'impression que les personnes en situation de pauvreté sont :", len.wrap=30){
  library(tidyverse)
  library(ggalluvial)
  {
    tempdatlong_base[, c("email.e1", "time", var.names)]->df
    df[, var.names]<-as.character(df[, var.names])

    reshape(df, v.names = var.names, timevar = "time", idvar = "email.e1", direction="wide")->dfw
    names(dfw)<-c("email.e1", "var_e1", "var_e2")
    dfw$other1<-sapply(1:nrow(dfw), FUN = function(h){
      if(!is.na(dfw$var_e1[h])&!is.na(dfw$var_e2[h])){as.character(dfw$var_e1[h])} else NA
    })
    dfw$other2<-sapply(1:nrow(dfw), FUN = function(h){
      if(!is.na(dfw$var_e1[h])&!is.na(dfw$var_e2[h])){as.character(dfw$var_e2[h])} else NA
    })
    dfw[, c(1, 2, 4, 5, 3)]->dfw

    listo<-list()
    for(i in 1:4){
      j<-i+1
      dfw[, c(1, j)]->tempdf
      tempdf$time<-i
      names(tempdf)<-c("email.e1", "value", "time")
      listo[[i]]<-tempdf
    }
    do.call("rbind", listo)->dfnew

    #dfw[!is.na(dfw$var_e1)&!is.na(dfw$var_e2) , ]->dfws
    #dfws$time<-3
    #dfws[, c("email.e1", "time","var_e1")]->dfws
    #names(dfws)[3]<-var.names

    #dfs<-df
    #dfs1<-dfs[dfs$time==1 ,]

    #rbind(df, dfws)->dfnew
    #names(dfnew)[3]<-"value"
    #dfnew$time[dfnew$time==3]<-5
    #  dfnew$time[dfnew$time==2]<-3
    #    dfnew$time[dfnew$time==5]<-2
    #
    dfnew$value<-ordered(x = dfnew$value, levels=ordering)
    length((levels(dfnew$value)))->lino
    aggregate(dfnew$value~dfnew$time, data = dfnew, FUN = function(k) round(prop.table(table(k))*100, 2))->a
    a[[2]][1 , ]<-round(prop.table(table(dfnew[dfnew$time==1 , ]$value, exclude = NULL))*100, 2)[1:lino]
    c(a$`dfnew$value`)->a
    data.frame( t(matrix(rev(a), nrow = 4)))->b
    #
    c(0, freq.cu(df = b, var = 1, debut = "haut"))->d4
    dmean4<-c()
    for(i in 1:length(d4)){
      if(d4[i]==0){dmean4[i]<-NULL} else {dmean4[i]<-mean(d4[c(i, i-1)])}
    }
    #
    c(0, freq.cu(df = b, var = 2, debut = "haut"))->d3
    dmean3<-c()
    for(i in 1:length(d3)){
      if(d3[i]==0){dmean3[i]<-NULL} else {dmean3[i]<-mean(d3[c(i, i-1)])}
    }
    #
    c(0, freq.cu(df = b, var = 3, debut = "haut"))->d2
    dmean2<-c()
    for(i in 1:length(d2)){
      if(d2[i]==0){dmean2[i]<-NULL} else {dmean2[i]<-mean(d2[c(i, i-1)])}
    }
    c(0, freq.cu(df = b, var = 4, debut = "haut"))->d1
    dmean1<-c()
    for(i in 1:length(d1)){
      if(d1[i]==0){dmean1[i]<-NULL} else {dmean1[i]<-mean(d1[c(i, i-1)])}
    }
    #
    length(dfnew$time[dfnew$time==1#&!is.na(dfnew$value)
                      ])->dimy1
    length(dfnew$time[dfnew$time==1&!is.na(dfnew$value)
                      ])->dimy1.nona
    length(dfnew$time[dfnew$time==1&is.na(dfnew$value)])->dimy1na
    length(dfnew$time[dfnew$time==2&!is.na(dfnew$value)])->dimy2
    length(dfnew$time[dfnew$time==3&!is.na(dfnew$value)])->dimy3
    length(dfnew$time[dfnew$time==4&!is.na(dfnew$value)])->dimy4

    #
    data.frame("time"= rep(1, times=nrow(b)), "label"=rev(c(matrix(round(prop.table(table(dfnew[dfnew$time==1 , ]$value))*100, 2)[1:lino]))),
               "value"=dmean1[-c(1)])->edf1
    edf1$value<-edf1$value/100*(dimy1)+dimy1na#+dimy1na)+dimy1na
    data.frame("time"= rep(2, times=nrow(b)), "label"=b[, 3], "value"=dmean2[-c(1)])->edf2
    edf2$value<-(edf2$value/100*dimy2)+(dimy1-dimy2)
    data.frame("time"= rep(3, times=nrow(b)), "label"=b[, 2], "value"=dmean3[-c(1)])->edf3
    edf3$value<-edf3$value/100*dimy3+(dimy1-dimy3)
    data.frame("time"= rep(4, times=nrow(b)), "label"=b[, 1], "value"=dmean4[-c(1)])->edf4
    edf4$value<-edf4$value/100*dimy4+(dimy1-dimy4)

    rbind(edf1, edf2, edf3, edf4)->edf
    #names(edf)<-c("time", "label", "email.e1")

    g<-ggplot(dfnew,
              aes(x=time, stratum=value, alluvium=email.e1, fill=value,
                  label=value))+
      #geom_text(stat = "stratum", label.strata = FALSE) +
      #geom_text(stat = "stratum") +
      #stat_stratum(geom = "errorbar") +
      #geom_text(size = 3, position = position_stack(vjust = 0.5))+
      #geom_text(stat="count", aes(label = ..count.., y=..count..), position = position_stack(vjust = 0.5))+
      #label = round(a, 3))
      #)+
      geom_flow(data=dfnew, stat = "alluvium", lode.guidance = "rightleft", alpha=0.9
                #, color = "darkgray"
      )+
      geom_stratum(width=1/3.5, colour="white", size=2, alpha=1)+
      #geom_text(stat = "count", aes(label = ..count.., y = ..count..) )+
      scale_fill_manual(name=wrap.it(QUESTION, len = len.wrap),
                        values=c(nega, stab, nesp, posi),
                        breaks=levels(dfnew$value))+
      guides(fill = guide_legend(title.position = "top"))+
      theme_void()+
      annotate("text", x = c(1.5, 3.5), y = dimy1+15, size=6,
               label=c("Enquête 1", "Enquête 2"), family="Calibri Light" )+
      annotate("text", x = c(1, 2.5, 4), y = dimy1+7, size=4.5,
               label=c(dimy1.nona, dimy2, dimy4), family="Calibri Light" )+
      geom_segment(aes(x=2.25, xend = 2.75 , y=dimy1+15, yend = dimy1+15), size=1.1,colour=gray(level = 0.4),
                   arrow = arrow(length = unit(0.4,"cm")))
    #geom_text(stat = "stratum")
    g+#geom_label(data = edf, aes(x=time, 'email.e1'=value, label=label))+
      annotate(geom="text", x=edf$time+0.005, y=edf$value-0.3, label=round(edf$label,1),
               color=gray(level = 0.9))+
      annotate(geom="text", x=edf$time, y=edf$value, label=round(edf$label,1),
               color=gray(level = 0))+

      #geom_label(data=edf, aes(x=time, y = value, label=label))+
      ggtitle(var.names)+
      theme(plot.title = element_text(family="Calibri Light", face="bold"),
            legend.position= c(0.5, 0.45),#"bottom",
            legend.text = element_text(size = leg.size, family = "Calibri Light" ),
            #legend.title =  element_blank(),
            legend.justification = c(0.5, 1))
  }
}
