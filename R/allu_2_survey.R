#' @export
allu.2.survey<-function(data=data, 
                        var.bas.gauche, 
                        var.haut.droite,
                        lab.bas.gauche,
                        lab.haut.droite, 
                        flip=FALSE,
                        atridata=filter(atridata, names%in%c("Q1Q", "Q2Q")), 
                        labs.size=4,
                        labs.perc.ignore=0.05, 
                        labs.force=0.004, labs.center.force=0.5,
                        labs.decal=0.05 , ... ){
  library(ggplot2)
  library(forcats)
  library(ggrepel)
  
  nb.ind.min<-labs.perc.ignore*nrow(data)
  
  
  if(!is.null(atridata)){
    var1=as.character(atridata[1 , "names"])
    var2=as.character(atridata[2 , "names"])
    lab1=as.character(atridata[1 , "question.text"])
    lab2=as.character(atridata[2 , "question.text"])
  }
  var1<-var.bas.gauche
  var2<-var.haut.droite
  lab1<-as.character(lab.bas.gauche)
  lab2<-as.character(lab.haut.droite)
  lab1<-wrap.it(x = lab1, len = 30)
  lab2<-wrap.it(x = lab2, len = 30)
  props<-data.frame(table(data[ , c(var1, var2)]))%>%
    group_by_at(var1)%>%
    mutate(prop1=(Freq/sum(Freq))*100)%>%
    ungroup()%>%
    group_by_at(var2)%>%
    mutate(prop2=(Freq/sum(Freq))*100)
  props<-as.data.frame(props)
  props[ , var1]<-as.character( props[ , var1] )
  props[ , var2]<-as.character( props[ , var2] )
  
  
  df<-data[ , c("id", var1, var2)]%>%
    pivot_longer(cols = c(2, 3))%>%
    rename(response=value, subject=id)%>%
    mutate(labs=factor(name, levels = c(var1, var2), labels = c(lab1, lab2), ordered = TRUE))#%>%
    #mutate(labs=fct_rev(labs))
  
  lab.num<-data.frame(table("name"=df$name, "response"=df$response))%>%
    group_by(name)%>%
    mutate(PROP=Freq/sum(Freq)*100)
  
  df<-left_join(x = df, y = lab.num)%>%
    rowwise()%>%
    mutate(labs.response=if(is.na(response)){NA}else {paste(response, "\n(n=", Freq, "/", round(PROP,1), "%)", sep="")})
  
  
  stat_bar<-df%>%
    group_by(name, response)%>%
    summarise(nb=n())%>%
    group_by(name)%>%
    mutate(propG=(nb/sum(nb))*100)

    p<-ggplot(data = df, aes(x = labs, stratum = response, alluvium = subject,
                           y = 1, fill = response)) +
    scale_x_discrete(expand = c(.1, .1)) +
    xlab("")+ylab("")+
    geom_flow(color="darkgray") +
    geom_stratum(alpha = 1, colour=NA, alpha=0.5) +

    geom_text(data=subset(df, df$response!=""&!is.na(df$response)), inherit.aes = TRUE, 
              aes(label=labs.response), stat = "stratum", size = 4, family="Ubuntu Condensed") +
    theme(legend.position = "none") #+
    if(isTRUE(flip)){
      p<-p+coord_flip()
    }
    #ggplot2::coord_flip()
  newdat <- layer_data(p)
  newdat.l<-reshape(data = newdat, timevar = "x", idvar = "alluvium", direction = "wide")
  newdat.l<-merge(x = newdat.l, y = props[ , c(var1, var2, "prop1")], 
                  by.x=c("stratum.1","stratum.2"),
                  by.y=c(var1, var2), 
                  all.x = TRUE, all.y = FALSE)
  newdat.l<-merge(x = newdat.l, y = props[ , c(var1, var2,  "prop2")], 
                  by.x=c("stratum.1","stratum.2"),
                  by.y=c(var1, var2), 
                  all.x = TRUE, all.y = FALSE)
  newdat.l<-merge(x = newdat.l, y = props[ , c(var1, var2,  "Freq")], 
                  by.x=c("stratum.1","stratum.2"),
                  by.y=c(var1, var2), 
                  all.x = TRUE, all.y = FALSE)
  newdat.l$FREQ.PROP<-round(newdat.l$Freq/nrow(data)*100, 1)
  newdat.l$FREQ.LAB<-paste("n=", newdat.l$Freq, "\n", newdat.l$FREQ.PROP, "%", sep = "")
    
  newdat.w<-reshape(data = newdat.l, direction = "long", idvar = "alluvium",
                    varying = list(c("prop1", "prop2"), c("y.1", "y.2"), c("xmin.1", "xmin.2")))
  
  if(isTRUE(flip)){
    dirrepel<-"y"
    newdat.w$x.value.stratum1<- newdat.w$xmin.1+labs.decal
    newdat.w$x.value.stratum2<- newdat.w$xmin.1-labs.decal
    newdat.w$y.value.stratum1<- newdat.w$y.1
    newdat.w$y.value.stratum2<- newdat.w$y.1
  } else {
    message("not.flip")
    dirrepel<-"x"
    newdat.w$x.value.stratum1<-newdat.w$xmax.1+labs.decal
    newdat.w$x.value.stratum2<-newdat.w$xmin.1-labs.decal
    newdat.w$y.value.stratum1<-newdat.w$y.1
    newdat.w$y.value.stratum2<-newdat.w$y.1
  }
  
  
  p<-p+
    geom_label_repel(data = subset(newdat.w, newdat.w$time==1&newdat.w$Freq>nb.ind.min),
                     label.padding=0.1, box.padding = 0,#&newdat.w$prop1>0), 
                     force = labs.force,alpha=0.8, colour=gray(0.1),
                     size=labs.size,min.segment.length = 0.5,direction = dirrepel,
                     aes(x =  x.value.stratum1 , y = y.value.stratum1, fill=stratum.1,
                        label = sapply(prop1, function(i){if(is.na(i)){""} else {paste(round(i, 1), "% ", "\u279E", sep="")}})),
                     inherit.aes = FALSE)+
    geom_label_repel(data = subset(newdat.w, newdat.w$time==2&newdat.w$Freq>nb.ind.min),
                     label.padding = 0.1,#&newdat.w$prop1>0),
                     force = labs.force, alpha=0.8,colour=gray(0.1),
                     size=labs.size,min.segment.length = 0.5,
                     direction=dirrepel,
                     aes(x =  x.value.stratum2 , y = y.value.stratum2, fill=stratum.2,
                         label = sapply(prop1, function(i){if(is.na(i)){""} else {paste("\u279E", round(i, 1), "%", sep="")}})),
                     inherit.aes = FALSE)+
    #geom_point(data = subset(newdat.l,!is.na(newdat.l$Freq)&newdat.l$Freq>nb.ind.min), 
    #                 aes(x =  xmax.1+0.5*(xmin.2-xmax.1) , 
    #                     y = y.2+(y.1-y.2)*0.2),
    #                 colour=gray(level = 0.1),
    #                 inherit.aes = FALSE)+
    geom_label_repel(data = subset(newdat.l,!is.na(newdat.l$Freq)&newdat.l$Freq>nb.ind.min), direction = "both",fontface="bold", 
                     force = labs.center.force, label.size=NA,size=labs.size,box.padding	=0.05,
                     aes(x =  xmax.1+0.5*(xmin.2-xmax.1) , 
                         y = y.2+(y.1-y.2)*0.2,#ymax.1+(ymin.2-ymax.1)/4, 
                         label = paste("n=", Freq, "\n", round(Freq/nrow(data)*100, 1), "%", sep = ""),
                         colour=stratum.1),#colour=gray(0.2),
                     fill=gray(0.8), alpha=0.7,
                     inherit.aes = FALSE)
  return(p)
  
}
