#' gg_plot_my_var_mystyle
#' 
#' voir gg_plot.my.var. Reprend gg_plot.my.var avec une pré-configuration MRIE (ne fonctionne que pour des geom_bar() ).
#' 
#' @param gg_obj voir gg_plot.my.var
#' @param colpal voir gg_plot.my.var
#' @param base.size voir gg_plot.my.var
#' @param label.size voir gg_plot.my.var
#' @param labels.n if TRUE : affiche les n() et les %. Si FALSE, que les %.
#' @param axis.lab.size
#' @export
gg_plot_my_var_mystyle<-function(gg_obj=test,  
                               colpal=c("wes", "Darjeeling1"), 
                               base.size=15, label.size=4.5, labels.n=TRUE, axis.lab.size=10, wrap.len.axis.lab=25){
  if(colpal[1]=="wes"){
    library(wesanderson)
    if(!is.null(gg_obj$infos$exclude.recod)){
      if(gg_obj$infos$exclude.recod%in%levels(gg_obj$data$VAR)){
      nl<-length(levels(gg_obj$data$VAR))-1
      wes_palette(name = colpal[2], n = nl, type = "continuous")->wesal
      names(wesal)<-levels(gg_obj$data$VAR)[levels(gg_obj$data$VAR)!=gg_obj$infos$exclude.recod]
      c(gray(level = 0.5, alpha = 0.6))->norep
      names(norep)<-gg_obj$infos$exclude.recod
      c(wesal, norep)->wesal
      } else {
        nl<-length(levels(gg_obj$data$VAR))
        wes_palette(name = colpal[2], n = nl, type = "continuous")->wesal
        names(wesal)<-levels(gg_obj$data$VAR)[levels(gg_obj$data$VAR)!=gg_obj$infos$exclude.recod]
      }
    } else {
      wes_palette(name = colpal[2], n = length(unique(gg_obj$data$VAR)), type = "continuous")->wesal
    }
  } else {
    if(colpal[1]=="viridis"){
      library(viridis)
      if(!is.null(gg_obj$infos$exclude.recod)){
        if(gg_obj$infos$exclude.recod%in%levels(gg_obj$data$VAR)){
          nl<-length(levels(gg_obj$data$VAR))-1
          viridis(n=nl, option = colpal[2])->wesal
          names(wesal)<-levels(gg_obj$data$VAR)[levels(gg_obj$data$VAR)!=gg_obj$infos$exclude.recod]
          c(gray(level = 0.5, alpha = 0.6))->norep
          names(norep)<-gg_obj$infos$exclude.recod
          c(wesal, norep)->wesal
        } else {
          nl<-length(levels(gg_obj$data$VAR))
          viridis(n=nl, option = colpal[2])->wesal
          names(wesal)<-levels(gg_obj$data$VAR)[levels(gg_obj$data$VAR)!=gg_obj$infos$exclude.recod]
        }
      } else {
        wes_palette(name = colpal[2], n = length(unique(gg_obj$data$VAR)), type = "continuous")->wesal
      }
      # if(!is.null(gg_obj$infos$exclude.recod)){
      #   nl<-length(levels(gg_obj$data$VAR))-1
      #   viridis(n=nl, option = colpal[2])->wesal
      #   names(wesal)<-levels(gg_obj$data$VAR)[levels(gg_obj$data$VAR)!=gg_obj$infos$exclude.recod]
      #   c(gray(level = 0.5, alpha = 0.6))->norep
      #   names(norep)<-gg_obj$infos$exclude.recod
      #   c(wesal, norep)->wesal
      # } else {
      #   viridis(n=length(unique(gg_obj$data$VAR)), option = colpal[2])->wesal
      # }
    } else {
      if(colpal[1]=="manual"){
        colpal[[2]]->colman
        levels(gg_obj$data$VAR)->levs
        if(names(colman)%inALLboth%levs){
          wesal<-colman
        } else {
          if(length(colman)==1){
            rep(colman, times=length(levs))->wesal
            names(wesal)<-levs
          } else {
          levs[!levs%in%names(colman)]->oubilev
          stop(paste(c("Error in colpal : '", paste(oubilev, collapse = "' '"))))}
        }
      }
    }
  }
  
if("PROP.groupe"%in%names(gg_obj$data)){
  gg_obj$data$PROP.USE<-gg_obj$data$PROP.groupe
} else {
  gg_obj$data$PROP.USE<-gg_obj$data$PROP
}

if(labels.n==TRUE){
  gg_obj$data$LABELS<-paste(gg_obj$data$PROP.USE,"%", " (n=", gg_obj$data$EFFECTIFS, ")", sep="")
} else { gg_obj$data$LABELS<-paste(gg_obj$data$PROP.USE,"%", sep="")}
  
  #print(gg_obj$data$LABELS)
  
  p<-gg_obj$data %>%
  ggplot(aes(x=VAR, y=PROP.USE, fill=VAR))+
  geom_bar(stat="identity", position="dodge", alpha=0.5)+
  scale_size_discrete(range=c(4, 7))+
  scale_fill_manual(values=wesal)+
  geom_text(aes(label=LABELS, y=0), fontface="bold",
            hjust=0, alpha=1, colour=gray(0.2), family="Ubuntu Condensed", size=label.size)+
  ylab("%")+
  theme_MRIE_hc(base_size = base.size, base_family = "Ubuntu", coord_flip=TRUE)+
  theme(legend.position = "none", axis.text.y = element_text(colour=c(gray(0), gray(0.3)), size=axis.lab.size))+
  scale_x_discrete(labels=function(x){wrap.it(x, wrap.len.axis.lab)})+
  coord_flip()+
  xlab("")+labs(title = gg_obj$question)
  
  if("PROP.groupe"%in%names(gg_obj$data)){
    p<-p+ facet_wrap(.~Groupes)+
      theme(strip.text = element_text(size=axis.lab.size))
  }
  
  return(p)
  }