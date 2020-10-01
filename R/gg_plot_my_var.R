#' @export
data_plot.my.var<-function(data=tosave$DATA, 
                         var=NULL, var.pattern=NULL, var.exclude=NULL, var.order=NULL,
                         id=NULL, 
                         equivalence.data=NULL, equivalence.var=NULL, equivalence.text=NULL, equivalence.gsub.patt=NULL, 
                         exclude=c(NA, "", " "), exclude.recod="non-réponse",
                         question.lab=NULL, 
                         var.group=NULL, var.group.order=c(), 
                         Rnd=1
                         ){
  library(dplyr)
  library(tidyr)
  if(is.null(var)){
    if(!is.null(var.pattern)|!is.null(var.exclude)){
      if(is.null(var.exclude)){
        data[ , grepl(var.pattern, names(data), fixed=TRUE)]->data.wide.for.plot
      } else {
        if(is.null(var.pattern)){
          data[ , !grepl(var.exclude, names(data), fixed=TRUE)]->data.wide.for.plot
        } else {
          data[ , grepl(var.pattern, names(data), fixed=TRUE)&!grepl(var.exclude, names(data), fixed=TRUE)]->data.wide.for.plot
      }
      }
    } else {
      stop("Vous devez renseigner la variable var ou alternativement var.pattern/var.exclude")
    }
  } else {
    if(length(var)==nrow(data)){
      data.frame(var)->data.wide.for.plot
    } else {
      message("coucou")
      message(var%in%names(data))
    data.frame(data[ , var])->data.wide.for.plot
    }
  }
  if(!is.null(id)){
    if(length(id)==1&(inherits(x = id, what = "character")|inherits(x = id, what = "numeric") )){
    data.wide.for.plot<-cbind("ID"=data[ , id], data.wide.for.plot)
    } else {
      if(length(id)>1){
        data.wide.for.plot<-cbind("ID"=id, data.wide.for.plot)
      }
    }
  } else {
    data.wide.for.plot$ID<-1:nrow(data.wide.for.plot)
  }
  
  if(!is.null(var.group)){
    if(length(var.group)==1&(inherits(var.group, "character")|inherits(var.group, "numeric")) ){
      cbind(data.wide.for.plot, "Groupes"=data[ , var.group])->data.wide.for.plot
    } else {
      if(length(var.group)==nrow(data)){
        cbind(data.wide.for.plot, "Groupes"=var.group)->data.wide.for.plot
      }
    }
  }
  data.wide.for.plot<-as.data.frame(data.wide.for.plot)
  if(ncol(select(data.wide.for.plot, !one_of("Groupes")))>2){
  data.long.for.plot<-reshape(data.wide.for.plot, direction = "long", varying = list(names(data.wide.for.plot)[names(data.wide.for.plot)!="ID"&names(data.wide.for.plot)!="Groupes"]), 
                              times = names(data.wide.for.plot)[names(data.wide.for.plot)!="ID"&names(data.wide.for.plot)!="Groupes"], idvar = "ID")
  if(!is.null(equivalence.data)&is.null(question.lab)){
    if(is.null(equivalence.text)|is.null(equivalence.var)){
      stop("Vous devez préciser la variable d'équivalence et la variable donnant le texte de substitution")
    } else {
  sapply(1:nrow(data.long.for.plot), function(i){
    equivalence.data[equivalence.data[ , equivalence.var]==data.long.for.plot$time[i] , ][ , equivalence.text]->res0
    if(!is.null(equivalence.gsub.patt)){
    gsub(pattern = equivalence.gsub.patt,#"45. Quels sont les 2 sujets qui vous semblent les plus importants à aborder en comité de résidents ? (2 réponses attendues)_"
         replacement = "", x = res0, fixed = TRUE)->res0
    }
    return(res0[1])
  })->data.long.for.plot$Labs
    }
  } else {
    if(!is.null(question.lab)){
      data.long.for.plot$Labs<-question.lab
    } else {
    data.long.for.plot$Labs<-data.long.for.plot$time
    }
  }
  } else {
    data.long.for.plot<-data.wide.for.plot
    if(!is.null(equivalence.data)&is.null(question.lab)){
    if(length(var)==1&inherits(x = var, what = "character")){
      sapply(1:nrow(data.long.for.plot), function(i){
        equivalence.data[equivalence.data[ , equivalence.var]==var , ][ , equivalence.text]->res0
        if(!is.null(equivalence.gsub.patt)){
        gsub(pattern = equivalence.gsub.patt,#"45. Quels sont les 2 sujets qui vous semblent les plus importants à aborder en comité de résidents ? (2 réponses attendues)_"
             replacement = "", x = res0, fixed = TRUE)->res0
        }
        return(res0[1])
      })->data.long.for.plot$Labs
    } else {data.long.for.plot$Labs<-"Réponses"}
    } else {
      #message("coucou71")
      #print(head(data.long.for.plot))
      #data.long.for.plot$Labs<-data.long.for.plot$time
      if(!is.null(question.lab)){
        data.long.for.plot$Labs<-question.lab
      } else {
        message("coucou84")
      data.long.for.plot$Labs<-"Réponses"
      }
    }
  }
  
  names(data.long.for.plot)[!names(data.long.for.plot)%in%c("ID", "time", "Labs", "Groupes")]->myna
  
  if(!is.null(exclude.recod)){
    if(!is.null(exclude)){
      list("a"=exclude)->lire
      names(lire)<-exclude.recod
      data.long.for.plot[ , myna]<-basiques::tranfofacto(var = data.long.for.plot[ , myna], codage = lire)
      data.long.for.plot->data.long.for.plot.corr
    } else {
        warning("Si vous spécifiez 'exclude.recod' vous devez spécifier 'exclude' => 'exclude.recod' non pris en compte.")
      }
  } else {
    if(!is.null(exclude)){
      subset(data.long.for.plot, !data.long.for.plot[ , myna]%in%exclude)->data.long.for.plot.corr     
    } else {
      data.long.for.plot->data.long.for.plot.corr
    }
  }
  
  

  

  
  droplevels(x = data.long.for.plot.corr[ , myna])->data.long.for.plot.corr[ , myna]
  #print(head(data.long.for.plot.corr))
  if(is.null(var.group)){
  table(data.long.for.plot.corr[ , myna], exclude = NULL)->tab
  tab.df<-data.frame(tab)
  names(tab.df)<-c("VAR", "EFFECTIFS")
  tab.df$PROP<-round(tab.df$EFFECTIFS/length(unique(data.wide.for.plot$ID))*100, Rnd)
  } else {
    table("Groupes"=data.long.for.plot.corr$Groupes, "VAR"=data.long.for.plot.corr[ , myna], exclude = NULL)->tab
    tab.df<-data.frame(tab)
    names(tab.df)[names(tab.df)=="Freq"]<-"EFFECTIFS"
    tab.df$PROP<-round(tab.df$EFFECTIFS/length(unique(data.wide.for.plot$ID))*100, Rnd)
  }
  message("coucou143")
  if(!is.null(var.group)){
    if(length(var.group)==1&(inherits(var.group, "character")|inherits(var.group, "numeric")) ){
      data.frame(table(data[ , var.group], exclude = NULL))->tabG
    } else {
      if(length(var.group)==nrow(data)){
        data.frame(table(var.group, exclude = NULL))->tabG
              }
    }
    tab.df$EFFECTIFS.groupe<-sapply(1:nrow(tab.df), function(i){
      subset(tabG, tabG$Var1==tab.df$Groupes[i])$Freq
    })
    tab.df$PROP.groupe<-sapply(1:nrow(tab.df), function(i){
      round(tab.df$EFFECTIFS[i]/subset(tabG, tabG$Var1==tab.df$Groupes[i])$Freq*100, Rnd)
    })
    tab.df<-as.data.frame(tab.df)
    tab.df<-tab.df[order(tab.df$Groupes, tab.df$EFFECTIFS, decreasing = TRUE) , ]
  }
  message(paste("nrow data.long.for.plot = ", nrow(data.long.for.plot), "  |  nrow data.long.for.plot.corr = ", nrow(data.long.for.plot.corr), sep = ""))
  message(paste("unique.id data.long.for.plot = ", 
                length(unique(data.long.for.plot$ID)), 
                "  |  unique.id data.long.for.plot.corr = ", 
                length(unique(data.long.for.plot.corr$ID)), sep = ""))
  message("Nombre d'individus = ", length(unique(data.wide.for.plot$ID)))
  
  list("nrow.data.long.for.plot"=nrow(data.long.for.plot), "nrow.data.long.for.plot.corr"=nrow(data.long.for.plot.corr), 
       "unique.id.data.long.for.plot"=length(unique(data.long.for.plot$ID)), "unique.id.data.long.for.plot.corr"=length(unique(data.long.for.plot.corr$ID)),
       "Nombre.individus"=length(unique(data.wide.for.plot$ID)), 
       "exclude.recod"=exclude.recod)->infos
  
  tab.df%>%group_by(VAR)%>%summarise_at("EFFECTIFS", sum)->eff
  
  tab.df$VAR<-factor(x = tab.df$VAR, levels = eff$VAR[order(eff$EFFECTIFS, decreasing = FALSE)], ordered = TRUE)
  
  if(!is.null(var.group)&!is.null(var.group.order)){
    tab.df$Groupes<-factor(x = tab.df$Groupes, levels = var.group.order, ordered = TRUE)
  }
  if(!is.null(var.order)){
    tab.df$VAR<-factor(x = tab.df$VAR, levels = var.order, ordered = TRUE)
  }
  
  res<-list("data"=tab.df,  "question"=unique(data.long.for.plot.corr$Labs), "infos"=infos)
  return(res)
}
                         

#' @export
gg_plot.my.var <- function(gg_obj=test, type=c("lollipop", "bar", "polar", "circumpolar", "bar*style"), 
                           orientation="h", label.space=0.1, label.space.x=0, label.size.prop=1,
                           colpal=c("wes", "Darjeeling1"), base.size=15){
  if("Groupes"%in%names(gg_obj$data)){
  gg_obj$data$VAR_A_LAB<-gg_obj$data$PROP.groupe
  } else {
    gg_obj$data$VAR_A_LAB<-gg_obj$data$PROP 
  }
  
  if(grepl("polar", type)){
    if(type=="polar"){
      p<-ggplot(gg_obj$data, aes(x = "", y = EFFECTIFS, fill = VAR))
    
      p<-p+ geom_bar(width = 1,stat="identity")+
        coord_polar("y", start = 0)
      labcor<-1.6
    }
    if(type=='circumpolar'){
      p<-ggplot(gg_obj$data, aes(x = 2, y = EFFECTIFS, fill = VAR))
        
      p<-p+ geom_bar(width = 1,stat="identity")+
        coord_polar("y", start = 0)+
        xlim(0.5, 2.5)
      labcor<-2.5
    }
    
    
    
    
    if(is.null(colpal)){
      p<-p+scale_fill_grey(start = 0.7, end = 0.2)
    } else {
      if(colpal[1]=="wes"){
        library(wesanderson)
        if(!is.null(gg_obj$infos$exclude.recod)){
          nl<-length(levels(gg_obj$data$VAR))-1
          wes_palette(name = colpal[2], n = nl, type = "continuous")->wesal
          names(wesal)<-levels(gg_obj$data$VAR)[levels(gg_obj$data$VAR)!=gg_obj$infos$exclude.recod]
          c(gray(level = 0.5, alpha = 0.6))->norep
          names(norep)<-gg_obj$infos$exclude.recod
          c(wesal, norep)->wesal
        } else {
          wes_palette(name =  colpal[2], n = length(unique(gg_obj$data$VAR)), type = "continuous")->wesal
        }
      } else {
        if(colpal[1]=="viridis"){
          library(viridis)
          if(!is.null(gg_obj$infos$exclude.recod)){
            nl<-length(levels(gg_obj$data$VAR))-1
            viridis(n=nl, option = colpal[2])->wesal
            names(wesal)<-levels(gg_obj$data$VAR)[levels(gg_obj$data$VAR)!=gg_obj$infos$exclude.recod]
            c(gray(level = 0.5, alpha = 0.6))->norep
            names(norep)<-gg_obj$infos$exclude.recod
            c(wesal, norep)->wesal
          } else {
            viridis(n=length(unique(gg_obj$data$VAR)), option = colpal[2])->wesal
          }
        } else {
          if(colpal[1]=="manual"){
            colpal[[2]]->colman
            levels(gg_obj$data$VAR)->levs
            if(names(colman)%inALLboth%levs){
              wesal<-colman
            } else {
              levs[!levs%in%names(colman)]->oubilev
              stop(paste(c("Error in colpal : '", paste(oubilev, collapse = "' '"))))}
          }
      }
        p<-p+scale_fill_manual(values=wesal)
      }
    }

    library(ggrepel)
    message("here")
    if(length(unique(gg_obj$data$VAR))<4){
    p<-p+ geom_label(aes(label=paste("n = ", EFFECTIFS, " / ", VAR_A_LAB, "%", sep="")), #, x=labcor), 
                           colour="white", alpha=0.9, position = position_stack(vjust = 0.5), size=base.size*label.size.prop, show.legend = FALSE)
    } else {
      p<-p+ geom_label_repel(aes(label=paste("n = ", EFFECTIFS, " / ", VAR_A_LAB, "%", sep=""), x=labcor), 
                       force = 5,
                       colour="white", alpha=0.9, direction = "x",min.segment.length = 0,point.padding = 5,
                       position = position_stack(vjust = 0.5), size=base.size*label.size.prop, show.legend = FALSE)
    }
    p<-p+theme_void( base_size = base.size )+
      ggtitle(label = gg_obj$question)+theme(legend.title = element_blank())
    
    
  } else {
    p<-ggplot(data = gg_obj$data, aes(x=VAR, fill=VAR, y=EFFECTIFS))
    
    if(type=="lollipop"){
      p<-p+geom_segment( aes(x=VAR, xend=VAR, y=0, yend=EFFECTIFS, colour=VAR), size=base.size/7) +
        geom_point(aes(colour=VAR), size=base.size/2, alpha=0.6, show.legend = FALSE)
    }
    if(type=="bar"){
      p<-p+ geom_bar(stat="identity")
    }
    
   p<-p+ ggtitle(label = gg_obj$question)+
      xlab("")+ylab("Effectifs")
    #theme_minimal()+
    #Uggthemes::theme_hc()+
    if(orientation=="h"){
      p<-p+dataviz::theme_MRIE_hc(coord_flip = TRUE, base_size = base.size)+coord_flip()
    } else {
      p<-p+dataviz::theme_MRIE_hc(coord_flip = FALSE, base_size = base.size)
    }
   

   if(is.null(colpal)){
     rep(gray(0.4), times= length(unique(gg_obj$data$VAR)))->fillb
     rep(gray(0.2), times= length(unique(gg_obj$data$VAR)))->coloub
     p<-p+scale_fill_manual(values=fillb )+scale_colour_manual(values=coloub)
     
   } else {
     if(colpal[1]=="wes"){
       library(wesanderson)
       if(!is.null(gg_obj$infos$exclude.recod)){
         nl<-length(levels(gg_obj$data$VAR))-1
         wes_palette(name = colpal[2], n = nl, type = "continuous")->wesal
         names(wesal)<-levels(gg_obj$data$VAR)[levels(gg_obj$data$VAR)!=gg_obj$infos$exclude.recod]
         c(gray(level = 0.5, alpha = 0.6))->norep
         names(norep)<-gg_obj$infos$exclude.recod
         c(wesal, norep)->wesal
       } else {
         wes_palette(name = colpal[2], n = length(unique(gg_obj$data$VAR)), type = "continuous")->wesal
       }
     } else {
       if(colpal[1]=="viridis"){
         library(viridis)
         if(!is.null(gg_obj$infos$exclude.recod)){
           nl<-length(levels(gg_obj$data$VAR))-1
           viridis(n=nl, option = colpal[2])->wesal
           names(wesal)<-levels(gg_obj$data$VAR)[levels(gg_obj$data$VAR)!=gg_obj$infos$exclude.recod]
           c(gray(level = 0.5, alpha = 0.6))->norep
           names(norep)<-gg_obj$infos$exclude.recod
           c(wesal, norep)->wesal
         } else {
           viridis(n=length(unique(gg_obj$data$VAR)), option = colpal[2])->wesal
         }
       } else {
         if(colpal[1]=="manual"){
           colpal[[2]]->colman
           levels(gg_obj$data$VAR)->levs
           if(names(colman)%inALLboth%levs){
             wesal<-colman
           } else {
             levs[!levs%in%names(colman)]->oubilev
             stop(paste(c("Error in colpal : '", paste(oubilev, collapse = "' '"))))}
         }
       }
     }
     p<-p+scale_fill_manual(values=wesal)+scale_colour_manual(values=wesal)
   }
   
   
    p<-p+theme(legend.position="none")
    ggplot_build(p)->buikd_p
    #if(orientation=="h"){
    unlist(lapply(buikd_p$data, function(fg){fg$y}))->vecy
      max(vecy, na.rm = TRUE)-min(vecy, na.rm = TRUE)->rangey
    #} else {
    #max(buikd_p$data[[1]]$x, na.rm = TRUE)-min(buikd_p$data[[1]]$x, na.rm = TRUE)->rangey
    #}
    message(rangey)
    if(orientation=="h"){
      depl<-"x"
      p<-p+geom_label(aes(x=VAR, y=EFFECTIFS, label=paste("n = ", EFFECTIFS, " / ", VAR_A_LAB, "%", sep=""), colour=VAR), 
                            fill=add.alpha(col = gray(1), 0.9), size=base.size*label.size.prop, nudge_y = label.space*rangey, nudge_x = label.space.x*rangey)
    }
    if(orientation=="v"){
      library(ggrepel)
      depl<-"y"
      p<-p+ylim(min(vecy, na.rm = TRUE), (max(vecy, na.rm = TRUE)+(label.space*rangey) ))+geom_label_repel(aes(x=VAR, y=EFFECTIFS, label=paste("n = ", EFFECTIFS, " / ", VAR_A_LAB, "%", sep=""), colour=VAR), segment.alpha = 0, direction = depl,
                            ylim = c(min(vecy, na.rm = TRUE), (max(vecy, na.rm = TRUE)+(label.space*rangey) )),
                      fill=add.alpha(col = gray(1), 0.9), size=base.size*label.size.prop, nudge_y = label.space*rangey)
    }

  }

   
   
  if("Groupes"%in%names(gg_obj$data)){
    p<-p+facet_wrap(Groupes~.)+
      labs(subtitle = "Pourcentages par groupe")
  }
  return(p)
}

#' @export
gg.round_my_rounds<-function(gg_obj=resd, 
                             ORDER=NULL, 
                             colpal=c("wes", "Darjeeling1"), 
                             sizepal=c(5, "PROP"), 
                             LENs=30, 
                             lab.size=3, family.L="Ubuntu",
                             NUDGE.X = 0.8, x.limit=c(-2.5, 2.5), y.limit=c(-1.1, 1.1), 
                             SIZE.Q=5, family.Q="Ubuntu Condensed"){
  if(length(ORDER)>1&length(ORDER)==length(unique(gg_obj$data$VAR))){
    message("coucou")
    gg_obj$data$VAR<-factor(gg_obj$data$VAR, levels = ORDER, ordered = TRUE)
  }
  
  n <-  length(levels(gg_obj$data$VAR))
  pts.circle <- data.frame(t(sapply(1:n,function(r)c(cos(2*r*pi/n),sin(2*r*pi/n)))))
  names(pts.circle)<-c("x", "y")
  cbind(gg_obj$data, pts.circle)->df
  
  df$ALIGN.X<-sapply(1:nrow(df), function(i){if(df$x[i]>=0){0} else {1}})
  df$NUDGE.X<-sapply(1:nrow(df), function(i){if(df$x[i]>=0){NUDGE.X} else {-NUDGE.X}})
  df$POS.X<-sapply(1:nrow(df), function(i){if(df$x[i]>=0){2} else {-2}})
  
  p<-ggplot(data = df, aes(x=x, y=y, colour=VAR))+
    scale_x_continuous(limits = x.limit)+
    scale_y_continuous(limits = y.limit)+
    coord_equal()+
    geom_label_repel(force = 0.2, aes(label=#wrap.it(
                                        paste(VAR, "\n(n=", EFFECTIFS, "/", PROP, "%)", sep=""),#, 50), 
                                      hjust=ALIGN.X), nudge_x = df$NUDGE.X, direction = "y", alpha=0.8, size=lab.size, family=family.L)+
    geom_point(aes(size=EFFECTIFS), alpha=0.7)+
    
    theme_void(base_family = "Ubuntu")+
    theme(legend.position = "none")
  
  if(!is.null(sizepal)){
    if(inherits(sizepal, "character")&"PROP"%in%sizepal){
      which(sizepal=="PROP")->PROP.pos
      if(PROP.pos==1){
        sizepal[1]<-as.numeric(sizepal[2])*min(gg_obj$data$EFFECTIFS)/max(gg_obj$data$EFFECTIFS)
      }
      if(PROP.pos==2){
        sizepal[2]<-as.numeric(sizepal[1])*max(gg_obj$data$EFFECTIFS)/min(gg_obj$data$EFFECTIFS)
      }
      sizepal<-as.numeric(sizepal)
    }
    if(is.numeric(sizepal)){
      
      p<-p+
        scale_size_continuous(range = sizepal)
    }
  }
  if(is.null(colpal)){
    rep(gray(0.4), times= length(unique(gg_obj$data$VAR)))->fillb
    rep(gray(0.2), times= length(unique(gg_obj$data$VAR)))->coloub
    p<-p+scale_fill_manual(values=fillb )+scale_colour_manual(values=coloub)
    
  } else {
    if(length(colpal)==2){
      if(colpal[1]=="wes"){
        library(wesanderson)
        if(!is.null(gg_obj$infos$exclude.recod)){
          nl<-length(levels(gg_obj$data$VAR))-1
          wes_palette(name = colpal[2], n = nl, type = "continuous")->wesal
          names(wesal)<-levels(gg_obj$data$VAR)[levels(gg_obj$data$VAR)!=gg_obj$infos$exclude.recod]
          c(gray(level = 0.5, alpha = 0.6))->norep
          names(norep)<-gg_obj$infos$exclude.recod
          c(wesal, norep)->wesal
        } else {
          wes_palette(name = colpal[2], n = length(unique(gg_obj$data$VAR)), type = "continuous")->wesal
        }
      } else {
        if(colpal[1]=="viridis"){
          library(viridis)
          if(!is.null(gg_obj$infos$exclude.recod)){
            nl<-length(levels(gg_obj$data$VAR))-1
            viridis(n=nl, option = colpal[2])->wesal
            names(wesal)<-levels(gg_obj$data$VAR)[levels(gg_obj$data$VAR)!=gg_obj$infos$exclude.recod]
            c(gray(level = 0.5, alpha = 0.6))->norep
            names(norep)<-gg_obj$infos$exclude.recod
            c(wesal, norep)->wesal
          } else {
            viridis(n=length(unique(gg_obj$data$VAR)), option = colpal[2])->wesal
          }
        } else {
          if(colpal[1]=="manual"){
            colpal[[2]]->colman
            levels(gg_obj$data$VAR)->levs
            if(names(colman)%inALLboth%levs){
              wesal<-colman
            } else {
              levs[!levs%in%names(colman)]->oubilev
              stop(paste(c("Error in colpal : '", paste(oubilev, collapse = "' '"))))}
          }
        }
      }
    } else {
      if(length(colpal)==length(unique(gg_obj$data$VAR))){
        wesal<-colpal
      }
    }
    p<-p+scale_colour_manual(values=wesal)
  }
  p<-p+annotate(geom = "text", x = 0, y = 0, label=gg_obj$question, size=SIZE.Q, family=family.Q)
  return(p)
}


