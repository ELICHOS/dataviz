#' @export
fonkytonk3<-function(Funkyquest=trap, QUESTION="Test",
                     ordering=nombri,
                     len.wrap=10,
                    Title.Size=6,
                    Enquete.size=4,
                    legend.size=2,
                    text.size=2.5, iflegend=FALSE, marges.du.plot=c(5, 5, 5, 5),
                    widthcat=list("big"="3", "small"=c("1", "2")), leg.pos="top", ADD.MEAN=add.mean){
  blanklab<-""
  Funkyquest->df
  df<-subset(df, !is.na(df$value)&df$value%in%ordering)
  if(inherits(list.ech[[1]]$Q1.Nb.e1, "factor")){
  df$value<-droplevels(x =  df$value)
  }
  data.frame(prop.table(table(df$value, df$times, exclude=""), margin = 2))->df
  df$Var1<-ordered(x = df$Var1, ordering)
  df[order(df$Var2, match(df$Var1, ordering)),]->df
  nrow(df)/length(unique(df$Var2))->length.each
  df$times<-df$Var2
  df$Var2<-c(rep(1, length.each), rep(1.5, length.each), rep(2, length.each),rep(2.5, length.each) )
  df$text_y<-unlist(c(by(data = df$Freq[dim(df)[1]:1], INDICES = df$Var2, FUN = function(x){cumsum(x)-x/2} )))


  df$xtext<-sapply(1:nrow(df), FUN = function(x){
    if(df$Freq[x]<=0.02){
      as.numeric(df$Var2[x])
    }else{
      as.numeric(df$Var2[x])
    }
  }
  )
  # enq1<-dim(pan1data)[1]
  # enq2<-dim(pan2data)[1]
  # enq3<-dim(pan3data)[1]
  # enq4<-
  df$blanc<-sapply(1:nrow(df), function(i){
    if(df$Var1[i]=="Plus nombreuses"|df$Var1[i]=="S'accroitre"|df$Var1[i]=="Ne sait pas"|df$Var1[i]=="Se sont dégradées"|df$Var1[i]=="Plus difficile aujourd'hui"){"blanc"} else {"black"}
  })
  big<-widthcat[["big"]]
  petit<-widthcat[["small"]]
  df$widthcat<-sapply(1:nrow(df), function(i){
    if(df$times[i]%in%petit#|df$Var2[i]!=max(df$Var2)
       ){0.4} else {
      if(df$times[i]%in%big#|df$Var2[i]==max(df$Var2)
         ){0.9}}
  }
  )
  if(ADD.MEAN==TRUE){
    dfmean<-df %>% 
      group_by(Var1) %>%
      summarise("Var2"=max(df$Var2)+0.5,"Freq"=mean(Freq), 'times'="moyenne")
    dfmean$text_y<-unlist(c(by(data = dfmean$Freq[dim(dfmean)[1]:1], INDICES = dfmean$Var2, FUN = function(x){cumsum(x)-x/2} )))
    dfmean$xtext<-sapply(1:nrow(dfmean), FUN = function(x){
      if(dfmean$Freq[x]<=0.02){
        as.numeric(dfmean$Var2[x])
      }else{
        as.numeric(dfmean$Var2[x])
      }
    }
    )
    dfmean$blanc<-sapply(1:nrow(dfmean), function(i){
      if(dfmean$Var1[i]=="Plus nombreuses"|dfmean$Var1[i]=="S'accroitre"|dfmean$Var1[i]=="Ne sait pas"|dfmean$Var1[i]=="Se sont dégradées"|dfmean$Var1[i]=="Plus difficile aujourd'hui"){"blanc"} else {"black"}
    })
    dfmean$widthcat<-0.65*min(df$widthcat)
    df<-rbind(df, dfmean)

  }
  df$ALPHA<-sapply(1:nrow(df), function(i){
    if(df$times[i]=="moyenne"){
      0
    } else {1}
  })
  ggplot(data = df , aes(x=Var2, y = Freq, fill=Var1, width=widthcat, alpha=ALPHA))+
    geom_bar(position = "fill",stat = "identity",  width = 1) +
    scale_fill_manual(name="",#wrap.it(QUESTION, len = len.wrap),
                      labels=paste(ordering, blanklab),
                      values=c(nega, stab, nesp, posi))+
    scale_alpha_continuous(range=c(0.8, 1),guide = 'none')+
    guides(fill=guide_legend(ncol=length(levels(df$Var1)),
                             reverse=TRUE))+
    geom_text(aes(label=paste(round(Freq*100, 0), "%", sep=""),colour=factor(blanc),
                  y=rev(text_y), x=xtext, fill=Var1, fontface=2 ), size=text.size, force =1,
              show.legend = FALSE)+
    scale_color_manual(values=c(gray(level = 0), gray(level = 1)))+
    scale_x_continuous(labels = unique(df[ , c("Var2", "times")])$times, breaks = unique(df[ , c("Var2", "times")])$Var2)+
    coord_flip() +
    theme_minimal()+
    theme(axis.title = element_blank(), axis.text.x = element_blank())+
    #annotate("text", x = c(1, 2, 3), y = 0, size=Enquete.size,
    #         label=c(paste("Enqu?te #1"),
    #                 paste("Enqu?te #2"), paste("Enqu?te #3")
    #         ),
    #         family="Calibri Light", face="bold", hjust=0 )+
    ggtitle(wrap.it(QUESTION, len = len.wrap))+
    if(iflegend==TRUE){
      theme(text= element_text(family="Calibri Light"),
            legend.position= leg.pos,#legend.key.size = legend.size,
            #legend.title =  element_blank(),
            #legend.text=element_text(size=legend.size, face="bold"),
            #plot.title = element_text(family="Calibri Light" , face = "bold",
             #                         size = Title.Size, hjust = 0, vjust=0, margin=margin(0,0,30,0))# plot.title = element_text(family = "sans", size = 18, margin=margin(0,0,30,0))
            #legend.justification = c(0.5, 1),
            #plot.margin=unit(marges.du.plot, "cm")
      ) } else {
        theme(text= element_text(family="Calibri Light"),
              legend.position= "hidden",
              legend.title =  element_blank(),
              legend.text=element_text(size=legend.size),
              plot.title = element_text(family="Calibri Light" , face = "bold",
                                        size = Title.Size, hjust = 0, vjust=0),
              legend.justification = c(0.5, 1)
        ) }
}
#
fonkytonk.repel<-function(varname.pattern="Q1.Nb", ordering=nombri, QUESTION="Test", len.wrap=10,
                          Title.Size=6,
                          Enquete.size=4,
                          legend.size=2,
                          text.size=2, iflegend=FALSE, marges.du.plot=c(1,0.2,0.5,0.2)){
  pan1data[, c(names(pan1data)[grepl(pattern ="email.", x = names(pan1data))],
               names(pan1data)[grepl(pattern =varname.pattern, x = names(pan1data))])]->df1
  df1$enq<-"1"
  pan2data[, c(names(pan2data)[grepl(pattern ="email.", x = names(pan2data))],
               names(pan2data)[grepl(pattern =varname.pattern, x = names(pan2data))])]->df2
  df2$enq<-"2"
  c("email", "varna", "enq")->vecnam
  names(df2)<-vecnam
  names(df1)<-vecnam
  df<-rbind(df1, df2)
  df<-df[!is.na(df$varna), ]
  #
  data.frame(prop.table(table(df$varna, df$enq, exclude=""), margin = 2))->df
  df$Var1<-ordered(x = df$Var1, ordering)
  df[order(df$Var2, match(df$Var1, ordering)),]->df
  df$text_y<-unlist(c(by(data = df$Freq[dim(df)[1]:1], INDICES = df$Var2, FUN = function(x){cumsum(x)-x/2} )))


  df$xtext<-sapply(1:nrow(df), FUN = function(x){
    if(df$Freq[x]<=0.02){
      as.numeric(df$Var2[x])
    }else{
      as.numeric(df$Var2[x])
    }
  }
  )
  #enq1<-dim(pan1data)[1]
  #enq2<-dim(pan2data)[1]
  df$blanc<-sapply(1:nrow(df), function(i){
    if(df$Var1[i]=="Plus nombreuses"|df$Var1[i]=="S'accroitre"|df$Var1[i]=="Ne sait pas"|df$Var1[i]=="Se sont d?grad?es"|df$Var1[i]=="Plus difficile aujourd'hui"){"blanc"} else {"black"}
  })

  ggplot(data = df , aes(x=Var2, y = Freq, fill=Var1))+
    geom_bar(position = "fill",stat = "identity",  width = 0.5) +
    scale_fill_manual(name=wrap.it(QUESTION, len = len.wrap),
                      labels=paste(ordering, blanklab),
                      values=c(nega, stab, nesp, posi))+
    guides(fill=guide_legend(ncol=length(levels(df$Var1)),
                             reverse=TRUE))+
    geom_text_repel(aes(label=paste(round(Freq*100, 0), "%", sep=""),colour=factor(blanc),
                        y=rev(text_y), x=xtext, fill=Var1, fontface=2 ), size=text.size,
                    force =1,direction = "y",point.padding = NA,
                    show.legend = FALSE)+
    scale_color_manual(values=c(gray(level = 0), gray(level = 1)))+
    coord_flip() +
    theme_void()+
    annotate("text", x = c(1.5, 2.5), y = 0, size=Enquete.size,
             label=c(paste("Enqu?te #1"),
                     paste("Enqu?te #2")
             ),
             family="Calibri Light", face="bold", hjust=0 )+
    ggtitle(wrap.it(QUESTION, len = len.wrap))+
    if(iflegend==TRUE){
      theme(text= element_text(family="Calibri Light"),
            legend.position= "bottom",
            legend.title =  element_blank(),
            legend.text=element_text(size=legend.size, face="bold"),
            plot.title = element_text(family="Calibri Light" , face = "bold",
                                      size = Title.Size, hjust = 0, vjust=0),
            legend.justification = c(0.5, 1),
            plot.margin=unit(marges.du.plot, "cm")
      ) } else {
        theme(text= element_text(family="Calibri Light"),
              legend.position= "hidden",
              legend.title =  element_blank(),
              legend.text=element_text(size=legend.size),
              plot.title = element_text(family="Calibri Light" , face = "bold",
                                        size = Title.Size, hjust = 0, vjust=0),
              legend.justification = c(0.5, 1)
        ) }
}
