ggbarplot_multi<-function(data=tosave$DATA, vec.names="Q45SujetComite.A", vec.exclude=NULL, moda.keep="Oui", 
                          equivalence.data=tosave$EQUIVALENCE, equivalence.var="structure_name", 
                          equivalence.pattern.gsub="45. Quels sont les 2 sujets qui vous semblent les plus importants à aborder en comité de résidents ? (2 réponses attendues)_",
                          wrap_log=TRUE){
  
  
  mytheme<-theme_light()+
    theme(legend.position = "bottom", text = element_text(family = "Calibri Light"), 
          legend.text = element_text(size=font.size.plot), title = element_text(size=font.size.plot+2))
  mytheme.barplot<-theme_void()+
    theme(legend.position = "bottom", text = element_text(family = "Calibri Light"),
          legend.text = element_text(size=font.size.plot), title = element_text(size=font.size.plot+2))
  
  data[ , grepl(vec.names, names(data), fixed=TRUE)]->data.wide
 
   if(!is.null(vec.exclude)){
    
    data.wide[ , !grepl(vec.exclude, names(data), fixed=TRUE)]->data.wide
    
    
   }
  
  data.long<-reshape(data.wide, direction = "long", varying = list(names(data.wide)), times = names(data.wide))
  
  names(data.long)[names(data.long)=="time"]<-"Modalites"
  names(data.long)[names(data.long)==names(data.wide)[1]]<-"Reponses"
  
  if(!is.null(equivalence.data)){
    sapply(1:nrow(data.long), function(i){
      equivalence.data[equivalence.data[ , equivalence.var]==as.character(data.long$Modalites[i]) , ]$text->res0
      gsub(pattern = equivalence.pattern.gsub,
           replacement = "", x = res0, fixed = TRUE)->res
      return(res)
    })->data.long$Labs
  } else {
    data.long$Labs<-data.long$Modalites
  }
  
  if(wrap_log==TRUE){
    data.long$Labs<-wrap.it(data.long$Labs, len = 20)
  }
  
  subset(data.long, data.long$Reponses==moda.keep)->data.long
  
  data.frame(table(data.long$Labs, data.long$Reponses))->tab
  data.long$Labs<-factor(x = data.long$Labs, levels = rev(as.character(tab$Var1[order(tab$Freq, decreasing = TRUE)])), ordered = TRUE)

    
  p<-ggplot(data = data.long, aes(x=Labs, fill=Labs))+
    geom_bar()+
    coord_flip()+
    mytheme+
    theme(legend.position = "none", axis.title = element_blank(), text=element_text(size = 20))
  return(p)
  
  
}