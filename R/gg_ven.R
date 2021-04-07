#' @export
gg_venn<-function(data, lev, autolab=NULL, HIGH_COL=gray(0.1),
                  vars=c("Q2Q", "QP2", "Q1Q"), 
                  label.groups=c(as.character(atridata$question.text[ atridata$names=="Q2Q" ]),
                                        as.character(atridata$question.text[ atridata$names=="QP2.MOUI." ]), 
                                                     as.character(atridata$question.text[ atridata$names=="Q1Q" ])),
                  label.groups.size=2,
                  label.count=TRUE, label.which="both", label_alpha=0.7,label.groups.alpha=0.8,
                  PROP.COUNT.LABEL=TRUE, rnd=1, add.exclude=NULL, exclude.pos=c(0.7, 0.8), add.rayon=1, MARGS=margin(0, 0.5, 2, 0), sequential, ...
                  ){
  # SOURCE: https://github.com/gaospecial/ggVennDiagram
  library(sf)
  length(vars)->vars_len
  #####
  df<-data %>%
    select( {{vars}} )
  
  data.res<-lapply(df, function(x){sapply(x, function(i){
    if(is.na(i)){NA}else{
      if(i==lev){TRUE}else{NA}
    }
    })})
  
  data.res<-data.frame(table(data.res, exclude = NULL), stringsAsFactors = FALSE)
  data.res[sapply(data.res, function(x){class(x)=="factor"})][]<-lapply( data.res[sapply(data.res, function(x){class(x)=="factor"})], as.character)
  data.res[is.na(data.res)]<-"_"
  
  li<-lapply(1:ncol(df), function(j){
    sapply(1:length(df[ , j]), function(i){
      if(is.na(df[i , j])){NA}else{
      if(df[i , j]==lev){paste0("common", i)} else {NA} #paste0("not.common", j)}
      }
    })
  })
  
  names(li)<-names(df)
  li<-lapply(li, function(x){x[!is.na(x)]})
  
  if(is.null(label.groups)){
    category_names<-names(li)
  } else {
    category_names<-as.character(label.groups)
  }
  #####################
  x<-li
  a <- x[[1]][!is.na(x[[1]])]
  b <- x[[2]][!is.na(x[[2]])]
  if(vars_len==3){
  d <- x[[3]][!is.na(x[[3]])]
  } else {
    d<-c("")
  }
  
  A <- setdiff(a, union(b,d))
  B <- setdiff(b, union(a,d))
  C <- setdiff(d, union(a,b))
  AB <- setdiff(intersect(a,b),d)
  AC <- setdiff(intersect(a,d),b)
  BC <- setdiff(intersect(b,d),a)
  ABC <- multi_intersect(a,b,d)
  
  items<-list(A=A,B=B,C=C,AB=AB,AC=AC,BC=BC,ABC=ABC)
  items<-items[lengths(items)>0&items!=""]
  
  values <- sapply(items, length)
  
  counts<-data.frame(group=names(items),count=values, stringsAsFactors = F)
  #####################
  if(vars_len==3){
  region_data <- three_dimension_circle_regions(200)
  category <- data.frame(x = c(2, -3.5, 7.5),
                         y = c(8.5, -4.6, -4.6),
                         var= vars,
                         label = category_names)
  }
  if(vars_len==2){
    region_data <- two_dimension_circle_regions(200)
    category <- data.frame(x = c(-3.5, 7.5),
                           y = c(4.6, -4.6),
                           var= vars,
                           label = category_names)
  }
  category$GLOCOUNT<-sapply(category$var, function(x){
    x<-as.character(x)
    sum(sapply(data[ , x], function(i){i==lev}), na.rm = TRUE)
  })
  category$GLOPROP<-category$GLOCOUNT/nrow(data)
  if(PROP.COUNT.LABEL){
    category$label<-paste0(category$label, "\n(n=", category$GLOCOUNT, "/", round(category$GLOPROP*100, 1), "%)")
  }
  #####################
  polygon <- region_data[[1]]
  center <- region_data[[2]]
  dat.polygon<-merge(polygon,counts)
  res.counts<-counts
  res.counts$A<-sapply(1:nrow(res.counts), function(i){
    if(grepl("A", res.counts$group[i])){
      category_names[1]
    } else NA
  })
  res.counts$B<-sapply(1:nrow(res.counts), function(i){
    if(grepl("B", res.counts$group[i])){
      category_names[2]
    } else NA
  })
  if(vars_len==3){
  res.counts$C<-sapply(1:nrow(res.counts), function(i){
    if(grepl("C", res.counts$group[i])){
      category_names[3]
    } else NA
  })
  }
  #####
  counts <- counts %>%
    mutate(percent=paste(round(.data$count*100/sum(.data$count),digits = rnd),"%",sep="")) %>%
    mutate(label = paste(.data$count,"\n","(",.data$percent,")",sep=""))
  data.output <- merge(counts,center)
  restant<-nrow(data)-sum(data.output$count)
  inclus<-sum(data.output$count)
  disp.text<-paste0(
    "Les pourcentages sont calculés sur les personnes concernées uniquement.\n",
    "Dans les cercles sont représentés au total ", inclus, " personnes, soit ", round(inclus/(restant+inclus)*100, 1), "% des personnes interrogées\n( ",round(restant/(restant+inclus)*100, 1), "% des personnes ne sont pas concernées).")
  list(
    "max.x"=max(dat.polygon$x),
    "min.x"=min(dat.polygon$x),
    "min.y"=min(dat.polygon$y),
    "max.y"=max(dat.polygon$y)
  )->bbox
  addx<-bbox$min.x+( exclude.pos[1]*(bbox$max.x-bbox$min.x) )
  addy<-bbox$min.y+( exclude.pos[2]*(bbox$max.y-bbox$min.y) )
  ######
  
    counts <- counts %>%
      mutate(percent=paste(round(.data$count*100/sum(.data$count),digits = rnd),"%",sep="")) %>%
      mutate(label = paste(.data$count,"\n","(",.data$percent,")",sep=""))
    data.output <- merge(counts,center)
  
  #####
  if(sequential==TRUE){
    lipi<-lapply(1:length(vars), function(i){
      category.vi<-subset(category, category$var==vars[i])
      data.output.vi<-subset(data.output, grepl(pattern = LETTERS[i], x = as.character(data.output$group)))
      dat.polygon$FILLs<-grepl(LETTERS[i], dat.polygon$group)
      
      p <- ggplot() + aes_string("x","y") +
        geom_polygon(aes_string(group="group", fill="FILLs"), data = dat.polygon, ... )+
        geom_label(aes(label=label),data=category.vi,size=label.groups.size,
                   family="Ubuntu Condensed",fill=gray(level = 0.95), alpha= label.groups.alpha,
                   fontface="bold",color="black",
                   hjust="inward",vjust="inward") +
        theme_void(base_family="Ubuntu Condensed") + scale_fill_manual(values=c("TRUE"=gray(0.7), "FALSE"="white"))+
        coord_fixed() +
        theme(legend.position = "none")
      
      ######
     # if (!is.null(label.count)){
     #    if (label.which == "count"){
     #      p<-p + geom_label(aes(label=count),data=data.output.vi,label.size = NA, family="Ubuntu", alpha=label_alpha)
     #    }
     #    else if (label.which == "percent"){
     #      p<-p + geom_label(aes_string(label="percent"),data=data.output.vi,label.size = NA, family="Ubuntu", alpha=label_alpha)
     #    }
     #    else if (label.which == "both"){
     #      p<-p + geom_label(aes_string(label="label"),data=data.output.vi,label.size = NA,family="Ubuntu", alpha=label_alpha)
     #    }
     #    if(!is.null(add.exclude)){
     #      if(isTRUE(add.exclude)){
     #        p<-p+annotate(geom = "text", x = addx, y = addy, label=disp.text, 
     #                      family="Ubuntu Condensed", hjust=0)+
     #          coord_fixed(clip = 'off')+
     #          theme(plot.margin = MARGS)
     #      }
     #    }
     #  }
    })
    names(lipi)<-vars
  }

  p <- ggplot() + aes_string("x","y") +
    geom_polygon(aes_string(fill="count",group="group"),data = dat.polygon, ... )+
    geom_label(aes(label=label),data=category,size=label.groups.size,
               family="Ubuntu Condensed",fill=gray(level = 0.95), alpha= label.groups.alpha,
               fontface="bold",color="black",
               hjust="inward",vjust="inward") +
    theme_void(base_family="Ubuntu Condensed") + scale_fill_gradient(low="white",high = HIGH_COL) +
    coord_fixed() +
    theme(legend.position = "none")

    ######
  if (!is.null(label.count)){
    if (label.which == "count"){
      p<-p + geom_label(aes(label=count),data=data.output,size=label.groups.size/1, label.size = NA, family="Ubuntu", alpha=label_alpha)
    }
    else if (label.which == "percent"){
      p<-p + geom_label(aes_string(label="percent"),data=data.output,size=label.groups.size/1, label.size = NA, family="Ubuntu", alpha=label_alpha)
    }
    else if (label.which == "both"){
      p<-p + geom_label(aes_string(label="label"),data=data.output,size=label.groups.size/1, label.size = NA,family="Ubuntu", alpha=label_alpha)
    }
    if(!is.null(add.exclude)){
      if(isTRUE(add.exclude)){
      p<-p+annotate(geom = "text", x = addx, y = addy, label=disp.text, 
                    family="Ubuntu Condensed", hjust=0)+
        coord_fixed(clip = 'off')+
        theme(plot.margin = MARGS)
        }
    }
  }
  
  if(sequential==TRUE){
    lipi$Ensemble<-p
    p<-lipi 
  }
  res<-list("p"=p, "text.exclude"=disp.text, "data"=data.output, "dat.polygon"=dat.polygon, "category"=category)
  return(res)
}
  