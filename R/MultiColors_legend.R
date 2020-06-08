#' @export
MultiColors.legend<-function(p, prop.leg=0.4){
  library(grid)
  library(gridExtra)
  library(cowplot)
  
getLegend <- function(p) {
  g <- ggplotGrob(p)
  k <- which(g$layout$name=="guide-box")
  if(length(k)>0){
  g$grobs[[k]]->res
    return(res)
  }
}
justify <- function(x, hjust="center", vjust="center", draw=TRUE){
  w <- sum(x$widths)
  h <- sum(x$heights)
  xj <- switch(hjust,
               center = 0.5,
               left = 0.5*w,
               right=unit(1,"npc") - 0.5*w)
  yj <- switch(vjust,
               center = 0.5,
               bottom = 0.5*h,
               top=unit(1,"npc") - 0.5*h)
  x$vp <- viewport(x=xj, y=yj)
  if(draw) grid.draw(x)
  return(x)
}

ggplot_build(plot = p)->p.b
p.b$plot$data[order(as.numeric(p.b$plot$data[ , p$labels$x]), (0-as.numeric(p.b$plot$data[ , p$labels$fill]))) , ]->df
df[ , p$labels$fill]<-as.character(df[ , p$labels$fill])
df[ , p$labels$fill]<-factor(df[ , p$labels$fill], levels = as.character(df[ , p$labels$fill]), ordered = TRUE)

veccol<-unlist(unique(p.b$data[[1]]["fill"]))
names(veccol)<-levels(df[ , p$labels$fill])

lapply(levels(p.b$plot$data[ , p$labels$x]), function(gr1){
  pm<-subset(pl.mo$data, pl.mo$data[ , pl.mo$labels$x]==gr1) %>% 
    ggplot(aes_string(x=pl.mo$labels$x, fill=pl.mo$labels$fill))+
    geom_bar()+
    scale_fill_manual(values=veccol, name=paste(gr1, " : ", sep=""), guide=guide_legend(reverse = TRUE, ncol = 1))+
    coord_flip()+
    theme(legend.text = element_text(size = prop.leg*p$theme$text$size, 
                                     family = p$theme$text$family), 
          legend.title = element_text(size = prop.leg*p$theme$title$size, 
                                      family = p$theme$text$family))
  getLegend(pm)->res
  #res$grobs->res
  return(res)
})->resp
names(resp)<-levels(p.b$plot$data[ , p$labels$x])
resp[lengths(resp)>0]->resp2
tgt <- lapply(resp2, justify, vjust="top", draw=FALSE)
grid.arrange(grobs=tgt, ncol=length(tgt))->legendary
#cowplot::plot_grid(plotlist = resp[lengths(resp)>0], ncol=length(resp[lengths(resp)>0]), align = 'v')->legendary

g <- ggplotGrob(p)
k <- which(g$layout$name=="guide-box")
g$grobs[[k]] <- legendary
grid.draw(g)
}
