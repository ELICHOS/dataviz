clust.on.aura<-function(var="CLUSTER.PAM", plot.in.r=FALSE,
                        optional.filename="GrandLyon",
                        colors=colo, TEXTCOM=TRUE, QUELCOM="préfecture",
                        DEVICE="png", type.zone=NULL,
                        select.zone="07",
                        legend.LOG=FALSE, title.size=10){
  library(ggrepel)
  library(extrafont)
  #extrafont::loadfonts(device = "pdf")
  ####
  if(!is.null(type.zone)){
    shp.clust.df<-subset(shp.clust.df, shp.clust.df[, type.zone]==select.zone)
    centro<-subset(centro, as.numeric(centro$id)%in%shp.clust.df$INSEE_COM)
  }
  ####
  if(is.null(QUELCOM)){
    centro.def<-centro
  } else {
  subset(shp.clust.df, grepl(shp.clust.df$STATUT, pattern = QUELCOM)==TRUE&grepl(shp.clust.df$STATUT, pattern = "Sous")==FALSE)->quelcom.df
  subset(centro, as.numeric(centro$id)%in%quelcom.df$INSEE_COM)->centro.def
  }
  ####
  p2 <- ggplot() +
    geom_polygon(data = shp.clust.df,
                 aes_string(x = "long", y = "lat", group = "group", fill= var))

  if(is.null(colors)){
    p2<-p2+scale_fill_brewer(type = "qual", palette = "Set1", name="")
  } else {
    p2<-p2+scale_fill_manual(values = colors, name="", guide=guide_legend(ncol=1))
  }
  p3<-p2+
    coord_equal() +
    theme_void()

    if(TEXTCOM==TRUE){
      if(is.null(QUELCOM)){
      p4<-p3+
      geom_path(data = shp.clust.df,
                aes(x = long, y = lat, group = group), colour=gray( level = 0.5), size=0.1)+
        #geom_path(data = df.unary.aura,
        #          aes(x = long, y = lat, group = group), colour=gray( level = 0.3), size=2.5)+
      geom_text(data=centro.def, aes(x=x.centro, y=y.centro, label=NOM_COM2), size=1)
      } else {
        p4<-p3+
          #geom_path(data = df.unary.aura,
          #          aes(x = long, y = lat, group = group), colour=gray(level = 0.3), size=2.5)+
          geom_point(data=centro.def, aes(x=x.centro, y=y.centro),shape=21,
                     size=6, colour=gray(0))+
          geom_label_repel(data = centro.def, mapping = aes(x=x.centro, y=y.centro, label=NOM_COM), size=7, colour=alpha(colour = gray(0.2), alpha = 0.7), alpha=0.8, nudge_x = 8000, nudge_y = 8000, family="Calibri Light")
      }
    } else {
      p4<-p3#+geom_path(data = df.unary.aura,
            #    aes(x = long, y = lat, group = group), colour=gray(level = 0.3), size=2.5)
    }
  if(legend.LOG==FALSE){
    mytheme<-theme(
      legend.position = "none" ,
    title = element_text(size=title.size),
    text = element_text(family = "Calibri Light"))
  } else {
    mytheme<-theme(
      title = element_text(size=title.size),
      text = element_text(family = "Calibri Light"),
      legend.position = "right",
      legend.text =element_text(size = 8),
      legend.spacing.y = unit(1, 'cm'),
      legend.key.size = unit(1, "cm")
    )
  }
  p4<-p4+ggtitle(label = "Classification des communes de la r?gion AURA",
                 subtitle = "Donn?es INSEE, Recensement de la population 2015, Base IRCOM 2014 \nR?alisation: MRIE 2018")+
    mytheme
  namoplo<-gsub(pattern = "\\.", replacement = "_", x = var)
  if(!is.null(optional.filename)){
    namoplo<-paste(namoplo, optional.filename, sep="_")
  }
  if("pdf"%in%DEVICE){
  ggsave(plot = p4, filename = paste(namoplo, "pdf", sep = "."), device = cairo_pdf, width = 20, height = 20)
  }
  if("png"%in%DEVICE){
  ggsave(plot = p4, filename = paste(namoplo, "png", sep = "."), width = 20, height = 20, dpi=600)
  }
  if(plot.in.r==TRUE){
    return(p4)
  }
}
